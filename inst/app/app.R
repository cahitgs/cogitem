# Cogitem Shiny application: generate, browse and administer Raven-like
# 3x3 matrix items. Launched with cogitem::run_app().

library(shiny)
library(bslib)
library(grid)

shapes <- cogitem::cog_shapes()
rules <- cogitem::cog_rules()
shape_choices <- stats::setNames(shapes, cogitem::shape_labels()[shapes])
rule_choices_all <- stats::setNames(rules, cogitem::rule_labels()[rules])
outer_shape_choices <- shape_choices[shape_choices %in% cogitem::layer_shapes("outer")]
inner_shape_choices <- shape_choices[shape_choices %in% cogitem::layer_shapes("inner")]
ITEMS_PER_PAGE <- 1L
MAX_OPTIONS <- 8
PKG_ROOT <- local({
  # under pkgload the shimmed system.file() may return inst/; use the source root
  root <- system.file(package = "cogitem")
  if (basename(root) == "inst" && file.exists(file.path(dirname(root), "DESCRIPTION"))) dirname(root) else root
})

# Runs in a background R process (callr::r_bg). `root` is either an installed
# library entry (has Meta/) or the package source directory under pkgload.
generate_worker <- function(root, args, progress_file, stop_file) {
  if (dir.exists(file.path(root, "Meta"))) {
    loadNamespace("cogitem", lib.loc = c(dirname(root), .libPaths()))
  } else {
    pkgload::load_all(root, quiet = TRUE)
  }
  warnings <- character(0)
  progress <- function(done, n, attempts) {
    writeLines(sprintf("%d %d %d", done, n, attempts), progress_file)
    !file.exists(stop_file)
  }
  gen <- if (isTRUE(args$layered)) {
    function() cogitem::generate_layered(c(args$shape, args$rule),
                                         c(args$inner_shape, args$inner_rule),
                                         n = args$n, seed = args$seed, n_options = args$n_options,
                                         perceptual = args$perceptual, progress = progress)
  } else {
    function() cogitem::generate_items(args$shape, args$rule, n = args$n, seed = args$seed,
                                       n_options = args$n_options, perceptual = args$perceptual,
                                       progress = progress)
  }
  items <- withCallingHandlers(
    gen(),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(items = items, warnings = warnings)
}

read_progress <- function(file) {
  if (!file.exists(file)) return(NULL)
  v <- suppressWarnings(tryCatch(scan(file, what = integer(), quiet = TRUE),
                                 error = function(e) NULL))
  if (length(v) != 3 || anyNA(v)) return(NULL)
  list(done = v[1], n = v[2], attempts = v[3])
}

generate_from_args <- function(args, progress = NULL) {
  if (isTRUE(args$layered)) {
    cogitem::generate_layered(c(args$shape, args$rule), c(args$inner_shape, args$inner_rule),
                              n = args$n, seed = args$seed, n_options = args$n_options,
                              perceptual = args$perceptual, progress = progress)
  } else {
    cogitem::generate_items(args$shape, args$rule, n = args$n, seed = args$seed,
                            n_options = args$n_options, perceptual = args$perceptual,
                            progress = progress)
  }
}


app_theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#1f2430", primary = "#2f4f8f", secondary = "#d9dfe8",
  success = "#047857", danger = "#b91c1c",
  base_font = font_collection("Manrope", "system-ui", "sans-serif"),
  heading_font = font_collection("Fraunces", "Georgia", "serif"),
  font_scale = 0.95, "border-radius" = "0.5rem"
)

section <- function(title, class = NULL) h2(class = paste("cog-section", class), title)

ui <- page_navbar(
  title = tags$span(class = "cog-brand", tags$img(src = "cogitem-logo.png", alt = ""), "Cogitem"),
  window_title = "Cogitem",
  id = "nav",
  theme = app_theme,
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "cogitem-logo.png"),
    tags$link(rel = "stylesheet", href = "fonts/fonts.css"),
    tags$link(rel = "stylesheet", href = "cogitem.css"),
    tags$script(src = "cogitem.js")
  ),
  nav_panel(
    "Generate",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        section("Item model"),
        selectInput("shape", "Shape", shape_choices, selected = "annulus"),
        selectInput("rule", "Rule", rule_choices_all, selected = "dist-of-3"),
        input_switch("layered", "Two layers: inner figure with its own rule", FALSE),
        conditionalPanel(
          "input.layered",
          selectInput("inner_shape", "Inner shape", inner_shape_choices, selected = "dice"),
          selectInput("inner_rule", "Inner rule", rule_choices_all, selected = "dist-of-3")
        ),
        section("Item set", "mt-2"),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("n", "Items", 20, min = 1, max = 5000, step = 1),
          textInput("seed", "Seed", "", placeholder = "random")
        ),
        sliderInput("n_options", "Response options", min = 2, max = 8, value = 4, step = 1),
        input_switch("perceptual", "Perceptual audit (drops look-alike items, slower)", TRUE),
        conditionalPanel("output.generating != 'yes'",
                         actionButton("generate", "Generate items", class = "btn-primary w-100")),
        conditionalPanel("output.generating == 'yes'", uiOutput("generate_progress")),
        uiOutput("downloads")
      ),
      uiOutput("summary"),
      conditionalPanel(
        "output.n_pages > 0",
        div(class = "cog-viewer-head",
            span(class = "n", textOutput("page_label", inline = TRUE)),
            div(class = "cog-viewer-nav",
                actionButton("prev_page", "Previous", class = "btn-sm btn-outline-secondary"),
                actionButton("next_page", "Next", class = "btn-sm btn-outline-secondary")))
      ),
      uiOutput("gallery")
    )
  ),
  nav_panel(
    "Browse",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        section("Item"),
        uiOutput("browse_controls"),
        input_switch("show_answer", "Show answer", TRUE),
        radioButtons("layout", "Options layout", c("vertical", "horizontal"), inline = TRUE),
        section("Design features", "mt-2"),
        uiOutput("browse_features")
      ),
      div(class = "cog-paper cog-browse", plotOutput("browse_plot", height = "600px")),
      uiOutput("browse_audit")
    )
  ),
  nav_panel(
    "Administer",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        section("Session"),
        textInput("participant", "Participant id", "P001"),
        input_switch("feedback", "Feedback after each answer", TRUE),
        input_switch("shuffle_items", "Shuffle item order", FALSE),
        numericInput("max_items", "Items to administer (0 = all)", 0, min = 0, step = 1),
        actionButton("start", "Start", class = "btn-primary w-100"),
        section("Item set", "mt-3"),
        uiOutput("admin_status"),
        fileInput("upload", "Load a saved item set (.rds)", accept = ".rds")
      ),
      div(class = "cog-test", uiOutput("admin_ui"))
    )
  ),
  nav_panel(
    "About",
    div(class = "cog-paper cog-about",
        tags$img(src = "cogitem-logo.png", alt = "cogitem logo"),
        h3("Cogitem for R"),
        p("Rule-based automatic generation of Raven-like 3x3 matrix items. ",
          "Every item combines one figural carrier (15 shape families) with one ",
          "relational rule (identity, distribution of two or three, two-axis progression, ",
          "rotation, mirror, addition, subtraction, multiplication, AND, OR, XOR, XNOR), ",
          "or two carriers with two rules in a two-layer item. ",
          "Response options are guaranteed to be visually distinct, generation is seeded ",
          "and every item carries a design-feature record for LLTM-style analyses."),
        p("The perceptual audit renders every option and matrix cell and rejects items in ",
          "which two of them look alike, even when they are nominally different."),
        p("Item sets are saved as .rds files (readable with readRDS() in R) and can be ",
          "reloaded in the Administer tab."),
        p(class = "cog-muted mb-0", sprintf("cogitem version %s", utils::packageVersion("cogitem"))))
  ),
  nav_spacer(),
  nav_item(span(class = "cog-version", sprintf("v%s", utils::packageVersion("cogitem"))))
)

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  nice_shape <- function(s) {
    l <- cogitem::shape_labels()
    if (!is.null(s) && s %in% names(l)) unname(l[[s]]) else (s %||% "mixed")
  }
  nice_rule <- function(r) {
    l <- cogitem::rule_labels()
    if (!is.null(r) && r %in% names(l)) unname(l[[r]]) else (r %||% "mixed")
  }
  stat <- function(label, value, sub = NULL) {
    div(class = "cog-stat", div(class = "k", label), div(class = "v", value),
        if (!is.null(sub)) div(class = "s", sub))
  }
  items <- reactiveVal(getOption("cogitem.preloaded"))
  page <- reactiveVal(1L)
  # Two-layer mode restricts the (outer) shape to hollow carriers.
  observeEvent(input$layered, {
    choices <- if (isTRUE(input$layered)) outer_shape_choices else shape_choices
    sel <- if (isTRUE(input$shape %in% choices)) input$shape else choices[[1]]
    updateSelectInput(session, "shape", choices = choices, selected = sel)
  }, ignoreInit = TRUE)

  # Keep the rule lists consistent with the selected shapes.
  observeEvent(list(input$shape, input$layered), {
    req(input$shape)
    ok <- if (isTRUE(input$layered)) cogitem::layer_rules(input$shape) else cogitem::supported_rules(input$shape)
    sel <- if (isTRUE(input$rule %in% ok)) input$rule else ok[1]
    updateSelectInput(session, "rule", choices = rule_choices_all[rule_choices_all %in% ok],
                      selected = sel)
  })
  observeEvent(input$inner_shape, {
    ok <- cogitem::layer_rules(input$inner_shape)
    sel <- if (isTRUE(input$inner_rule %in% ok)) input$inner_rule else ok[1]
    updateSelectInput(session, "inner_rule", choices = rule_choices_all[rule_choices_all %in% ok],
                      selected = sel)
  })

  # Generation runs in a background R process so that the UI stays responsive,
  # shows progress and can be stopped early (the items produced so far are kept).
  gen <- reactiveValues(proc = NULL, files = NULL, status = NULL, stop_requested = NULL)
  accept_items <- function(res) {
    if (is.null(res) || length(res) == 0) {
      showNotification("No items were generated.", type = "warning")
      return(invisible())
    }
    items(res)
    page(1L)
    reset_admin()
  }
  generation_args <- function() {
    seed <- suppressWarnings(as.integer(input$seed))
    list(shape = input$shape, rule = input$rule, n = as.integer(input$n),
         seed = if (is.na(seed)) NULL else seed, n_options = as.integer(input$n_options),
         perceptual = isTRUE(input$perceptual), layered = isTRUE(input$layered),
         inner_shape = input$inner_shape, inner_rule = input$inner_rule)
  }

  observeEvent(input$generate, {
    if (!is.null(gen$proc)) return()
    args <- generation_args()
    if (!requireNamespace("callr", quietly = TRUE)) {
      # synchronous fallback (no Stop button)
      withProgress(message = "Generating items...", value = 0.3, {
        res <- tryCatch(
          withCallingHandlers(generate_from_args(args), warning = function(w) {
            showNotification(conditionMessage(w), type = "warning", duration = 8)
            invokeRestart("muffleWarning")
          }),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
        )
      })
      accept_items(res)
      return()
    }
    files <- list(progress = tempfile("cogitem-progress-"), stop = tempfile("cogitem-stop-"))
    gen$files <- files
    gen$status <- list(done = 0L, n = args$n, attempts = 0L)
    gen$stop_requested <- NULL
    gen$proc <- callr::r_bg(generate_worker,
                            args = list(root = PKG_ROOT, args = args,
                                        progress_file = files$progress, stop_file = files$stop),
                            supervise = TRUE)
  })

  observeEvent(input$cancel_generate, {
    req(gen$proc)
    file.create(gen$files$stop)
    gen$stop_requested <- Sys.time()
  })

  observe({
    p <- gen$proc
    if (is.null(p)) return()
    invalidateLater(400)
    st <- read_progress(isolate(gen$files$progress))
    if (!is.null(st)) gen$status <- st
    if (p$is_alive()) {
      asked <- isolate(gen$stop_requested)
      if (!is.null(asked) && as.numeric(difftime(Sys.time(), asked, units = "secs")) > 10) {
        p$kill()
        gen$proc <- NULL
        showNotification("Generation aborted.", type = "warning")
      }
      return()
    }
    gen$proc <- NULL
    res <- tryCatch(p$get_result(), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    unlink(unlist(isolate(gen$files)))
    if (is.null(res)) return()
    for (w in res$warnings) showNotification(w, type = "warning", duration = 8)
    accept_items(res$items)
  })

  session$onSessionEnded(function() {
    p <- isolate(gen$proc)
    if (!is.null(p) && p$is_alive()) p$kill()
  })

  output$generating <- renderText(if (is.null(gen$proc)) "no" else "yes")
  outputOptions(output, "generating", suspendWhenHidden = FALSE)
  output$generate_progress <- renderUI({
    st <- gen$status
    req(st)
    pct <- if (st$n > 0) min(100, round(100 * st$done / st$n)) else 0
    tagList(
      div(class = "cog-muted small mb-1",
          sprintf("Generating %d / %d (%d attempts)", st$done, st$n, st$attempts)),
      div(class = "progress mb-2", style = "height: 8px;",
          div(class = "progress-bar", role = "progressbar", style = sprintf("width: %d%%;", pct))),
      actionButton("cancel_generate",
                   if (is.null(gen$stop_requested)) "Stop and keep items so far" else "Stopping...",
                   class = "btn-outline-danger w-100")
    )
  })

  observeEvent(input$upload, {
    res <- tryCatch({
      x <- readRDS(input$upload$datapath)
      if (!inherits(x, "cog_item_set")) stop("The file does not contain a cogitem item set.")
      x
    }, error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(res)) {
      items(res)
      page(1L)
      reset_admin()
      showNotification(sprintf("Loaded %d items", length(res)), type = "message")
    }
  })

  # Summary cards ----------------------------------------------------------------
  output$summary <- renderUI({
    x <- items()
    if (is.null(x)) {
      return(div(class = "cog-empty",
                 tags$img(src = "cogitem-logo.png", alt = ""),
                 h3("No items yet"),
                 p("Choose a shape and a rule on the left and press Generate items. ",
                   "A saved item set (.rds) can be loaded in the Administer tab.")))
    }
    m <- attr(x, "meta")
    div(class = "cog-stats",
        stat("Items", length(x),
             if (isTRUE(m$stopped)) sprintf("stopped early, %d requested", m$n_requested)
             else sprintf("%s, %s", nice_shape(m$shape), nice_rule(m$rule))),
        stat("Seed", format(m$seed %||% NA), "reproduces this set"),
        stat("Duplicates skipped", m$duplicates_skipped %||% 0,
             sprintf("%s attempts", format(m$attempts %||% NA))),
        stat("Dropped by audit",
             if (isTRUE(m$perceptual)) m$perceptual_skipped %||% 0 else "off",
             if (isTRUE(m$perceptual)) "perceptual audit" else "audit was off"))
  })

  n_pages <- reactive({
    x <- items()
    if (is.null(x) || length(x) == 0) 0L else ceiling(length(x) / ITEMS_PER_PAGE)
  })
  observeEvent(input$prev_page, page(max(1L, page() - 1L)))
  observeEvent(input$next_page, page(min(n_pages(), page() + 1L)))
  output$page_label <- renderText(sprintf("Item %d of %d", page(), n_pages()))
  output$n_pages <- renderText(n_pages())
  outputOptions(output, "n_pages", suspendWhenHidden = FALSE)

  output$gallery <- renderUI({
    x <- items()
    if (is.null(x) || length(x) == 0) return(NULL)
    i <- min(page(), length(x))
    it <- x[[i]]
    div(class = "cog-viewer",
        plotOutput("viewer_plot", height = "640px"),
        div(class = "cap",
            tags$b(sprintf("Item %d", i)),
            span(sprintf("%s, %s", nice_shape(it$shape), nice_rule(it$rule))),
            span(class = "key", sprintf("key %d, %d options", it$correct, it$n_options))))
  })
  output$viewer_plot <- renderPlot({
    x <- items()
    req(!is.null(x), length(x) > 0)
    plot(x[[min(page(), length(x))]], show_answer = TRUE, bg = NA)
  }, bg = "transparent")

  # Downloads --------------------------------------------------------------------
  output$downloads <- renderUI({
    req(items())
    tagList(
      section("Export", "mt-2"),
      div(class = "cog-downloads",
          downloadButton("dl_items", "Item set (.rds)", class = "btn-outline-secondary"),
          downloadButton("dl_features", "Features CSV", class = "btn-outline-secondary"),
          downloadButton("dl_audit", "Audit CSV", class = "btn-outline-secondary"),
          downloadButton("dl_booklet", "PDF booklet", class = "btn-outline-secondary"))
    )
  })
  stem <- reactive({
    m <- attr(items(), "meta")
    sprintf("cogitem-%s-%s-%d-s%s", m$shape %||% "mixed", m$rule %||% "mixed",
            length(items()), format(m$seed %||% "na"))
  })
  output$dl_items <- downloadHandler(
    filename = function() paste0(stem(), ".rds"),
    content = function(file) saveRDS(items(), file)
  )
  output$dl_features <- downloadHandler(
    filename = function() paste0(stem(), "-features.csv"),
    content = function(file) utils::write.csv(cogitem::item_features(items()), file, row.names = FALSE)
  )
  output$dl_audit <- downloadHandler(
    filename = function() paste0(stem(), "-audit.csv"),
    content = function(file) {
      withProgress(message = "Auditing items...", {
        utils::write.csv(cogitem::audit_items(items()), file, row.names = FALSE)
      })
    }
  )
  output$dl_booklet <- downloadHandler(
    filename = function() paste0(stem(), "-booklet.pdf"),
    content = function(file) {
      withProgress(message = "Rendering PDF...", {
        cogitem::save_booklet(items(), file)
      })
    }
  )

  # Browse -----------------------------------------------------------------------
  output$browse_controls <- renderUI({
    x <- items()
    if (is.null(x) || length(x) == 0) return(div(class = "cog-hint", "Generate or load items first."))
    sliderInput("browse_idx", "Item", min = 1, max = length(x), value = 1, step = 1)
  })
  browse_item <- reactive({
    x <- items()
    req(!is.null(x), length(x) > 0, input$browse_idx)
    x[[min(input$browse_idx, length(x))]]
  })
  output$browse_plot <- renderPlot({
    plot(browse_item(), show_answer = isTRUE(input$show_answer),
         layout = input$layout, bg = NA)
  }, bg = "transparent")
  output$browse_features <- renderUI({
    f <- cogitem::item_features(browse_item())
    keep <- vapply(f, function(v) !is.na(v[1]) && !identical(v[1], ""), logical(1))
    vals <- vapply(f[keep], function(v) as.character(v[1]), "")
    rows <- lapply(seq_along(vals), function(i) list(tags$dt(names(vals)[i]), tags$dd(vals[[i]])))
    tags$dl(class = "cog-kv", !!!unlist(rows, recursive = FALSE))
  })
  output$browse_audit <- renderUI({
    a <- tryCatch(cogitem::audit_item(browse_item()), error = function(e) NULL)
    if (is.null(a)) return(NULL)
    div(class = "cog-paper cog-audit",
        span(class = paste("cog-dot", if (a$flagged) "bad" else "ok")),
        strong("Perceptual audit:"),
        span(if (a$flagged) "two renderings look alike" else "passed"),
        span(class = "cog-muted",
             paste0(sprintf("smallest option distance %.2f (options %s)", a$min_option_distance, a$option_pair),
                    if (!is.na(a$min_cell_distance))
                      sprintf(", smallest cell distance %.2f (%s)", a$min_cell_distance, a$cell_pair))))
  })

  # Administer -------------------------------------------------------------------
  admin <- reactiveValues(order = NULL, pos = 0L, state = "idle", picked = NULL, log = list())
  reset_admin <- function() {
    admin$order <- NULL; admin$pos <- 0L; admin$state <- "idle"; admin$picked <- NULL; admin$log <- list()
  }

  observeEvent(input$start, {
    x <- items()
    if (is.null(x) || length(x) == 0) {
      showNotification("Generate or load items first.", type = "warning"); return()
    }
    ord <- seq_along(x)
    if (isTRUE(input$shuffle_items)) ord <- sample(ord)
    if (!is.na(input$max_items) && input$max_items > 0) ord <- ord[seq_len(min(length(ord), input$max_items))]
    admin$order <- ord
    admin$pos <- 1L
    admin$state <- "playing"
    admin$picked <- NULL
    admin$log <- list()
    session$sendCustomMessage("cog-item-start", list(item = admin$pos))
  })

  current_item <- reactive({
    req(admin$state %in% c("playing", "feedback"), admin$pos >= 1)
    items()[[admin$order[admin$pos]]]
  })

  record <- function(chosen, rt, hovers) {
    it <- current_item()
    # hover trace "option@deltaMs;..." (1-based option, delta = time since the
    # previous hover)
    hv <- character(0)
    if (length(hovers)) {
      ts <- vapply(hovers, function(h) as.numeric(h$t), 0)
      hv <- sprintf("%d@%d", vapply(hovers, function(h) as.integer(h$i), 0L),
                    as.integer(c(0, diff(ts))))
    }
    admin$log[[length(admin$log) + 1]] <- data.frame(
      participant = input$participant,
      item_number = admin$pos,
      item_id = it$id,
      rule = it$rule,
      shape = it$shape,
      correct_option = it$correct,
      chosen_option = if (is.na(chosen)) NA_integer_ else as.integer(chosen),
      skipped = is.na(chosen),
      correct = !is.na(chosen) && chosen == it$correct,
      rt_ms = as.integer(rt),
      hover_count = length(hovers),
      hover_trace = paste(hv, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  advance <- function() {
    if (admin$pos >= length(admin$order)) {
      admin$state <- "done"
    } else {
      admin$pos <- admin$pos + 1L
      admin$picked <- NULL
      admin$state <- "playing"
      session$sendCustomMessage("cog-item-start", list(item = admin$pos))
    }
  }

  observeEvent(input$admin_pick, {
    p <- input$admin_pick
    # a late click that belongs to a previous item is ignored
    req(admin$state == "playing", identical(as.integer(p$item), admin$pos))
    record(as.integer(p$i), p$rt, p$hovers)
    admin$picked <- as.integer(p$i)
    if (isTRUE(input$feedback)) admin$state <- "feedback" else advance()
  })
  observeEvent(input$admin_skip, {
    p <- input$admin_skip
    req(admin$state == "playing", identical(as.integer(p$item), admin$pos))
    record(NA, p$rt, p$hovers)
    advance()
  })
  observeEvent(input$admin_next, advance())
  observeEvent(input$admin_restart, reset_admin())

  output$admin_status <- renderUI({
    x <- items()
    n <- if (is.null(x)) 0 else length(x)
    div(class = "cog-hint", sprintf("%d item(s) available.", n),
        if (admin$state != "idle") sprintf(" Progress: %d / %d.", min(admin$pos, length(admin$order)), length(admin$order)))
  })

  output$admin_ui <- renderUI({
    if (admin$state == "idle") {
      return(div(class = "cog-paper",
                 h3("Administration"),
                 p("Items are shown one at a time. Click the option that completes the matrix ",
                   "or press its number key. The chosen option, the response time and the hover ",
                   "trace of every option are logged and can be downloaded as CSV at the end."),
                 p(class = "cog-muted mb-0", "Press Start in the sidebar to begin.")))
    }
    if (admin$state == "done") {
      res <- do.call(rbind, admin$log)
      answered <- res[!res$skipped, , drop = FALSE]
      med_rt <- if (nrow(answered)) stats::median(answered$rt_ms) else NA_real_
      return(div(class = "cog-results",
        h3(sprintf("Finished: %d / %d correct", sum(res$correct), nrow(res))),
        div(class = "cog-stats",
            stat("Correct", sum(res$correct), sprintf("of %d items", nrow(res))),
            stat("Accuracy", sprintf("%d%%", round(100 * mean(res$correct))),
                 sprintf("%d skipped", sum(res$skipped))),
            stat("Median response time",
                 if (is.na(med_rt)) "-" else sprintf("%.1f s", med_rt / 1000), "answered items"),
            stat("Participant", input$participant, format(Sys.time(), "%Y-%m-%d %H:%M"))),
        div(class = "cog-paper",
            tableOutput("admin_results"),
            div(class = "cog-feedback",
                downloadButton("dl_results", "Download results CSV", class = "btn-primary"),
                actionButton("admin_restart", "Restart", class = "btn-outline-secondary")))))
    }
    it <- current_item()
    enabled <- admin$state == "playing"
    total <- length(admin$order)
    opts <- lapply(seq_len(it$n_options), function(i) {
      cls <- "cog-opt"
      if (!enabled) cls <- paste(cls, "disabled")
      if (!is.null(admin$picked) && i == admin$picked) {
        cls <- paste(cls, if (admin$picked == it$correct) "correct" else "wrong")
      }
      if (!enabled && i == it$correct && !identical(admin$picked, it$correct)) cls <- paste(cls, "correct")
      div(class = cls, role = "button", tabindex = "0", `aria-label` = sprintf("Option %d", i),
          onclick = sprintf("cogPick(%d, %s)", i, tolower(as.character(enabled))),
          onmouseenter = sprintf("cogHover(%d)", i),
          span(class = "key", i),
          plotOutput(paste0("admin_opt_", i), width = "100%", height = "100%"))
    })
    tagList(
      tags$script(HTML(sprintf("window.cogN = %d; window.cogEnabled = %s;",
                               it$n_options, tolower(as.character(enabled))))),
      div(class = "cog-test-head",
          span(class = "n", sprintf("Item %d of %d", admin$pos, total)),
          if (enabled) tags$button(class = "btn btn-sm btn-outline-secondary", onclick = "cogSkip()", "Skip")),
      div(class = "cog-progress",
          tags$span(style = sprintf("width: %.1f%%;", 100 * (admin$pos - 1) / max(1, total)))),
      layout_columns(
        col_widths = c(6, 6),
        div(class = "cog-paper", plotOutput("admin_matrix", height = "440px")),
        div(class = "cog-paper",
            p(class = "cog-question", "Which option completes the matrix? Click it or press its number."),
            div(class = "cog-opts", !!!opts),
            if (admin$state == "feedback")
              div(class = "cog-feedback",
                  actionButton("admin_next", if (admin$pos >= total) "Finish" else "Next", class = "btn-primary"),
                  span(class = "cog-muted",
                       if (identical(admin$picked, it$correct)) "Correct."
                       else sprintf("The correct option was %d.", it$correct)))))
    )
  })

  output$admin_matrix <- renderPlot({
    grid.newpage()
    grid.draw(cogitem::matrix_grob(current_item()))
  }, bg = "transparent")

  lapply(seq_len(MAX_OPTIONS), function(i) {
    output[[paste0("admin_opt_", i)]] <- renderPlot({
      it <- current_item()
      req(i <= it$n_options)
      grid.newpage()
      grid.draw(cogitem::shape_grob(it$options[[i]], vp = viewport(width = 0.85, height = 0.85)))
    }, bg = "transparent")
  })

  output$admin_results <- renderTable({
    res <- do.call(rbind, admin$log)
    data.frame(Item = res$item_number,
               Rule = vapply(res$rule, nice_rule, ""),
               Shape = vapply(res$shape, nice_shape, ""),
               Chosen = ifelse(is.na(res$chosen_option), "skipped", as.character(res$chosen_option)),
               Key = res$correct_option,
               Result = ifelse(res$correct, "correct", ifelse(res$skipped, "skipped", "wrong")),
               `RT (s)` = sprintf("%.1f", res$rt_ms / 1000),
               Hovers = res$hover_count,
               check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "rllcccrr")

  output$dl_results <- downloadHandler(
    filename = function() sprintf("cogitem-results-%s-%s.csv", input$participant,
                                  format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) utils::write.csv(do.call(rbind, admin$log), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
