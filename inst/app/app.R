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
THUMBS_PER_PAGE <- 12
MAX_OPTIONS <- 8
BASE_FONT <- "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"
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

app_theme <- bs_theme(version = 5, bg = "#ffffff", fg = "#18181b", primary = "#6d28d9",
                      secondary = "#e4e4e7", success = "#047857", danger = "#b91c1c",
                      base_font = BASE_FONT)

css <- "
.cog-card { border: 1px solid #d4d4d8; border-radius: 12px; padding: 12px; background: #f4f4f5; }
.cog-thumb { border: 1px solid #d4d4d8; border-radius: 10px; padding: 6px; background: #f4f4f5; }
.cog-thumb .cap { font-size: 0.75rem; padding: 2px 6px; }
.cog-opt { display: inline-block; width: 136px; height: 136px; margin: 6px; border-radius: 10px;
           border: 2px solid #e4e4e7; background: #ffffff; cursor: pointer; }
.cog-opt:hover { border-color: #6d28d9; }
.cog-opt.disabled { cursor: default; }
.cog-opt.disabled:hover { border-color: #e4e4e7; }
.cog-opt.correct, .cog-opt.disabled.correct:hover { border-color: #047857; }
.cog-opt.wrong, .cog-opt.disabled.wrong:hover { border-color: #b91c1c; }
.cog-muted { opacity: 0.7; }
"

js <- "
window.cogStart = Date.now(); window.cogHovers = []; window.cogItem = 0; window.cogLocked = false;
Shiny.addCustomMessageHandler('cog-item-start', function(m) {
  window.cogStart = Date.now(); window.cogHovers = [];
  window.cogItem = m.item; window.cogLocked = false;
});
function cogHover(i) {
  if (!window.cogHovers) window.cogHovers = [];
  window.cogHovers.push({i: i, t: Date.now() - window.cogStart});
}
// One answer per item: the first click locks the options until the server
// starts the next item, and every event names the item it belongs to.
function cogPick(i, enabled) {
  if (!enabled || window.cogLocked) return;
  window.cogLocked = true;
  Shiny.setInputValue('admin_pick', {i: i, item: window.cogItem, rt: Date.now() - window.cogStart,
    hovers: window.cogHovers || []}, {priority: 'event'});
}
function cogSkip() {
  if (window.cogLocked) return;
  window.cogLocked = true;
  Shiny.setInputValue('admin_skip', {item: window.cogItem, rt: Date.now() - window.cogStart,
    hovers: window.cogHovers || []}, {priority: 'event'});
}
"

ui <- page_navbar(
  title = "Cogitem",
  id = "nav",
  theme = app_theme,
  header = tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  nav_panel(
    "Generate",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        selectInput("shape", "Shape", shape_choices, selected = "annulus"),
        selectInput("rule", "Rule", rule_choices_all, selected = "dist-of-3"),
        checkboxInput("layered", "Two layers: inner figure with its own rule", FALSE),
        conditionalPanel(
          "input.layered",
          selectInput("inner_shape", "Inner shape", inner_shape_choices, selected = "dice"),
          selectInput("inner_rule", "Inner rule", rule_choices_all, selected = "dist-of-3")
        ),
        numericInput("n", "Number of items", 20, min = 1, max = 5000, step = 1),
        textInput("seed", "Seed (blank = random)", ""),
        sliderInput("n_options", "Response options", min = 2, max = 8, value = 4, step = 1),
        checkboxInput("perceptual", "Perceptual audit (drop ambiguous items; slower)", TRUE),
        conditionalPanel("output.generating != 'yes'",
                         actionButton("generate", "Generate", class = "btn-primary w-100")),
        conditionalPanel("output.generating == 'yes'", uiOutput("generate_progress")),
        hr(),
        uiOutput("downloads")
      ),
      uiOutput("summary"),
      div(class = "d-flex justify-content-between align-items-center my-2",
          actionButton("prev_page", "Previous", class = "btn-sm"),
          textOutput("page_label", inline = TRUE),
          actionButton("next_page", "Next", class = "btn-sm")),
      uiOutput("gallery")
    )
  ),
  nav_panel(
    "Browse",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        uiOutput("browse_controls"),
        checkboxInput("show_answer", "Show answer", TRUE),
        radioButtons("layout", "Layout", c("vertical", "horizontal"), inline = TRUE),
        hr(),
        tableOutput("browse_features")
      ),
      plotOutput("browse_plot", height = "600px"),
      uiOutput("browse_audit")
    )
  ),
  nav_panel(
    "Administer",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        textInput("participant", "Participant id", "P001"),
        checkboxInput("feedback", "Show feedback after each answer", TRUE),
        checkboxInput("shuffle_items", "Shuffle item order", FALSE),
        numericInput("max_items", "Items to administer (0 = all)", 0, min = 0, step = 1),
        fileInput("upload", "Or load a saved item set (.rds)", accept = ".rds"),
        actionButton("start", "Start", class = "btn-primary w-100"),
        hr(),
        uiOutput("admin_status")
      ),
      uiOutput("admin_ui")
    )
  ),
  nav_panel(
    "About",
    div(class = "cog-card", style = "max-width: 820px;",
        h4("Cogitem for R"),
        p("Rule-based automatic generation of Raven-like 3x3 matrix items. ",
          "Every item combines one figural carrier (15 shape families) with one ",
          "relational rule (identity, distribution of two or three, two-axis progression, ",
          "rotation, mirror, addition, subtraction, multiplication, AND, OR, XOR, XNOR). ",
          "Response options are guaranteed to be visually distinct, generation is seeded ",
          "and every item carries a design-feature record for LLTM-style analyses."),
        p("The perceptual audit renders every option and matrix cell and rejects items in ",
          "which two of them look alike, even when they are nominally different."),
        p("Item sets are saved as .rds files (also readable with readRDS() in R) and can be ",
          "reloaded in the Administer tab."),
        p(class = "cog-muted", sprintf("cogitem version %s", utils::packageVersion("cogitem"))))
  )
)

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
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
      return(div(class = "cog-card cog-muted",
                 "Choose a shape and a rule, then press Generate. ",
                 "A saved item set (.rds) can also be loaded in the Administer tab."))
    }
    m <- attr(x, "meta")
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Items", length(x),
                if (isTRUE(m$stopped)) sprintf("stopped early (%d requested)", m$n_requested),
                theme = "primary"),
      value_box("Seed", format(m$seed %||% NA), theme = "secondary"),
      value_box("Duplicates skipped", m$duplicates_skipped %||% 0, theme = "secondary"),
      value_box("Ambiguous dropped",
                if (isTRUE(m$perceptual)) m$perceptual_skipped %||% 0 else "audit off",
                theme = "secondary")
    )
  })

  n_pages <- reactive({
    x <- items()
    if (is.null(x) || length(x) == 0) 1L else ceiling(length(x) / THUMBS_PER_PAGE)
  })
  observeEvent(input$prev_page, page(max(1L, page() - 1L)))
  observeEvent(input$next_page, page(min(n_pages(), page() + 1L)))
  output$page_label <- renderText(sprintf("Page %d / %d", page(), n_pages()))

  output$gallery <- renderUI({
    x <- items()
    if (is.null(x) || length(x) == 0) return(NULL)
    start <- (page() - 1L) * THUMBS_PER_PAGE
    idx <- seq(start + 1L, min(length(x), start + THUMBS_PER_PAGE))
    cards <- lapply(seq_along(idx), function(k) {
      it <- x[[idx[k]]]
      div(class = "cog-thumb",
          div(class = "cap cog-muted", sprintf("#%d  %s / %s  answer %d", idx[k], it$shape,
                                               it$rule, it$correct)),
          plotOutput(paste0("thumb_", k), height = "250px"))
    })
    layout_column_wrap(width = 1 / 3, !!!cards)
  })

  lapply(seq_len(THUMBS_PER_PAGE), function(k) {
    output[[paste0("thumb_", k)]] <- renderPlot({
      x <- items()
      i <- (page() - 1L) * THUMBS_PER_PAGE + k
      req(!is.null(x), i <= length(x))
      plot(x[[i]], show_answer = TRUE, bg = NA, labels = FALSE)
    }, bg = "transparent")
  })

  # Downloads --------------------------------------------------------------------
  output$downloads <- renderUI({
    req(items())
    tagList(
      downloadButton("dl_items", "Item set (.rds)", class = "btn-sm w-100 mb-1"),
      downloadButton("dl_features", "Features CSV", class = "btn-sm w-100 mb-1"),
      downloadButton("dl_audit", "Audit CSV", class = "btn-sm w-100 mb-1"),
      downloadButton("dl_booklet", "PDF booklet", class = "btn-sm w-100 mb-1")
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
    if (is.null(x) || length(x) == 0) return(div(class = "cog-muted", "Generate or load items first."))
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
  output$browse_features <- renderTable({
    f <- cogitem::item_features(browse_item())
    keep <- vapply(f, function(v) !is.na(v[1]) && !identical(v[1], ""), logical(1))
    data.frame(Feature = names(f)[keep], Value = vapply(f[keep], function(v) as.character(v[1]), ""))
  }, striped = TRUE, spacing = "xs")
  output$browse_audit <- renderUI({
    a <- tryCatch(cogitem::audit_item(browse_item()), error = function(e) NULL)
    if (is.null(a)) return(NULL)
    status <- if (a$flagged) span(class = "text-danger", "flagged: two renderings look alike")
              else span(class = "text-success", "passed")
    div(class = "cog-card mt-2",
        strong("Perceptual audit: "), status, br(),
        span(class = "cog-muted",
             sprintf("smallest option distance %.2f (options %s)", a$min_option_distance, a$option_pair),
             if (!is.na(a$min_cell_distance))
               sprintf(" | smallest cell distance %.2f (%s)", a$min_cell_distance, a$cell_pair)))
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
    div(class = "cog-muted", sprintf("%d item(s) available.", n),
        if (admin$state != "idle") sprintf(" Progress: %d / %d.", min(admin$pos, length(admin$order)), length(admin$order)))
  })

  output$admin_ui <- renderUI({
    if (admin$state == "idle") {
      return(div(class = "cog-card",
                 h5("Administration"),
                 p("Items are shown one at a time. Click the option that completes the matrix. ",
                   "The chosen option, response time and the hover trace of every option are logged ",
                   "and can be downloaded as CSV at the end."),
                 p(class = "cog-muted", "Press Start in the sidebar to begin.")))
    }
    if (admin$state == "done") {
      res <- do.call(rbind, admin$log)
      return(tagList(
        div(class = "cog-card",
            h5(sprintf("Finished: %d / %d correct", sum(res$correct), nrow(res))),
            tableOutput("admin_results"),
            downloadButton("dl_results", "Download results CSV", class = "btn-primary"),
            actionButton("admin_restart", "Restart", class = "ms-2"))
      ))
    }
    it <- current_item()
    enabled <- admin$state == "playing"
    opts <- lapply(seq_len(it$n_options), function(i) {
      cls <- "cog-opt"
      if (!enabled) cls <- paste(cls, "disabled")
      if (!is.null(admin$picked) && i == admin$picked) {
        cls <- paste(cls, if (admin$picked == it$correct) "correct" else "wrong")
      }
      if (!enabled && i == it$correct && !identical(admin$picked, it$correct)) cls <- paste(cls, "correct")
      div(class = cls,
          onclick = sprintf("cogPick(%d, %s)", i, tolower(as.character(enabled))),
          onmouseenter = sprintf("cogHover(%d)", i),
          plotOutput(paste0("admin_opt_", i), width = "132px", height = "132px"))
    })
    tagList(
      div(class = "d-flex justify-content-between align-items-center mb-2",
          span(class = "cog-muted", sprintf("Item %d / %d", admin$pos, length(admin$order))),
          if (enabled) tags$button(class = "btn btn-sm btn-outline-secondary", onclick = "cogSkip()", "Skip")),
      layout_columns(
        col_widths = c(6, 6),
        div(class = "cog-card", plotOutput("admin_matrix", height = "440px")),
        div(class = "cog-card",
            p(class = "cog-muted", "Which option completes the matrix?"),
            div(!!!opts),
            if (admin$state == "feedback")
              actionButton("admin_next", if (admin$pos >= length(admin$order)) "Finish" else "Next",
                           class = "btn-primary mt-2"))
      )
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
    res[, c("item_number", "rule", "shape", "chosen_option", "correct", "rt_ms",
            "hover_count")]
  }, striped = TRUE, spacing = "xs")

  output$dl_results <- downloadHandler(
    filename = function() sprintf("cogitem-results-%s-%s.csv", input$participant,
                                  format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) utils::write.csv(do.call(rbind, admin$log), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
