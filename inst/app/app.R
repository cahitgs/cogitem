# Cogitem Shiny application: generate, browse and administer Raven-like
# 3x3 matrix items. Launched with cogitem::run_app().

library(shiny)
library(bslib)
library(grid)

shapes <- cogitem::cog_shapes()
rules <- cogitem::cog_rules()
shape_choices <- stats::setNames(shapes, cogitem::shape_labels()[shapes])
rule_choices_all <- stats::setNames(rules, cogitem::rule_labels()[rules])
THUMBS_PER_PAGE <- 12
MAX_OPTIONS <- 8

app_theme <- bs_theme(
  version = 5,
  bg = "#0f0f12", fg = "#e4e4e7", primary = "#a78bfa", secondary = "#23252e",
  success = "#34d399", danger = "#f87171",
  base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"
)

css <- "
.cog-card { background: #1a1b22; border: 1px solid #2e303a; border-radius: 12px; padding: 12px; }
.cog-thumb { background: #1a1b22; border: 1px solid #2e303a; border-radius: 10px; padding: 6px; }
.cog-thumb .cap { font-size: 0.75rem; color: #9ca3af; padding: 2px 6px; }
.cog-opt { display: inline-block; width: 136px; height: 136px; margin: 6px; border-radius: 10px;
           background: #23252e; border: 2px solid transparent; cursor: pointer; }
.cog-opt:hover { border-color: #a78bfa; }
.cog-opt.correct { border-color: #34d399; }
.cog-opt.wrong { border-color: #f87171; }
.cog-opt.disabled { cursor: default; }
.cog-opt.disabled:hover { border-color: transparent; }
.cog-opt.disabled.correct:hover { border-color: #34d399; }
.cog-opt.disabled.wrong:hover { border-color: #f87171; }
.cog-muted { color: #9ca3af; }
.cog-light .cog-opt { background: #ffffff; }
.cog-light .cog-card, .cog-light .cog-thumb { background: #f4f4f5; border-color: #d4d4d8; }
"

js <- "
window.cogStart = Date.now(); window.cogHovers = [];
Shiny.addCustomMessageHandler('cog-item-start', function(m) {
  window.cogStart = Date.now(); window.cogHovers = [];
});
function cogHover(i) {
  if (!window.cogHovers) window.cogHovers = [];
  window.cogHovers.push({i: i, t: Date.now() - window.cogStart});
}
function cogPick(i, enabled) {
  if (!enabled) return;
  Shiny.setInputValue('admin_pick', {i: i, rt: Date.now() - window.cogStart,
    hovers: window.cogHovers || []}, {priority: 'event'});
}
function cogSkip() {
  Shiny.setInputValue('admin_skip', {rt: Date.now() - window.cogStart,
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
        numericInput("n", "Number of items", 20, min = 1, max = 5000, step = 1),
        textInput("seed", "Seed (blank = random)", ""),
        sliderInput("n_options", "Response options", min = 2, max = 8, value = 4, step = 1),
        actionButton("generate", "Generate", class = "btn-primary w-100"),
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
      plotOutput("browse_plot", height = "640px")
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
        fileInput("upload", "Or load an items JSON file", accept = ".json"),
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
        p("Items exported as JSON are interchangeable with the Cogitem web application: ",
          tags$a(href = "https://cahitgs.github.io/itemgen1/", "cahitgs.github.io/itemgen1")),
        p(class = "cog-muted", sprintf("cogitem version %s", utils::packageVersion("cogitem"))))
  ),
  nav_spacer(),
  nav_item(selectInput("theme", NULL, c("dark", "light"), selected = "dark", width = "110px"))
)

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  items <- reactiveVal(getOption("cogitem.preloaded"))
  page <- reactiveVal(1L)
  theme <- reactive(input$theme %||% "dark")

  # Keep the rule list consistent with the selected shape.
  observeEvent(input$shape, {
    ok <- cogitem::supported_rules(input$shape)
    sel <- if (isTRUE(input$rule %in% ok)) input$rule else ok[1]
    updateSelectInput(session, "rule", choices = rule_choices_all[rule_choices_all %in% ok],
                      selected = sel)
  })

  observeEvent(input$generate, {
    seed <- suppressWarnings(as.integer(input$seed))
    if (is.na(seed)) seed <- NULL
    withProgress(message = "Generating items...", value = 0.3, {
      res <- tryCatch(
        cogitem::generate_items(input$shape, input$rule, n = input$n, seed = seed,
                                n_options = input$n_options),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL },
        warning = function(w) {
          showNotification(conditionMessage(w), type = "warning")
          suppressWarnings(cogitem::generate_items(input$shape, input$rule, n = input$n,
                                                   seed = seed, n_options = input$n_options))
        }
      )
    })
    if (!is.null(res)) {
      items(res)
      page(1L)
      reset_admin()
    }
  })

  observeEvent(input$upload, {
    res <- tryCatch(cogitem::read_items_json(input$upload$datapath),
                    error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
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
                 "Items can also be loaded from a JSON file in the Administer tab."))
    }
    m <- attr(x, "meta")
    f <- cogitem::item_features(x)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Items", length(x), theme = "primary"),
      value_box("Seed", format(m$seed %||% NA), theme = "secondary"),
      value_box("Duplicates skipped", m$duplicates_skipped %||% 0, theme = "secondary"),
      value_box("Mean difficulty", sprintf("%.2f", mean(f$difficulty)), theme = "secondary")
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
      div(class = if (theme() == "light") "cog-thumb cog-light" else "cog-thumb",
          div(class = "cap", sprintf("#%d  %s / %s  d%d  answer %d", idx[k], it$shape,
                                     it$rule, it$difficulty, it$correct)),
          plotOutput(paste0("thumb_", k), height = "250px"))
    })
    layout_column_wrap(width = 1 / 3, !!!cards)
  })

  lapply(seq_len(THUMBS_PER_PAGE), function(k) {
    output[[paste0("thumb_", k)]] <- renderPlot({
      x <- items()
      i <- (page() - 1L) * THUMBS_PER_PAGE + k
      req(!is.null(x), i <= length(x))
      plot(x[[i]], theme = theme(), show_answer = TRUE, bg = NA, labels = FALSE)
    }, bg = "transparent")
  })

  # Downloads --------------------------------------------------------------------
  output$downloads <- renderUI({
    req(items())
    tagList(
      downloadButton("dl_json", "Items JSON", class = "btn-sm w-100 mb-1"),
      downloadButton("dl_features", "Features CSV", class = "btn-sm w-100 mb-1"),
      downloadButton("dl_booklet", "PDF booklet", class = "btn-sm w-100 mb-1")
    )
  })
  stem <- reactive({
    m <- attr(items(), "meta")
    sprintf("cogitem-%s-%s-%d-s%s", m$shape %||% "mixed", m$rule %||% "mixed",
            length(items()), format(m$seed %||% "na"))
  })
  output$dl_json <- downloadHandler(
    filename = function() paste0(stem(), ".json"),
    content = function(file) cogitem::write_items_json(items(), file)
  )
  output$dl_features <- downloadHandler(
    filename = function() paste0(stem(), "-features.csv"),
    content = function(file) utils::write.csv(cogitem::item_features(items()), file, row.names = FALSE)
  )
  output$dl_booklet <- downloadHandler(
    filename = function() paste0(stem(), "-booklet.pdf"),
    content = function(file) {
      withProgress(message = "Rendering PDF...", {
        cogitem::save_booklet(items(), file, theme = "light")
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
    plot(browse_item(), theme = theme(), show_answer = isTRUE(input$show_answer),
         layout = input$layout, bg = NA)
  }, bg = "transparent")
  output$browse_features <- renderTable({
    f <- cogitem::item_features(browse_item())
    keep <- vapply(f, function(v) !is.na(v[1]) && !identical(v[1], ""), logical(1))
    data.frame(Feature = names(f)[keep], Value = vapply(f[keep], function(v) as.character(v[1]), ""))
  }, striped = TRUE, spacing = "xs")

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
    session$sendCustomMessage("cog-item-start", list())
  })

  current_item <- reactive({
    req(admin$state %in% c("playing", "feedback"), admin$pos >= 1)
    items()[[admin$order[admin$pos]]]
  })

  record <- function(chosen, rt, hovers) {
    it <- current_item()
    # hover trace in the web-app format: "optionIndex@deltaMs;..." (0-based index,
    # delta = time since the previous hover)
    hv <- character(0)
    if (length(hovers)) {
      ts <- vapply(hovers, function(h) as.numeric(h$t), 0)
      hv <- sprintf("%d@%d", vapply(hovers, function(h) as.integer(h$i) - 1L, 0L),
                    as.integer(c(0, diff(ts))))
    }
    admin$log[[length(admin$log) + 1]] <- data.frame(
      participant = input$participant,
      questionNumber = admin$pos,
      itemId = it$id,
      rule = it$rule,
      shape = it$shape,
      difficulty = it$difficulty,
      correctIndex = it$correct - 1L,
      chosenIndex = if (is.na(chosen)) -1L else chosen - 1L,
      skipped = is.na(chosen),
      correct = !is.na(chosen) && chosen == it$correct,
      durationMs = as.integer(rt),
      hoverCount = length(hovers),
      hoverTrace = paste(hv, collapse = ";"),
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
      session$sendCustomMessage("cog-item-start", list())
    }
  }

  observeEvent(input$admin_pick, {
    req(admin$state == "playing")
    p <- input$admin_pick
    record(as.integer(p$i), p$rt, p$hovers)
    admin$picked <- as.integer(p$i)
    if (isTRUE(input$feedback)) admin$state <- "feedback" else advance()
  })
  observeEvent(input$admin_skip, {
    req(admin$state == "playing")
    p <- input$admin_skip
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
        div(class = if (theme() == "light") "cog-card cog-light" else "cog-card",
            plotOutput("admin_matrix", height = "440px")),
        div(class = if (theme() == "light") "cog-card cog-light" else "cog-card",
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
    grid.draw(cogitem::matrix_grob(current_item(), theme = theme()))
  }, bg = "transparent")

  lapply(seq_len(MAX_OPTIONS), function(i) {
    output[[paste0("admin_opt_", i)]] <- renderPlot({
      it <- current_item()
      req(i <= it$n_options)
      grid.newpage()
      grid.draw(cogitem::shape_grob(it$options[[i]], theme = theme(),
                                    vp = viewport(width = 0.85, height = 0.85)))
    }, bg = "transparent")
  })

  output$admin_results <- renderTable({
    res <- do.call(rbind, admin$log)
    res[, c("questionNumber", "rule", "shape", "difficulty", "chosenIndex", "correct",
            "durationMs", "hoverCount")]
  }, striped = TRUE, spacing = "xs")

  output$dl_results <- downloadHandler(
    filename = function() sprintf("cogitem-results-%s-%s.csv", input$participant,
                                  format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) utils::write.csv(do.call(rbind, admin$log), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
