# Item-level drawing: the 3x3 matrix panel, the response-option panel and
# their combination, plus file export helpers.

# Text whose size is a fraction of the viewport height (computed at draw time).
fit_text_grob <- function(label, frac = 0.5, col = "black", fontface = "bold",
                          x = 0.5, y = 0.5, name = NULL) {
  gTree(label = label, frac = frac, col = col, fontface = fontface, x = x, y = y,
        name = name, cl = "cog_fit_text")
}

#' @exportS3Method grid::makeContent
makeContent.cog_fit_text <- function(x) {
  h <- convertHeight(unit(1, "npc"), "points", valueOnly = TRUE)
  setChildren(x, gList(textGrob(x$label, x = x$x, y = x$y,
                                gp = gpar(fontsize = max(4, h * x$frac), col = x$col,
                                          fontface = x$fontface))))
}

# One matrix or option cell: rounded background + shape (85 % of the cell).
cell_grob <- function(s, missing = FALSE, border = NA, border_lwd = 1,
                      show_qmark = TRUE) {
  bg <- roundrectGrob(r = unit(0.08, "snpc"),
                      gp = gpar(fill = PALETTE$cell, col = border, lwd = border_lwd))
  if (missing) {
    return(gTree(children = gList(bg, if (show_qmark) fit_text_grob("?", 0.5, PALETTE$muted) else nullGrob())))
  }
  if (is_blank_cell(s)) {
    return(gTree(children = gList(
      roundrectGrob(r = unit(0.08, "snpc"),
                    gp = gpar(fill = NA, col = PALETTE$muted, lty = "dashed", lwd = 1.2))
    )))
  }
  gTree(children = gList(bg, shape_grob(s, vp = viewport(width = 0.85, height = 0.85))))
}

#' Grid grobs for the matrix and the response options of an item
#'
#' `matrix_grob()` draws the 3x3 question matrix with the missing cell shown
#' as "?", `options_grob()` the numbered response options, and
#' `item_grob()` both together. All three return `gTree`s that scale to the
#' viewport they are drawn in; [plot.cog_item()] is the convenience wrapper.
#'
#' @param item A `cog_item`.
#' @param show_answer Highlight the correct option?
#' @param labels Number the options?
#' @param highlight Optional option index to outline (for feedback).
#' @param highlight_kind `"correct"` or `"wrong"`; colour of the highlight.
#' @param layout `"vertical"` (matrix above options) or `"horizontal"`
#'   (matrix left, options right).
#' @param ncol Number of option columns (default: up to four).
#' @param name Grob name.
#' @return A `gTree`.
#' @examples
#' library(grid)
#' it <- generate_item("star", "progression", seed = 11)
#' grid.newpage()
#' grid.draw(matrix_grob(it))
#' @name item_grobs
NULL

#' @rdname item_grobs
#' @export
matrix_grob <- function(item, name = NULL) {
  pad <- 0.04
  gap <- 0.02
  cell <- (1 - 2 * pad - 2 * gap) / 3
  kids <- list(roundrectGrob(r = unit(0.03, "snpc"),
                             gp = gpar(fill = PALETTE$panel, col = PALETTE$border, lwd = 1)))
  for (r in 0:2) {
    for (c in 0:2) {
      cx <- pad + c * (cell + gap) + cell / 2
      cy <- 1 - (pad + r * (cell + gap) + cell / 2)
      missing <- r == 2 && c == 2
      kids[[length(kids) + 1]] <- gTree(
        children = gList(cell_grob(item$cells[[r + 1]][[c + 1]], missing = missing)),
        vp = viewport(x = cx, y = cy, width = cell, height = cell)
      )
    }
  }
  gTree(children = do.call(gList, kids), name = name,
        vp = viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
}

#' @rdname item_grobs
#' @export
options_grob <- function(item, show_answer = FALSE, labels = TRUE,
                         highlight = NULL, highlight_kind = c("correct", "wrong"),
                         ncol = NULL, name = NULL) {
  highlight_kind <- match.arg(highlight_kind)
  n <- item$n_options
  ncol <- ncol %||% min(n, 4)
  nrow <- ceiling(n / ncol)
  label_h <- if (labels) 0.22 else 0
  lay <- grid.layout(nrow, ncol,
                     widths = unit(rep(1, ncol), "null"),
                     heights = unit(rep(1 + label_h, nrow), "null"),
                     respect = TRUE)
  kids <- list()
  for (i in seq_len(n)) {
    r <- (i - 1) %/% ncol + 1
    c <- (i - 1) %% ncol + 1
    border <- NA
    border_lwd <- 1
    if (show_answer && i == item$correct) {
      border <- PALETTE$success; border_lwd <- 3
    }
    if (!is.null(highlight) && i == highlight) {
      border <- if (highlight_kind == "correct") PALETTE$success else PALETTE$danger
      border_lwd <- 3
    }
    body <- gTree(
      children = gList(cell_grob(item$options[[i]], border = border, border_lwd = border_lwd)),
      vp = viewport(x = 0.5, y = 1, just = c("centre", "top"),
                    width = unit(0.9, "snpc"), height = unit(0.9, "snpc"))
    )
    lab <- if (labels) {
      fit_text_grob(as.character(i), frac = 0.5, col = PALETTE$muted, fontface = "plain")
    } else nullGrob()
    lab_vp <- viewport(x = 0.5, y = 0, just = c("centre", "bottom"),
                       height = unit(label_h / (1 + label_h), "npc"))
    kids[[length(kids) + 1]] <- gTree(
      children = gList(body, gTree(children = gList(lab), vp = lab_vp)),
      vp = viewport(layout.pos.row = r, layout.pos.col = c)
    )
  }
  gTree(children = do.call(gList, kids), name = name,
        vp = viewport(layout = lay))
}

#' @rdname item_grobs
#' @export
item_grob <- function(item, show_answer = FALSE, labels = TRUE,
                      layout = c("vertical", "horizontal"), name = NULL) {
  stopifnot(inherits(item, "cog_item"))
  layout <- match.arg(layout)
  n <- item$n_options
  if (layout == "vertical") {
    ncol <- min(n, 4)
    nrow <- ceiling(n / ncol)
    opt_h <- nrow * (12 / 4) * (if (labels) 1.22 else 1) + 0.3
    lay <- grid.layout(3, 1, widths = unit(12, "null"),
                       heights = unit(c(12, 0.6, opt_h), "null"), respect = TRUE)
    mat_vp <- viewport(layout.pos.row = 1, layout.pos.col = 1)
    opt_vp <- viewport(layout.pos.row = 3, layout.pos.col = 1)
  } else {
    ncol <- 2
    nrow <- ceiling(n / ncol)
    opt_w <- 2 * 4
    lay <- grid.layout(1, 3, widths = unit(c(12, 0.8, opt_w), "null"),
                       heights = unit(max(12, nrow * 4 * (if (labels) 1.22 else 1)), "null"),
                       respect = TRUE)
    mat_vp <- viewport(layout.pos.row = 1, layout.pos.col = 1)
    opt_vp <- viewport(layout.pos.row = 1, layout.pos.col = 3)
  }
  gTree(
    children = gList(
      gTree(children = gList(matrix_grob(item)), vp = mat_vp),
      gTree(children = gList(options_grob(item, show_answer = show_answer,
                                          labels = labels, ncol = ncol)),
            vp = opt_vp)
    ),
    name = name,
    vp = viewport(layout = lay)
  )
}

#' Plot an item or a set of items
#'
#' `plot.cog_item()` draws one item (matrix plus options) on a new page;
#' `plot.cog_item_set()` draws a contact sheet of the first `n` items.
#'
#' @param x A `cog_item` or `cog_item_set`.
#' @param show_answer Outline the correct option?
#' @param labels Number the options?
#' @param layout `"vertical"` or `"horizontal"`.
#' @param bg Background colour; `NA` for transparent. Defaults to white.
#' @param newpage Call [grid::grid.newpage()] first?
#' @param n,ncol Number of items and columns of the contact sheet.
#' @param captions Print id, rule and difficulty above each item of the sheet?
#' @param ... Ignored.
#' @return The input, invisibly.
#' @examples
#' items <- generate_items("petals", "dist-of-3", n = 4, seed = 2)
#' plot(items[[1]], show_answer = TRUE)
#' plot(items, n = 4, ncol = 2)
#' @export
plot.cog_item <- function(x, show_answer = FALSE, labels = TRUE,
                          layout = c("vertical", "horizontal"), bg = NULL,
                          newpage = TRUE, ...) {
  bg <- if (is.null(bg)) PALETTE$bg else bg
  if (newpage) grid.newpage()
  if (!is.na(bg)) grid.rect(gp = gpar(fill = bg, col = NA))
  grid.draw(item_grob(x, show_answer = show_answer, labels = labels,
                      layout = layout))
  invisible(x)
}

#' @rdname plot.cog_item
#' @export
plot.cog_item_set <- function(x, n = 6, ncol = 3, show_answer = FALSE,
                              captions = TRUE, bg = NULL, newpage = TRUE, ...) {
  bg <- if (is.null(bg)) PALETTE$bg else bg
  n <- min(n, length(x))
  if (n == 0) stop("Empty item set", call. = FALSE)
  ncol <- min(ncol, n)
  nrow <- ceiling(n / ncol)
  if (newpage) grid.newpage()
  if (!is.na(bg)) grid.rect(gp = gpar(fill = bg, col = NA))
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
  for (i in seq_len(n)) {
    it <- x[[i]]
    pushViewport(viewport(layout.pos.row = (i - 1) %/% ncol + 1,
                          layout.pos.col = (i - 1) %% ncol + 1))
    if (captions) {
      pushViewport(viewport(y = 1, height = 0.08, just = "top"))
      grid.draw(fit_text_grob(sprintf("%d. %s / %s  (d=%s)", i, it$shape, it$rule,
                                      format(it$difficulty)),
                              frac = 0.45, col = PALETTE$muted, fontface = "plain"))
      popViewport()
      pushViewport(viewport(y = 0, height = 0.9, just = "bottom", width = 0.94))
    } else {
      pushViewport(viewport(width = 0.94, height = 0.94))
    }
    grid.draw(item_grob(it, show_answer = show_answer))
    popViewport(2)
  }
  popViewport()
  invisible(x)
}

open_device <- function(file, width, height, dpi) {
  ext <- tolower(tools::file_ext(file))
  cairo <- isTRUE(capabilities("cairo"))
  raster_args <- list(filename = file, width = width, height = height, units = "in", res = dpi)
  if (cairo && .Platform$OS.type == "windows") raster_args$type <- "cairo"
  switch(
    ext,
    png = do.call(grDevices::png, raster_args),
    jpg = , jpeg = do.call(grDevices::jpeg, raster_args),
    tif = , tiff = do.call(grDevices::tiff, raster_args),
    pdf = grDevices::pdf(file, width = width, height = height),
    svg = grDevices::svg(file, width = width, height = height),
    stop(sprintf("Unsupported file extension '%s' (use png, jpeg, tiff, pdf or svg)", ext),
         call. = FALSE)
  )
}

#' Save items to image or PDF files
#'
#' `save_item()` writes one item to a file whose format is taken from the
#' extension (png, jpeg, tiff, pdf, svg). `save_items()` writes every item
#' of a set to its own file. `save_booklet()` writes a multi-page PDF with
#' one item per page, optionally followed by an answer key, which is the
#' usual format for paper-and-pencil administration or supplementary
#' material.
#'
#' @param item A `cog_item`.
#' @param x A `cog_item_set`.
#' @param file Output file name.
#' @param dir Output directory (created if needed).
#' @param format File extension for `save_items()`.
#' @param prefix File-name prefix for `save_items()`.
#' @param width,height Device size in inches.
#' @param dpi Resolution for raster formats.
#' @param answer_key Append an answer-key page to the booklet?
#' @param ... Passed to [plot.cog_item()] (`show_answer`, `layout`,
#'   `labels`, `bg`).
#' @return The file path(s), invisibly.
#' @examples
#' it <- generate_item("bars", "subtraction", seed = 4)
#' f <- tempfile(fileext = ".png")
#' save_item(it, f, width = 4, height = 5)
#' file.exists(f)
#' @export
save_item <- function(item, file, width = 5, height = 6.5, dpi = 150, ...) {
  open_device(file, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(item, newpage = FALSE, ...)
  invisible(file)
}

#' @rdname save_item
#' @export
save_items <- function(x, dir, format = "png", prefix = "item", width = 5, height = 6.5,
                       dpi = 150, ...) {
  stopifnot(inherits(x, "cog_item_set"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  files <- character(length(x))
  for (i in seq_along(x)) {
    files[i] <- file.path(dir, sprintf("%s-%03d.%s", prefix, i, format))
    save_item(x[[i]], files[i], width = width, height = height, dpi = dpi, ...)
  }
  invisible(files)
}

#' @rdname save_item
#' @export
save_booklet <- function(x, file, width = 8.27, height = 11.69, answer_key = TRUE, ...) {
  stopifnot(inherits(x, "cog_item_set"), length(x) > 0)
  grDevices::pdf(file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (i in seq_along(x)) {
    grid.newpage()
    grid.rect(gp = gpar(fill = PALETTE$bg, col = NA))
    pushViewport(viewport(y = 1, height = 0.06, just = "top"))
    grid.draw(fit_text_grob(sprintf("Item %d / %d", i, length(x)), frac = 0.4,
                            col = PALETTE$muted, fontface = "plain"))
    popViewport()
    pushViewport(viewport(y = 0.02, height = 0.9, width = 0.9, just = "bottom"))
    plot(x[[i]], newpage = FALSE, bg = NA, ...)
    popViewport()
  }
  if (answer_key) {
    grid.newpage()
    grid.rect(gp = gpar(fill = PALETTE$bg, col = NA))
    key <- vapply(seq_along(x), function(i) sprintf("%3d: %d", i, x[[i]]$correct), "")
    ncol <- ceiling(length(key) / 40)
    cols <- split(key, ceiling(seq_along(key) / 40))
    pushViewport(viewport(width = 0.9, height = 0.9))
    grid.text("Answer key", x = 0, y = 1, just = c("left", "top"),
              gp = gpar(fontsize = 16, fontface = "bold", col = PALETTE$text))
    for (j in seq_along(cols)) {
      grid.text(paste(cols[[j]], collapse = "\n"), x = (j - 1) / ncol, y = 0.93,
                just = c("left", "top"), gp = gpar(fontsize = 10, family = "mono", col = PALETTE$text))
    }
    popViewport()
  }
  invisible(file)
}
