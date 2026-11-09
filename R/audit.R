# Image-similarity screen (experimental): render shapes to small rasters and
# measure how different the renderings are, independent of the symbolic
# visual_signature(). Catches cases in which two configurations are nominally
# different (e.g. size 0.80 vs 0.82) but render nearly identically. It is a
# fixed-resolution proxy, not a test of human perception.

DEFAULT_PERCEPTUAL_THRESHOLD <- 0.15

# Render several shapes side by side on one capture device and return a list
# of px x px x 3 sRGB arrays (one per shape). One device per call keeps the
# audit fast.
render_tiles <- function(shapes, px) {
  k <- length(shapes)
  ncol <- min(k, 6)
  nrow <- ceiling(k / ncol)
  draw_all <- function() {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow, ncol)))
    for (i in seq_len(k)) {
      pushViewport(viewport(layout.pos.row = (i - 1) %/% ncol + 1,
                            layout.pos.col = (i - 1) %% ncol + 1))
      grid.draw(shape_grob(shapes[[i]], vp = viewport(width = 0.85, height = 0.85)))
      popViewport()
    }
    popViewport()
  }
  engine <- getOption("cogitem.raster", "auto")
  if (engine == "auto") {
    engine <- if (requireNamespace("ragg", quietly = TRUE)) {
      "ragg"
    } else if (requireNamespace("png", quietly = TRUE)) {
      "png"
    } else {
      stop("The perceptual audit needs the 'ragg' package (or 'png' as a fallback): ",
           "install.packages('ragg')", call. = FALSE)
    }
  }
  if (engine == "ragg") {
    cap <- ragg::agg_capture(width = px * ncol, height = px * nrow, units = "px",
                             background = PALETTE$cell, res = 72)
    on.exit(grDevices::dev.off(), add = TRUE)
    draw_all()
    img <- cap(native = FALSE)
    rgb <- grDevices::col2rgb(img) / 255
    full <- array(t(rgb), dim = c(nrow(img), ncol(img), 3))
  } else if (engine == "png") {
    # dev.capture() is not available for file devices: draw to a PNG file and
    # read it back
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    args <- list(f, width = px * ncol, height = px * nrow, bg = PALETTE$cell, res = 72)
    if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
    do.call(grDevices::png, args)
    open <- TRUE
    on.exit(if (open) grDevices::dev.off(), add = TRUE)
    draw_all()
    grDevices::dev.off()
    open <- FALSE
    if (!file.exists(f)) {
      stop("The png device did not write a file (a headless system without cairo?); ",
           "install the 'ragg' package or set options(cogitem.raster = 'ragg')", call. = FALSE)
    }
    full <- to_rgb_array(png::readPNG(f))
  } else {
    stop(sprintf("Unknown raster engine '%s' (option cogitem.raster)", engine), call. = FALSE)
  }
  lapply(seq_len(k), function(i) {
    r <- (i - 1) %/% ncol
    c <- (i - 1) %% ncol
    full[(r * px + 1):((r + 1) * px), (c * px + 1):((c + 1) * px), , drop = FALSE]
  })
}

# Grey, grey+alpha, RGB or RGBA array from png::readPNG() -> h x w x 3.
to_rgb_array <- function(arr) {
  d <- dim(arr)
  if (length(d) == 2) return(array(arr, dim = c(d, 3)))
  if (d[3] == 2) return(array(arr[, , 1], dim = c(d[1:2], 3)))
  arr[, , 1:3, drop = FALSE]
}

# Vectorised sRGB (0-1, N x 3) -> CIELAB (D65); L in 0-100, a/b about -128..128.
srgb_to_lab <- function(m) {
  lin <- ifelse(m <= 0.04045, m / 12.92, ((m + 0.055) / 1.055)^2.4)
  M <- matrix(c(0.4124564, 0.3575761, 0.1804375,
                0.2126729, 0.7151522, 0.0721750,
                0.0193339, 0.1191920, 0.9503041), 3, 3, byrow = TRUE)
  xyz <- lin %*% t(M)
  t <- sweep(xyz, 2, c(0.95047, 1.0, 1.08883), "/")
  f <- ifelse(t > (6 / 29)^3, t^(1 / 3), t / (3 * (6 / 29)^2) + 4 / 29)
  cbind(116 * f[, 2] - 16, 500 * (f[, 1] - f[, 2]), 200 * (f[, 2] - f[, 3]))
}

# sRGB array -> CIELAB array (same dims).
to_lab <- function(arr) {
  d <- dim(arr)
  array(srgb_to_lab(matrix(arr, ncol = 3)), dim = d)
}

# Separable box blur with edge replication; r = radius in pixels.
blur_cols <- function(mat, r) {
  n <- nrow(mat)
  k <- rep(1 / (2 * r + 1), 2 * r + 1)
  padded <- rbind(matrix(rep(mat[1, ], each = r), nrow = r), mat,
                  matrix(rep(mat[n, ], each = r), nrow = r))
  out <- stats::filter(padded, k, sides = 2)
  matrix(as.numeric(out), nrow = nrow(padded))[(r + 1):(r + n), , drop = FALSE]
}

box_blur <- function(mat, r) {
  if (r <= 0) return(mat)
  t(blur_cols(t(blur_cols(mat, r)), r))
}

lab_distance_map <- function(A, B) {
  sqrt((A[, , 1] - B[, , 1])^2 + (A[, , 2] - B[, , 2])^2 + (A[, , 3] - B[, , 3])^2) / 100
}

# Blurred CIELAB tiles plus their total ink (distance from the background).
prepare_tiles <- function(shapes, px, blur) {
  tiles <- render_tiles(shapes, px)
  r <- max(1, round(blur))
  bg_lab <- as.numeric(srgb_to_lab(t(grDevices::col2rgb(PALETTE$cell) / 255)))
  BG <- array(rep(bg_lab, each = px * px), dim = c(px, px, 3))
  lapply(tiles, function(A) {
    L <- to_lab(A)
    for (k in 1:3) L[, , k] <- box_blur(L[, , k], r)
    list(L = L, ink = sum(lab_distance_map(L, BG)))
  })
}

tile_distance <- function(a, b) {
  diff <- lab_distance_map(a$L, b$L)
  denom <- a$ink + b$ink
  if (denom <= 1e-9) return(0)
  min(1, max(sum(diff) / denom, max(diff)))
}

# Prepared tiles of a list of shapes: one tile set for single-layer shapes,
# one tile set per layer for two-layer stacks, each layer rendered on its own
# at full tile size. A blurred tile of the whole cell cannot resolve the inner
# carrier, which fills only 36 % of the cell, whereas a person looking at it
# can; so the layers are compared one by one and the distance between two
# stacks is the larger of the two layer distances.
prepare_layers <- function(shapes, px, blur) {
  if (length(shapes) && all(vapply(shapes, is_stack, logical(1)))) {
    return(lapply(1:2, function(k) {
      prepare_tiles(lapply(shapes, function(s) s$layers[[k]]), px, blur)
    }))
  }
  list(prepare_tiles(shapes, px, blur))
}

layer_distance <- function(layers, i, j) {
  max(vapply(layers, function(tiles) tile_distance(tiles[[i]], tiles[[j]]), numeric(1)))
}

#' Image-similarity distance between two shape configurations (experimental)
#'
#' Renders both configurations to small rasters, converts them to CIELAB,
#' blurs them slightly (so that a sub-pixel shift does not count as a
#' difference) and returns a distance in `[0, 1]`: 0 for pixel-identical
#' images, about 1 for images whose ink does not overlap at all. The value is
#' the larger of (a) the total colour difference relative to the total amount
#' of ink in both images and (b) the strongest local difference, so that a
#' small but salient change (a marker jumping to another corner) and a
#' global change (another colour) both score high, while a 2-4 % change of
#' size scores low.
#'
#' Calibration on known pairs (96 px, 3 px blur): identical 0;
#' 1 % size change about 0.10; 2.5 % size change 0.16; heptagon vs octagon
#' 0.18; pentagon vs hexagon 0.29; gray vs green stroke 0.33; blue vs purple
#' stroke 0.45; one size bucket of the generator (0.65 vs 0.80) 0.63; marker
#' in another corner 0.84. Two renderings are flagged as near-identical when
#' the distance falls below 0.15 (the default `threshold` of
#' [audit_items()]), a constant chosen by inspecting rendered examples; the
#' metric has not been validated against human observers and is provided as
#' an experimental development tool. Small count differences (15 vs 16
#' spikes, 7 vs 8 sides) are handled by the symbolic design constraints of
#' [is_item_valid()] rather than by this metric.
#'
#' Options and cells of two-layer items (see [generate_layered()]) are
#' compared layer by layer, each layer rendered on its own at full tile
#' size, and the distance is the larger of the two layer distances, because
#' a blurred tile of the whole cell cannot resolve the inner carrier, which
#' fills only 36 % of the cell.
#'
#' Requires the **ragg** package or, as a fallback, the **png** package
#' (the engine can be forced with `options(cogitem.raster = "png")`).
#'
#' @param a,b [cog_shape()] objects.
#' @param px Raster size in pixels (the rendered cell is `px` x `px`).
#' @param blur Blur radius in pixels.
#' @return A number between 0 and 1.
#' @examples
#' \donttest{
#' a <- cog_shape("dice", list(dotCount = 3), size = 0.65)
#' b <- cog_shape("dice", list(dotCount = 3), size = 0.95)
#' perceptual_distance(a, b)
#' perceptual_distance(a, a)
#' }
#' @export
perceptual_distance <- function(a, b, px = 96, blur = 3) {
  stopifnot(inherits(a, "cog_shape"), inherits(b, "cog_shape"))
  if (is_stack(a) != is_stack(b)) return(1)
  layer_distance(prepare_layers(list(a, b), px, blur), 1, 2)
}

# Pairs of cells that must look different: nominally different, non-blank,
# non-missing cells sharing a row or a column.
cell_pairs <- function(item) {
  cells <- item$cells
  sig <- function(s) visual_signature(s)
  at <- function(r, c) if (r == 3 && c == 3) NULL else cells[[r]][[c]]
  out <- list()
  add <- function(a, b, label) {
    if (!is.null(a) && !is.null(b) && !is_blank_cell(a) && !is_blank_cell(b) && sig(a) != sig(b)) {
      out[[length(out) + 1]] <<- list(a = a, b = b, label = label)
    }
  }
  for (r in 1:3) for (i in 1:2) for (j in (i + 1):3) {
    add(at(r, i), at(r, j), sprintf("r%dc%d-r%dc%d", r, i, r, j))
  }
  # columns carry the rule too, except for the row-wise arithmetic/logic rules
  if (!(item$rule %in% ROW_ONLY_RULES) && !isTRUE(item$meta$row_only)) {
    for (c in 1:3) for (i in 1:2) for (j in (i + 1):3) {
      add(at(i, c), at(j, c), sprintf("r%dc%d-r%dc%d", i, c, j, c))
    }
  }
  out
}

#' Image-similarity screen for items (experimental)
#'
#' Checks that every pair of response options, and every pair of
#' nominally different cells that share a row or a column of the matrix,
#' renders differently according to [perceptual_distance()]. The symbolic
#' checks of [is_item_valid()] cannot see, for example, that two shapes of
#' size 0.80 and 0.82 render almost identically; this screen can. It is an
#' experimental development tool: it flags renderings that are nearly
#' identical at a fixed resolution and blur, and it does not establish that
#' observers tell the remaining pairs apart. `audit_item()` screens one
#' item, `audit_items()` a whole set and
#' returns one row per item with the smallest option distance, the smallest
#' cell distance, the pairs where they occur and a `flagged` column. Items
#' with `flagged = TRUE` should be dropped (`x[!audit_items(x)$flagged]`) or
#' regenerated with `generate_items(perceptual = TRUE)`, which applies this
#' audit during generation.
#'
#' @param item A `cog_item`.
#' @param x A `cog_item_set`.
#' @param threshold Distances below this value count as near-identical.
#' @param px,blur Passed to [perceptual_distance()].
#' @return `audit_item()` returns a list with `min_option_distance`,
#'   `option_pair`, `min_cell_distance`, `cell_pair`, `flagged` and the full
#'   `option_distances` matrix; `audit_items()` a data frame.
#' @examples
#' \donttest{
#' items <- generate_items("dice", "dist-of-3", n = 5, seed = 3)
#' audit_items(items)
#' }
#' @export
audit_item <- function(item, threshold = DEFAULT_PERCEPTUAL_THRESHOLD, px = 96, blur = 3) {
  stopifnot(inherits(item, "cog_item"))

  # distinct shapes to render: options first, then differing cells
  cp <- cell_pairs(item)
  shapes <- c(item$options, unlist(lapply(cp, function(p) list(p$a, p$b)), recursive = FALSE))
  sigs <- vapply(shapes, visual_signature, character(1))
  keep <- !duplicated(sigs)
  layers <- prepare_layers(shapes[keep], px, blur)
  index <- match(sigs, sigs[keep])
  dist_ij <- function(i, j) layer_distance(layers, index[i], index[j])

  n <- length(item$options)
  D <- matrix(NA_real_, n, n)
  for (i in seq_len(n - 1)) for (j in (i + 1):n) D[i, j] <- D[j, i] <- dist_ij(i, j)
  min_opt <- min(D, na.rm = TRUE)
  w <- which(D == min_opt, arr.ind = TRUE)[1, ]
  opt_pair <- sprintf("%d-%d", min(w), max(w))

  min_cell <- NA_real_
  cell_pair <- NA_character_
  for (k in seq_along(cp)) {
    d <- dist_ij(n + 2 * k - 1, n + 2 * k)
    if (is.na(min_cell) || d < min_cell) {
      min_cell <- d
      cell_pair <- cp[[k]]$label
    }
  }
  flagged <- min_opt < threshold || (!is.na(min_cell) && min_cell < threshold)
  list(min_option_distance = min_opt, option_pair = opt_pair,
       min_cell_distance = min_cell, cell_pair = cell_pair,
       flagged = flagged, option_distances = D)
}

#' @rdname audit_item
#' @export
audit_items <- function(x, threshold = DEFAULT_PERCEPTUAL_THRESHOLD, px = 96, blur = 3) {
  items <- as_item_list(x)
  rows <- lapply(items, function(it) {
    a <- audit_item(it, threshold = threshold, px = px, blur = blur)
    data.frame(id = it$id, shape = it$shape, rule = it$rule,
               min_option_distance = round(a$min_option_distance, 3),
               option_pair = a$option_pair,
               min_cell_distance = round(a$min_cell_distance, 3),
               cell_pair = a$cell_pair,
               flagged = a$flagged, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
