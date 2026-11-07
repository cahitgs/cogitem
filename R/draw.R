# Rendering of shape configurations with grid. Every shape is drawn in a
# square viewport whose native coordinates run 0-100 left to right and
# 0-100 TOP to bottom (yscale is reversed), so the geometry matches the SVG
# renderers of the Cogitem web application unit for unit.

THEMES <- list(
  dark = list(
    name = "dark",
    bg = "#0f0f12", panel = "#1a1b22", cell = "#23252e", border = "#2e303a",
    text = "#e4e4e7", muted = "#9ca3af", accent = "#a78bfa",
    success = "#34d399", danger = "#f87171",
    map = c()
  ),
  light = list(
    name = "light",
    bg = "#ffffff", panel = "#f4f4f5", cell = "#ffffff", border = "#d4d4d8",
    text = "#18181b", muted = "#71717a", accent = "#6d28d9",
    success = "#047857", danger = "#b91c1c",
    # dark-on-light equivalents of the generator palette, chosen so that every
    # pair stays distinguishable (blue is pushed towards cyan, purple towards
    # violet)
    map = c("#e4e4e7" = "#27272a", "#a78bfa" = "#7c3aed", "#34d399" = "#047857",
            "#f59e0b" = "#b45309", "#60a5fa" = "#0369a1")
  )
)

get_theme <- function(theme) {
  if (is.list(theme)) return(theme)
  theme <- match.arg(theme, names(THEMES))
  THEMES[[theme]]
}

# Translate a CSS colour (hex, rgb(), rgba(), 'none', 'transparent') to an R
# colour, applying the theme's palette mapping for dark-palette strokes.
css_color <- function(col, theme) {
  if (is.null(col) || length(col) == 0 || is.na(col)) return(NA)
  col <- trimws(col)
  if (col %in% c("none", "transparent", "")) return(NA)
  if (startsWith(col, "rgba(") || startsWith(col, "rgb(")) {
    nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col), ",")[[1]])
    alpha <- if (length(nums) >= 4) nums[4] else 1
    return(grDevices::rgb(nums[1], nums[2], nums[3], alpha = alpha * 255, maxColorValue = 255))
  }
  mapped <- theme$map[tolower(col)]
  if (length(mapped) == 1 && !is.na(mapped)) return(unname(mapped))
  col
}

#' Grid grob for a shape configuration
#'
#' Creates a grid graphical object that draws one shape. The grob scales
#' itself to the viewport it is drawn in (stroke widths included), so it can
#' be placed in any layout. The `plot()` method for `cog_shape` objects draws
#' it on a new page.
#'
#' @param s A [cog_shape()].
#' @param theme `"light"` (default, for print) or `"dark"` (the web app's
#'   palette), or a theme list as returned internally.
#' @param name,vp Passed to [grid::gTree()].
#' @return A `gTree`.
#' @examples
#' library(grid)
#' grid.newpage()
#' grid.draw(shape_grob(cog_shape("spike-ring", list(spikeCount = 8))))
#' @export
shape_grob <- function(s, theme = "light", name = NULL, vp = NULL) {
  stopifnot(inherits(s, "cog_shape"))
  gTree(shape = s, theme = get_theme(theme), name = name, vp = vp, cl = "cog_shape_grob")
}

#' @exportS3Method grid::makeContent
makeContent.cog_shape_grob <- function(x) {
  w_in <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
  h_in <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)
  lw <- min(w_in, h_in) * 96 / 100 # lwd per viewBox unit (lwd 1 = 1/96 inch)
  setChildren(x, build_shape(x$shape, x$theme, lw))
}

#' @rdname shape_grob
#' @param x A `cog_shape` (plot method).
#' @param ... Ignored.
#' @export
plot.cog_shape <- function(x, theme = "light", ...) {
  th <- get_theme(theme)
  grid.newpage()
  grid.rect(gp = gpar(fill = th$bg, col = NA))
  grid.draw(shape_grob(x, theme = th, vp = viewport(width = 0.8, height = 0.8)))
  invisible(x)
}

# Square viewport with SVG-like coordinates, rotated clockwise by `angle`.
box_vp <- function(angle = 0) {
  viewport(width = unit(1, "snpc"), height = unit(1, "snpc"),
           xscale = c(0, 100), yscale = c(100, 0), angle = -angle)
}

poly_points <- function(n, r, cx = 50, cy = 50, offset = -pi / 2) {
  a <- (seq_len(n) - 1) / n * 2 * pi + offset
  list(x = cx + r * cos(a), y = cy + r * sin(a))
}

# Rectangle(s) given by top-left corner, width and height in the flipped
# native coordinates. Centre-based placement is required because a native
# height converts to a negative device height when the y-scale is reversed.
rect_tl <- function(x, y, w, h, gp, vp = NULL) {
  rectGrob(x + w / 2, y + h / 2, width = w, height = h, default.units = "native",
           gp = gp, vp = vp)
}

build_shape <- function(s, theme, lw) {
  if (is_blank_cell(s)) return(gList())
  stroke <- css_color(s$stroke, theme)
  fill <- css_color(s$fill, theme)
  p <- s$params
  sw <- s$strokeWidth
  lwd_of <- function(w) max(0.3, w * lw)
  gp_outline <- function(w = sw, fill_col = fill, join = "round") {
    gpar(col = stroke, fill = fill_col, lwd = lwd_of(w), linejoin = join, lineend = "round")
  }
  size <- s$size

  children <- switch(
    s$kind,
    annulus = {
      base_r <- 50 * 0.9 * size
      rc <- clamp(round(p$ringCount %||% 1), 1, 4)
      gap <- p$gap %||% 0.15
      grobs <- list()
      for (i in 0:(rc - 1)) {
        r <- base_r * (1 - i * gap)
        if (r <= 0) break
        grobs[[length(grobs) + 1]] <- circleGrob(
          50, 50, r, default.units = "native",
          gp = gpar(col = stroke, fill = if (i == 0) fill else NA, lwd = lwd_of(sw))
        )
      }
      do.call(gList, grobs)
    },
    dice = {
      # The die scales with `size` like every other carrier (side = 85 units at
      # size 1). The web app's Dice.tsx scales only the padding, which makes
      # the size axis nearly invisible; that formula is deliberately not kept.
      dots <- clamp(round(p$dotCount %||% 1), 1, 9)
      side <- 100 * 0.85 * size
      padding <- (100 - side) / 2
      cell <- side / 3
      dot_r <- cell * 0.18
      pos <- DICE_POSITIONS[[dots]]
      gList(
        roundrectGrob(50, 50, width = side, height = side,
                      default.units = "native", r = unit(0.04, "snpc"),
                      gp = gpar(col = stroke, fill = fill, lwd = lwd_of(sw))),
        circleGrob(padding + cell * (pos[, 1] + 0.5), padding + cell * (pos[, 2] + 0.5),
                   r = dot_r, default.units = "native", gp = gpar(fill = stroke, col = NA))
      )
    },
    polygon = {
      sides <- clamp(round(p$sides %||% 3), 3, 12)
      pts <- poly_points(sides, 50 * 0.85 * size)
      gList(polygonGrob(pts$x, pts$y, default.units = "native", gp = gp_outline()))
    },
    star = {
      n <- clamp(round(p$points %||% 5), 4, 10)
      ir <- clamp(p$innerRatio %||% 0.4, 0.2, 0.7)
      r_out <- 50 * 0.9 * size
      a <- (seq_len(2 * n) - 1) / (2 * n) * 2 * pi - pi / 2
      r <- ifelse(seq_len(2 * n) %% 2 == 1, r_out, r_out * ir)
      gList(polygonGrob(50 + r * cos(a), 50 + r * sin(a), default.units = "native",
                        gp = gp_outline()))
    },
    arrow = {
      r <- 50 * 0.85 * size
      head_ratio <- clamp(p$headRatio %||% 0.4, 0.25, 0.7)
      shaft_w <- clamp(p$shaftWidth %||% 0.35, 0.15, 0.6)
      total <- 2 * r
      head_len <- total * head_ratio
      W <- r * 0.7
      sh <- W * shaft_w
      top <- -r
      hb <- top + head_len
      bottom <- top + total
      xs <- c(0, W, sh, sh, -sh, -sh, -W)
      ys <- c(top, hb, hb, bottom, bottom, hb, hb)
      gList(polygonGrob(50 + xs, 50 + ys, default.units = "native", gp = gp_outline()))
    },
    petals = {
      r <- 50 * 0.9 * size
      n <- clamp(round(p$petalCount %||% 6), 3, 12)
      wf <- clamp(p$petalWidth %||% 0.12, 0.04, 0.4)
      hw <- wf * 2 * pi
      t <- seq(0, 1, length.out = 14)
      xs <- numeric(0); ys <- numeric(0); lens <- integer(0)
      for (i in 0:(n - 1)) {
        a <- i / n * 2 * pi - pi / 2
        tip <- c(50 + r * cos(a), 50 + r * sin(a))
        c1 <- c(50 + r * 0.5 * cos(a - hw), 50 + r * 0.5 * sin(a - hw))
        c2 <- c(50 + r * 0.5 * cos(a + hw), 50 + r * 0.5 * sin(a + hw))
        b1x <- (1 - t)^2 * 50 + 2 * (1 - t) * t * c1[1] + t^2 * tip[1]
        b1y <- (1 - t)^2 * 50 + 2 * (1 - t) * t * c1[2] + t^2 * tip[2]
        b2x <- (1 - t)^2 * tip[1] + 2 * (1 - t) * t * c2[1] + t^2 * 50
        b2y <- (1 - t)^2 * tip[2] + 2 * (1 - t) * t * c2[2] + t^2 * 50
        xs <- c(xs, b1x, b2x[-1]); ys <- c(ys, b1y, b2y[-1])
        lens <- c(lens, length(b1x) + length(b2x) - 1L)
      }
      gList(polygonGrob(xs, ys, id.lengths = lens, default.units = "native", gp = gp_outline()))
    },
    "spike-ring" = {
      r_out <- 50 * 0.9 * size
      n <- clamp(round(p$spikeCount %||% 8), 4, 16)
      depth <- clamp(p$spikeDepth %||% 0.35, 0.15, 0.6)
      r_in <- r_out * (1 - depth)
      half <- (pi / n) * 0.5
      xs <- numeric(0); ys <- numeric(0)
      for (i in 0:(n - 1)) {
        centre <- -pi / 2 + i / n * 2 * pi
        a <- centre - pi / n + half
        xs <- c(xs, 50 + r_in * cos(a), 50 + r_out * cos(centre))
        ys <- c(ys, 50 + r_in * sin(a), 50 + r_out * sin(centre))
        a <- centre + pi / n - half
        xs <- c(xs, 50 + r_in * cos(a)); ys <- c(ys, 50 + r_in * sin(a))
      }
      gList(polygonGrob(xs, ys, default.units = "native", gp = gp_outline()))
    },
    hammer = {
      handle_len <- clamp(p$handleLength %||% 0.55, 0.3, 0.8) * 100 * size
      head_w <- clamp(p$headWidth %||% 0.6, 0.4, 0.75) * 100 * size
      head_t <- clamp(p$headThickness %||% 0.18, 0.1, 0.25) * 100 * size
      handle_w <- head_t * 0.45
      total_h <- handle_len + head_t
      top <- 50 - total_h / 2
      fill_col <- if (is.na(fill)) stroke else fill
      gList(
        rect_tl(50 - head_w / 2, top, head_w, head_t,
                gp = gp_outline(fill_col = fill_col, join = "mitre")),
        rect_tl(50 - handle_w / 2, top + head_t, handle_w, handle_len,
                gp = gp_outline(fill_col = fill_col, join = "mitre"))
      )
    },
    bars = {
      n <- clamp(round(p$barCount %||% 3), 1, 6)
      len <- 80 * size
      spread <- 70 * size
      pos <- if (n == 1) 50 else 50 - spread / 2 + spread * (0:(n - 1)) / (n - 1)
      gList(segmentsGrob(50 - len / 2, pos, 50 + len / 2, pos, default.units = "native",
                         gp = gpar(col = stroke, lwd = lwd_of(max(1, sw * 1.5)),
                                   lineend = "round")))
    },
    "grid-dots" = {
      rows <- clamp(round(p$rows %||% 2), 1, 4)
      cols <- clamp(round(p$cols %||% 2), 1, 4)
      dot_frac <- clamp(p$dotSize %||% 0.07, 0.03, 0.14)
      inner <- 70 * size
      dot_r <- 100 * dot_frac * size
      xs <- if (cols == 1) 50 else 50 - inner / 2 + inner * (0:(cols - 1)) / (cols - 1)
      ys <- if (rows == 1) 50 else 50 - inner / 2 + inner * (0:(rows - 1)) / (rows - 1)
      g <- expand.grid(x = xs, y = ys)
      has_fill <- !is.na(fill)
      gList(circleGrob(g$x, g$y, r = dot_r, default.units = "native",
                       gp = gpar(fill = if (has_fill) fill else stroke,
                                 col = if (has_fill) stroke else NA,
                                 lwd = lwd_of(sw))))
    },
    checkerboard = {
      rows <- clamp(round(p$rows %||% 3), 1, 4)
      cols <- clamp(round(p$cols %||% 3), 1, 4)
      max_pattern <- 2^(rows * cols) - 1
      pattern <- as.integer(clamp(round(p$pattern %||% 0), 0, max_pattern))
      inner <- 78 * size
      cw <- inner / cols
      ch <- inner / rows
      x0 <- 50 - inner / 2
      y0 <- 50 - inner / 2
      inset <- max(1.5, min(cw, ch) * 0.12)
      outline_w <- max(0.8, sw * 0.6)
      g <- expand.grid(c = 0:(cols - 1), r = 0:(rows - 1))
      filled <- bitwAnd(pattern, bitwShiftL(1L, as.integer(g$r * cols + g$c))) != 0
      grobs <- list(rect_tl(x0 + g$c * cw, y0 + g$r * ch, cw, ch,
                            gp = gpar(col = stroke, fill = NA, lwd = lwd_of(outline_w))))
      if (any(filled)) {
        grobs[[2]] <- rect_tl(x0 + g$c[filled] * cw + inset, y0 + g$r[filled] * ch + inset,
                              cw - 2 * inset, ch - 2 * inset,
                              gp = gpar(col = NA, fill = stroke))
      }
      do.call(gList, grobs)
    },
    "box-lines" = {
      half <- 40 * size
      l <- 50 - half; r <- 50 + half; t <- 50 - half; b <- 50 + half
      mask <- as.integer(clamp(round(p$lineMask %||% 0), 0, 63))
      w <- max(0.8, sw)
      segs <- list()
      if (bitwAnd(mask, 1L)) segs[[length(segs) + 1]] <- c(50, t, 50, 50)
      if (bitwAnd(mask, 2L)) segs[[length(segs) + 1]] <- c(r, 50, 50, 50)
      if (bitwAnd(mask, 4L)) segs[[length(segs) + 1]] <- c(50, b, 50, 50)
      if (bitwAnd(mask, 8L)) segs[[length(segs) + 1]] <- c(l, 50, 50, 50)
      if (bitwAnd(mask, 16L)) segs[[length(segs) + 1]] <- c(l, t, r, b)
      if (bitwAnd(mask, 32L)) segs[[length(segs) + 1]] <- c(r, t, l, b)
      grobs <- list(rectGrob(50, 50, width = 2 * half, height = 2 * half,
                             default.units = "native",
                             gp = gpar(col = stroke, fill = fill, lwd = lwd_of(w))))
      if (length(segs)) {
        m <- do.call(rbind, segs)
        grobs[[2]] <- segmentsGrob(m[, 1], m[, 2], m[, 3], m[, 4], default.units = "native",
                                   gp = gpar(col = stroke, lwd = lwd_of(w)))
      }
      do.call(gList, grobs)
    },
    "nested-polygon" = {
      outer_r <- 50 * 0.9 * size
      outer_n <- clamp(round(p$outerSides %||% 4), 3, 8)
      inner_n <- clamp(round(p$innerSides %||% 4), 3, 8)
      inner_r <- outer_r * clamp(p$innerScale %||% 0.5, 0.25, 0.75)
      po <- poly_points(outer_n, outer_r)
      pi_ <- poly_points(inner_n, inner_r)
      gList(
        polygonGrob(po$x, po$y, default.units = "native", gp = gp_outline()),
        polygonGrob(pi_$x, pi_$y, default.units = "native", gp = gp_outline(fill_col = NA))
      )
    },
    "sector-pie" = build_sector_pie(s, stroke, lwd_of),
    "block-letter" = {
      idx <- clamp(round(p$patternIndex %||% 0), 0, length(BLOCK_LETTER_PRESETS) - 1)
      pattern <- BLOCK_LETTER_PRESETS[idx + 1]
      inner <- 78 * size
      cw <- inner / 3
      x0 <- 50 - inner / 2
      y0 <- 50 - inner / 2
      inset <- max(1.5, cw * 0.1)
      outline_w <- max(0.8, sw * 0.6)
      g <- expand.grid(c = 0:2, r = 0:2)
      filled <- bitwAnd(pattern, bitwShiftL(1L, as.integer(g$r * 3 + g$c))) != 0
      grobs <- list(rect_tl(x0 + g$c * cw, y0 + g$r * cw, cw, cw,
                            gp = gpar(col = stroke, fill = NA, lwd = lwd_of(outline_w),
                                      alpha = 0.3)))
      if (any(filled)) {
        grobs[[2]] <- rect_tl(x0 + g$c[filled] * cw + inset, y0 + g$r[filled] * cw + inset,
                              cw - 2 * inset, cw - 2 * inset,
                              gp = gpar(col = stroke, fill = stroke, lwd = lwd_of(outline_w * 0.5)))
      }
      do.call(gList, grobs)
    },
    gList(textGrob(paste0(s$kind, "?"), gp = gpar(col = theme$muted)))
  )

  rotation <- s$rotation
  if (s$kind == "bars") {
    orientation <- round(p$orientation %||% 0) %% 3
    rotation <- rotation + c(0, 90, 45)[orientation + 1]
  }
  main <- gTree(children = children, vp = box_vp(rotation))

  if (s$kind == "hammer") {
    marker_pos <- round(p$markerPos %||% 0)
    if (marker_pos > 0) {
      ms <- clamp(p$markerSize %||% 0.1, 0.05, 0.18) * 100
      inset <- 8
      xy <- switch(marker_pos,
                   c(inset, inset),
                   c(100 - inset - ms, inset),
                   c(inset, 100 - inset - ms),
                   c(100 - inset - ms, 100 - inset - ms))
      marker <- rect_tl(xy[1], xy[2], ms, ms, gp = gpar(fill = stroke, col = NA),
                        vp = box_vp(0))
      return(gList(main, marker))
    }
  }
  gList(main)
}

DICE_POSITIONS <- list(
  matrix(c(1, 1), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 2, 2, 0, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 2, 1, 1, 2, 0, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 2, 1, 0, 1, 2, 2, 0, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 2, 1, 0, 1, 1, 1, 2, 2, 0, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 1, 0, 2, 1, 0, 1, 2, 2, 0, 2, 1, 2, 2), ncol = 2, byrow = TRUE),
  matrix(c(0, 0, 0, 1, 0, 2, 1, 0, 1, 1, 1, 2, 2, 0, 2, 1, 2, 2), ncol = 2, byrow = TRUE)
)

# 3x3 asymmetric glyph presets; bit i = row(i %/% 3), col(i %% 3), LSB top-left.
BLOCK_LETTER_PRESETS <- vapply(
  c("011001111", "111001001", "110100111", "001011011",
    "111100100", "001010111", "100110011", "001011110"),
  function(b) strtoi(b, base = 2), integer(1), USE.NAMES = FALSE
)

# Sector pie with per-sector pattern fills. Hatch lines and dots are clipped
# to each sector geometrically, so no device pattern support is required.
build_sector_pie <- function(s, stroke, lwd_of) {
  p <- s$params
  r <- 50 * 0.85 * s$size
  count <- clamp(round(p$sectorCount %||% 4), 2, 8)
  packed <- round(p$sectorPatterns %||% 0)
  if (packed == 0 && !is.null(p$fillMask)) {
    packed <- fill_mask_to_patterns(round(p$fillMask), count, 1)
  }
  pats <- unpack_sector_patterns(packed, count)
  sw <- s$strokeWidth
  sector_lwd <- lwd_of(max(0.5, sw * 0.7))
  grobs <- list()
  for (i in seq_len(count)) {
    a0 <- (i - 1) / count * 2 * pi - pi / 2
    a1 <- i / count * 2 * pi - pi / 2
    arc <- seq(a0, a1, length.out = 25)
    xs <- c(50, 50 + r * cos(arc))
    ys <- c(50, 50 + r * sin(arc))
    pid <- pats[i]
    grobs[[length(grobs) + 1]] <- polygonGrob(
      xs, ys, default.units = "native",
      gp = gpar(col = stroke, fill = if (pid == 1) stroke else NA, lwd = sector_lwd)
    )
    if (pid >= 2) {
      grobs <- c(grobs, sector_pattern_grobs(pid, r, a0, a1, stroke, lwd_of(1)))
    }
  }
  grobs[[length(grobs) + 1]] <- circleGrob(50, 50, r, default.units = "native",
                                           gp = gpar(col = stroke, fill = NA, lwd = lwd_of(sw)))
  do.call(gList, grobs)
}

cross2 <- function(ux, uy, vx, vy) ux * vy - uy * vx

# Clip the line P(t) = p0 + t * d to the convex sector (centre 50,50; radius r;
# angles a0 < a1, a1 - a0 <= pi). Returns c(x1, y1, x2, y2) or NULL.
clip_line_to_sector <- function(p0, d, r, a0, a1) {
  cx <- 50; cy <- 50
  fx <- p0[1] - cx; fy <- p0[2] - cy
  A <- d[1]^2 + d[2]^2
  B <- 2 * (fx * d[1] + fy * d[2])
  C <- fx^2 + fy^2 - r^2
  disc <- B^2 - 4 * A * C
  if (disc <= 0) return(NULL)
  sq <- sqrt(disc)
  tlo <- (-B - sq) / (2 * A)
  thi <- (-B + sq) / (2 * A)
  # half-plane: cross(u0, P - C) >= 0 ; cross(u1, P - C) <= 0
  for (k in 1:2) {
    ang <- if (k == 1) a0 else a1
    sign <- if (k == 1) 1 else -1
    ux <- cos(ang); uy <- sin(ang)
    c0 <- sign * cross2(ux, uy, fx, fy)
    cd <- sign * cross2(ux, uy, d[1], d[2])
    if (abs(cd) < 1e-12) {
      if (c0 < 0) return(NULL)
    } else if (cd > 0) {
      tlo <- max(tlo, -c0 / cd)
    } else {
      thi <- min(thi, -c0 / cd)
    }
  }
  if (tlo >= thi) return(NULL)
  c(p0[1] + tlo * d[1], p0[2] + tlo * d[2], p0[1] + thi * d[1], p0[2] + thi * d[2])
}

sector_pattern_grobs <- function(pid, r, a0, a1, colour, lwd) {
  out <- list()
  if (pid == 2) {
    g <- expand.grid(x = seq(3, 97, by = 6), y = seq(3, 97, by = 6))
    vx <- g$x - 50; vy <- g$y - 50
    inside <- sqrt(vx^2 + vy^2) <= r - 1.4 &
      cross2(cos(a0), sin(a0), vx, vy) >= 0 &
      cross2(cos(a1), sin(a1), vx, vy) <= 0
    if (any(inside)) {
      out[[1]] <- circleGrob(g$x[inside], g$y[inside], r = 1.2, default.units = "native",
                             gp = gpar(fill = colour, col = NA))
    }
    return(out)
  }
  lines <- list()
  add_family <- function(p0s, d) {
    for (p0 in p0s) {
      seg <- clip_line_to_sector(p0, d, r, a0, a1)
      if (!is.null(seg)) lines[[length(lines) + 1]] <<- seg
    }
  }
  if (pid %in% c(3)) add_family(lapply(seq(2, 98, by = 4), function(y) c(0, y)), c(1, 0))
  if (pid %in% c(4)) add_family(lapply(seq(2, 98, by = 4), function(x) c(x, 0)), c(0, 1))
  if (pid %in% c(5, 7)) add_family(lapply(seq(-102, 102, by = 6), function(k) c(k, 0)), c(1, 1))
  if (pid %in% c(6, 7)) add_family(lapply(seq(0, 204, by = 6), function(k) c(k, 0)), c(-1, 1))
  if (length(lines)) {
    m <- do.call(rbind, lines)
    out[[1]] <- segmentsGrob(m[, 1], m[, 2], m[, 3], m[, 4], default.units = "native",
                             gp = gpar(col = colour, lwd = lwd, lineend = "butt"))
  }
  out
}
