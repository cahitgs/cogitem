# Attributes a shape family actually draws. Perturbing an attribute the
# renderer ignores (the fill of a checkerboard, the stroke width of unfilled
# dots) would create a nominally different but identical-looking option, so
# such attributes are normalised away here and skipped by the distractor
# generator.
FILL_IGNORED_SHAPES <- c("checkerboard", "bars", "sector-pie", "block-letter")

attribute_visible <- function(s, what) {
  switch(
    what,
    fill = !(s$kind %in% FILL_IGNORED_SHAPES),
    strokeWidth = !(s$kind == "grid-dots" && (is.null(s$fill) || is.na(s$fill))),
    TRUE
  )
}

# Rotate a rows x cols bit pattern (bit r * cols + c = cell r, c filled, row 0
# at the top) clockwise by `steps` quarter turns.
rotate_pattern_cw <- function(rows, cols, pattern, steps) {
  steps <- steps %% 4
  bits <- bitwAnd(as.integer(pattern), bitwShiftL(1L, 0:(rows * cols - 1))) != 0
  m <- matrix(bits, nrow = rows, ncol = cols, byrow = TRUE)
  for (k in seq_len(steps)) m <- t(m[nrow(m):1, , drop = FALSE])
  idx <- which(as.vector(t(m)))
  list(rows = nrow(m), cols = ncol(m),
       pattern = if (length(idx)) sum(bitwShiftL(1L, idx - 1L)) else 0L)
}

# Rotate a box-lines mask (spokes 1 top, 2 right, 4 bottom, 8 left; diagonals
# 16 top-left to bottom-right, 32 top-right to bottom-left) clockwise.
rotate_mask_cw <- function(mask, steps) {
  mask <- as.integer(mask)
  spokes <- bitwAnd(mask, c(1L, 2L, 4L, 8L)) != 0
  diags <- bitwAnd(mask, c(16L, 32L)) != 0
  for (k in seq_len(steps %% 4)) {
    spokes <- spokes[c(4, 1, 2, 3)]
    diags <- diags[2:1]
  }
  sum(c(1L, 2L, 4L, 8L)[spokes]) + sum(c(16L, 32L)[diags])
}

gcd2 <- function(a, b) if (b == 0) a else gcd2(b, a %% b)
lcm2 <- function(a, b) a * b / gcd2(a, b)

# Sector count and per-sector textures of a pie (legacy fillMask accepted).
pie_params <- function(p) {
  n <- clamp(round(p$sectorCount %||% 6), 2, 8)
  packed <- p$sectorPatterns %||%
    (if (!is.null(p$fillMask)) fill_mask_to_patterns(round(p$fillMask), n, 1) else 0)
  list(n = as.integer(n), pats = as.integer(unpack_sector_patterns(packed, n)))
}

# Indices k of the visible dividers of a pie (divider k sits at k * 360 / n,
# between sectors k and k + 1). The divider between two solid sectors is
# invisible: both are filled with the stroke colour and the edge is drawn in
# the same colour.
pie_visible_boundaries <- function(pats) {
  nxt <- c(pats[-1], pats[1])
  which(!(pats == 1 & nxt == 1))
}

# The pie re-expressed with the smallest sector count whose grid carries
# every visible divider, so that pies drawing the same picture share one
# signature (an all-solid pie of 2 or 5 sectors is the same disc).
pie_canonical <- function(n, pats) {
  ks <- pie_visible_boundaries(pats)
  if (!length(ks)) return(list(n = 2L, pats = c(1L, 1L)))
  m <- 1
  for (k in ks) m <- lcm2(m, n / gcd2(k, n))
  m <- as.integer(max(m, 2))
  list(n = m, pats = vapply(seq_len(m), function(j) pats[floor((j - 0.5) / m * n) + 1], integer(1)))
}

# Do all textures of a sector pie look the same after rotating by theta
# degrees? Solid, empty and dots at any angle, cross-hatch at multiples of 90,
# parallel and diagonal lines only at 180.
textures_invariant <- function(pats, theta) {
  all(vapply(unique(pats), function(t) {
    if (t %in% c(0, 1, 2)) TRUE else if (t == 7) theta %% 90 == 0 else theta %% 180 == 0
  }, logical(1)))
}

# Rotation-canonical form of a pattern carrier: the quarter turns of the
# rotation are folded into the pattern itself, so two configurations that draw
# the same picture get the same signature (a glyph turned by 180 degrees and
# its rotated twin preset, a 2x3 dot grid turned by 90 degrees and a 3x2
# grid, horizontal bars turned by 90 degrees and vertical bars).
canonical_shape <- function(s) {
  rot <- s$rotation %% 360
  k <- floor((rot + 1e-9) / 90) %% 4
  residual <- rot - 90 * k
  p <- s$params
  if (s$kind == "block-letter") {
    idx <- clamp(round(p$patternIndex %||% 0), 0, length(BLOCK_LETTER_PRESETS) - 1)
    g <- rotate_pattern_cw(3, 3, BLOCK_LETTER_PRESETS[idx + 1], k)
    s$params <- list(glyph = g$pattern)
    s$rotation <- residual
  } else if (s$kind == "checkerboard") {
    rows <- max(1, round(p$rows %||% 3))
    cols <- max(1, round(p$cols %||% 3))
    g <- rotate_pattern_cw(rows, cols, round(p$pattern %||% 0), k)
    s$params$rows <- g$rows
    s$params$cols <- g$cols
    s$params$pattern <- g$pattern
    s$rotation <- residual
  } else if (s$kind == "box-lines") {
    s$params$lineMask <- rotate_mask_cw(round(p$lineMask %||% 0), k)
    s$rotation <- residual
  } else if (s$kind == "grid-dots") {
    if (k %% 2 == 1) {
      rows <- p$rows
      s$params$rows <- p$cols
      s$params$cols <- rows
    }
    s$rotation <- residual
  } else if (s$kind == "bars") {
    orientation <- round(p$orientation %||% 0) %% 3
    s$params$orientation <- 0
    s$rotation <- (rot + c(0, 90, 45)[orientation + 1]) %% 180
  } else if (s$kind == "sector-pie") {
    pp <- pie_params(p)
    can <- pie_canonical(pp$n, pp$pats)
    n <- can$n
    pats <- can$pats
    s$params$sectorCount <- n
    s$params$sectorPatterns <- pack_sector_patterns(pats)
    s$params$fillMask <- NULL
    turns <- rot / (360 / n)
    if (abs(turns - round(turns)) < 1e-6 && round(turns) %% n != 0 &&
        textures_invariant(pats, rot)) {
      sh <- round(turns) %% n
      s$params$sectorPatterns <- pack_sector_patterns(pats[((seq_len(n) - 1 - sh) %% n) + 1])
      s$rotation <- 0
    }
  }
  s
}

#' Visual signature of a shape configuration
#'
#' Two configurations that produce the same picture for a human observer get
#' the same signature. Rotation is normalised by the shape's rotational
#' symmetry (an annulus looks the same at any angle, a square every 90
#' degrees, an arrow never), so visually identical response options can be
#' detected and eliminated during generation. Colours are serialised as they
#' are drawn (display palette, lower-case hex), so `"#A78BFA"`, `"#a78bfa"`
#' and a colour name for the same RGB value share one signature. This is the
#' first of the package's two option-distinctness checks; the renderer-based
#' [audit_item()] is the second.
#'
#' @param s A `cog_shape`.
#' @return A character string.
#' @examples
#' a <- cog_shape("polygon", list(sides = 4), rotation = 0)
#' b <- cog_shape("polygon", list(sides = 4), rotation = 90)
#' identical(visual_signature(a), visual_signature(b)) # TRUE: a square
#' @export
visual_signature <- function(s) {
  if (is_stack(s)) {
    return(sprintf("stack[%s|%s]", visual_signature(s$layers[[1]]),
                   visual_signature(s$layers[[2]])))
  }
  s <- canonical_shape(s)
  nms <- sort(names(s$params))
  params <- paste0(nms, "=", vapply(nms, function(k) fmt_num(s$params[[k]]), ""),
                   collapse = ",")
  fold <- rotation_symmetry_fold(s)
  eff_rot <- if (fold >= 360) 0 else round(s$rotation %% (360 / fold))
  fill <- if (attribute_visible(s, "fill")) canonical_colour(s$fill) else NA_character_
  if (is.na(fill)) fill <- "none"
  stroke <- canonical_colour(s$stroke)
  if (is.na(stroke)) stroke <- "none"
  sw <- if (attribute_visible(s, "strokeWidth")) fmt_num(s$strokeWidth) else "NA"
  sprintf("%s(sz=%s,rot=%s,sw=%s,stk=%s,fill=%s,%s)",
          s$kind, fmt_num(round2(s$size)), fmt_num(eff_rot),
          sw, stroke, fill, params)
}

fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  as.character(round(as.numeric(x), 10))
}

#' Rotational symmetry fold of a shape configuration
#'
#' Returns N when the shape looks identical after rotation by 360/N degrees.
#' Fully asymmetric shapes return 1; a fully symmetric annulus returns 360.
#'
#' @inheritParams visual_signature
#' @return A number.
#' @export
rotation_symmetry_fold <- function(s) {
  p <- s$params
  switch(
    s$kind,
    annulus = 360,
    dice = {
      n <- p$dotCount %||% 1
      if (n %in% DICE_90_SYMMETRIC) 4 else if (n %in% DICE_180_SYMMETRIC) 2 else 1
    },
    polygon = max(1, round(p$sides %||% 3)),
    star = max(1, round(p$points %||% 5)),
    petals = max(1, round(p$petalCount %||% 6)),
    "spike-ring" = max(1, round(p$spikeCount %||% 8)),
    arrow = 1,
    hammer = 1,
    bars = 2,
    "grid-dots" = {
      rows <- round(p$rows %||% 2)
      cols <- round(p$cols %||% 2)
      if (rows == cols) 4 else 2
    },
    checkerboard = grid_pattern_fold(round(p$rows %||% 3), round(p$cols %||% 3),
                                     round(p$pattern %||% 0)),
    "box-lines" = box_lines_fold(round(p$lineMask %||% 0)),
    "nested-polygon" = {
      outer <- max(3, round(p$outerSides %||% 4))
      inner <- max(3, round(p$innerSides %||% 4))
      gcd(outer, inner)
    },
    "sector-pie" = sector_pie_fold(p),
    "block-letter" = grid_pattern_fold(3, 3, p$glyph %||% BLOCK_LETTER_PRESETS[clamp(round(p$patternIndex %||% 0), 0, 7) + 1]),
    1
  )
}

gcd <- function(a, b) {
  if (b == 0) return(max(1, a))
  gcd(b, a %% b)
}

# Rotational symmetry of a filled-cell pattern on a rows x cols grid (bit
# r * cols + c set = cell filled): 4 when a square pattern is invariant under
# 90 degrees, 2 when invariant under 180 degrees, otherwise 1.
grid_pattern_fold <- function(rows, cols, pattern) {
  if (is.na(rows) || is.na(cols) || rows < 1 || cols < 1) return(1)
  pattern <- as.integer(pattern)
  filled <- function(r, c) bitwAnd(pattern, bitwShiftL(1L, as.integer(r * cols + c))) != 0
  g <- expand.grid(r = 0:(rows - 1), c = 0:(cols - 1))
  inv180 <- all(mapply(function(r, c) filled(r, c) == filled(rows - 1 - r, cols - 1 - c), g$r, g$c))
  if (!inv180) return(1)
  if (rows == cols &&
      all(mapply(function(r, c) filled(r, c) == filled(c, rows - 1 - r), g$r, g$c))) return(4)
  2
}

# Box with spokes (bits 1, 2, 4, 8 = top, right, bottom, left) and diagonals
# (16 = top-left to bottom-right, 32 = top-right to bottom-left). The box
# itself is square, so only the lines decide the symmetry.
box_lines_fold <- function(mask) {
  mask <- as.integer(mask)
  bit <- function(b) bitwAnd(mask, b) != 0
  spokes <- c(bit(1L), bit(2L), bit(4L), bit(8L))
  diags <- c(bit(16L), bit(32L))
  rot90 <- function(s) s[c(4, 1, 2, 3)]
  if (identical(spokes, rot90(spokes)) && diags[1] == diags[2]) return(4)
  if (identical(spokes, rot90(rot90(spokes)))) return(2)
  1
}

# Sector pie: the sector sequence must repeat under the rotation and every
# texture present must look the same after it (solid, empty and dots at any
# angle, cross-hatch at multiples of 90 degrees, parallel and diagonal lines
# only at 180 degrees).
sector_pie_fold <- function(p) {
  n <- clamp(round(p$sectorCount %||% 6), 2, 8)
  packed <- p$sectorPatterns %||%
    (if (!is.null(p$fillMask)) fill_mask_to_patterns(round(p$fillMask), n, 1) else 0)
  pats <- unpack_sector_patterns(packed, n)
  if (all(pats == 1)) return(360) # a solid disc
  invariant <- function(theta) textures_invariant(pats, theta)
  for (d in seq(n, 2)) {
    if (n %% d != 0) next
    k <- n / d
    shifted <- pats[((seq_len(n) - 1 + k) %% n) + 1]
    if (identical(shifted, pats) && invariant(360 / d)) return(d)
  }
  1
}

#' Structural signature of an item
#'
#' Two items with the same signature present the same matrix and the same
#' correct answer to the examinee. Used to remove duplicates in
#' [generate_items()].
#'
#' @param item A `cog_item`.
#' @return A character string.
#' @export
item_signature <- function(item) {
  cell_sig <- paste(
    vapply(item$cells, function(row) {
      paste(vapply(row, visual_signature, ""), collapse = "|")
    }, ""),
    collapse = "/"
  )
  sprintf("%s:%s:%s#%s", item$shape, item$rule, cell_sig,
          visual_signature(item$options[[item$correct]]))
}

# Minimum count difference that is still perceptible at cell size. Small
# carriers (size below 0.6) need one more step for many-sided polygons, and
# hairline rings (stroke width below 2) one more ring.
min_perceptible_count_delta <- function(kind, hi, size = 0.75, sw = 2) {
  switch(
    kind,
    petals = , "spike-ring" = if (hi >= 10) 3 else if (hi >= 7) 2 else 1,
    star = , polygon = , "nested-polygon" = , "sector-pie" =
      if (hi >= 7) (if (size < 0.6) 3 else 2) else 1,
    dice = if (hi >= 7) 2 else 1,
    bars = if (hi >= 6) 2 else 1,
    annulus = if (sw < 2) 2 else 1,
    1
  )
}

# Signature with the primary count parameter normalised away.
count_stripped_signature <- function(s) {
  spec <- PRIMARY_PARAM[[s$kind]]
  if (is.null(spec)) return(visual_signature(s))
  visual_signature(shape_param(s, spec$name, 0))
}

# TRUE when a and b differ only by an imperceptibly small count.
# Sector pies that draw the same textures over the same angles and differ
# only in divider lines running through hatched or dotted sectors: such lines
# are too faint against the texture to be counted (uniform horizontal lines
# with 2 vs 3 sectors score 0.15, cross-hatch 0.12, 2 vs 4 sectors 0.06).
# Dividers through empty sectors are clearly visible, and dividers between
# solid sectors do not exist (see pie_canonical()).
pie_divider_confusable <- function(a, b) {
  a <- canonical_shape(a)
  b <- canonical_shape(b)
  if ((a$rotation - b$rotation) %% 360 != 0) return(FALSE)
  pa <- pie_params(a$params)
  pb <- pie_params(b$params)
  if (identical(pa, pb)) return(FALSE)
  neutral <- function(s) {
    s$params$sectorCount <- 2
    s$params$sectorPatterns <- 0
    s$rotation <- 0
    visual_signature(s)
  }
  if (neutral(a) != neutral(b)) return(FALSE)
  grid <- 840L # lcm of the sector counts 2..8
  tex <- function(p) p$pats[floor((seq_len(grid) - 0.5) / grid * p$n) + 1]
  ta <- tex(pa)
  if (!identical(ta, tex(pb))) return(FALSE)
  bounds <- function(p) pie_visible_boundaries(p$pats) * (grid %/% p$n)
  ba <- bounds(pa)
  bb <- bounds(pb)
  only_one <- c(setdiff(ba, bb), setdiff(bb, ba))
  length(only_one) > 0 && all(ta[(only_one %% grid) + 1] >= 2)
}

# The side count of the inner polygon of a nested polygon is hard to read
# when the inner radius (cell units) is small: at radius 17, 4 vs 6 sides
# score 0.15-0.17, 6 vs 8 0.11; below radius 15 every pair without a
# triangle stays below 0.15; from radius 19 the full-size polygon rule
# (two sides from seven on) applies.
nested_inner_confusable <- function(a, b) {
  ia <- round(a$params$innerSides %||% 4)
  ib <- round(b$params$innerSides %||% 4)
  if (ia == ib) return(FALSE)
  if (visual_signature(shape_param(a, "innerSides", 4)) !=
      visual_signature(shape_param(b, "innerSides", 4))) return(FALSE)
  lo <- min(ia, ib)
  d <- abs(ia - ib)
  if (lo <= 3) return(FALSE)
  r <- 45 * min(a$size, b$size) * clamp(a$params$innerScale %||% 0.5, 0.25, 0.75)
  if (r < 15) return(TRUE)
  if (r < 19) return(d < 3)
  max(ia, ib) >= 7 && d < 2
}

# Spikes shorter than 6 cell units or closer together than 9 units cannot be
# counted; two such rings with 8 or more spikes each look alike (16 vs 10
# spikes at size 0.55 and depth 0.2 score 0.15).
spikes_illegible <- function(a, b, av, bv) {
  size <- min(a$size, b$size)
  amplitude <- (a$params$spikeDepth %||% 0.35) * 45 * size
  legible <- function(cnt) amplitude >= 6 && 2 * pi * 45 * size / cnt >= 9
  !(legible(av) && legible(bv)) && min(av, bv) >= 8
}

count_confusable <- function(a, b) {
  if (a$kind != b$kind) return(FALSE)
  if (a$kind == "sector-pie" && pie_divider_confusable(a, b)) return(TRUE)
  if (a$kind == "nested-polygon" && nested_inner_confusable(a, b)) return(TRUE)
  spec <- PRIMARY_PARAM[[a$kind]]
  if (is.null(spec)) return(FALSE)
  av <- round(a$params[[spec$name]] %||% spec$min)
  bv <- round(b$params[[spec$name]] %||% spec$min)
  if (av == bv) return(FALSE)
  if (count_stripped_signature(a) != count_stripped_signature(b)) return(FALSE)
  if (a$kind == "spike-ring" && spikes_illegible(a, b, av, bv)) return(TRUE)
  abs(av - bv) < min_perceptible_count_delta(a$kind, max(av, bv), min(a$size, b$size),
                                             min(a$strokeWidth, b$strokeWidth))
}

popcount <- function(x) sum(as.integer(intToBits(as.integer(x))))

# TRUE when two checkerboards of the same geometry differ in too few cells to
# be told apart (one cell out of nine or more).
pattern_confusable <- function(a, b) {
  if (a$kind != "checkerboard" || b$kind != "checkerboard") return(FALSE)
  pa <- round(a$params$pattern %||% 0)
  pb <- round(b$params$pattern %||% 0)
  if (pa == pb) return(FALSE)
  if (visual_signature(shape_param(a, "pattern", 0)) !=
      visual_signature(shape_param(b, "pattern", 0))) return(FALSE)
  bits <- round(a$params$rows %||% 3) * round(a$params$cols %||% 3)
  min_hamming <- if (bits >= 9) 2 else 1
  popcount(bitwXor(as.integer(pa), as.integer(pb))) < min_hamming
}

# Smallest differences along one attribute that the eye still resolves at
# cell size (calibrated with perceptual_distance(): a size step of 0.05 or a
# stroke-width step of 1 unit score around 0.15, the audit threshold).
MIN_SIZE_DELTA <- 0.1
MIN_STROKE_DELTA <- 2
ROTATION_MIN_SHIFT <- 12    # cell units a vertex must move away from a symmetric position
ROTATION_MIN_OFFSET_ASYM <- 4 # degrees for carriers without rotational symmetry

# A colour as the renderer draws it: generator palette colours are replaced by
# their display equivalents (PALETTE$map, see css_color()), everything else is
# normalised to lower-case hex, so that "#A78BFA", "#a78bfa", the display
# colour "#7c3aed" and a colour name for the same RGB value compare equal.
# Memoised: generation computes thousands of signatures over a handful of
# colours.
.colour_cache <- new.env(parent = emptyenv())
canonical_colour <- function(col) {
  if (is.null(col) || length(col) != 1 || is.na(col)) return(NA_character_)
  key <- as.character(col)
  hit <- .colour_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- css_color(key)
  if (!is.na(out)) {
    rgb <- tryCatch(grDevices::col2rgb(out, alpha = TRUE), error = function(e) NULL)
    out <- if (is.null(rgb)) {
      tolower(out)
    } else if (rgb[4, 1] < 255) {
      tolower(grDevices::rgb(rgb[1], rgb[2], rgb[3], rgb[4], maxColorValue = 255))
    } else {
      tolower(grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255))
    }
  } else {
    out <- NA_character_
  }
  assign(key, out, envir = .colour_cache)
  out
}

# CIELAB distance below which a stroke or fill colour change scores under the
# audit threshold for every carrier (perceptual_distance() is close to
# 0.0075 x delta E; the display palette's closest pair is 44 units apart).
MIN_COLOUR_DELTA_E <- 20

# TRUE when a and b differ in nothing but their stroke and/or fill colours and
# every differing colour pair lies within MIN_COLOUR_DELTA_E as drawn. A
# colour against no fill is always a visible change.
colour_confusable <- function(a, b) {
  if (a$kind != b$kind) return(FALSE)
  sa <- canonical_colour(a$stroke)
  sb <- canonical_colour(b$stroke)
  fa <- if (attribute_visible(a, "fill")) canonical_colour(a$fill) else NA_character_
  fb <- if (attribute_visible(b, "fill")) canonical_colour(b$fill) else NA_character_
  stroke_differs <- !identical(sa, sb)
  fill_differs <- !identical(fa, fb)
  if (!stroke_differs && !fill_differs) return(FALSE)
  # cheap test first: generated colours are always far apart
  near <- function(x, y) {
    !is.na(x) && !is.na(y) && sqrt(sum((colour_lab(x) - colour_lab(y))^2)) < MIN_COLOUR_DELTA_E
  }
  if (stroke_differs && !near(sa, sb)) return(FALSE)
  if (fill_differs && !near(fa, fb)) return(FALSE)
  a$stroke <- b$stroke <- "#000000"
  a$fill <- b$fill <- NA_character_
  visual_signature(a) == visual_signature(b)
}

# CIELAB coordinates of a canonical colour, memoised.
.lab_cache <- new.env(parent = emptyenv())
colour_lab <- function(col) {
  hit <- .lab_cache[[col]]
  if (!is.null(hit)) return(hit)
  lab <- as.numeric(srgb_to_lab(t(grDevices::col2rgb(col) / 255)))
  assign(col, lab, envir = .lab_cache)
  lab
}

# TRUE when a and b differ in nothing but `attr` (through the visual signature).
same_except <- function(a, b, attr) {
  if (a$kind != b$kind) return(FALSE)
  neutral <- switch(attr, size = 0.5, strokeWidth = 1, rotation = 0, 0)
  a[[attr]] <- neutral
  b[[attr]] <- neutral
  visual_signature(a) == visual_signature(b)
}

size_confusable <- function(a, b) {
  d <- abs(a$size - b$size)
  d > 1e-9 && round(d, 6) < MIN_SIZE_DELTA && same_except(a, b, "size")
}

stroke_width_confusable <- function(a, b) {
  d <- abs(a$strokeWidth - b$strokeWidth)
  d > 0 && d < MIN_STROKE_DELTA && attribute_visible(a, "strokeWidth") &&
    same_except(a, b, "strokeWidth")
}

# Angular periods (degrees) after which the carrier, or each of its
# components, looks the same, with the radius (cell units) of each component:
# a nested heptagon/octagon has periods 51.4 and 45 although its exact
# symmetry fold is 1.
symmetry_components <- function(s) {
  p <- s$params
  outer_r <- 50 * 0.9 * s$size
  if (s$kind == "nested-polygon") {
    n <- pmax(1, round(c(p$outerSides %||% 4, p$innerSides %||% 4)))
    return(list(period = 360 / n,
                radius = c(outer_r, outer_r * clamp(p$innerScale %||% 0.5, 0.25, 0.75))))
  }
  list(period = 360 / rotation_symmetry_fold(s), radius = outer_r)
}

# TRUE when a and b differ only by a rotation that leaves every component
# within a few degrees of one of its symmetric positions.
rotation_confusable <- function(a, b) {
  delta <- (a$rotation - b$rotation) %% 360
  if (delta == 0 || !same_except(a, b, "rotation")) return(FALSE)
  comp <- symmetry_components(a)
  invisible <- vapply(seq_along(comp$period), function(k) {
    p <- comp$period[k]
    m <- delta %% p
    offset <- min(m, p - m)
    if (p >= 360) return(offset < ROTATION_MIN_OFFSET_ASYM)
    comp$radius[k] * sin(offset * pi / 180) < ROTATION_MIN_SHIFT
  }, logical(1))
  all(invisible)
}

# Nominally different but not reliably distinguishable by eye (symbolic rules).
confusable <- function(a, b) {
  if (visual_signature(a) == visual_signature(b)) return(FALSE)
  if (is_stack(a) && is_stack(b)) {
    # two stacks are confusable when every layer is identical or confusable,
    # the inner layer judged at the scale it is drawn
    return(all(vapply(1:2, function(k) {
      x <- layer_at_scale(a, k)
      y <- layer_at_scale(b, k)
      visual_signature(x) == visual_signature(y) || confusable(x, y)
    }, logical(1))))
  }
  count_confusable(a, b) || pattern_confusable(a, b) || size_confusable(a, b) ||
    stroke_width_confusable(a, b) || rotation_confusable(a, b) ||
    colour_confusable(a, b)
}
