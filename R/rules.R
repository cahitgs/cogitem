# Rule generators. Each builds a 3x3 matrix of shape configurations whose
# bottom-right cell is missing, a correct answer and rule-derived distractor
# candidates, then hands everything to new_item() which shuffles the options
# and attaches a calibrated difficulty.

new_item <- function(rule, shape, cells, correct, distractors, meta = list()) {
  opts <- c(list(correct), distractors)
  tiers <- c("correct", vapply(distractors, function(d) attr(d, "tier") %||% "perturbation",
                               character(1)))
  perm <- sample.int(length(opts))
  options <- lapply(opts[perm], untag_tier)
  cells <- lapply(cells, function(row) lapply(row, untag_tier))
  item <- structure(
    list(
      id = next_item_id(),
      type = "3x3",
      rule = rule,
      shape = shape,
      cells = cells,
      options = options,
      correct = which(perm == 1L),
      n_options = length(options),
      difficulty = NA_integer_,
      meta = c(meta, list(distractor_tiers = tiers[perm]))
    ),
    class = "cog_item"
  )
  item$difficulty <- calibrate_difficulty(item)
  item
}

#' Construct a matrix item by hand
#'
#' Builds a `cog_item` from explicit cell and option configurations, for
#' example to encode a hand-designed item or to reproduce an item from the
#' literature. Generated items are produced by [generate_items()].
#'
#' @param cells A list of three rows, each a list of three [cog_shape()]
#'   objects. The bottom-right cell holds the correct answer (it is drawn as
#'   "?" and never shown).
#' @param options A list of [cog_shape()] objects; the response options.
#' @param correct Integer position of the correct option in `options`.
#' @param rule,shape Labels for the relational rule and figural carrier.
#'   Any string is accepted; rules outside [cog_rules()] receive a neutral
#'   difficulty load.
#' @param id Optional identifier.
#' @param meta Optional list of design features recorded in
#'   [item_features()].
#' @return An object of class `cog_item`.
#' @examples
#' base <- cog_shape("polygon", list(sides = 4))
#' cells <- lapply(1:3, function(r) lapply(1:3, function(c) base))
#' opts <- list(base, cog_shape("polygon", list(sides = 5)),
#'              cog_shape("polygon", list(sides = 6)),
#'              cog_shape("polygon", list(sides = 3)))
#' it <- cog_item(cells, opts, correct = 1, rule = "identity", shape = "polygon")
#' is_item_valid(it)
#' @export
cog_item <- function(cells, options, correct, rule, shape, id = NULL, meta = list()) {
  stopifnot(is.list(cells), length(cells) == 3)
  for (row in cells) {
    stopifnot(is.list(row), length(row) == 3)
    for (s in row) if (!inherits(s, "cog_shape")) stop("cells must contain cog_shape objects")
  }
  stopifnot(is.list(options), length(options) >= 2)
  for (s in options) if (!inherits(s, "cog_shape")) stop("options must contain cog_shape objects")
  correct <- as.integer(correct)
  stopifnot(length(correct) == 1, correct >= 1, correct <= length(options))
  item <- structure(
    list(
      id = id %||% next_item_id(),
      type = "3x3",
      rule = as.character(rule),
      shape = as.character(shape),
      cells = cells,
      options = options,
      correct = correct,
      n_options = length(options),
      difficulty = NA_integer_,
      meta = meta
    ),
    class = "cog_item"
  )
  item$difficulty <- calibrate_difficulty(item)
  item
}

# Identity: all nine cells identical ------------------------------------------
gen_identity <- function(shape, n_options) {
  target <- random_base_shape(shape)
  cells <- lapply(1:3, function(r) lapply(1:3, function(c) target))
  distractors <- make_distinct_distractors(target, list(), n_options - 1)
  new_item("identity", shape, cells, target, distractors)
}

# Distribution of three: Latin square of three variants -----------------------
gen_dist3 <- function(shape, n_options) {
  v <- random_variants(shape)
  cells <- list(
    list(v[[1]], v[[2]], v[[3]]),
    list(v[[2]], v[[3]], v[[1]]),
    list(v[[3]], v[[1]], v[[2]])
  )
  correct <- v[[2]]
  siblings <- list(v[[1]], v[[3]])
  distractors <- make_distinct_distractors(correct, siblings, n_options - 1)
  new_item("dist-of-3", shape, cells, correct, distractors,
           meta = list(axis = attr(v, "axis")))
}

# Distribution of two: two variants plus a blank cell per row/column ----------
gen_dist2 <- function(shape, n_options) {
  if (!(shape %in% DIST_OF_2_SHAPES)) {
    stop(sprintf("dist-of-2 is not supported for shape '%s'", shape), call. = FALSE)
  }
  v <- random_variants(shape)
  A <- v[[1]]
  B <- v[[2]]
  cells <- lapply(0:2, function(r) {
    lapply(0:2, function(c) {
      slot <- (c - r) %% 3
      if (slot == 0) A else if (slot == 1) B else blank_cell()
    })
  })
  distractors <- make_distinct_distractors(A, list(B), n_options - 1)
  new_item("dist-of-2", shape, cells, A, distractors,
           meta = list(axis = attr(v, "axis")))
}

# Progression: primary axis along columns, secondary axis along rows ----------
pick_primary_progression <- function(shape) {
  switch(
    shape,
    annulus = {
      start <- rint(1, 2)
      list(kind = "param", name = "ringCount", values = start + 0:2)
    },
    dice = {
      step <- rpick(c(1, 2, 3))
      start <- rint(1, 9 - 2 * step)
      list(kind = "param", name = "dotCount", values = start + step * 0:2)
    },
    polygon = {
      start <- rint(3, 6)
      list(kind = "param", name = "sides", values = start + 0:2)
    },
    star = {
      start <- rint(4, 6)
      list(kind = "param", name = "points", values = start + 0:2)
    },
    arrow = , hammer = {
      step <- rpick(c(45, 90))
      start <- rpick(c(0, 45, 90, 135))
      list(kind = "attr", name = "rotation", values = (start + step * 0:2) %% 360)
    },
    petals = {
      start <- rint(3, 6)
      list(kind = "param", name = "petalCount", values = start + 0:2)
    },
    "spike-ring" = {
      step <- rpick(c(1, 2))
      start <- rint(4, 8 - 2 * step)
      list(kind = "param", name = "spikeCount", values = start + step * 0:2)
    },
    bars = {
      start <- rint(1, 4)
      list(kind = "param", name = "barCount", values = start + 0:2)
    },
    "grid-dots" = {
      start <- rint(1, 2)
      list(kind = "param", name = "rows", values = start + 0:2)
    },
    checkerboard = list(kind = "param", name = "rows", values = c(2, 3, 4)),
    "box-lines" = list(kind = "attr", name = "size", values = c(0.5, 0.7, 0.9)),
    "nested-polygon" = {
      start <- rint(3, 6)
      list(kind = "param", name = "outerSides", values = start + 0:2)
    },
    "sector-pie" = {
      start <- rint(2, 6)
      list(kind = "param", name = "sectorCount", values = start + 0:2)
    },
    "block-letter" = list(kind = "param", name = "patternIndex", values = rsample(0:7, 3)),
    stop(sprintf("No progression axis for shape '%s'", shape), call. = FALSE)
  )
}

pick_secondary_progression <- function(shape) {
  which_axis <- rpick(SECONDARY_AXES[[shape]])
  if (which_axis == "size") {
    # smallest step starts at 0.6: at 0.5 the finest carriers (3 vs 4 rings)
    # no longer separate perceptibly
    list(kind = "attr", name = "size", values = c(0.6, 0.76, 0.92))
  } else {
    list(kind = "attr", name = "strokeWidth", values = c(1, 3, 6))
  }
}

apply_axis <- function(s, axis, value) {
  if (axis$kind == "param") {
    s$params[[axis$name]] <- value
  } else {
    s[[axis$name]] <- value
  }
  s
}

gen_progression <- function(shape, n_options) {
  base <- random_base_shape(shape)
  primary <- pick_primary_progression(shape)
  secondary <- pick_secondary_progression(shape)
  if (primary$kind == "attr" && secondary$kind == "attr" && primary$name == secondary$name) {
    secondary <- list(kind = "attr", name = "strokeWidth", values = c(1, 3, 6))
  }
  cells <- lapply(1:3, function(r) {
    lapply(1:3, function(c) {
      cell <- apply_axis(base, primary, primary$values[c])
      apply_axis(cell, secondary, secondary$values[r])
    })
  })
  correct <- cells[[3]][[3]]
  siblings <- list(
    tag_tier(cells[[3]][[2]], "neighbor-col"),
    tag_tier(cells[[2]][[3]], "neighbor-row"),
    tag_tier(cells[[2]][[2]], "neighbor-diag")
  )
  distractors <- make_distinct_distractors(correct, siblings, n_options - 1)
  new_item("progression", shape, cells, correct, distractors,
           meta = list(primary_axis = primary$name, secondary_axis = secondary$name))
}

# Rotation: sliding window rotation(r, c) = start + (r + c) * step -------------
gen_rotation <- function(shape, n_options) {
  probe <- random_base_shape(shape)
  if (rotation_symmetry_fold(probe) >= 4) {
    stop(sprintf("Rotation rule requires an asymmetric shape, got '%s'", shape), call. = FALSE)
  }
  step <- rpick(c(45, 60, 90))
  start <- rpick(c(0, 15, 30, 45, 60, 90))
  cells <- lapply(0:2, function(r) {
    lapply(0:2, function(c) shape_set(probe, rotation = (start + (r + c) * step) %% 360))
  })
  correct <- cells[[3]][[3]]
  rot <- correct$rotation
  pool <- list(
    tag_tier(shape_set(correct, rotation = (rot + step) %% 360), "next-step"),
    tag_tier(shape_set(correct, rotation = (rot - step) %% 360), "prev-step"),
    tag_tier(shape_set(correct, rotation = (rot + 180) %% 360), "opposite"),
    tag_tier(shape_set(correct, rotation = (rot + 90) %% 360), "perpendicular"),
    tag_tier(shape_set(correct, rotation = (rot + 2 * step) %% 360), "overshoot")
  )
  distractors <- make_distinct_distractors(correct, pool, n_options - 1)
  new_item("rotation", shape, cells, correct, distractors,
           meta = list(step = step, start = start))
}

# Boolean logic on bit-mask carriers ------------------------------------------
apply_bool_op <- function(op, a, b, mask) {
  a <- as.integer(a)
  b <- as.integer(b)
  mask <- as.integer(mask)
  switch(
    op,
    and = bitwAnd(bitwAnd(a, b), mask),
    or = bitwAnd(bitwOr(a, b), mask),
    xor = bitwAnd(bitwXor(a, b), mask),
    xnor = bitwAnd(bitwNot(bitwXor(a, b)), mask)
  )
}

gen_bool <- function(shape, op, n_options) {
  if (!(shape %in% BOOL_SHAPES)) {
    stop(sprintf("Boolean rules are not supported for shape '%s'", shape), call. = FALSE)
  }
  base <- random_base_shape(shape)
  if (shape == "sector-pie") {
    bit_count <- clamp(round(base$params$sectorCount %||% 6), 3, 6)
    base$params$sectorCount <- bit_count
  } else {
    rows <- clamp(round(base$params$rows %||% 3), 2, 3)
    cols <- clamp(round(base$params$cols %||% 3), 2, 3)
    bit_count <- rows * cols
    base$params$rows <- rows
    base$params$cols <- cols
  }
  full <- 2^bit_count - 1

  rows_abc <- list()
  keys <- character(0)
  attempts <- 0
  while (length(rows_abc) < 3 && attempts < 200) {
    attempts <- attempts + 1
    a <- rint(1, full - 1)
    b <- rint(1, full - 1)
    cc <- apply_bool_op(op, a, b, full)
    if (cc == a || cc == b) next
    if (cc == 0 || cc == full) next
    key <- paste(a, b)
    if (key %in% keys) next
    keys <- c(keys, key)
    rows_abc[[length(rows_abc) + 1]] <- c(a, b, cc)
  }
  if (length(rows_abc) < 3) {
    stop(sprintf("Could not find 3 distinct rows for %s on %s", op, shape), call. = FALSE)
  }

  puzzle_pattern <- if (shape == "sector-pie") rint(1, 7) else 1
  make_shape <- function(mask) {
    cell <- base
    if (shape == "sector-pie") {
      cell$params$sectorPatterns <- fill_mask_to_patterns(mask, bit_count, puzzle_pattern)
    } else {
      cell$params$pattern <- as.numeric(mask)
    }
    cell
  }

  cells <- lapply(rows_abc, function(row) lapply(row, make_shape))
  correct <- cells[[3]][[3]]
  a2 <- rows_abc[[3]][1]
  b2 <- rows_abc[[3]][2]
  c2 <- rows_abc[[3]][3]

  pool <- list()
  if (a2 != c2) pool[[length(pool) + 1]] <- tag_tier(make_shape(a2), "operand")
  if (b2 != c2 && b2 != a2) pool[[length(pool) + 1]] <- tag_tier(make_shape(b2), "operand")
  for (wo in setdiff(c("and", "or", "xor", "xnor"), op)) {
    w <- apply_bool_op(wo, a2, b2, full)
    if (w != c2 && w != 0 && w != full) pool[[length(pool) + 1]] <- tag_tier(make_shape(w), "wrong-op")
  }
  for (bit in 0:(bit_count - 1)) {
    second <- (bit + 1) %% bit_count
    flipped <- bitwXor(bitwXor(as.integer(c2), bitwShiftL(1L, as.integer(bit))),
                       bitwShiftL(1L, as.integer(second)))
    if (flipped != 0 && flipped != full && flipped != c2) {
      pool[[length(pool) + 1]] <- tag_tier(make_shape(flipped), "bit-flip")
    }
  }

  distractors <- make_distinct_distractors(correct, pool, n_options - 1)
  new_item(op, shape, cells, correct, distractors,
           meta = list(operator = op, bit_count = bit_count,
                       operands = sprintf("%d,%d,%d", a2, b2, c2)))
}

# Mirror: 180-degree flip of row 1 into row 3 (or col 1 into col 3) -----------
gen_mirror <- function(shape, n_options) {
  if (!(shape %in% MIRROR_SHAPES)) {
    stop(sprintf("mirror rule is not supported for shape '%s'", shape), call. = FALSE)
  }
  base <- random_base_shape(shape)
  axis <- rpick(c("horizontal", "vertical"))
  rotations <- rsample(seq(0, 315, by = 45), 3)
  base_row <- lapply(rotations, function(r) shape_set(base, rotation = r))
  flip <- function(s) shape_set(s, rotation = (s$rotation + 180) %% 360)

  if (axis == "horizontal") {
    cells <- list(base_row, base_row, lapply(base_row, flip))
  } else {
    cells <- lapply(1:3, function(r) list(base_row[[r]], base_row[[r]], flip(base_row[[r]])))
  }
  correct <- cells[[3]][[3]]
  rot <- correct$rotation
  pool <- list(
    tag_tier(shape_set(correct, rotation = (rot + 180) %% 360), "unmirrored"),
    tag_tier(shape_set(correct, rotation = (rot + 90) %% 360), "wrong-axis"),
    tag_tier(shape_set(correct, rotation = (rot + 45) %% 360), "off-45")
  )
  distractors <- make_distinct_distractors(correct, pool, n_options - 1)
  new_item("mirror", shape, cells, correct, distractors, meta = list(mirror_axis = axis))
}

# Arithmetic on the primary count parameter -----------------------------------
apply_arith_op <- function(op, a, b) {
  switch(op, addition = a + b, subtraction = a - b, multiplication = a * b)
}

sample_arith_rows <- function(op, min, max) {
  valid <- list()
  for (a in min:max) {
    for (b in min:max) {
      cc <- apply_arith_op(op, a, b)
      if (cc < min || cc > max) next
      if (a == b || a == cc || b == cc) next
      valid[[length(valid) + 1]] <- c(a, b, cc)
    }
  }
  if (length(valid) < 3) return(list())
  rsample(valid, 3)
}

gen_arith <- function(shape, op, n_options) {
  spec <- PRIMARY_PARAM[[shape]]
  if (is.null(spec)) {
    stop(sprintf("Arithmetic rules need a count-parameterised shape, not '%s'", shape),
         call. = FALSE)
  }
  max_count <- min(spec$max, ARITH_PERCEPTIBLE_MAX[shape], na.rm = TRUE)
  rows <- sample_arith_rows(op, spec$min, max_count)
  if (length(rows) < 3) {
    stop(sprintf("Cannot find 3 distinct %s rows for %s", op, shape), call. = FALSE)
  }
  base <- random_base_shape(shape)
  with_count <- function(v) shape_param(base, spec$name, v)
  cells <- lapply(rows, function(row) lapply(row, with_count))
  correct <- cells[[3]][[3]]
  a2 <- rows[[3]][1]
  b2 <- rows[[3]][2]
  c2 <- rows[[3]][3]

  pool <- list()
  push <- function(cfg, tier) {
    if (!count_confusable(correct, cfg)) pool[[length(pool) + 1]] <<- tag_tier(cfg, tier)
  }
  if (a2 != c2) push(cells[[3]][[1]], "operand")
  if (b2 != c2 && b2 != a2) push(cells[[3]][[2]], "operand")
  if (c2 + 1 <= max_count) push(with_count(c2 + 1), "off-by-one")
  if (c2 - 1 >= spec$min) push(with_count(c2 - 1), "off-by-one")
  wrong_op <- rpick(setdiff(c("addition", "subtraction", "multiplication"), op))
  wrong_val <- apply_arith_op(wrong_op, a2, b2)
  if (wrong_val >= spec$min && wrong_val <= max_count && wrong_val != c2) {
    push(with_count(wrong_val), "wrong-op")
  }

  distractors <- make_distinct_distractors(correct, pool, n_options - 1)
  new_item(op, shape, cells, correct, distractors,
           meta = list(operator = op, operands = sprintf("%d,%d,%d", a2, b2, c2),
                       count_max = max_count))
}

# Dispatcher ------------------------------------------------------------------
generate_one <- function(shape, rule, n_options) {
  switch(
    rule,
    identity = gen_identity(shape, n_options),
    "dist-of-3" = gen_dist3(shape, n_options),
    "dist-of-2" = gen_dist2(shape, n_options),
    progression = gen_progression(shape, n_options),
    rotation = gen_rotation(shape, n_options),
    addition = , subtraction = , multiplication = gen_arith(shape, rule, n_options),
    mirror = gen_mirror(shape, n_options),
    and = , or = , xor = , xnor = gen_bool(shape, rule, n_options),
    stop(sprintf("Unknown rule '%s'", rule), call. = FALSE)
  )
}
