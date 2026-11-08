#' Check that an item is solvable and its options are visually distinct
#'
#' An item passes when (a) every response option has a different
#' [visual_signature()], (b) no two options differ only by an imperceptibly
#' small count (for example 15 versus 16 spikes) or by a single cell of a
#' large checkerboard, (c) no two differing cells of the same row (and, for
#' rules that also run down the columns, of the same column) differ only by
#' an imperceptibly small count, (d) the keyed option reproduces the missing
#' cell, (e) the rule is visible in the matrix (three distinct cells in every
#' row of a distribution item, both axes visible in a progression item) and
#' (f) the rule relation itself holds: identical cells for identity, a Latin
#' square for distribution of three, ordered axes for progression, constant
#' steps for rotation, the flipped row or column for mirror and `a op b = c`
#' in every row for the arithmetic and bitwise rules, so hand-made items are
#' verified too. [generate_items()] discards items that fail this check.
#' These rules are symbolic; the renderer-based [audit_items()] complements
#' them.
#'
#' @param item A `cog_item`.
#' @param n_options Optional required number of options.
#' @return `TRUE` or `FALSE`.
#' @export
is_item_valid <- function(item, n_options = NULL) {
  if (!inherits(item, "cog_item")) return(FALSE)
  opts <- item$options
  if (!is.null(n_options) && length(opts) < n_options) return(FALSE)
  if (length(opts) < 2) return(FALSE)

  sigs <- vapply(opts, visual_signature, character(1))
  if (anyDuplicated(sigs)) return(FALSE)
  n <- length(opts)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (confusable(opts[[i]], opts[[j]])) return(FALSE)
    }
  }

  target <- item$cells[[3]][[3]]
  if (is_blank_cell(target)) return(FALSE)
  if (visual_signature(opts[[item$correct]]) != visual_signature(target)) return(FALSE)

  # Cells that differ must differ perceptibly in count within every row,
  # otherwise the rule itself is invisible (e.g. 15 vs 16 spikes in a row).
  # Columns carry the rule too, except for the row-wise arithmetic and
  # logic rules, whose column values are unrelated.
  cell_pair_ok <- function(a, b) {
    is_blank_cell(a) || is_blank_cell(b) || !count_confusable(a, b)
  }
  for (r in 1:3) for (i in 1:2) for (j in (i + 1):3) {
    if (!cell_pair_ok(item$cells[[r]][[i]], item$cells[[r]][[j]])) return(FALSE)
  }
  if (!(item$rule %in% ROW_ONLY_RULES)) {
    for (c in 1:3) for (i in 1:2) for (j in (i + 1):3) {
      if (!cell_pair_ok(item$cells[[i]][[c]], item$cells[[j]][[c]])) return(FALSE)
    }
  }

  rule_satisfied(item)
}

# Does the matrix instantiate its rule? Checked from the cells (plus the
# generator's axis names when present), so hand-made items are verified too.
# Two-layer items are checked layer by layer. Unknown rules are not checked.
rule_satisfied <- function(item) {
  if (identical(item$type, "layered")) {
    for (k in 1:2) {
      lay <- if (k == 1) item$meta$outer else item$meta$inner
      if (is.null(lay)) return(FALSE)
      cells <- lapply(item$cells, function(row) lapply(row, function(s) s$layers[[k]]))
      if (!rule_holds(lay$rule, lay$shape, cells, lay)) return(FALSE)
    }
    return(TRUE)
  }
  rule_holds(item$rule, item$shape, item$cells, item$meta %||% list())
}

rule_holds <- function(rule, shape, cells, meta = list()) {
  sig <- visual_signature
  at <- function(r, c) cells[[r]][[c]]
  row_sigs <- function(r) vapply(cells[[r]], sig, character(1))
  col_sigs <- function(c) vapply(1:3, function(r) sig(at(r, c)), character(1))
  switch(
    rule,
    identity = length(unique(unlist(lapply(1:3, row_sigs)))) == 1,
    "dist-of-3" = {
      sets <- lapply(1:3, function(r) sort(row_sigs(r)))
      all(lengths(lapply(sets, unique)) == 3) &&
        identical(sets[[1]], sets[[2]]) && identical(sets[[1]], sets[[3]]) &&
        all(vapply(1:3, function(c) length(unique(col_sigs(c))) == 3, logical(1)))
    },
    "dist-of-2" = {
      visible <- lapply(1:3, function(r) {
        row <- cells[[r]]
        blank <- vapply(row, is_blank_cell, logical(1))
        if (sum(blank) != 1) return(character(0))
        sort(vapply(row[!blank], sig, character(1)))
      })
      all(lengths(visible) == 2) && all(lengths(lapply(visible, unique)) == 2) &&
        identical(visible[[1]], visible[[2]]) && identical(visible[[1]], visible[[3]])
    },
    progression = progression_holds(cells, meta),
    rotation = {
      rot <- function(r, c) at(r, c)$rotation %% 360
      steps <- c(vapply(1:3, function(r) (rot(r, 2) - rot(r, 1)) %% 360, numeric(1)),
                 vapply(1:3, function(r) (rot(r, 3) - rot(r, 2)) %% 360, numeric(1)),
                 vapply(1:3, function(c) (rot(2, c) - rot(1, c)) %% 360, numeric(1)),
                 vapply(1:3, function(c) (rot(3, c) - rot(2, c)) %% 360, numeric(1)))
      same_otherwise <- length(unique(unlist(lapply(1:3, function(r) {
        vapply(cells[[r]], function(s) sig(shape_set(s, rotation = 0)), character(1))
      })))) == 1
      same_otherwise && length(unique(round(steps, 6))) == 1 && steps[1] != 0
    },
    mirror = {
      flip_sig <- function(s) sig(shape_set(s, rotation = (s$rotation + 180) %% 360))
      horizontal <- identical(row_sigs(1), row_sigs(2)) &&
        identical(vapply(cells[[1]], flip_sig, character(1)), row_sigs(3))
      vertical <- identical(col_sigs(1), col_sigs(2)) &&
        identical(vapply(1:3, function(r) flip_sig(at(r, 1)), character(1)), col_sigs(3))
      horizontal || vertical
    },
    addition = , subtraction = , multiplication = {
      spec <- PRIMARY_PARAM[[shape]]
      if (is.null(spec)) return(FALSE)
      val <- function(r, c) {
        v <- at(r, c)$params[[spec$name]]
        if (is.null(v)) NA_real_ else round(as.numeric(v))
      }
      all(vapply(1:3, function(r) {
        v <- c(val(r, 1), val(r, 2), val(r, 3))
        !anyNA(v) && length(unique(v)) == 3 && apply_arith_op(rule, v[1], v[2]) == v[3]
      }, logical(1)))
    },
    and = , or = , xor = , xnor = {
      mask <- function(s) {
        p <- s$params
        if (s$kind == "sector-pie") {
          n <- round(p$sectorCount %||% 6)
          packed <- p$sectorPatterns %||%
            (if (!is.null(p$fillMask)) fill_mask_to_patterns(p$fillMask, n, 1) else 0)
          c(patterns_to_fill_mask(packed, n), n)
        } else if (s$kind == "checkerboard") {
          c(round(p$pattern %||% 0), round(p$rows %||% 3) * round(p$cols %||% 3))
        } else {
          NULL
        }
      }
      all(vapply(1:3, function(r) {
        m <- lapply(1:3, function(c) mask(at(r, c)))
        if (any(vapply(m, is.null, logical(1)))) return(FALSE)
        bits <- m[[1]][2]
        if (m[[2]][2] != bits || m[[3]][2] != bits) return(FALSE)
        apply_bool_op(rule, m[[1]][1], m[[2]][1], 2^bits - 1) == m[[3]][1]
      }, logical(1)))
    },
    TRUE
  )
}

# Progression: every row and column shows three different cells; when the
# generator recorded the axes, the primary attribute must change in order
# along the columns (and nothing else), the secondary one along the rows.
progression_holds <- function(cells, meta) {
  sig <- visual_signature
  row_sigs <- function(r) vapply(cells[[r]], sig, character(1))
  col_sigs <- function(c) vapply(1:3, function(r) sig(cells[[r]][[c]]), character(1))
  distinct <- all(vapply(1:3, function(r) length(unique(row_sigs(r))) == 3, logical(1))) &&
    all(vapply(1:3, function(c) length(unique(col_sigs(c))) == 3, logical(1)))
  if (!distinct) return(FALSE)
  primary <- meta$primary_axis
  secondary <- meta$secondary_axis
  if (is.null(primary) || is.null(secondary)) return(TRUE)
  attrs <- c("size", "strokeWidth", "rotation")
  value <- function(s, name) {
    v <- if (name %in% attrs) s[[name]] else s$params[[name]]
    if (is.null(v) || length(v) != 1) NA_real_ else as.numeric(v)
  }
  neutral <- function(s, name) {
    if (name %in% attrs) s[[name]] <- if (name == "rotation") 0 else 1 else s$params[[name]] <- 0
    s
  }
  ordered <- function(v, name) {
    if (name == "patternIndex") return(length(unique(v)) == 3)
    if (anyNA(v)) return(FALSE)
    if (name == "rotation") {
      d <- diff(v) %% 360
      return(d[1] != 0 && abs(d[1] - d[2]) < 1e-6)
    }
    d <- diff(v)
    all(d > 1e-9) || all(d < -1e-9)
  }
  for (r in 1:3) {
    if (!ordered(vapply(1:3, function(c) value(cells[[r]][[c]], primary), numeric(1)), primary)) return(FALSE)
    if (length(unique(vapply(cells[[r]], function(s) sig(neutral(s, primary)), character(1)))) != 1) return(FALSE)
  }
  for (c in 1:3) {
    if (!ordered(vapply(1:3, function(r) value(cells[[r]][[c]], secondary), numeric(1)), secondary)) return(FALSE)
    if (length(unique(vapply(1:3, function(r) sig(neutral(cells[[r]][[c]], secondary)), character(1)))) != 1) return(FALSE)
  }
  TRUE
}
