#' Check that an item is solvable and its options are visually distinct
#'
#' An item passes when (a) every response option has a different
#' [visual_signature()], (b) no two options differ only by an imperceptibly
#' small count (for example 15 versus 16 spikes) or by a single cell of a
#' large checkerboard, (c) no two differing cells of the same row (and, for
#' rules that also run down the columns, of the same column) differ only by
#' an imperceptibly small count, (d) the keyed option reproduces the missing
#' cell and (e) the rule is visible in the matrix (three distinct cells in
#' the first row of a distribution item, both axes visible in a progression
#' item, and so on). [generate_items()] discards items that fail this check.
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

  row_sigs <- function(r) vapply(item$cells[[r]], visual_signature, character(1))
  col_sigs <- function(c) vapply(1:3, function(r) visual_signature(item$cells[[r]][[c]]), character(1))

  if (item$rule == "dist-of-3") {
    if (length(unique(row_sigs(1))) != 3) return(FALSE)
  }
  if (item$rule == "dist-of-2") {
    for (r in 1:3) {
      row <- item$cells[[r]]
      blanks <- vapply(row, is_blank_cell, logical(1))
      if (sum(blanks) != 1) return(FALSE)
      visible <- vapply(row[!blanks], visual_signature, character(1))
      if (length(visible) != 2 || length(unique(visible)) != 2) return(FALSE)
    }
  }
  if (item$rule == "progression") {
    if (length(unique(row_sigs(1))) != 3) return(FALSE)
    if (length(unique(col_sigs(1))) != 3) return(FALSE)
  }
  if (item$rule %in% c("addition", "subtraction", "multiplication")) {
    for (r in 1:3) if (length(unique(row_sigs(r))) != 3) return(FALSE)
  }
  if (item$rule == "mirror") {
    row_visible <- any(row_sigs(3) != row_sigs(1))
    col_visible <- any(col_sigs(3) != col_sigs(1))
    if (!row_visible && !col_visible) return(FALSE)
  }
  TRUE
}
