RULE_DERIVED_TIERS <- c(
  "sibling", "neighbor-col", "neighbor-row", "neighbor-diag", "operand",
  "off-by-one", "wrong-op", "bit-flip", "next-step", "prev-step", "opposite",
  "perpendicular", "overshoot", "unmirrored", "wrong-axis", "off-45"
)

as_item_list <- function(x) {
  if (inherits(x, "cog_item")) return(list(x))
  if (inherits(x, "cog_item_set")) return(unclass(x))
  if (is.list(x) && all(vapply(x, inherits, logical(1), "cog_item"))) return(x)
  stop("x must be a cog_item, a cog_item_set or a list of cog_item objects", call. = FALSE)
}

meta_chr <- function(m, name) {
  v <- m[[name]]
  if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
}

meta_num <- function(m, name) {
  v <- m[[name]]
  if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v[1])
}

#' Design features of items (Q-matrix input)
#'
#' Returns one row per item with the design features that are known at
#' generation time: the rule and its family, the figural carrier, the number
#' of options, the calibrated difficulty and its components, how the
#' distractors were constructed and rule-specific parameters (varying axis,
#' progression axes, rotation step, mirror axis, operator, operands, bit
#' count). These columns are the natural predictors for a linear logistic test
#' model (LLTM) or an explanatory IRT model of empirical item difficulty.
#'
#' @param x A `cog_item`, a `cog_item_set` or a list of items.
#' @return A data frame.
#' @examples
#' items <- generate_items("sector-pie", "xor", n = 3, seed = 3)
#' item_features(items)[, c("rule", "shape", "difficulty", "bit_count")]
#' @seealso [q_matrix()]
#' @export
item_features <- function(x) {
  items <- as_item_list(x)
  rows <- lapply(items, function(it) {
    tiers <- it$meta$distractor_tiers
    if (is.null(tiers)) tiers <- character(0)
    dtiers <- tiers[tiers != "correct"]
    m <- it$meta %||% list()
    data.frame(
      id = it$id,
      shape = it$shape,
      rule = it$rule,
      rule_family = unname(RULE_FAMILY[it$rule]) %||% NA_character_,
      n_options = it$n_options,
      correct_position = it$correct,
      difficulty = it$difficulty,
      rule_load = unname(RULE_LOAD[it$rule]) %||% NA_real_,
      shape_bonus = unname(SHAPE_BONUS[it$shape]) %||% NA_real_,
      n_rule_distractors = sum(dtiers %in% RULE_DERIVED_TIERS),
      n_perturbation_distractors = sum(!(dtiers %in% RULE_DERIVED_TIERS)),
      distractor_tiers = paste(dtiers, collapse = ";"),
      axis = meta_chr(m, "axis"),
      primary_axis = meta_chr(m, "primary_axis"),
      secondary_axis = meta_chr(m, "secondary_axis"),
      step = meta_num(m, "step"),
      mirror_axis = meta_chr(m, "mirror_axis"),
      operator = meta_chr(m, "operator"),
      operands = meta_chr(m, "operands"),
      bit_count = meta_num(m, "bit_count"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(id = character(0), shape = character(0), rule = character(0))
  }
  rownames(out) <- NULL
  out$rule_family[is.na(out$rule_family)] <- "other"
  out$rule_load[is.na(out$rule_load)] <- 2
  out$shape_bonus[is.na(out$shape_bonus)] <- 0
  out
}

#' Q-matrix of item design features
#'
#' Builds a design matrix (items x features) from [item_features()] with
#' [stats::model.matrix()]. The default formula codes rule and shape as dummy
#' variables; any combination of feature columns can be used, for example
#' `~ rule_family + shape_bonus + n_options`. The result can be passed to
#' LLTM software such as `eRm::LLTM()` (after dropping the intercept) or used
#' as item covariates in `lme4::glmer()` / `TAM`.
#'
#' @param x A `cog_item_set` (or anything accepted by [item_features()]).
#' @param formula A one-sided formula over the columns of [item_features()].
#' @param intercept Keep the intercept column?
#' @return A numeric matrix with item ids as row names.
#' @examples
#' items <- generate_mixed(subset(supported_combinations(), shape == "dice"),
#'                         n_each = 1, seed = 5)
#' Q <- q_matrix(items, ~ rule)
#' dim(Q)
#' @export
q_matrix <- function(x, formula = ~ rule + shape, intercept = FALSE) {
  f <- item_features(x)
  mm <- stats::model.matrix(formula, data = f)
  if (!intercept) mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  rownames(mm) <- f$id
  mm
}
