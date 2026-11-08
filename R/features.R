RULE_DERIVED_TIERS <- c(
  "sibling", "neighbor-col", "neighbor-row", "neighbor-diag", "operand",
  "off-by-one", "wrong-op", "bit-flip", "next-step", "prev-step", "opposite",
  "perpendicular", "overshoot", "unmirrored", "wrong-axis", "off-45"
)

# A distractor counts as rule-derived when every manipulation it carries is
# a rule-specific candidate (two-layer tiers look like "inner:x" or "both:x+y").
tier_is_rule_derived <- function(tier) {
  parts <- strsplit(sub("^[a-z]+:", "", tier), "+", fixed = TRUE)[[1]]
  length(parts) > 0 && all(parts %in% RULE_DERIVED_TIERS)
}

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
#' of options, how the distractors were constructed and rule-specific
#' parameters (varying axis,
#' progression axes, rotation step, mirror axis, operator, operands, bit
#' count), plus the number of layers and the shape and rule of each layer of
#' two-layer items ([generate_layered()]). These columns are the natural predictors for a linear logistic test
#' model (LLTM) or an explanatory IRT model of empirical item difficulty.
#'
#' @param x A `cog_item`, a `cog_item_set` or a list of items.
#' @return A data frame.
#' @examples
#' items <- generate_items("sector-pie", "xor", n = 3, seed = 3)
#' item_features(items)[, c("rule", "shape", "bit_count")]
#' @seealso [q_matrix()]
#' @export
item_features <- function(x) {
  items <- as_item_list(x)
  rows <- lapply(items, function(it) {
    tiers <- it$meta$distractor_tiers
    if (is.null(tiers)) tiers <- character(0)
    dtiers <- tiers[tiers != "correct"]
    rule_derived <- vapply(dtiers, tier_is_rule_derived, logical(1))
    m <- it$meta %||% list()
    data.frame(
      id = it$id,
      shape = it$shape,
      rule = it$rule,
      rule_family = if (identical(it$type, "layered")) "layered" else
        unname(RULE_FAMILY[it$rule]) %||% NA_character_,
      n_options = it$n_options,
      correct_position = it$correct,
      n_layers = if (identical(it$type, "layered")) 2L else 1L,
      n_rule_distractors = sum(rule_derived),
      n_perturbation_distractors = sum(!rule_derived),
      distractor_tiers = paste(dtiers, collapse = ";"),
      axis = meta_chr(m, "axis"),
      primary_axis = meta_chr(m, "primary_axis"),
      secondary_axis = meta_chr(m, "secondary_axis"),
      step = meta_num(m, "step"),
      mirror_axis = meta_chr(m, "mirror_axis"),
      operator = meta_chr(m, "operator"),
      operands = meta_chr(m, "operands"),
      bit_count = meta_num(m, "bit_count"),
      outer_shape = meta_chr(m$outer %||% list(), "shape"),
      outer_rule = meta_chr(m$outer %||% list(), "rule"),
      inner_shape = meta_chr(m$inner %||% list(), "shape"),
      inner_rule = meta_chr(m$inner %||% list(), "rule"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(id = character(0), shape = character(0), rule = character(0))
  }
  rownames(out) <- NULL
  out$rule_family[is.na(out$rule_family)] <- "other"
  out
}

#' Q-matrix of item design features
#'
#' Builds a design matrix (items x features) from [item_features()] for an
#' explanatory item response model such as the linear logistic test model
#' (LLTM). Numeric and logical features are kept as they are. A categorical
#' feature is expanded into 0/1 indicator columns: the first categorical
#' feature that is defined for every item keeps one column per level
#' (cell-means coding, as is usual for a Q-matrix); every further complete
#' categorical feature, and every categorical feature when
#' `intercept = TRUE`, drops its first level as the reference level, because
#' the complete indicator sets of two features (or of one feature and the
#' intercept) both sum to one in every row and would be linearly dependent.
#' A missing value means that the attribute does not apply to the item (the
#' rotation step of a distribution item, say) and is coded 0 in every column
#' of that feature; such incomplete features keep all their observed levels.
#' Only additive formulas are supported.
#'
#' The parameters of an LLTM are identified only when the matrix has full
#' column rank, so the function warns when it does not. The usual causes are
#' a feature that is constant in the set (`n_options` in a set generated with
#' a single option count) or two features that are confounded (`shape` and
#' `rule` in a set with one carrier per rule); drop the term or vary the
#' design. `eRm::LLTM()` requires full column rank; `lme4::glmer()` and `TAM`
#' accept the columns as item covariates.
#'
#' @param x A `cog_item_set` (or anything accepted by [item_features()]).
#' @param formula A one-sided additive formula over the columns of
#'   [item_features()].
#' @param intercept Add a column of ones? Every complete categorical feature
#'   is then reference-coded.
#' @return A numeric matrix with item ids as row names.
#' @examples
#' items <- generate_mixed(subset(supported_combinations(), shape == "dice"),
#'                         n_each = 1, seed = 5)
#' Q <- q_matrix(items, ~ rule)
#' dim(Q)
#' q_matrix(items, ~ rule_family + n_rule_distractors)[1:3, ]
#' @export
q_matrix <- function(x, formula = ~ rule + shape, intercept = FALSE) {
  f <- item_features(x)
  tt <- stats::terms(formula)
  if (length(attr(tt, "order")) && any(attr(tt, "order") > 1)) {
    stop("q_matrix() supports additive formulas only (no interactions)", call. = FALSE)
  }
  vars <- attr(tt, "term.labels")
  unknown <- setdiff(vars, names(f))
  if (length(unknown)) {
    stop("Unknown feature(s): ", paste(unknown, collapse = ", "),
         ". See names(item_features(x)).", call. = FALSE)
  }
  cols <- list()
  if (intercept) cols[["(Intercept)"]] <- rep(1, nrow(f))
  complete_seen <- isTRUE(intercept)
  for (v in vars) {
    val <- f[[v]]
    if (is.numeric(val) || is.logical(val)) {
      val <- as.numeric(val)
      val[is.na(val)] <- 0
      cols[[v]] <- val
    } else {
      val <- as.character(val)
      levels <- sort(unique(val[!is.na(val)]))
      complete <- nrow(f) > 0 && !anyNA(val)
      if (complete && complete_seen) levels <- levels[-1]
      if (complete) complete_seen <- TRUE
      for (lv in levels) {
        cols[[paste0(v, lv)]] <- as.numeric(!is.na(val) & val == lv)
      }
    }
  }
  mm <- if (length(cols)) do.call(cbind, cols) else matrix(numeric(0), nrow = nrow(f), ncol = 0)
  rownames(mm) <- f$id
  if (ncol(mm) > 1 && nrow(mm) > 0) {
    rank <- qr(mm)$rank
    if (rank < ncol(mm)) {
      const <- colnames(mm)[apply(mm, 2, function(z) length(unique(z)) == 1)]
      warning(sprintf(paste0("The Q-matrix has %d columns but rank %d, so the parameters of an ",
                             "LLTM would not be identified%s. Drop a term or vary the design."),
                      ncol(mm), rank,
                      if (length(const)) sprintf(" (constant column%s: %s)", if (length(const) > 1) "s" else "",
                                                 paste(const, collapse = ", ")) else ""),
              call. = FALSE)
    }
  }
  mm
}
