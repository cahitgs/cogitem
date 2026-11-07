#' Calibrated a priori difficulty of an item
#'
#' Difficulty on a 1 (easy) to 5 (hard) scale derived from the item's design
#' features rather than from response data:
#' `raw = rule_load + shape_bonus + option_adjust + instance_adjust`, rounded
#' and clamped to 1-5. `rule_load` ranges from 1 (identity) to 5 (XOR/XNOR),
#' `shape_bonus` adds 1 for visually busy carriers (hammer, checkerboard,
#' nested polygon, sector pie, block letter), `option_adjust` adds 0.5 for six
#' or more options and `instance_adjust` adds 0.5 for logic items with nine
#' or more bits. The components are also reported by [item_features()], so
#' the rating can be validated against empirical (for example Rasch) item
#' difficulties.
#'
#' @param item A `cog_item`.
#' @return An integer between 1 and 5.
#' @export
calibrate_difficulty <- function(item) {
  rule_load <- RULE_LOAD[item$rule]
  if (is.na(rule_load)) rule_load <- 2
  shape_bonus <- SHAPE_BONUS[item$shape]
  if (is.na(shape_bonus)) shape_bonus <- 0
  option_adjust <- if (item$n_options >= 6) 0.5 else 0
  raw <- rule_load + shape_bonus + option_adjust + instance_adjust(item)
  as.integer(clamp(floor(raw + 0.5), 1, 5))
}

instance_adjust <- function(item) {
  if (item$rule %in% c("and", "or", "xor", "xnor")) {
    c0 <- item$cells[[1]][[1]]
    bits <- round(c0$params$rows %||% 0) * round(c0$params$cols %||% 0)
    if (bits == 0) bits <- round(c0$params$sectorCount %||% 0)
    return(if (bits >= 9) 0.5 else 0)
  }
  0
}
