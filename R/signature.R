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

#' Visual signature of a shape configuration
#'
#' Two configurations that produce the same picture for a human observer get
#' the same signature. Rotation is normalised by the shape's rotational
#' symmetry (an annulus looks the same at any angle, a square every 90
#' degrees, an arrow never), so visually identical response options can be
#' detected and eliminated during generation. This is the mechanism behind
#' the package's option-uniqueness guarantee.
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
  nms <- sort(names(s$params))
  params <- paste0(nms, "=", vapply(nms, function(k) fmt_num(s$params[[k]]), ""),
                   collapse = ",")
  fold <- rotation_symmetry_fold(s)
  eff_rot <- if (fold >= 360) 0 else round(s$rotation %% (360 / fold))
  fill <- if (is.null(s$fill) || is.na(s$fill) || !attribute_visible(s, "fill")) "none" else s$fill
  sw <- if (attribute_visible(s, "strokeWidth")) fmt_num(s$strokeWidth) else "NA"
  sprintf("%s(sz=%s,rot=%s,sw=%s,stk=%s,fill=%s,%s)",
          s$kind, fmt_num(round2(s$size)), fmt_num(eff_rot),
          sw, s$stroke, fill, params)
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
    checkerboard = 1,
    "box-lines" = 1,
    "nested-polygon" = {
      outer <- max(3, round(p$outerSides %||% 4))
      inner <- max(3, round(p$innerSides %||% 4))
      gcd(outer, inner)
    },
    "sector-pie" = 1,
    "block-letter" = 1,
    1
  )
}

gcd <- function(a, b) {
  if (b == 0) return(max(1, a))
  gcd(b, a %% b)
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

# Minimum count difference that is still perceptible at cell size.
min_perceptible_count_delta <- function(kind, hi) {
  switch(
    kind,
    petals = , "spike-ring" = if (hi >= 10) 3 else if (hi >= 7) 2 else 1,
    star = , polygon = , "nested-polygon" = , "sector-pie" = if (hi >= 7) 2 else 1,
    dice = if (hi >= 7) 2 else 1,
    bars = if (hi >= 6) 2 else 1,
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
count_confusable <- function(a, b) {
  if (a$kind != b$kind) return(FALSE)
  spec <- PRIMARY_PARAM[[a$kind]]
  if (is.null(spec)) return(FALSE)
  av <- round(a$params[[spec$name]] %||% spec$min)
  bv <- round(b$params[[spec$name]] %||% spec$min)
  if (av == bv) return(FALSE)
  if (count_stripped_signature(a) != count_stripped_signature(b)) return(FALSE)
  abs(av - bv) < min_perceptible_count_delta(a$kind, max(av, bv))
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

# Nominally different but not reliably distinguishable by eye (symbolic rules).
confusable <- function(a, b) {
  if (visual_signature(a) == visual_signature(b)) return(FALSE)
  if (is_stack(a) && is_stack(b)) {
    # two stacks are confusable when every layer is identical or confusable
    return(all(vapply(1:2, function(k) {
      x <- a$layers[[k]]
      y <- b$layers[[k]]
      visual_signature(x) == visual_signature(y) || confusable(x, y)
    }, logical(1))))
  }
  count_confusable(a, b) || pattern_confusable(a, b)
}
