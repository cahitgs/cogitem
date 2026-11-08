# Two-layer items: an outer carrier and an inner carrier, each following its
# own rule, superimposed cell by cell (what com() does in matRiks). The outer
# layer is restricted to hollow carriers, pinned to one large size and must
# not use size or fill, so its centre stays free; the inner layer is drawn at
# INNER_SCALE of the cell on a small white disc. The two layers therefore
# never overlap and each keeps its own perceptual guarantees.

OUTER_SHAPES <- c("polygon", "spike-ring")
INNER_SHAPES <- c("dice", "polygon", "star", "arrow", "hammer", "bars", "grid-dots",
                  "checkerboard", "box-lines", "block-letter", "annulus")
LAYER_RULE_EXCLUDED <- "dist-of-2" # blank cells make no sense inside a layer
OUTER_SIZE <- 0.95
INNER_SCALE <- 0.36          # inner layer viewport as a fraction of the cell box
INNER_LWD_BOOST <- 1.5       # inner strokes are drawn a little heavier than pure scaling
KNOCKOUT_RADIUS <- 20        # white disc under the inner layer (cell units, 0-100)
MAX_SPIKE_DEPTH_OUTER <- 0.5 # keeps the ring of an outer spike-ring outside the disc

#' Carriers and rules available for two-layer items
#'
#' `layer_shapes()` lists the shape families that can serve as the outer
#' layer (hollow carriers whose centre stays free) or as the inner layer of a
#' two-layer item; `layer_rules()` lists the rules a layer of the given shape
#' can follow (every rule supported for the shape except distribution of
#' two, whose blank cells make no sense inside a layer).
#'
#' @param layer `"outer"` or `"inner"`.
#' @param shape One of [cog_shapes()].
#' @return A character vector.
#' @examples
#' layer_shapes("outer")
#' layer_rules("dice")
#' @seealso [generate_layered()]
#' @export
layer_shapes <- function(layer = c("outer", "inner")) {
  layer <- match.arg(layer)
  if (layer == "outer") OUTER_SHAPES else INNER_SHAPES
}

#' @rdname layer_shapes
#' @export
layer_rules <- function(shape) {
  shape <- match.arg(shape, SHAPES)
  r <- supported_rules(shape)
  r[!(r %in% LAYER_RULE_EXCLUDED)]
}

# A cell or option of a two-layer item: `layers` holds the outer and the inner
# shape; the remaining fields exist only so that generic code sees a shape.
stack_shape <- function(outer, inner) {
  structure(
    list(kind = "stack", size = 1, rotation = 0, fill = NA_character_,
         stroke = outer$stroke, strokeWidth = outer$strokeWidth, params = list(),
         layers = list(outer, inner)),
    class = "cog_shape"
  )
}

is_stack <- function(s) identical(s$kind, "stack")

all_item_shapes <- function(item) c(do.call(c, item$cells), item$options)

# Grid children of a stacked cell: outer layer, white disc, inner layer.
build_stack <- function(s, lw) {
  outer <- build_shape(s$layers[[1]], lw)
  disc <- circleGrob(0.5, 0.5, r = unit(KNOCKOUT_RADIUS / 100, "snpc"),
                     gp = gpar(fill = PALETTE$cell, col = NA))
  inner <- gTree(children = build_shape(s$layers[[2]], lw * INNER_SCALE * INNER_LWD_BOOST),
                 vp = viewport(width = unit(INNER_SCALE, "snpc"),
                               height = unit(INNER_SCALE, "snpc")))
  do.call(gList, c(as.list(outer), list(disc, inner)))
}

# An outer-layer item is usable when neither its rule nor its distractors rely
# on size or fill (the layer is pinned to one size and keeps its centre free)
# and, for spike rings, the ring stays outside the central disc.
outer_layer_ok <- function(item) {
  m <- item$meta
  if (identical(m$axis, "size") || identical(m$secondary_axis, "size")) return(FALSE)
  if (any(m$distractor_tiers %in% c("size", "fill"))) return(FALSE)
  if (item$shape == "spike-ring") {
    depth <- vapply(all_item_shapes(item), function(s) s$params$spikeDepth %||% 0.35, numeric(1))
    if (any(depth > MAX_SPIKE_DEPTH_OUTER)) return(FALSE)
  }
  TRUE
}

pin_outer_size <- function(item) {
  item$cells <- lapply(item$cells, function(row) lapply(row, shape_set, size = OUTER_SIZE))
  item$options <- lapply(item$options, shape_set, size = OUTER_SIZE)
  item
}

# Superimpose two single-layer items. The keyed option stacks both correct
# answers; distractors are, in this order, wrong in the inner layer, wrong in
# the outer layer and wrong in both (using the next wrong option of each
# layer so that no distractor repeats another), repeated until n_options - 1
# distractors exist.
compose_layered <- function(outer, inner, n_options) {
  cells <- lapply(1:3, function(r) {
    lapply(1:3, function(c) stack_shape(outer$cells[[r]][[c]], inner$cells[[r]][[c]]))
  })
  oc <- outer$options[[outer$correct]]
  ic <- inner$options[[inner$correct]]
  o_wrong <- outer$options[-outer$correct]
  i_wrong <- inner$options[-inner$correct]
  o_tiers <- outer$meta$distractor_tiers[-outer$correct]
  i_tiers <- inner$meta$distractor_tiers[-inner$correct]
  k_o <- length(o_wrong)
  k_i <- length(i_wrong)

  cands <- list()
  tiers <- character(0)
  push <- function(shape, tier) {
    cands[[length(cands) + 1]] <<- shape
    tiers <<- c(tiers, tier)
  }
  need <- function() length(cands) < n_options - 1
  round <- 1L
  while (need() && round <= max(k_o, k_i)) {
    i <- min(round, k_i)
    o <- min(round, k_o)
    push(stack_shape(oc, i_wrong[[i]]), paste0("inner:", i_tiers[i]))
    if (need()) push(stack_shape(o_wrong[[o]], ic), paste0("outer:", o_tiers[o]))
    if (need()) {
      i2 <- min(round + 1L, k_i)
      o2 <- min(round + 1L, k_o)
      push(stack_shape(o_wrong[[o2]], i_wrong[[i2]]),
           paste0("both:", o_tiers[o2], "+", i_tiers[i2]))
    }
    round <- round + 1L
  }

  opts <- c(list(stack_shape(oc, ic)), cands)
  all_tiers <- c("correct", tiers)
  perm <- sample.int(length(opts))
  layer_meta <- function(it) {
    c(list(shape = it$shape, rule = it$rule),
      it$meta[setdiff(names(it$meta), "distractor_tiers")])
  }
  structure(
    list(
      id = next_item_id(),
      type = "layered",
      rule = paste(outer$rule, inner$rule, sep = "+"),
      shape = paste(outer$shape, inner$shape, sep = "+"),
      cells = cells,
      options = opts[perm],
      correct = which(perm == 1L),
      n_options = length(opts),
      meta = list(
        distractor_tiers = all_tiers[perm],
        outer = layer_meta(outer),
        inner = layer_meta(inner),
        row_only = outer$rule %in% ROW_ONLY_RULES || inner$rule %in% ROW_ONLY_RULES
      )
    ),
    class = "cog_item"
  )
}

make_layered_one <- function(outer_spec, inner_spec, n_options) {
  outer <- generate_one(outer_spec[1], outer_spec[2], n_options)
  if (!is_item_valid(outer, n_options) || !outer_layer_ok(outer)) {
    stop("outer layer rejected", call. = FALSE)
  }
  outer <- pin_outer_size(outer)
  inner <- generate_one(inner_spec[1], inner_spec[2], n_options)
  if (!is_item_valid(inner, n_options)) stop("inner layer rejected", call. = FALSE)
  compose_layered(outer, inner, n_options)
}

check_layer_spec <- function(spec, which) {
  if (is.list(spec)) spec <- c(spec$shape %||% spec[[1]], spec$rule %||% spec[[2]])
  spec <- as.character(spec)
  if (length(spec) != 2) {
    stop(sprintf("`%s` must be c(shape, rule)", which), call. = FALSE)
  }
  shapes <- layer_shapes(which)
  if (!(spec[1] %in% shapes)) {
    stop(sprintf("The %s layer shape must be one of: %s", which, paste(shapes, collapse = ", ")),
         call. = FALSE)
  }
  rules <- layer_rules(spec[1])
  if (!(spec[2] %in% rules)) {
    stop(sprintf("Rule '%s' is not available for the %s layer shape '%s'; choose from: %s",
                 spec[2], which, spec[1], paste(rules, collapse = ", ")), call. = FALSE)
  }
  spec
}

#' Generate two-layer items (two rules per item)
#'
#' Builds items in which every cell superimposes two figures, an outer and an
#' inner carrier, each following its own rule, in the way `com()` stacks
#' matrices in matRiks. The outer layer is one of `layer_shapes("outer")`
#' drawn large with a free centre; the inner layer is one of
#' `layer_shapes("inner")` drawn on a small disc in the middle. Both layers
#' are generated and validated like single-layer items ([generate_items()]),
#' then combined cell by cell. Solving the item requires both rules, the
#' classic "number of rules" manipulation of matrix-item difficulty.
#'
#' Response options: the keyed option stacks the two correct answers; the
#' distractors are wrong in the inner layer, wrong in the outer layer or
#' wrong in both, in that order of construction, and are recorded as such in
#' `meta$distractor_tiers` (`"inner:<tier>"`, `"outer:<tier>"`,
#' `"both:<tier>+<tier>"`). All option-uniqueness guarantees of single-layer
#' items apply layer by layer, and the perceptual audit works unchanged.
#'
#' @param outer,inner Character vectors `c(shape, rule)` describing the outer
#'   and the inner layer; see [layer_shapes()] and [layer_rules()].
#' @inheritParams generate_items
#' @return A `cog_item_set` whose items have `type = "layered"`,
#'   `shape = "<outer>+<inner>"` and `rule = "<outer>+<inner>"`.
#' @examples
#' items <- generate_layered(c("polygon", "progression"), c("dice", "dist-of-3"),
#'                           n = 3, seed = 1)
#' items
#' plot(items[[1]], show_answer = TRUE)
#' item_features(items)[, c("outer_rule", "inner_rule", "distractor_tiers")]
#' @seealso [generate_items()], [layer_shapes()], [audit_items()]
#' @export
generate_layered <- function(outer, inner, n = 10, seed = NULL, n_options = 4,
                             max_attempts = NULL, perceptual = FALSE,
                             perceptual_threshold = DEFAULT_PERCEPTUAL_THRESHOLD,
                             progress = NULL) {
  outer <- check_layer_spec(outer, "outer")
  inner <- check_layer_spec(inner, "inner")
  stopifnot(length(n) == 1, n >= 1, n_options >= 2, n_options <= 8)
  n <- as.integer(n)
  n_options <- as.integer(n_options)
  seed <- as.integer(seed %||% random_seed())
  max_attempts <- as.integer(max_attempts %||% max(200, n * 20))

  res <- generate_loop(function() make_layered_one(outer, inner, n_options),
                       n, seed, n_options, max_attempts, perceptual, perceptual_threshold,
                       progress, what = sprintf("%s+%s / %s+%s", outer[1], inner[1], outer[2], inner[2]))

  new_item_set(res$items, meta = list(
    shape = paste(outer[1], inner[1], sep = "+"),
    rule = paste(outer[2], inner[2], sep = "+"),
    layers = list(outer = outer, inner = inner),
    n_requested = n, seed = seed, n_options = n_options,
    duplicates_skipped = res$duplicates, invalid_skipped = res$invalid,
    perceptual = perceptual,
    perceptual_threshold = if (perceptual) perceptual_threshold else NA_real_,
    perceptual_skipped = res$ambiguous, attempts = res$attempts,
    stopped = res$stopped,
    reached_ceiling = length(res$items) < n && !res$stopped
  ))
}
