# Two-layer items: an outer carrier and an inner carrier, each following its
# own rule, superimposed cell by cell (what com() does in matRiks). The outer
# layer is restricted to hollow carriers, pinned to one large size and must
# not use size or fill, so its centre stays free; the inner layer is drawn at
# INNER_SCALE of the cell on a small white disc. The two layers therefore
# never overlap and each keeps its own distinctness checks.

OUTER_SHAPES <- c("polygon", "spike-ring")
INNER_SHAPES <- c("dice", "polygon", "star", "arrow", "hammer", "bars", "grid-dots",
                  "checkerboard", "box-lines", "block-letter", "annulus")
LAYER_RULE_EXCLUDED <- "dist-of-2" # blank cells make no sense inside a layer
OUTER_SIZE <- 0.95
INNER_SCALE <- 0.36          # inner layer viewport as a fraction of the cell box
INNER_LWD_BOOST <- 1.5       # inner strokes are drawn a little heavier than pure scaling
KNOCKOUT_RADIUS <- 20        # white disc under the inner layer (cell units, 0-100) at INNER_SCALE
INNER_CLEARANCE <- 0.636     # half diagonal of the drawn inner content per unit of viewport side
MAX_SPIKE_DEPTH_OUTER <- 0.5 # keeps the ring of an outer spike-ring outside the disc
LAYER_OPTIONS <- 4L          # options each layer is generated with (at most)
LAYER_TRIES <- 8L            # draws of a layer before an attempt is given up

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
new_stack <- function(outer, inner, inner_scale = INNER_SCALE) {
  structure(
    list(kind = "stack", size = 1, rotation = 0, fill = NA_character_,
         stroke = outer$stroke, strokeWidth = outer$strokeWidth, params = list(),
         layers = list(outer, inner), inner_scale = inner_scale),
    class = "cog_shape"
  )
}

stack_shape <- function(outer, inner, inner_scale = INNER_SCALE) new_stack(outer, inner, inner_scale)

# Radius (cell units) of the clear interior of an outer carrier: the incircle
# of a polygon or the inner boundary of a spike ring, minus half the stroke
# and a small margin (the renderer draws polygons at 50 * 0.85 * size and
# spike rings at 50 * 0.9 * size).
outer_clear_radius <- function(s) {
  p <- s$params
  r <- switch(
    s$kind,
    polygon = 50 * 0.85 * s$size * cos(pi / clamp(round(p$sides %||% 3), 3, 12)),
    "spike-ring" = 50 * 0.9 * s$size * (1 - clamp(p$spikeDepth %||% 0.35, 0.15, 0.6)),
    KNOCKOUT_RADIUS
  )
  r - s$strokeWidth / 2 - 1
}

# Inner-layer scale for an item: INNER_SCALE unless the narrowest outer
# carrier (a triangle, a deep spike ring) leaves less room; then the inner
# viewport shrinks so that its drawn content, and the disc under it, stay
# inside the clear interior.
inner_scale_for <- function(outer_shapes) {
  clear <- min(vapply(outer_shapes, outer_clear_radius, numeric(1)))
  min(INNER_SCALE, floor(clear / INNER_CLEARANCE) / 100)
}

is_stack <- function(s) identical(s$kind, "stack")

# Layer k of a stack as it is drawn: the inner layer fills INNER_SCALE of the
# cell with strokes scaled by INNER_SCALE * INNER_LWD_BOOST, so the symbolic
# perceptibility rules (count, size, stroke width, rotation) judge it at its
# real size.
layer_at_scale <- function(s, k) {
  x <- s$layers[[k]]
  if (k == 2L) {
    sc <- s$inner_scale %||% INNER_SCALE
    x$size <- x$size * sc
    x$strokeWidth <- x$strokeWidth * sc * INNER_LWD_BOOST
  }
  x
}

# Visibility rank of a distractor manipulation (1 = most visible): rule-derived
# candidates and count steps first, the subtle stroke-width and fallback
# tweaks last. Used to order the wrong options of a layer before combining.
PERTURBATION_RANK <- c(count1 = 1, marker = 1, color = 2, size = 3, count2 = 3,
                       fill = 4, rotation = 4, strokeWidth = 5, fallback = 6)
tier_rank <- function(tiers) {
  vapply(tiers, function(t) {
    if (tier_is_rule_derived(t)) return(1)
    r <- PERTURBATION_RANK[t]
    if (is.na(r)) 2.5 else unname(r)
  }, numeric(1), USE.NAMES = FALSE)
}

all_item_shapes <- function(item) c(do.call(c, item$cells), item$options)

# Grid children of a stacked cell: outer layer, white disc, inner layer.
build_stack <- function(s, lw) {
  sc <- s$inner_scale %||% INNER_SCALE
  outer <- build_shape(s$layers[[1]], lw)
  disc <- circleGrob(0.5, 0.5, r = unit(KNOCKOUT_RADIUS * sc / INNER_SCALE / 100, "snpc"),
                     gp = gpar(fill = PALETTE$cell, col = NA))
  inner <- gTree(children = build_shape(s$layers[[2]], lw * sc * INNER_LWD_BOOST),
                 vp = viewport(width = unit(sc, "snpc"),
                               height = unit(sc, "snpc")))
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
# answers. Distractor candidates are, in matRiks order, wrong in the inner
# layer, wrong in the outer layer and wrong in both (the layers' wrong options
# ordered from the most to the least visible manipulation, the pairs for
# "both" starting with the second wrong option of each layer); the queues are
# visited in turn and a candidate is kept only when it is distinct from, and
# not confusable with, the key and every candidate kept before it, the inner
# layer being judged at the scale it is drawn. Fewer than n_options - 1
# survivors leave a short item, which the generation loop rejects.
compose_layered <- function(outer, inner, n_options) {
  sc <- inner_scale_for(all_item_shapes(outer))
  stack_shape <- function(o, i) new_stack(o, i, sc)
  cells <- lapply(1:3, function(r) {
    lapply(1:3, function(c) stack_shape(outer$cells[[r]][[c]], inner$cells[[r]][[c]]))
  })
  wrong <- function(item) {
    shapes <- item$options[-item$correct]
    tiers <- item$meta$distractor_tiers[-item$correct]
    ord <- order(tier_rank(tiers))
    list(shapes = shapes[ord], tiers = tiers[ord])
  }
  oc <- outer$options[[outer$correct]]
  ic <- inner$options[[inner$correct]]
  ow <- wrong(outer)
  iw <- wrong(inner)
  k_o <- length(ow$shapes)
  k_i <- length(iw$shapes)

  key <- stack_shape(oc, ic)
  cands <- list()
  tiers <- character(0)
  sigs <- visual_signature(key)
  need <- n_options - 1L
  accept <- function(shape, tier) {
    sig <- visual_signature(shape)
    if (sig %in% sigs || confusable(key, shape)) return(FALSE)
    for (c in cands) if (confusable(c, shape)) return(FALSE)
    sigs <<- c(sigs, sig)
    cands[[length(cands) + 1]] <<- shape
    tiers <<- c(tiers, tier)
    TRUE
  }

  q_inner <- lapply(seq_len(k_i), function(i) {
    list(shape = stack_shape(oc, iw$shapes[[i]]), tier = paste0("inner:", iw$tiers[i]))
  })
  q_outer <- lapply(seq_len(k_o), function(o) {
    list(shape = stack_shape(ow$shapes[[o]], ic), tier = paste0("outer:", ow$tiers[o]))
  })
  pairs <- expand.grid(o = seq_len(k_o), i = seq_len(k_i))
  later <- !(pairs$o == pairs$i & pairs$o > 1)
  pairs <- pairs[order(later, pairs$o + pairs$i, pairs$o), , drop = FALSE]
  q_both <- lapply(seq_len(nrow(pairs)), function(r) {
    o <- pairs$o[r]
    i <- pairs$i[r]
    list(shape = stack_shape(ow$shapes[[o]], iw$shapes[[i]]),
         tier = paste0("both:", ow$tiers[o], "+", iw$tiers[i]))
  })
  queues <- list(q_inner, q_outer, q_both)
  pos <- c(1L, 1L, 1L)
  while (length(cands) < need) {
    taken <- FALSE
    for (q in 1:3) {
      while (pos[q] <= length(queues[[q]])) {
        cand <- queues[[q]][[pos[q]]]
        pos[q] <- pos[q] + 1L
        if (accept(cand$shape, cand$tier)) {
          taken <- TRUE
          break
        }
      }
      if (length(cands) >= need) break
    }
    if (!taken) break
  }

  opts <- c(list(key), cands)
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
        row_only = outer$rule %in% ROW_ONLY_RULES || inner$rule %in% ROW_ONLY_RULES,
        inner_scale = sc
      )
    ),
    class = "cog_item"
  )
}

# Each layer is generated with at most LAYER_OPTIONS options of its own, so
# that the outer layer never has to resort to size or fill distractors; the
# composed item takes its n_options - 1 distractors from the combinations of
# the layers' wrong options.
make_layered_one <- function(outer_spec, inner_spec, n_options) {
  layer_n <- min(n_options, LAYER_OPTIONS)
  draw_layer <- function(spec, ok, what) {
    for (k in seq_len(LAYER_TRIES)) {
      it <- tryCatch(generate_one(spec[1], spec[2], layer_n), error = function(e) NULL)
      if (!is.null(it) && is_item_valid(it, layer_n) && ok(it)) return(it)
    }
    stop(what, " layer rejected", call. = FALSE)
  }
  outer <- pin_outer_size(draw_layer(outer_spec, outer_layer_ok, "outer"))
  inner <- draw_layer(inner_spec, function(it) TRUE, "inner")
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
#' `"both:<tier>+<tier>"`). Each layer is generated with at most four
#' options of its own; with more options the distractors are drawn from the
#' combinations of the layers' wrong options, so any option count from 2 to
#' 8 works. All option-distinctness checks of single-layer items apply
#' layer by layer, the inner layer being judged at the scale it is drawn
#' (36 % of the cell, or less when the narrowest outer carrier of the item,
#' such as a triangle, leaves less clear room; `meta$inner_scale`)
#' (an inner stroke-width or small size step does not count as a
#' difference). The perceptual audit is switched on by default for two-layer
#' items (`perceptual = TRUE`); it compares the layers one by one, each at
#' full tile size (see [perceptual_distance()]).
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
                             max_attempts = NULL, perceptual = TRUE,
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
