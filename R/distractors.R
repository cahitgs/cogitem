# Distractor construction with the visual-distinctness checks.
#
# Every candidate wrong answer is tagged (attr "tier") with the manipulation
# that produced it, so the item's feature record can report how its
# distractors were built.

tag_tier <- function(s, tier) {
  attr(s, "tier") <- tier
  s
}

untag_tier <- function(s) {
  attr(s, "tier") <- NULL
  s
}

# Primary count parameter shifted by +/- delta (within range).
param_tweaks <- function(correct, delta, tier) {
  spec <- PRIMARY_PARAM[[correct$kind]]
  if (is.null(spec)) return(list())
  cur <- correct$params[[spec$name]] %||% spec$min
  out <- list()
  if (cur + delta <= spec$max) {
    out[[length(out) + 1]] <- tag_tier(shape_param(correct, spec$name, cur + delta), tier)
  }
  if (cur - delta >= spec$min) {
    out[[length(out) + 1]] <- tag_tier(shape_param(correct, spec$name, cur - delta), tier)
  }
  out
}

# Candidate perturbations ordered by expected visual impact (tier 1 first).
candidate_perturbations <- function(correct) {
  out <- list()
  alt_strokes <- setdiff(STROKE_PALETTE, correct$stroke)

  # Tier 1: shape-defining count +/- 1
  out <- c(out, param_tweaks(correct, 1, "count1"))

  # Tier 1.5: hammer marker position
  if (correct$kind == "hammer") {
    cur_pos <- round(correct$params$markerPos %||% 0)
    for (p in setdiff(1:4, cur_pos)) {
      out[[length(out) + 1]] <- tag_tier(shape_param(correct, "markerPos", p), "marker")
    }
  }

  # Tier 2: colour swap
  if (length(alt_strokes) > 0) {
    first <- rpick(alt_strokes)
    out[[length(out) + 1]] <- tag_tier(shape_set(correct, stroke = first), "color")
    second <- setdiff(alt_strokes, first)
    if (length(second) > 0) {
      out[[length(out) + 1]] <- tag_tier(shape_set(correct, stroke = second[1]), "color")
    }
  }

  # Tier 3: big size delta
  out[[length(out) + 1]] <- tag_tier(shape_set(correct, size = round2(max(0.35, correct$size * 0.55))), "size")
  out[[length(out) + 1]] <- tag_tier(shape_set(correct, size = round2(min(1.0, correct$size * 1.25))), "size")

  # Tier 4: count +/- 2
  out <- c(out, param_tweaks(correct, 2, "count2"))

  # Tier 5: fill toggle (only for carriers that draw their fill; the thin
  # petals of a flower make the tint too weak to count)
  if (!attribute_visible(correct, "fill") || correct$kind == "petals") {
    # nothing to toggle
  } else if (is.na(correct$fill)) {
    out[[length(out) + 1]] <- tag_tier(shape_set(correct, fill = DISTRACTOR_FILL), "fill")
  } else {
    out[[length(out) + 1]] <- tag_tier(shape_set(correct, fill = NA_character_), "fill")
  }

  # Tier 6: rotation (only when not 90-degree symmetric)
  fold <- rotation_symmetry_fold(correct)
  if (fold < 4) {
    period <- 360 / fold
    rot_adds <- if (period == 180) c(45, 90, 135) else c(45, 90, 135, 180)
    for (add in rot_adds) {
      out[[length(out) + 1]] <- tag_tier(shape_set(correct, rotation = (correct$rotation + add) %% 360), "rotation")
    }
  }

  # Tier 7: stroke width (subtlest; skipped when the carrier draws no outline)
  if (attribute_visible(correct, "strokeWidth")) {
    out[[length(out) + 1]] <- tag_tier(shape_set(correct, strokeWidth = correct$strokeWidth + 3), "strokeWidth")
    if (correct$strokeWidth > 2) {
      out[[length(out) + 1]] <- tag_tier(shape_set(correct, strokeWidth = max(1, correct$strokeWidth - 2)), "strokeWidth")
    }
  }

  out
}

# Pick `count` distractors that are visually distinct from the correct answer,
# from each other and not confusable by an imperceptible count difference.
# `siblings` (rule-derived candidates such as the other distribution variants
# or the grid neighbours) are used first, then ordered perturbations, then a
# brute-force fallback.
make_distinct_distractors <- function(correct, siblings, count) {
  seen <- visual_signature(correct)
  out <- list()
  is_confusable <- function(c) {
    if (confusable(correct, c)) return(TRUE)
    for (o in out) if (confusable(o, c)) return(TRUE)
    FALSE
  }
  add <- function(s, default_tier) {
    sig <- visual_signature(s)
    if (sig %in% seen || is_confusable(s)) return(FALSE)
    seen <<- c(seen, sig)
    out[[length(out) + 1]] <<- tag_tier(s, attr(s, "tier") %||% default_tier)
    TRUE
  }

  for (s in siblings) {
    add(s, "sibling")
    if (length(out) >= count) return(out)
  }

  for (c in candidate_perturbations(correct)) {
    add(c, "perturbation")
    if (length(out) >= count) return(out)
  }

  attempts <- 0
  while (length(out) < count && attempts < 40) {
    attempts <- attempts + 1
    tweak <- shape_set(correct,
                       strokeWidth = correct$strokeWidth + 2 * attempts,
                       size = round2(max(0.3, correct$size - 0.1 * attempts)))
    add(tweak, "fallback")
  }
  out
}
