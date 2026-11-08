test_that("two-layer items combine two valid layers and three kinds of distractors", {
  set <- generate_layered(c("polygon", "progression"), c("dice", "dist-of-3"), n = 4, seed = 7)
  expect_s3_class(set, "cog_item_set")
  expect_equal(length(set), 4)
  for (it in set) {
    expect_equal(it$type, "layered")
    expect_true(is_item_valid(it))
    expect_equal(it$shape, "polygon+dice")
    expect_equal(it$rule, "progression+dist-of-3")
    shapes <- c(do.call(c, it$cells), it$options)
    expect_true(all(vapply(shapes, function(s) s$kind == "stack", TRUE)))
    # the outer layer is pinned to one size, the inner layer keeps its own
    expect_true(all(vapply(shapes, function(s) s$layers[[1]]$size == 0.95, TRUE)))
    tiers <- it$meta$distractor_tiers
    expect_equal(length(tiers), it$n_options)
    expect_equal(sum(tiers == "correct"), 1)
    expect_equal(tiers[it$correct], "correct")
    expect_true(any(startsWith(tiers, "inner:")))
    expect_true(any(startsWith(tiers, "outer:")))
    expect_true(any(startsWith(tiers, "both:")))
    # every distractor is wrong in at least one layer and no two options agree
    correct <- it$options[[it$correct]]
    sig <- function(s, k) visual_signature(s$layers[[k]])
    for (k in setdiff(seq_along(it$options), it$correct)) {
      o <- it$options[[k]]
      expect_true(sig(o, 1) != sig(correct, 1) || sig(o, 2) != sig(correct, 2))
    }
    expect_equal(anyDuplicated(vapply(it$options, visual_signature, "")), 0)
    # the keyed option reproduces the missing cell layer by layer
    expect_identical(visual_signature(correct), visual_signature(it$cells[[3]][[3]]))
  }
  f <- item_features(set)
  expect_equal(unique(f$n_layers), 2L)
  expect_equal(unique(f$rule_family), "layered")
  expect_equal(unique(f$outer_shape), "polygon")
  expect_equal(unique(f$outer_rule), "progression")
  expect_equal(unique(f$inner_shape), "dice")
  expect_equal(unique(f$inner_rule), "dist-of-3")
  expect_true(all(f$n_rule_distractors + f$n_perturbation_distractors == f$n_options - 1))
  # reproducible
  again <- generate_layered(c("polygon", "progression"), c("dice", "dist-of-3"), n = 4, seed = 7)
  expect_identical(vapply(again, item_signature, ""), vapply(set, item_signature, ""))
})

test_that("two-layer items draw, audit and work with more options", {
  set <- generate_layered(c("spike-ring", "progression"), c("checkerboard", "xor"),
                          n = 2, seed = 3, n_options = 6)
  expect_equal(length(set), 2)
  expect_true(all(vapply(set, function(it) it$n_options == 6, TRUE)))
  expect_true(set[[1]]$meta$row_only)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(set[[1]], show_answer = TRUE))
  expect_silent(plot(set, n = 2, ncol = 2))
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || isTRUE(capabilities("cairo")))
  a <- audit_item(set[[1]])
  expect_true(is.finite(a$min_option_distance))
  expect_gt(a$min_option_distance, 0)
  expect_true(is.finite(a$min_cell_distance))
})

test_that("layer specifications are checked", {
  expect_error(generate_layered(c("dice", "dist-of-3"), c("dice", "dist-of-3")), "outer layer shape")
  expect_error(generate_layered(c("polygon", "dist-of-2"), c("dice", "dist-of-3")), "not available")
  expect_error(generate_layered(c("polygon", "dist-of-3"), c("sector-pie", "xor")), "inner layer shape")
  expect_true(all(c("polygon", "spike-ring") %in% layer_shapes("outer")))
  expect_false("dist-of-2" %in% layer_rules("polygon"))
  expect_true("progression" %in% layer_rules("dice"))
  # list form of a layer specification
  set <- generate_layered(list(shape = "spike-ring", rule = "dist-of-3"),
                          list(shape = "bars", rule = "identity"), n = 1, seed = 5)
  expect_equal(length(set), 1)
})
test_that("two-layer items work with any option count up to eight", {
  for (cb in list(list(c("polygon", "progression"), c("dice", "dist-of-3"), 11L),
                  list(c("spike-ring", "identity"), c("arrow", "rotation"), 12L),
                  list(c("polygon", "dist-of-3"), c("checkerboard", "xor"), 13L))) {
    for (no in c(2L, 6L, 8L)) {
      set <- generate_layered(cb[[1]], cb[[2]], n = 3, seed = cb[[3]] + no, n_options = no)
      expect_equal(length(set), 3, info = sprintf("%s+%s with %d options", cb[[1]][1], cb[[2]][1], no))
      for (it in set) {
        expect_equal(it$n_options, no)
        expect_true(is_item_valid(it, n_options = no))
        expect_equal(length(it$meta$distractor_tiers), no)
        # the layers themselves never need more than four options
        expect_lte(it$meta$outer$n_options %||% 4L, 4L)
      }
    }
  }
  # with eight options all three kinds of distractors occur
  set <- generate_layered(c("polygon", "progression"), c("dice", "dist-of-3"), n = 2, seed = 21, n_options = 8)
  tiers <- unlist(lapply(set, function(it) it$meta$distractor_tiers))
  expect_true(any(startsWith(tiers, "inner:")))
  expect_true(any(startsWith(tiers, "outer:")))
  expect_true(any(startsWith(tiers, "both:")))
})

test_that("stacked options are compared at the scale the inner layer is drawn", {
  outer <- cog_shape("polygon", list(sides = 5), size = 0.95)
  inner <- function(n = 4, size = 0.75, sw = 3) cog_shape("dice", list(dotCount = n), size = size, strokeWidth = sw)
  st <- function(o, i) cogitem:::stack_shape(o, i)
  key <- st(outer, inner())
  # an inner stroke-width step of 3 units is 1.6 units on the disc: confusable
  expect_true(cogitem:::confusable(key, st(outer, inner(sw = 6))))
  # an inner size step of 0.1 is 0.036 on the disc: confusable
  expect_true(cogitem:::confusable(key, st(outer, inner(size = 0.85))))
  # a dot count step and an outer change are visible
  expect_false(cogitem:::confusable(key, st(outer, inner(n = 5))))
  expect_false(cogitem:::confusable(key, st(cog_shape("polygon", list(sides = 6), size = 0.95), inner())))
  # many-sided inner polygons need a larger count step than at full size
  # (two sides suffice at full size, three on the disc)
  poly <- function(n) cog_shape("polygon", list(sides = n), size = 0.75)
  expect_true(cogitem:::confusable(st(outer, poly(6)), st(outer, poly(8))))
  expect_false(cogitem:::confusable(poly(6), poly(8)))
  # and an item whose inner row shows 5, 7 and 9 sides is invalid
  cells <- lapply(1:3, function(r) lapply(1:3, function(c) st(outer, poly(3 + 2 * c))))
  bad <- cog_item(cells, list(st(outer, poly(9)), st(outer, poly(5)), st(outer, poly(3))),
                  correct = 1, rule = "progression", shape = "polygon+polygon")
  expect_false(is_item_valid(bad))
})

test_that("the perceptual audit compares two-layer stacks layer by layer", {
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  outer <- cog_shape("polygon", list(sides = 5), size = 0.95)
  dice <- function(n) cog_shape("dice", list(dotCount = n), size = 0.75, strokeWidth = 3)
  st <- cogitem:::stack_shape
  # five vs six dots on the inner disc: resolvable layer by layer
  expect_gt(perceptual_distance(st(outer, dice(5)), st(outer, dice(6))), 0.15)
  expect_equal(perceptual_distance(st(outer, dice(5)), st(outer, dice(5))), 0)
  # the outer layer alone counts as well
  expect_gt(perceptual_distance(st(outer, dice(5)), st(cog_shape("polygon", list(sides = 6), size = 0.95), dice(5))), 0.15)
  # the layer-wise distance is never below the inner layer alone
  expect_gte(perceptual_distance(st(outer, dice(2)), st(outer, dice(4))), perceptual_distance(dice(2), dice(4)) - 1e-9)
})

test_that("the inner layer shrinks inside narrow outer carriers", {
  tri <- cog_shape("polygon", list(sides = 3), size = 0.95, strokeWidth = 3)
  sq <- cog_shape("polygon", list(sides = 4), size = 0.95, strokeWidth = 3)
  expect_lt(cogitem:::outer_clear_radius(tri), 20)
  expect_gt(cogitem:::outer_clear_radius(sq), 25)
  expect_lt(cogitem:::inner_scale_for(list(tri, sq)), 0.3)
  expect_equal(cogitem:::inner_scale_for(list(sq)), 0.36)
  # polygon addition always has a triangle in every row
  set <- generate_layered(c("polygon", "addition"), c("grid-dots", "dist-of-3"), n = 2, seed = 5)
  expect_equal(length(set), 2)
  for (it in set) {
    sc <- it$meta$inner_scale
    expect_lt(sc, 0.36)
    shapes <- c(do.call(c, it$cells), it$options)
    expect_true(all(vapply(shapes, function(s) isTRUE(all.equal(s$inner_scale, sc)), TRUE)))
    # the disc stays inside the clear interior of every outer carrier
    disc <- cogitem:::KNOCKOUT_RADIUS * sc / cogitem:::INNER_SCALE
    expect_true(all(vapply(shapes, function(s) cogitem:::outer_clear_radius(s$layers[[1]]) >= disc, TRUE)))
    expect_true(is_item_valid(it))
  }
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(set[[1]], show_answer = TRUE))
})

