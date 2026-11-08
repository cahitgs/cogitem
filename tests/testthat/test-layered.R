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
