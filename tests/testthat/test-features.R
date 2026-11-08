test_that("item features and Q-matrix have one row per item", {
  specs <- subset(supported_combinations(), shape %in% c("dice", "sector-pie"))
  set <- generate_mixed(specs, n_each = 1, seed = 8)
  f <- item_features(set)
  expect_equal(nrow(f), length(set))
  expect_true(all(c("rule", "shape", "rule_family", "n_options", "correct_position",
                    "distractor_tiers", "bit_count", "operator") %in% names(f)))
  expect_false(any(c("difficulty", "rule_load", "shape_bonus") %in% names(f)))
  expect_true(all(f$n_rule_distractors + f$n_perturbation_distractors == f$n_options - 1))
  expect_true(all(!is.na(f$bit_count[f$rule_family == "logical"])))
  Q <- q_matrix(set, ~ rule + shape)
  expect_equal(nrow(Q), length(set))
  expect_identical(rownames(Q), f$id)
  expect_false("(Intercept)" %in% colnames(Q))
})

test_that("item sets persist with saveRDS/readRDS", {
  set <- generate_items("hammer", "rotation", n = 3, seed = 21)
  f <- tempfile(fileext = ".rds")
  saveRDS(set, f)
  back <- readRDS(f)
  expect_s3_class(back, "cog_item_set")
  expect_identical(vapply(back, item_signature, ""), vapply(set, item_signature, ""))
  expect_identical(attr(back, "meta")$seed, 21L)
  expect_true(all(vapply(back, is_item_valid, TRUE)))
})

