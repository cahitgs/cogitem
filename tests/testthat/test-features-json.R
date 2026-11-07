test_that("item features and Q-matrix have one row per item", {
  specs <- subset(supported_combinations(), shape %in% c("dice", "sector-pie"))
  set <- generate_mixed(specs, n_each = 1, seed = 8)
  f <- item_features(set)
  expect_equal(nrow(f), length(set))
  expect_true(all(c("rule", "shape", "rule_family", "difficulty", "rule_load",
                    "distractor_tiers", "bit_count", "operator") %in% names(f)))
  expect_true(all(f$difficulty >= 1 & f$difficulty <= 5))
  expect_true(all(f$n_rule_distractors + f$n_perturbation_distractors == f$n_options - 1))
  expect_true(all(!is.na(f$bit_count[f$rule_family == "logical"])))
  Q <- q_matrix(set, ~ rule + shape)
  expect_equal(nrow(Q), length(set))
  expect_identical(rownames(Q), f$id)
  expect_false("(Intercept)" %in% colnames(Q))
})

test_that("difficulty calibration follows the design formula", {
  it <- generate_item("sector-pie", "xor", seed = 1)
  expect_equal(it$difficulty, 5L) # xor (5) + sector-pie bonus (1) clamps to 5
  it2 <- generate_item("annulus", "identity", seed = 1)
  expect_equal(it2$difficulty, 1L)
  it3 <- generate_item("annulus", "identity", seed = 1, n_options = 6)
  expect_equal(it3$difficulty, 2L) # 1 + 0.5 rounds up
})

test_that("JSON export round-trips and matches the web-app schema", {
  set <- generate_items("hammer", "rotation", n = 3, seed = 21)
  f <- tempfile(fileext = ".json")
  write_items_json(set, f)
  raw <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(raw$meta$generator, "cogitem-r")
  expect_equal(length(raw$puzzles), 3)
  p <- raw$puzzles[[1]]
  expect_equal(p$type, "3x3")
  expect_equal(p$correctIndex, set[[1]]$correct - 1)
  expect_equal(length(p$cells), 3)
  expect_equal(length(p$cells[[1]]), 3)
  expect_true(is.null(p$cells[[1]][[1]]$fill))
  back <- read_items_json(f)
  expect_s3_class(back, "cog_item_set")
  expect_identical(vapply(back, item_signature, ""), vapply(set, item_signature, ""))
  expect_true(all(vapply(back, is_item_valid, TRUE)))
})
