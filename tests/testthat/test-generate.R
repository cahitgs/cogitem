test_that("every supported shape-rule combination yields valid items", {
  combos <- supported_combinations()
  expect_gt(nrow(combos), 50)
  for (i in seq_len(nrow(combos))) {
    set <- suppressWarnings(generate_items(combos$shape[i], combos$rule[i], n = 3, seed = 100 + i))
    expect_gte(length(set), 1, label = paste(combos$shape[i], combos$rule[i]))
    for (it in set) {
      expect_true(is_item_valid(it), label = paste(combos$shape[i], combos$rule[i]))
      expect_equal(it$rule, combos$rule[i])
      expect_equal(it$shape, combos$shape[i])
      expect_equal(it$n_options, 4)
      sigs <- vapply(it$options, visual_signature, "")
      expect_equal(anyDuplicated(sigs), 0)
      expect_identical(visual_signature(it$options[[it$correct]]),
                       visual_signature(it$cells[[3]][[3]]))
    }
  }
})

test_that("generation is reproducible for a given seed", {
  a <- generate_items("dice", "progression", n = 5, seed = 2024)
  b <- generate_items("dice", "progression", n = 5, seed = 2024)
  expect_identical(vapply(a, item_signature, ""), vapply(b, item_signature, ""))
  c <- generate_items("dice", "progression", n = 5, seed = 2025)
  expect_false(identical(vapply(a, item_signature, ""), vapply(c, item_signature, "")))
})

test_that("generation does not disturb the caller's RNG stream", {
  set.seed(1)
  x1 <- runif(1)
  set.seed(1)
  invisible(generate_items("annulus", "identity", n = 2, seed = 5))
  x2 <- runif(1)
  expect_equal(x1, x2)
})

test_that("items in a set are structurally distinct", {
  set <- generate_items("polygon", "dist-of-3", n = 30, seed = 77)
  expect_equal(anyDuplicated(vapply(set, item_signature, "")), 0)
})

test_that("option count can be varied", {
  set <- generate_items("annulus", "dist-of-3", n = 4, seed = 3, n_options = 6)
  expect_true(all(vapply(set, function(it) it$n_options, 1) == 6))
  expect_true(all(vapply(set, is_item_valid, TRUE)))
})

test_that("unsupported combinations are rejected", {
  expect_error(generate_items("annulus", "rotation"), "not supported")
  expect_false(is_supported("annulus", "mirror"))
  expect_true(is_supported("arrow", "mirror"))
  expect_true(all(supported_rules("sector-pie") %in% cog_rules()))
})

test_that("mixed sets and subsetting work", {
  specs <- subset(supported_combinations(), shape == "dice")
  mixed <- generate_mixed(specs, n_each = 1, seed = 11)
  expect_s3_class(mixed, "cog_item_set")
  expect_equal(length(mixed), nrow(specs))
  sub <- mixed[1:2]
  expect_s3_class(sub, "cog_item_set")
  expect_equal(length(sub), 2)
  both <- c(sub, mixed[3])
  expect_equal(length(both), 3)
  expect_output(print(mixed), "cog_item_set")
  expect_output(print(mixed[[1]]), "cog_item")
})

test_that("hand-made items can be constructed and validated", {
  base <- cog_shape("polygon", list(sides = 4))
  cells <- lapply(1:3, function(r) lapply(1:3, function(c) base))
  good <- cog_item(cells, list(base, cog_shape("polygon", list(sides = 5)),
                               cog_shape("polygon", list(sides = 6))),
                   correct = 1, rule = "identity", shape = "polygon")
  expect_true(is_item_valid(good))
  bad <- cog_item(cells, list(base, base, cog_shape("polygon", list(sides = 6))),
                  correct = 1, rule = "identity", shape = "polygon")
  expect_false(is_item_valid(bad))
})
test_that("the progress callback reports and can stop generation early", {
  calls <- list()
  set <- generate_items("dice", "dist-of-3", n = 6, seed = 5,
                        progress = function(done, n, attempts) {
                          calls[[length(calls) + 1]] <<- c(done, n, attempts)
                          TRUE
                        })
  expect_equal(length(set), 6)
  expect_true(length(calls) >= 6)
  expect_equal(calls[[length(calls)]][1:2], c(6, 6))
  expect_false(attr(set, "meta")$stopped)
  # the callback does not touch the random stream
  full <- generate_items("dice", "dist-of-3", n = 6, seed = 5)
  cells <- function(x) lapply(unclass(x), function(it) list(it$cells, it$options))
  expect_identical(cells(set), cells(full))
  part <- expect_silent(generate_items("dice", "dist-of-3", n = 6, seed = 5,
                                       progress = function(done, n, attempts) done < 3))
  expect_equal(length(part), 3)
  expect_true(attr(part, "meta")$stopped)
  expect_false(attr(part, "meta")$reached_ceiling)
  expect_identical(cells(part), cells(full)[1:3])
})

