skip_if_no_raster <- function() {
  if (!requireNamespace("ragg", quietly = TRUE) && !isTRUE(capabilities("cairo"))) {
    skip("perceptual audit needs ragg or cairo")
  }
}

test_that("perceptual distance separates identical, near-identical and distinct shapes", {
  skip_if_no_raster()
  a <- cog_shape("dice", list(dotCount = 3), size = 0.8)
  expect_equal(perceptual_distance(a, a), 0)

  # 1 % size change: nominally different, visually the same
  near <- cog_shape("dice", list(dotCount = 3), size = 0.81)
  expect_lt(perceptual_distance(a, near), 0.15)

  # the generator's size buckets must be clearly distinct after the dice fix
  expect_gt(perceptual_distance(cog_shape("dice", list(dotCount = 3), size = 0.65),
                                cog_shape("dice", list(dotCount = 3), size = 0.8)), 0.15)

  # colour change on a thin outline
  expect_gt(perceptual_distance(cog_shape("polygon", list(sides = 5), stroke = "#e4e4e7"),
                                cog_shape("polygon", list(sides = 5), stroke = "#34d399")), 0.15)

  # small but salient: hammer marker jumps to another corner
  h <- cog_shape("hammer", list(handleLength = 0.6, headWidth = 0.6, headThickness = 0.18,
                                markerPos = 1, markerSize = 0.1))
  expect_gt(perceptual_distance(h, shape_param(h, "markerPos", 2)), 0.15)

  # rotation of an asymmetric shape
  expect_gt(perceptual_distance(cog_shape("arrow", rotation = 0),
                                cog_shape("arrow", rotation = 90)), 0.5)
})

test_that("audit_items returns one row per item and flags ambiguous items", {
  skip_if_no_raster()
  set <- generate_items("annulus", "dist-of-3", n = 3, seed = 4)
  a <- audit_items(set)
  expect_equal(nrow(a), 3)
  expect_true(all(c("min_option_distance", "min_cell_distance", "flagged") %in% names(a)))
  expect_true(all(a$min_option_distance > 0))

  # a hand-made item whose two options differ only by 0.01 in the inner radius
  # ratio of a star must be flagged (no symbolic rule covers that parameter)
  base <- cog_shape("star", list(points = 6, innerRatio = 0.3), size = 0.8)
  cells <- lapply(1:3, function(r) lapply(1:3, function(c) base))
  bad <- cog_item(cells, list(base, cog_shape("star", list(points = 6, innerRatio = 0.31), size = 0.8),
                              cog_shape("star", list(points = 8, innerRatio = 0.3), size = 0.8)),
                  correct = 1, rule = "identity", shape = "star")
  expect_true(is_item_valid(bad))      # symbolic check cannot see it
  expect_true(audit_item(bad)$flagged) # perceptual audit can
  # a 1 % size step, by contrast, is now caught symbolically
  sq <- cog_shape("polygon", list(sides = 4), size = 0.8)
  expect_false(is_item_valid(cog_item(lapply(1:3, function(r) lapply(1:3, function(c) sq)),
                                      list(sq, cog_shape("polygon", list(sides = 4), size = 0.81),
                                           cog_shape("polygon", list(sides = 6))),
                                      correct = 1, rule = "identity", shape = "polygon")))
})

test_that("generate_items(perceptual = TRUE) records the audit in meta", {
  skip_if_no_raster()
  set <- generate_items("dice", "dist-of-3", n = 3, seed = 5, perceptual = TRUE)
  m <- attr(set, "meta")
  expect_true(m$perceptual)
  expect_equal(m$perceptual_threshold, 0.15)
  expect_true(is.numeric(m$perceptual_skipped))
  expect_false(any(audit_items(set)$flagged))
})
test_that("the png fallback renderer gives usable distances", {
  skip_if_not_installed("png")
  old <- options(cogitem.raster = "png")
  on.exit(options(old), add = TRUE)
  a <- cog_shape("dice", list(dotCount = 3), size = 0.65)
  b <- cog_shape("dice", list(dotCount = 3), size = 0.95)
  expect_equal(perceptual_distance(a, a), 0)
  expect_gt(perceptual_distance(a, b), 0.15)
  it <- generate_item("checkerboard", "xor", seed = 2)
  expect_false(audit_item(it)$flagged)
  options(cogitem.raster = "nonsense")
  expect_error(perceptual_distance(a, b), "raster engine")
})

