test_that("sector pattern packing round-trips", {
  pats <- c(0, 1, 2, 7, 3, 5)
  packed <- cogitem:::pack_sector_patterns(pats)
  expect_equal(cogitem:::unpack_sector_patterns(packed, 6), as.integer(pats))
  expect_equal(cogitem:::patterns_to_fill_mask(packed, 6), 2L + 4L + 8L + 16L + 32L)
  back <- cogitem:::fill_mask_to_patterns(cogitem:::patterns_to_fill_mask(packed, 6), 6, 1)
  expect_equal(cogitem:::unpack_sector_patterns(back, 6), as.integer(pats > 0))
})

test_that("visual signature normalises rotation by symmetry", {
  sq0 <- cog_shape("polygon", list(sides = 4), rotation = 0)
  sq90 <- cog_shape("polygon", list(sides = 4), rotation = 90)
  sq45 <- cog_shape("polygon", list(sides = 4), rotation = 45)
  expect_identical(visual_signature(sq0), visual_signature(sq90))
  expect_false(identical(visual_signature(sq0), visual_signature(sq45)))

  ann <- cog_shape("annulus", list(ringCount = 2), rotation = 33)
  expect_identical(visual_signature(ann), visual_signature(cog_shape("annulus", list(ringCount = 2))))

  arrow0 <- cog_shape("arrow", rotation = 0)
  arrow180 <- cog_shape("arrow", rotation = 180)
  expect_false(identical(visual_signature(arrow0), visual_signature(arrow180)))

  d4 <- cog_shape("dice", list(dotCount = 4))
  expect_equal(rotation_symmetry_fold(d4), 4)
  expect_equal(rotation_symmetry_fold(cog_shape("dice", list(dotCount = 2))), 2)
  expect_equal(rotation_symmetry_fold(cog_shape("nested-polygon",
                                                list(outerSides = 4, innerSides = 3))), 1)
  expect_equal(rotation_symmetry_fold(cog_shape("nested-polygon",
                                                list(outerSides = 6, innerSides = 4))), 2)
})

test_that("imperceptible count differences are detected", {
  a <- cog_shape("spike-ring", list(spikeCount = 15, spikeDepth = 0.3))
  b <- cog_shape("spike-ring", list(spikeCount = 16, spikeDepth = 0.3))
  expect_true(cogitem:::count_confusable(a, b))
  c <- cog_shape("spike-ring", list(spikeCount = 4, spikeDepth = 0.3))
  expect_false(cogitem:::count_confusable(a, c))
  d <- cog_shape("annulus", list(ringCount = 1))
  e <- cog_shape("annulus", list(ringCount = 2))
  expect_false(cogitem:::count_confusable(d, e))
})

test_that("blank cell sentinel is recognised", {
  expect_true(cogitem:::is_blank_cell(cogitem:::blank_cell()))
  expect_false(cogitem:::is_blank_cell(cog_shape("annulus")))
})
test_that("attributes a carrier does not draw never distinguish options", {
  a <- cog_shape("checkerboard", list(rows = 3, cols = 2, pattern = 35))
  b <- cog_shape("checkerboard", list(rows = 3, cols = 2, pattern = 35), fill = "#000000")
  expect_identical(visual_signature(a), visual_signature(b))
  g <- cog_shape("grid-dots", list(rows = 2, cols = 2), strokeWidth = 2)
  g_thick <- cog_shape("grid-dots", list(rows = 2, cols = 2), strokeWidth = 6)
  expect_identical(visual_signature(g), visual_signature(g_thick))
  gf <- cog_shape("grid-dots", list(rows = 2, cols = 2), strokeWidth = 2, fill = "#ffffff")
  gf_thick <- cog_shape("grid-dots", list(rows = 2, cols = 2), strokeWidth = 6, fill = "#ffffff")
  expect_false(identical(visual_signature(gf), visual_signature(gf_thick)))
  p <- cog_shape("polygon", list(sides = 5))
  expect_false(identical(visual_signature(p), visual_signature(cog_shape("polygon", list(sides = 5), fill = "#000000"))))
  # the reported case: six options of a checkerboard identity item
  it <- generate_item("checkerboard", "identity", seed = 1, n_options = 6)
  expect_false(any(it$meta$distractor_tiers == "fill"))
  expect_equal(anyDuplicated(vapply(it$options, visual_signature, "")), 0)
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  expect_gt(audit_item(it)$min_option_distance, 0.1)
})

test_that("rotation symmetry of pattern carriers follows the pattern", {
  cb <- function(pattern, rotation = 0) cog_shape("checkerboard", list(rows = 2, cols = 2, pattern = pattern), rotation = rotation)
  expect_equal(rotation_symmetry_fold(cb(9)), 2)   # two diagonal cells
  expect_equal(rotation_symmetry_fold(cb(15)), 4)  # all cells
  expect_equal(rotation_symmetry_fold(cb(1)), 1)   # one cell
  expect_identical(visual_signature(cb(9)), visual_signature(cb(9, 180)))
  expect_false(identical(visual_signature(cb(9)), visual_signature(cb(9, 90))))
  expect_false(identical(visual_signature(cb(1)), visual_signature(cb(1, 180))))
  bl <- function(mask, rotation = 0) cog_shape("box-lines", list(lineMask = mask), rotation = rotation)
  expect_equal(rotation_symmetry_fold(bl(5)), 2)   # top + bottom spokes
  expect_equal(rotation_symmetry_fold(bl(15)), 4)  # four spokes
  expect_equal(rotation_symmetry_fold(bl(48)), 4)  # both diagonals
  expect_equal(rotation_symmetry_fold(bl(16)), 2)  # one diagonal
  expect_equal(rotation_symmetry_fold(bl(1)), 1)   # one spoke
  expect_identical(visual_signature(bl(5)), visual_signature(bl(5, 180)))
  pie <- function(pats, rotation = 0) cog_shape("sector-pie", list(sectorCount = length(pats), sectorPatterns = cogitem:::pack_sector_patterns(pats)), rotation = rotation)
  expect_equal(rotation_symmetry_fold(pie(c(1, 0, 1, 0))), 2)
  expect_equal(rotation_symmetry_fold(pie(c(3, 3, 3, 3))), 2)  # horizontal lines: 180 only
  expect_equal(rotation_symmetry_fold(pie(c(1, 1, 1, 1))), 360) # all solid: a disc
  expect_equal(rotation_symmetry_fold(pie(c(7, 7, 7, 7, 7, 7, 7, 7))), 4)  # cross-hatch: 90
  expect_equal(rotation_symmetry_fold(pie(c(1, 2, 0, 0, 0))), 1)
  expect_identical(visual_signature(pie(c(1, 0, 1, 0))), visual_signature(pie(c(1, 0, 1, 0), 180)))
  for (i in 0:7) expect_equal(rotation_symmetry_fold(cog_shape("block-letter", list(patternIndex = i))), 1)
  # the generator never offers a rotated copy that renders identically
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  for (sh in c("box-lines", "checkerboard", "sector-pie")) {
    for (s in c(5, 8, 13)) {
      it <- generate_item(sh, "identity", seed = s, n_options = 8)
      expect_gt(audit_item(it)$min_option_distance, 0.02)
    }
  }
})

test_that("the reported box-lines item set has no identical options", {
  x <- generate_items("box-lines", "identity", n = 16, seed = 12, n_options = 8)
  expect_equal(length(x), 16)
  for (it in x) expect_equal(anyDuplicated(vapply(it$options, visual_signature, "")), 0)
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  expect_true(all(audit_items(x)$min_option_distance > 0.02))
})

test_that("tiny size, stroke-width and near-symmetric rotation steps count as confusable", {
  star <- function(size = 0.8, sw = 2, rot = 0) cog_shape("star", list(points = 6, innerRatio = 0.3), size = size, strokeWidth = sw, rotation = rot)
  expect_true(cogitem:::confusable(star(0.80), star(0.81)))
  expect_true(cogitem:::confusable(star(0.50), star(0.55)))
  expect_true(cogitem:::confusable(star(0.80), star(0.88)))
  expect_false(cogitem:::confusable(star(0.80), star(0.95)))
  expect_false(cogitem:::confusable(star(0.65), star(0.80)))
  expect_true(cogitem:::confusable(star(sw = 3), star(sw = 4)))
  expect_false(cogitem:::confusable(star(sw = 3), star(sw = 5)))
  expect_false(cogitem:::confusable(star(sw = 1), star(sw = 3)))
  nested <- function(o, i, rot = 0) cog_shape("nested-polygon", list(outerSides = o, innerSides = i, innerScale = 0.5), rotation = rot)
  expect_true(cogitem:::confusable(nested(7, 8), nested(7, 8, 45)))
  expect_true(cogitem:::confusable(nested(7, 8), nested(7, 8, 90)))
  expect_false(cogitem:::confusable(nested(5, 6), nested(5, 6, 45)))
  expect_false(cogitem:::confusable(nested(3, 4), nested(3, 4, 45)))
  arrow <- function(rot) cog_shape("arrow", list(headRatio = 0.45, shaftWidth = 0.35), rotation = rot)
  expect_false(cogitem:::confusable(arrow(0), arrow(10)))
  expect_true(cogitem:::confusable(arrow(0), arrow(2)))
  # a size difference plus another change is not confusable by the size rule
  expect_false(cogitem:::confusable(star(0.80), star(0.81, sw = 6)))
  # the mechanisms seen in the sweep no longer appear in generated options
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  for (cb in list(c("star", "dist-of-3", 12), c("arrow", "dist-of-3", 18), c("spike-ring", "dist-of-3", 12),
                  c("nested-polygon", "identity", 13))) {
    it <- generate_item(cb[1], cb[2], seed = as.integer(cb[3]), n_options = 8)
    expect_gt(audit_item(it)$min_option_distance, 0.1)
  }
})

test_that("count steps are judged with size and stroke width in mind", {
  ring <- function(n, sw) cog_shape("annulus", list(ringCount = n, gap = 0.2), size = 0.75, strokeWidth = sw)
  expect_true(cogitem:::confusable(ring(3, 1), ring(4, 1)))
  expect_false(cogitem:::confusable(ring(3, 3), ring(4, 3)))
  poly <- function(n, size) cog_shape("polygon", list(sides = n), size = size)
  expect_true(cogitem:::confusable(poly(6, 0.5), poly(8, 0.5)))
  expect_false(cogitem:::confusable(poly(6, 0.8), poly(8, 0.8)))
  expect_false(cogitem:::confusable(poly(5, 0.5), poly(8, 0.5)))
  bar <- function(size) cog_shape("bars", list(barCount = 1, orientation = 1), size = size)
  expect_true(cogitem:::confusable(bar(0.92), bar(1.0)))
  expect_false(cogitem:::confusable(bar(0.8), bar(1.0)))
  petals <- generate_item("petals", "identity", seed = 3, n_options = 8)
  expect_false(any(petals$meta$distractor_tiers == "fill"))
})


test_that("quarter turns are folded into the signature of pattern carriers", {
  cb <- function(rows, cols, pattern, rot = 0) cog_shape("checkerboard", list(rows = rows, cols = cols, pattern = pattern), rotation = rot)
  expect_identical(visual_signature(cb(2, 2, 1, 90)), visual_signature(cb(2, 2, 2)))   # top-left cell turned cw = top-right
  expect_false(identical(visual_signature(cb(2, 2, 1, 90)), visual_signature(cb(2, 2, 4))))
  expect_identical(visual_signature(cb(2, 3, 1, 90)), visual_signature(cb(3, 2, 2)))
  bl <- function(idx, rot = 0) cog_shape("block-letter", list(patternIndex = idx), rotation = rot)
  expect_identical(visual_signature(bl(7, 180)), visual_signature(bl(6, 90)))        # presets 6 and 7 are rotations of each other
  expect_false(identical(visual_signature(bl(7, 180)), visual_signature(bl(6, 270))))
  bx <- function(mask, rot = 0) cog_shape("box-lines", list(lineMask = mask), rotation = rot)
  expect_identical(visual_signature(bx(1, 90)), visual_signature(bx(2)))
  expect_identical(visual_signature(bx(16, 90)), visual_signature(bx(32)))
  expect_false(identical(visual_signature(bx(1, 90)), visual_signature(bx(8))))
  gd <- function(r, c, rot = 0) cog_shape("grid-dots", list(rows = r, cols = c, dotSize = 0.08), rotation = rot)
  expect_identical(visual_signature(gd(2, 3, 90)), visual_signature(gd(3, 2)))
  br <- function(o, rot = 0) cog_shape("bars", list(barCount = 3, orientation = o), rotation = rot)
  expect_identical(visual_signature(br(0, 90)), visual_signature(br(1)))
  expect_identical(visual_signature(br(0, 45)), visual_signature(br(2)))
  pie <- function(pats, rot = 0) cog_shape("sector-pie", list(sectorCount = 4, sectorPatterns = cogitem:::pack_sector_patterns(pats)), rotation = rot)
  expect_identical(visual_signature(pie(c(1, 0, 0, 0), 90)), visual_signature(pie(c(0, 1, 0, 0))))
  expect_false(identical(visual_signature(pie(c(1, 0, 0, 0), 90)), visual_signature(pie(c(0, 0, 0, 1)))))
  expect_false(identical(visual_signature(pie(c(3, 0, 0, 0), 90)), visual_signature(pie(c(0, 3, 0, 0)))))  # hatch turns with the pie
  skip_if_not(requireNamespace("ragg", quietly = TRUE) || requireNamespace("png", quietly = TRUE))
  expect_lt(perceptual_distance(bl(7, 180), bl(6, 90)), 0.01)
  expect_lt(perceptual_distance(bx(1, 90), bx(2)), 0.01)
  expect_lt(perceptual_distance(br(0, 90), br(1)), 0.01)
  expect_lt(perceptual_distance(gd(2, 3, 90), gd(3, 2)), 0.01)
  it <- generate_item("block-letter", "identity", seed = 1008 + which(supported_combinations()$shape == "block-letter" & supported_combinations()$rule == "identity"), n_options = 8)
  expect_gt(audit_item(it)$min_option_distance, 0.1)
})

test_that("sector pies are compared by what they draw", {
  pie <- function(n, pats, rot = 0) cog_shape("sector-pie", list(sectorCount = n, sectorPatterns = cogitem:::pack_sector_patterns(pats)), rotation = rot)
  # dividers between solid sectors are invisible: all-solid pies are one disc
  expect_identical(visual_signature(pie(2, c(1, 1))), visual_signature(pie(5, rep(1, 5))))
  expect_identical(visual_signature(pie(3, c(1, 1, 1))), visual_signature(pie(3, c(1, 1, 1), rot = 30)))
  # a divider through an empty half is visible
  expect_false(identical(visual_signature(pie(2, c(1, 0))), visual_signature(pie(4, c(1, 1, 0, 0)))))
  expect_false(cogitem:::confusable(pie(2, c(1, 0)), pie(4, c(1, 1, 0, 0))))
  expect_false(cogitem:::confusable(pie(2, c(0, 0)), pie(3, c(0, 0, 0))))
  # dividers through hatched sectors are too faint to count
  expect_true(cogitem:::confusable(pie(2, c(3, 3)), pie(3, c(3, 3, 3))))
  expect_true(cogitem:::confusable(pie(2, c(7, 7)), pie(4, c(7, 7, 7, 7))))
  expect_true(cogitem:::count_confusable(pie(2, c(3, 3)), pie(3, c(3, 3, 3))))
  expect_false(cogitem:::confusable(pie(2, c(3, 0)), pie(4, c(3, 3, 0, 0))))
  expect_false(cogitem:::confusable(pie(2, c(3, 3)), pie(2, c(3, 4))))
  expect_false(cogitem:::confusable(pie(2, c(4, 5)), pie(3, c(4, 5, 4))))
})

test_that("small inner polygons and shallow or crowded spikes cannot be counted", {
  nested <- function(inner, size = 0.75, scale = 0.5) cog_shape("nested-polygon", list(outerSides = 5, innerSides = inner, innerScale = scale), size = size, strokeWidth = 2)
  expect_true(cogitem:::confusable(nested(4), nested(6)))          # inner radius 17
  expect_false(cogitem:::confusable(nested(3), nested(6)))         # a triangle always shows
  expect_false(cogitem:::confusable(nested(4), nested(7)))         # three sides more
  expect_false(cogitem:::confusable(nested(4, 0.9, 0.6), nested(6, 0.9, 0.6)))  # radius 24
  expect_true(cogitem:::confusable(nested(6, 0.9, 0.6), nested(7, 0.9, 0.6)))
  expect_true(cogitem:::confusable(nested(4, 0.6, 0.35), nested(8, 0.6, 0.35)))  # radius 9
  spikes <- function(n, size, depth) cog_shape("spike-ring", list(spikeCount = n, spikeDepth = depth), size = size, strokeWidth = 2)
  expect_true(cogitem:::confusable(spikes(10, 0.55, 0.2), spikes(16, 0.55, 0.2)))
  expect_false(cogitem:::confusable(spikes(4, 0.55, 0.2), spikes(8, 0.55, 0.2)))
  expect_false(cogitem:::confusable(spikes(10, 0.75, 0.35), spikes(16, 0.75, 0.35)))
  expect_true(cogitem:::confusable(spikes(12, 0.45, 0.5), spikes(16, 0.45, 0.5)))
  expect_false(cogitem:::confusable(spikes(12, 0.75, 0.2), spikes(16, 0.75, 0.2)))
})

