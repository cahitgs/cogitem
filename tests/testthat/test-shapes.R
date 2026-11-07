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
