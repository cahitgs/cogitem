test_that("every shape family draws without error", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(42)
  for (kind in cog_shapes()) {
    for (k in 1:3) {
      s <- cogitem:::random_base_shape(kind)
      grid::grid.newpage()
      expect_silent(grid::grid.draw(shape_grob(s)))
    }
  }
  # patterned sector pie exercises the clipped hatch code
  pie <- cog_shape("sector-pie", list(sectorCount = 6,
                                      sectorPatterns = cogitem:::pack_sector_patterns(2:7)))
  grid::grid.newpage()
  expect_silent(grid::grid.draw(shape_grob(pie)))
  half <- cog_shape("sector-pie", list(sectorCount = 2,
                                       sectorPatterns = cogitem:::pack_sector_patterns(c(7, 2))))
  grid::grid.newpage()
  expect_silent(grid::grid.draw(shape_grob(half)))
})

test_that("items and item sets plot", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set <- generate_items("checkerboard", "and", n = 3, seed = 6)
  expect_silent(plot(set[[1]]))
  expect_silent(plot(set[[1]], show_answer = TRUE, layout = "horizontal"))
  expect_silent(plot(set, n = 3, ncol = 3))
  d2 <- generate_items("arrow", "dist-of-2", n = 1, seed = 6)
  expect_silent(plot(d2[[1]]))
})

test_that("files are written", {
  set <- generate_items("bars", "progression", n = 2, seed = 12)
  png <- tempfile(fileext = ".png")
  save_item(set[[1]], png, width = 3, height = 4, dpi = 72)
  expect_true(file.exists(png) && file.size(png) > 0)
  pdf <- tempfile(fileext = ".pdf")
  save_booklet(set, pdf)
  expect_true(file.exists(pdf) && file.size(pdf) > 0)
  dir <- tempfile("items")
  files <- save_items(set, dir, format = "pdf")
  expect_true(all(file.exists(files)))
  expect_error(save_item(set[[1]], tempfile(fileext = ".bmp")), "Unsupported")
})
