test_that("the Shiny server logic generates, browses and administers items", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("callr")
  app_dir <- system.file("app", package = "cogitem")
  expect_true(nzchar(app_dir))

  shiny::testServer(app_dir, {
    # Generate
    session$setInputs(shape = "dice", rule = "addition", n = 3, seed = "1",
                      n_options = 4)
    session$setInputs(generate = 1)
    for (k in 1:300) { # the worker is a background R process; poll like the app does
      if (!is.null(items())) break
      Sys.sleep(0.2)
      session$elapse(400)
    }
    expect_s3_class(items(), "cog_item_set")
    expect_equal(length(items()), 3)
    expect_true(grepl("Items", output$summary$html))

    # Browse
    session$setInputs(browse_idx = 2, show_answer = TRUE, layout = "vertical")
    expect_equal(browse_item()$id, items()[[2]]$id)
    expect_true(!is.null(output$browse_plot$src))

    # Administer: answer, feedback, next, skip, answer
    session$setInputs(participant = "T1", feedback = TRUE, shuffle_items = FALSE,
                      max_items = 0, start = 1)
    expect_equal(admin$state, "playing")
    first <- items()[[1]]
    session$setInputs(admin_pick = list(i = first$correct, rt = 1200,
                                        hovers = list(list(i = 1, t = 100), list(i = 2, t = 400))))
    expect_equal(admin$state, "feedback")
    expect_equal(length(admin$log), 1)
    expect_true(admin$log[[1]]$correct)
    expect_equal(admin$log[[1]]$hoverTrace, "0@0;1@300")
    session$setInputs(admin_next = 1)
    expect_equal(admin$pos, 2)
    session$setInputs(admin_skip = list(rt = 800, hovers = list()))
    expect_equal(admin$pos, 3)
    expect_true(admin$log[[2]]$skipped)
    third <- items()[[3]]
    wrong <- if (third$correct == 1) 2 else 1
    session$setInputs(admin_pick = list(i = wrong, rt = 950, hovers = list()))
    session$setInputs(admin_next = 1)
    expect_equal(admin$state, "done")
    res <- do.call(rbind, admin$log)
    expect_equal(nrow(res), 3)
    expect_equal(res$correct, c(TRUE, FALSE, FALSE))
    expect_equal(res$chosenIndex[2], -1)
    expect_true(grepl("Finished", output$admin_ui$html))
  })
})
