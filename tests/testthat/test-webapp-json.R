test_that("a JSON export of the Cogitem web application is read correctly", {
  # Minimal excerpt in the exact layout produced by the web app's
  # downloadPuzzlesJson(): meta + puzzles[], 0-based correctIndex, fill: null,
  # plus one non-matrix puzzle that must be skipped with a warning.
  json <- '{
    "meta": {"generator": "cogitem", "generatedAt": "2026-11-01T10:00:00.000Z",
             "spec": {"shape": "annulus", "rule": "dist-of-3", "count": 1, "seed": 5},
             "seed": 5, "count": 1, "duplicatesSkipped": 0},
    "puzzles": [
      {"id": "puz-abc-1", "type": "3x3", "rule": "dist-of-3", "shape": "annulus",
       "cells": [
         [{"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":1,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":3,"gap":0.15}}],
         [{"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":3,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":1,"gap":0.15}}],
         [{"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":3,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":1,"gap":0.15}},
          {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}}]
       ],
       "options": [
         {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#a78bfa","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}},
         {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}},
         {"kind":"annulus","size":0.75,"rotation":0,"fill":null,"stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":1,"gap":0.15}},
         {"kind":"annulus","size":0.75,"rotation":0,"fill":"rgba(167, 139, 250, 0.22)","stroke":"#e4e4e7","strokeWidth":2,"params":{"ringCount":2,"gap":0.15}}
       ],
       "correctIndex": 1, "optionCount": 4, "difficulty": 2},
      {"id": "puz-odd-2", "type": "odd-one-out", "rule": "odd-one-out", "shape": "dice",
       "options": [], "correctIndex": 0, "optionCount": 5, "difficulty": 2}
    ]
  }'
  f <- tempfile(fileext = ".json")
  writeLines(json, f)
  expect_warning(set <- read_items_json(f), "odd-one-out")
  expect_equal(length(set), 1)
  it <- set[[1]]
  expect_equal(it$id, "puz-abc-1")
  expect_equal(it$correct, 2L)
  expect_equal(it$n_options, 4)
  expect_true(is.na(it$cells[[1]][[1]]$fill))
  expect_equal(it$options[[4]]$fill, "rgba(167, 139, 250, 0.22)")
  expect_true(is_item_valid(it))
  expect_equal(attr(set, "meta")$seed, 5)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(it))
})
