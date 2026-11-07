shape_to_list <- function(s) {
  list(
    kind = s$kind,
    size = s$size,
    rotation = s$rotation,
    fill = if (is.null(s$fill) || is.na(s$fill)) NA else s$fill,
    stroke = s$stroke,
    strokeWidth = s$strokeWidth,
    params = if (length(s$params)) s$params else stats::setNames(list(), character(0))
  )
}

list_to_shape <- function(l) {
  fill <- l$fill
  if (is.null(fill) || (length(fill) == 1 && is.na(fill))) fill <- NA_character_
  params <- l$params %||% list()
  params <- lapply(params, function(v) if (is.numeric(v) || is.logical(v)) as.numeric(v) else v)
  cog_shape(kind = l$kind, params = params, size = l$size %||% 0.85,
            rotation = l$rotation %||% 0, fill = fill, stroke = l$stroke %||% "#e4e4e7",
            strokeWidth = l$strokeWidth %||% 2)
}

item_to_list <- function(item) {
  list(
    id = item$id,
    type = "3x3",
    rule = item$rule,
    shape = item$shape,
    cells = lapply(item$cells, function(row) lapply(row, shape_to_list)),
    options = lapply(item$options, shape_to_list),
    correctIndex = item$correct - 1L,
    optionCount = item$n_options,
    difficulty = item$difficulty,
    meta = item$meta
  )
}

list_to_item <- function(l) {
  cells <- lapply(l$cells, function(row) lapply(row, list_to_shape))
  options <- lapply(l$options, list_to_shape)
  meta <- l$meta %||% list()
  if (!is.null(meta$distractor_tiers)) meta$distractor_tiers <- unlist(meta$distractor_tiers)
  item <- structure(
    list(
      id = l$id %||% next_item_id(),
      type = "3x3",
      rule = l$rule,
      shape = l$shape,
      cells = cells,
      options = options,
      correct = as.integer(l$correctIndex) + 1L,
      n_options = length(options),
      difficulty = as.integer(l$difficulty %||% NA),
      meta = meta
    ),
    class = "cog_item"
  )
  if (is.na(item$difficulty)) item$difficulty <- calibrate_difficulty(item)
  item
}

#' Read and write items as JSON
#'
#' The JSON layout is the one produced by the "Download JSON" button of the
#' Cogitem web application (<https://cahitgs.github.io/itemgen1/>): a `meta`
#' object followed by a `puzzles` array of `3x3` items with 0-based
#' `correctIndex`. Files written by either tool can be read by the other, so
#' items generated in R can be administered in the browser and vice versa.
#' `read_items_json()` keeps only `3x3` matrix items and warns about other
#' puzzle types.
#'
#' @param x A `cog_item_set`.
#' @param path File path.
#' @param pretty Pretty-print the JSON?
#' @return `write_items_json()` returns `path` invisibly;
#'   `read_items_json()` returns a `cog_item_set`.
#' @examples
#' items <- generate_items("dice", "dist-of-3", n = 3, seed = 9)
#' f <- tempfile(fileext = ".json")
#' write_items_json(items, f)
#' back <- read_items_json(f)
#' identical(item_signature(items[[1]]), item_signature(back[[1]]))
#' @export
write_items_json <- function(x, path, pretty = TRUE) {
  stopifnot(inherits(x, "cog_item_set"))
  m <- attr(x, "meta") %||% list()
  payload <- list(
    meta = list(
      generator = "cogitem-r",
      version = as.character(utils::packageVersion("cogitem")),
      generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      spec = list(shape = m$shape %||% "mixed", rule = m$rule %||% "mixed",
                  count = length(x), seed = m$seed, n_options = m$n_options),
      seed = m$seed,
      count = length(x),
      duplicatesSkipped = m$duplicates_skipped %||% 0L
    ),
    puzzles = lapply(unclass(x), item_to_list)
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, na = "null", null = "null",
                       pretty = pretty, digits = NA)
  invisible(path)
}

#' @rdname write_items_json
#' @export
read_items_json <- function(path) {
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  puzzles <- if (!is.null(raw$puzzles)) raw$puzzles else raw
  types <- vapply(puzzles, function(p) p$type %||% "3x3", character(1))
  if (any(types != "3x3")) {
    warning(sprintf("Skipped %d non-matrix puzzle(s): %s", sum(types != "3x3"),
                    paste(unique(types[types != "3x3"]), collapse = ", ")), call. = FALSE)
  }
  items <- lapply(puzzles[types == "3x3"], list_to_item)
  meta <- raw$meta %||% list()
  new_item_set(items, meta = list(
    shape = meta$spec$shape %||% "mixed", rule = meta$spec$rule %||% "mixed",
    seed = meta$seed, n_options = meta$spec$n_options %||% NULL, source = path
  ))
}
