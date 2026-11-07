#' Create a shape configuration
#'
#' A shape configuration describes one cell of a matrix item: the figural
#' carrier (`kind`), its overall `size` (0-1 of the cell), `rotation` in
#' degrees, `fill` and `stroke` colours, `strokeWidth` (in 1/100 of the cell)
#' and a list of kind-specific `params` (for example `ringCount` for an
#' annulus or `sides` for a polygon). The representation is identical to the
#' `ShapeConfig` type of the Cogitem web application, so configurations can be
#' exchanged through JSON.
#'
#' @param kind One of [cog_shapes()].
#' @param params Named list of kind-specific numeric parameters.
#' @param size Overall size, 0-1.
#' @param rotation Rotation in degrees (clockwise, as in SVG).
#' @param fill Fill colour (CSS hex or `rgba()` string) or `NA` for none.
#' @param stroke Stroke colour.
#' @param strokeWidth Stroke width in viewBox units (cell = 100 units).
#' @return An object of class `cog_shape`.
#' @examples
#' s <- cog_shape("polygon", list(sides = 5), stroke = "#34d399")
#' visual_signature(s)
#' @export
cog_shape <- function(kind, params = list(), size = 0.85, rotation = 0,
                      fill = NA_character_, stroke = "#e4e4e7",
                      strokeWidth = 2) {
  kind <- match.arg(kind, SHAPES)
  if (is.null(fill)) fill <- NA_character_
  structure(
    list(
      kind = kind,
      size = as.numeric(size),
      rotation = as.numeric(rotation),
      fill = as.character(fill),
      stroke = as.character(stroke),
      strokeWidth = as.numeric(strokeWidth),
      params = as.list(params)
    ),
    class = "cog_shape"
  )
}

# Internal shortcut mirroring `defaultShape(kind, params, overrides)`.
default_shape <- function(kind, params = list(), ...) {
  cog_shape(kind, params = params, ...)
}

# Return a modified copy (`...` are top-level fields such as size, stroke).
shape_set <- function(s, ...) {
  mods <- list(...)
  for (nm in names(mods)) s[[nm]] <- mods[[nm]]
  s
}

# Return a copy with one param changed.
shape_param <- function(s, name, value) {
  s$params[[name]] <- value
  s
}

#' @export
print.cog_shape <- function(x, ...) {
  cat(sprintf("<cog_shape> %s\n", visual_signature(x)))
  invisible(x)
}

# Sentinel blank cell used by distribution-of-two items. Never rendered as a
# shape; the matrix renderer draws a dashed empty box instead.
blank_cell <- function() {
  cog_shape("annulus", params = list(blank = 1), size = 0, rotation = 0,
            fill = NA_character_, stroke = "transparent", strokeWidth = 0)
}

is_blank_cell <- function(s) {
  isTRUE(s$params$blank == 1)
}

# Sector-pie pattern helpers: 3 bits per sector packed into one integer.
#   0 empty, 1 solid, 2 dots, 3 horizontal, 4 vertical, 5 diagonal "\",
#   6 diagonal "/", 7 cross-hatch.
pack_sector_patterns <- function(patterns) {
  pack <- 0L
  for (i in seq_along(patterns)) {
    pack <- bitwOr(pack, bitwShiftL(bitwAnd(as.integer(patterns[i]), 7L),
                                    as.integer((i - 1) * 3)))
  }
  pack
}

unpack_sector_patterns <- function(packed, count) {
  packed <- as.integer(round(packed))
  vapply(seq_len(count), function(i) {
    bitwAnd(bitwShiftR(packed, as.integer((i - 1) * 3)), 7L)
  }, integer(1))
}

# Binary presence mask (bit i set when sector i is non-empty).
patterns_to_fill_mask <- function(packed, count) {
  pats <- unpack_sector_patterns(packed, count)
  mask <- 0L
  for (i in seq_len(count)) {
    if (pats[i] > 0) mask <- bitwOr(mask, bitwShiftL(1L, as.integer(i - 1)))
  }
  mask
}

# Materialise a fill mask as packed patterns using `pattern_id` for every
# filled sector.
fill_mask_to_patterns <- function(mask, count, pattern_id = 1) {
  mask <- as.integer(mask)
  pack <- 0L
  for (i in seq_len(count)) {
    if (bitwAnd(mask, bitwShiftL(1L, as.integer(i - 1))) != 0) {
      pack <- bitwOr(pack, bitwShiftL(bitwAnd(as.integer(pattern_id), 7L),
                                      as.integer((i - 1) * 3)))
    }
  }
  pack
}
