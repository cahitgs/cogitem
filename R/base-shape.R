# One randomised configuration of a shape family. Used as the uniform target
# of identity items and as the seed of the other rule generators.
random_base_shape <- function(kind) {
  stroke <- rpick(STROKE_PALETTE)
  size <- rpick(SIZE_BUCKETS)
  sw <- rint(2, 4)
  switch(
    kind,
    annulus = default_shape("annulus", list(ringCount = rint(1, 3), gap = 0.15),
                            stroke = stroke, size = size, strokeWidth = sw),
    dice = default_shape("dice", list(dotCount = rint(1, 9)),
                         stroke = stroke, size = size, strokeWidth = sw),
    polygon = default_shape("polygon", list(sides = rint(3, 8)),
                            stroke = stroke, size = size, strokeWidth = sw,
                            rotation = rpick(ROTATION_BUCKETS)),
    star = default_shape("star", list(points = rint(5, 8),
                                      innerRatio = rpick(c(0.3, 0.4, 0.5))),
                         stroke = stroke, size = size, strokeWidth = sw),
    arrow = default_shape("arrow", list(headRatio = rpick(c(0.35, 0.45, 0.55)),
                                        shaftWidth = 0.35),
                          stroke = stroke, size = size, strokeWidth = sw,
                          rotation = rpick(seq(0, 315, by = 45))),
    petals = default_shape("petals", list(petalCount = rint(5, 10),
                                          petalWidth = rpick(c(0.08, 0.12, 0.18))),
                           stroke = stroke, size = size, strokeWidth = sw),
    "spike-ring" = default_shape("spike-ring",
                                 list(spikeCount = rint(6, 12),
                                      spikeDepth = rpick(c(0.2, 0.3, 0.45))),
                                 stroke = stroke, size = size, strokeWidth = sw),
    hammer = default_shape("hammer",
                           list(handleLength = rpick(c(0.5, 0.6, 0.7)),
                                headWidth = rpick(c(0.5, 0.6, 0.7)),
                                headThickness = rpick(c(0.15, 0.18, 0.22)),
                                markerPos = rint(1, 4),
                                markerSize = 0.1),
                           stroke = stroke, size = size, strokeWidth = sw,
                           rotation = rpick(seq(0, 315, by = 45))),
    bars = default_shape("bars", list(barCount = rint(2, 5), orientation = rint(0, 2)),
                         stroke = stroke, size = size, strokeWidth = sw),
    "grid-dots" = default_shape("grid-dots",
                                list(rows = rint(2, 4), cols = rint(2, 4),
                                     dotSize = rpick(c(0.06, 0.08, 0.1))),
                                stroke = stroke, size = size, strokeWidth = sw),
    checkerboard = {
      rows <- rint(2, 4)
      cols <- rint(2, 4)
      default_shape("checkerboard",
                    list(rows = rows, cols = cols,
                         pattern = random_checker_pattern(rows, cols)),
                    stroke = stroke, size = size, strokeWidth = sw)
    },
    "box-lines" = default_shape("box-lines", list(lineMask = rint(1, 31)),
                                stroke = stroke, size = size, strokeWidth = sw),
    "nested-polygon" = default_shape("nested-polygon",
                                     list(outerSides = rint(3, 8),
                                          innerSides = rint(3, 8),
                                          innerScale = rpick(c(0.4, 0.5, 0.6))),
                                     stroke = stroke, size = size, strokeWidth = sw),
    "sector-pie" = {
      count <- rint(3, 6)
      pats <- vapply(seq_len(count), function(i) {
        if (runif1() < 0.3) 0 else rint(1, 7)
      }, numeric(1))
      default_shape("sector-pie",
                    list(sectorCount = count, sectorPatterns = pack_sector_patterns(pats)),
                    stroke = stroke, size = size, strokeWidth = sw)
    },
    "block-letter" = default_shape("block-letter", list(patternIndex = rint(0, 7)),
                                   stroke = stroke, size = size, strokeWidth = sw,
                                   rotation = rpick(c(0, 90, 180, 270))),
    stop(sprintf("random_base_shape: unknown shape '%s'", kind), call. = FALSE)
  )
}
