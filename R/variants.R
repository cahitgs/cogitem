# Variant generators: each returns three visually distinct configurations of
# one shape family that differ along a single, randomly chosen axis. Used by
# the distribution rules. The chosen axis is attached as attr "axis".

with_axis <- function(variants, axis) {
  attr(variants, "axis") <- axis
  variants
}

variants_annulus <- function() {
  axis <- rpick(c("ringCount", "size", "stroke", "strokeWidth"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 4)
  base_rings <- rint(1, 2)
  base_gap <- rpick(c(0.12, 0.15, 0.2))
  out <- switch(
    axis,
    ringCount = lapply(sort(rsample(1:4, 3)), function(rc) {
      default_shape("annulus", list(ringCount = rc, gap = base_gap),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("annulus", list(ringCount = base_rings + 1, gap = base_gap),
                    stroke = stroke, size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("annulus", list(ringCount = base_rings + 1, gap = base_gap),
                    stroke = stk, size = base_size, strokeWidth = base_sw)
    }),
    strokeWidth = lapply(sort(rsample(c(1, 2, 4, 6), 3)), function(sw) {
      default_shape("annulus", list(ringCount = base_rings + 1, gap = base_gap),
                    stroke = stroke, size = base_size, strokeWidth = sw)
    })
  )
  with_axis(out, axis)
}

variants_dice <- function() {
  base_dots <- rpick(c(2, 3, 6, 7)) # 180-degree-only symmetric faces
  axis <- rpick(c("dotCount", "rotation", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_rot <- rpick(c(0, 90))
  out <- switch(
    axis,
    dotCount = lapply(sort(rsample(1:9, 3)), function(d) {
      default_shape("dice", list(dotCount = d), stroke = stroke,
                    size = base_size, strokeWidth = base_sw, rotation = base_rot)
    }),
    rotation = lapply(sort(rsample(c(0, 45, 90, 135), 3)), function(r) {
      default_shape("dice", list(dotCount = base_dots), stroke = stroke,
                    size = base_size, strokeWidth = base_sw, rotation = r)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("dice", list(dotCount = base_dots), stroke = stroke,
                    size = sz, strokeWidth = base_sw, rotation = base_rot)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("dice", list(dotCount = base_dots), stroke = stk,
                    size = base_size, strokeWidth = base_sw, rotation = base_rot)
    })
  )
  with_axis(out, axis)
}

variants_polygon <- function() {
  axis <- rpick(c("sides", "size", "stroke", "strokeWidth"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 4)
  base_sides <- rint(3, 8)
  out <- switch(
    axis,
    sides = lapply(sort(rsample(3:8, 3)), function(s) {
      default_shape("polygon", list(sides = s), stroke = stroke,
                    size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("polygon", list(sides = base_sides), stroke = stroke,
                    size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("polygon", list(sides = base_sides), stroke = stk,
                    size = base_size, strokeWidth = base_sw)
    }),
    strokeWidth = lapply(sort(rsample(c(1, 2, 4, 6), 3)), function(sw) {
      default_shape("polygon", list(sides = base_sides), stroke = stroke,
                    size = base_size, strokeWidth = sw)
    })
  )
  with_axis(out, axis)
}

variants_star <- function() {
  axis <- rpick(c("points", "size", "stroke", "innerRatio"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_points <- rint(5, 8)
  base_ir <- rpick(c(0.3, 0.4, 0.5))
  out <- switch(
    axis,
    points = lapply(sort(rsample(c(4, 5, 6, 7, 8, 10), 3)), function(p) {
      default_shape("star", list(points = p, innerRatio = base_ir),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    innerRatio = lapply(sort(rsample(c(0.25, 0.35, 0.5, 0.6), 3)), function(ir) {
      default_shape("star", list(points = base_points, innerRatio = ir),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("star", list(points = base_points, innerRatio = base_ir),
                    stroke = stroke, size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("star", list(points = base_points, innerRatio = base_ir),
                    stroke = stk, size = base_size, strokeWidth = base_sw)
    })
  )
  with_axis(out, axis)
}

variants_arrow <- function() {
  axis <- rpick(c("rotation", "size", "stroke", "headRatio"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 4)
  base_hr <- rpick(c(0.35, 0.45, 0.55))
  fixed_rot <- rpick(c(0, 90, 180, 270))
  out <- switch(
    axis,
    rotation = lapply(rsample(seq(0, 315, by = 45), 3), function(a) {
      default_shape("arrow", list(headRatio = base_hr, shaftWidth = 0.35),
                    stroke = stroke, size = base_size, strokeWidth = base_sw,
                    rotation = a)
    }),
    headRatio = lapply(sort(rsample(c(0.3, 0.4, 0.5, 0.6), 3)), function(hr) {
      default_shape("arrow", list(headRatio = hr, shaftWidth = 0.35),
                    stroke = stroke, size = base_size, strokeWidth = base_sw,
                    rotation = fixed_rot)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("arrow", list(headRatio = base_hr, shaftWidth = 0.35),
                    stroke = stroke, size = sz, strokeWidth = base_sw,
                    rotation = fixed_rot)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("arrow", list(headRatio = base_hr, shaftWidth = 0.35),
                    stroke = stk, size = base_size, strokeWidth = base_sw,
                    rotation = fixed_rot)
    })
  )
  with_axis(out, axis)
}

variants_petals <- function() {
  axis <- rpick(c("petalCount", "size", "stroke", "petalWidth"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_count <- rint(5, 10)
  base_width <- rpick(c(0.08, 0.12, 0.18))
  out <- switch(
    axis,
    petalCount = lapply(sort(rsample(c(3, 4, 5, 6, 7, 8, 10, 12), 3)), function(n) {
      default_shape("petals", list(petalCount = n, petalWidth = base_width),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    petalWidth = lapply(sort(rsample(c(0.06, 0.1, 0.16, 0.22), 3)), function(w) {
      default_shape("petals", list(petalCount = base_count, petalWidth = w),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("petals", list(petalCount = base_count, petalWidth = base_width),
                    stroke = stroke, size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("petals", list(petalCount = base_count, petalWidth = base_width),
                    stroke = stk, size = base_size, strokeWidth = base_sw)
    })
  )
  with_axis(out, axis)
}

variants_spike_ring <- function() {
  axis <- rpick(c("spikeCount", "size", "stroke", "spikeDepth"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_count <- rint(6, 12)
  base_depth <- rpick(c(0.2, 0.3, 0.45))
  out <- switch(
    axis,
    spikeCount = lapply(sort(rsample(c(4, 5, 6, 8, 10, 12, 14, 16), 3)), function(n) {
      default_shape("spike-ring", list(spikeCount = n, spikeDepth = base_depth),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    spikeDepth = lapply(sort(rsample(c(0.15, 0.3, 0.45, 0.6), 3)), function(d) {
      default_shape("spike-ring", list(spikeCount = base_count, spikeDepth = d),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("spike-ring", list(spikeCount = base_count, spikeDepth = base_depth),
                    stroke = stroke, size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("spike-ring", list(spikeCount = base_count, spikeDepth = base_depth),
                    stroke = stk, size = base_size, strokeWidth = base_sw)
    })
  )
  with_axis(out, axis)
}

variants_hammer <- function() {
  axis <- rpick(c("rotation", "markerPos", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(1, 2)
  base_rot <- rpick(seq(0, 315, by = 45))
  base_marker <- rint(1, 4)
  dims <- list(handleLength = 0.6, headWidth = 0.6, headThickness = 0.18,
               markerSize = 0.1)
  mk <- function(marker, rot, sz = base_size, stk = stroke) {
    default_shape("hammer", c(dims, list(markerPos = marker)), stroke = stk,
                  size = sz, strokeWidth = base_sw, rotation = rot)
  }
  out <- switch(
    axis,
    rotation = lapply(rsample(seq(0, 315, by = 45), 3), function(r) mk(base_marker, r)),
    markerPos = lapply(rsample(1:4, 3), function(p) mk(p, base_rot)),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      mk(base_marker, base_rot, sz = sz)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      mk(base_marker, base_rot, stk = stk)
    })
  )
  with_axis(out, axis)
}

variants_bars <- function() {
  axis <- rpick(c("barCount", "orientation", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 4)
  base_count <- rint(2, 4)
  base_orient <- rint(0, 2)
  out <- switch(
    axis,
    barCount = lapply(sort(rsample(1:6, 3)), function(n) {
      default_shape("bars", list(barCount = n, orientation = base_orient),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    orientation = lapply(rsample(0:2, 3), function(o) {
      default_shape("bars", list(barCount = base_count, orientation = o),
                    stroke = stroke, size = base_size, strokeWidth = base_sw)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      default_shape("bars", list(barCount = base_count, orientation = base_orient),
                    stroke = stroke, size = sz, strokeWidth = base_sw)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      default_shape("bars", list(barCount = base_count, orientation = base_orient),
                    stroke = stk, size = base_size, strokeWidth = base_sw)
    })
  )
  with_axis(out, axis)
}

variants_grid_dots <- function() {
  axis <- rpick(c("rows", "cols", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_rows <- rint(2, 4)
  base_cols <- rint(2, 4)
  base_dot <- rpick(c(0.06, 0.08, 0.1))
  mk <- function(rows, cols, sz = base_size, stk = stroke) {
    default_shape("grid-dots", list(rows = rows, cols = cols, dotSize = base_dot),
                  stroke = stk, size = sz, strokeWidth = base_sw)
  }
  out <- switch(
    axis,
    rows = lapply(sort(rsample(1:4, 3)), function(r) mk(r, base_cols)),
    cols = lapply(sort(rsample(1:4, 3)), function(c) mk(base_rows, c)),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      mk(base_rows, base_cols, sz = sz)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      mk(base_rows, base_cols, stk = stk)
    })
  )
  with_axis(out, axis)
}

# Random checkerboard bit pattern with moderate fill (20-80 %).
random_checker_pattern <- function(rows, cols) {
  max_pattern <- 2^(rows * cols) - 1
  rint(ceiling(max_pattern * 0.2), floor(max_pattern * 0.8))
}

variants_checkerboard <- function() {
  axis <- rpick(c("pattern", "rows", "cols", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_rows <- rint(2, 4)
  base_cols <- rint(2, 4)
  mk <- function(rows, cols, pattern, sz = base_size, stk = stroke) {
    default_shape("checkerboard", list(rows = rows, cols = cols, pattern = pattern),
                  stroke = stk, size = sz, strokeWidth = base_sw)
  }
  out <- switch(
    axis,
    pattern = {
      patterns <- numeric(0)
      attempts <- 0
      while (length(patterns) < 3 && attempts < 40) {
        attempts <- attempts + 1
        p <- random_checker_pattern(base_rows, base_cols)
        if (!(p %in% patterns)) patterns <- c(patterns, p)
      }
      while (length(patterns) < 3) {
        patterns <- c(patterns, random_checker_pattern(base_rows, base_cols))
      }
      lapply(patterns, function(p) mk(base_rows, base_cols, p))
    },
    rows = lapply(sort(rsample(2:4, 3)), function(r) {
      mk(r, base_cols, random_checker_pattern(r, base_cols))
    }),
    cols = lapply(sort(rsample(2:4, 3)), function(c) {
      mk(base_rows, c, random_checker_pattern(base_rows, c))
    }),
    size = {
      pat <- random_checker_pattern(base_rows, base_cols)
      lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
        mk(base_rows, base_cols, pat, sz = sz)
      })
    },
    stroke = {
      pat <- random_checker_pattern(base_rows, base_cols)
      lapply(rsample(STROKE_PALETTE, 3), function(stk) {
        mk(base_rows, base_cols, pat, stk = stk)
      })
    }
  )
  with_axis(out, axis)
}

variants_box_lines <- function() {
  axis <- rpick(c("lineMask", "size", "stroke", "strokeWidth"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_mask <- rint(1, 31)
  mk <- function(mask, sz = base_size, stk = stroke, sw = base_sw) {
    default_shape("box-lines", list(lineMask = mask), stroke = stk, size = sz,
                  strokeWidth = sw)
  }
  out <- switch(
    axis,
    lineMask = {
      masks <- numeric(0)
      attempts <- 0
      while (length(masks) < 3 && attempts < 60) {
        attempts <- attempts + 1
        m <- rint(1, 47)
        if (!(m %in% masks)) masks <- c(masks, m)
      }
      while (length(masks) < 3) masks <- c(masks, rint(1, 47))
      lapply(masks, mk)
    },
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) mk(base_mask, sz = sz)),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) mk(base_mask, stk = stk)),
    strokeWidth = lapply(sort(rsample(c(1, 2, 4, 6), 3)), function(sw) mk(base_mask, sw = sw))
  )
  with_axis(out, axis)
}

variants_nested_polygon <- function() {
  axis <- rpick(c("outerSides", "innerSides", "innerScale", "size"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_outer <- rint(3, 7)
  base_inner <- rint(3, 7)
  base_scale <- rpick(c(0.4, 0.5, 0.6))
  mk <- function(outer, inner, scale, sz = base_size) {
    default_shape("nested-polygon",
                  list(outerSides = outer, innerSides = inner, innerScale = scale),
                  stroke = stroke, size = sz, strokeWidth = base_sw)
  }
  out <- switch(
    axis,
    outerSides = lapply(sort(rsample(3:8, 3)), function(s) mk(s, base_inner, base_scale)),
    innerSides = lapply(sort(rsample(3:8, 3)), function(s) mk(base_outer, s, base_scale)),
    innerScale = lapply(sort(rsample(c(0.3, 0.45, 0.6, 0.75), 3)), function(s) {
      mk(base_outer, base_inner, s)
    }),
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      mk(base_outer, base_inner, base_scale, sz = sz)
    })
  )
  with_axis(out, axis)
}

random_sector_patterns <- function(count) {
  vapply(seq_len(count), function(i) {
    if (runif1() < 0.25) 0 else rint(1, 7)
  }, numeric(1))
}

variants_sector_pie <- function() {
  axis <- rpick(c("sectorCount", "sectorPatterns", "size", "stroke"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 3)
  base_count <- rint(3, 6)
  base_patterns <- pack_sector_patterns(random_sector_patterns(base_count))
  mk <- function(count, patterns, sz = base_size, stk = stroke) {
    default_shape("sector-pie", list(sectorCount = count, sectorPatterns = patterns),
                  stroke = stk, size = sz, strokeWidth = base_sw)
  }
  out <- switch(
    axis,
    sectorCount = lapply(sort(rsample(c(3, 4, 5, 6, 8), 3)), function(n) {
      mk(n, pack_sector_patterns(random_sector_patterns(n)))
    }),
    sectorPatterns = {
      packs <- numeric(0)
      attempts <- 0
      while (length(packs) < 3 && attempts < 60) {
        attempts <- attempts + 1
        p <- pack_sector_patterns(random_sector_patterns(base_count))
        if (!(p %in% packs)) packs <- c(packs, p)
      }
      while (length(packs) < 3) {
        packs <- c(packs, pack_sector_patterns(random_sector_patterns(base_count)))
      }
      lapply(packs, function(p) mk(base_count, p))
    },
    size = lapply(sort(rsample(c(0.5, 0.65, 0.8, 0.95), 3)), function(sz) {
      mk(base_count, base_patterns, sz = sz)
    }),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) {
      mk(base_count, base_patterns, stk = stk)
    })
  )
  with_axis(out, axis)
}

variants_block_letter <- function() {
  axis <- rpick(c("patternIndex", "stroke", "size"))
  stroke <- rpick(STROKE_PALETTE)
  base_size <- rpick(SIZE_BUCKETS)
  base_sw <- rint(2, 4)
  base_idx <- rint(0, 7)
  base_rot <- rpick(c(0, 90, 180, 270))
  mk <- function(idx, sz = base_size, stk = stroke) {
    default_shape("block-letter", list(patternIndex = idx), stroke = stk,
                  size = sz, strokeWidth = base_sw, rotation = base_rot)
  }
  out <- switch(
    axis,
    patternIndex = lapply(rsample(0:7, 3), mk),
    stroke = lapply(rsample(STROKE_PALETTE, 3), function(stk) mk(base_idx, stk = stk)),
    size = lapply(rsample(SIZE_BUCKETS, 3), function(sz) mk(base_idx, sz = sz))
  )
  with_axis(out, axis)
}

VARIANT_GENERATORS <- list(
  annulus = variants_annulus,
  dice = variants_dice,
  polygon = variants_polygon,
  star = variants_star,
  arrow = variants_arrow,
  petals = variants_petals,
  "spike-ring" = variants_spike_ring,
  hammer = variants_hammer,
  bars = variants_bars,
  "grid-dots" = variants_grid_dots,
  checkerboard = variants_checkerboard,
  "box-lines" = variants_box_lines,
  "nested-polygon" = variants_nested_polygon,
  "sector-pie" = variants_sector_pie,
  "block-letter" = variants_block_letter
)

random_variants <- function(shape) {
  fn <- VARIANT_GENERATORS[[shape]]
  if (is.null(fn)) stop(sprintf("No variant generator for shape '%s'", shape), call. = FALSE)
  fn()
}
