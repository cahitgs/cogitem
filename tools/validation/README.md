# Validation scripts

Scripts used for the technical evaluation reported in the manuscript. Run them
from the package root (they load the package with `pkgload::load_all(".")`, or
`library(cogitem)` when the package is installed). Each script writes CSV files
of per-item results and prints a summary.

## Core properties (no raster engine needed)

```sh
# independent re-check of every generated item: rule relation re-derived from
# the raw cell parameters, key = missing cell, parameter-distinct options;
# single-layer sets as in sweep-attempts.R, two-layer sets for every
# admissible pair (3 items each, 4/6/8 options)
Rscript tools/validation/recheck-rules.R <outdir> recheck 2,3,4,5,6,7,8 4,6,8 3

# generation statistics: attempts per accepted item, invalid candidates and
# structural duplicates for every shape x rule combination and option count
Rscript tools/validation/sweep-attempts.R <outdir> attempts 2,3,4,5,6,7,8

# cross-platform reproducibility: reference set + MD5 hash of its structure
# (the GitHub Actions workflow "reproducibility" compares Ubuntu, Windows, macOS)
Rscript tools/validation/reproducibility.R <outdir>

# scripted administration session in a headless browser: known clicks, hover
# events and delays; the downloaded results CSV is compared with the script
# (needs shiny, bslib, callr, chromote and a Chrome/Edge installation)
Rscript tools/validation/check-log.R <outdir> [port]
```

## Image-similarity screen (experimental; needs ragg or png)

The screen (`audit_items()`, `perceptual_distance()`) is a fixed-resolution
development tool, off by default since version 0.1.3. These scripts measure its
distances for generated sets; they were used to set the symbolic design
constraints and to decide the two-layer design (pinned inner size, enlarged
dots and markers, no shrunken inner layer).

```sh
# single-layer items: every shape-rule combination, 2 seeds x 8 items per
# option count, screen OFF during generation, distances measured afterwards
Rscript tools/validation/sweep-options.R <outdir> single 2,3,4,5,6,7,8

# twelve representative two-layer pairs, 6 items each
Rscript tools/validation/sweep-layered.R <outdir> layered 2,3,4,5,6,7,8

# every admissible two-layer pair, 3 items each
Rscript tools/validation/sweep-layered-all.R <outdir> layered-all 4,6,8 3

# composite-cell distances of two-layer options at 96 px and at a display-size
# tile (default 192 px; pass 144 for the application's cell size)
Rscript tools/validation/audit-display-size.R <outdir> display 4,6,8 144
```

The numbers in the manuscript were obtained with the package version named
there (see the Technical Evaluation section and the Availability statement).
