# Validation scripts

Scripts used for the technical evaluation reported in the manuscript. Run them
from the package root (they load the package with `pkgload::load_all(".")`, or
`library(cogitem)` when the package is installed). Each script writes a CSV of
per-item results and prints a summary. They need **ragg** (or **png**) for the
perceptual distances.

```sh
# single-layer items: every shape-rule combination, 2 seeds x 8 items per
# option count, perceptual audit OFF (measures the symbolic guarantee alone)
Rscript tools/validation/sweep-options.R <outdir> single 2,3,4,5,6,7,8

# twelve representative two-layer pairs, default audit, 6 items each
Rscript tools/validation/sweep-layered.R <outdir> layered 2,3,4,5,6,7,8

# every admissible two-layer pair (408), default audit, 3 items each
Rscript tools/validation/sweep-layered-all.R <outdir> layered-all 4,6,8 3
```

The numbers in the manuscript were obtained with cogitem 0.1.0 (commit
0ac34d7 for the single-layer sweep, f538199 or later for the two-layer sweeps).
