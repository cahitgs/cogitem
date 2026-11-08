# cogitem 0.1.2

* Two-layer items with four or more options always contain a distractor that
  is wrong only in the inner layer and one that is wrong only in the outer
  layer; compositions without them are rejected and regenerated (before, about
  0.5 % of items lacked an inner-only distractor, so the outer rule alone
  identified their key).

# cogitem 0.1.1

* Reproducibility: `generate_items()`, `generate_layered()`, `generate_mixed()`
  and `sample_items()` fix the RNG kind (Mersenne-Twister, inversion,
  rejection sampling) for the duration of the call and restore the caller's
  RNG state afterwards; item identifiers are derived from the seeded stream
  instead of a session counter. The same seed therefore yields identical
  items, identifiers included, in any R session with the same package
  version.
* `visual_signature()` serialises colours as they are drawn (display palette,
  lower-case hex), so `"#A78BFA"`, `"#a78bfa"` and a colour name for the same
  RGB value share one signature; options that differ only in stroke or fill
  colours closer than 20 CIELAB units count as confusable, in line with the
  perceptual audit.
* Two-layer items: the inner layer keeps its line weight when it is shrunk
  inside a narrow outer carrier (strokes are scaled by a fixed factor, not by
  the item's inner scale). Before, the six combinations of a polygon under
  addition or subtraction (which always contain a triangle) with an annulus
  under progression, addition or subtraction could not be generated at all,
  because the thinner rings failed the hairline-ring rule.
* `q_matrix()` reference-codes every complete categorical feature after the
  first (and all of them with `intercept = TRUE`) and warns when the matrix
  is not of full column rank, naming constant columns.
* The documentation, the package description and the application describe
  the two option-distinctness checks (symbolic and perceptual) and the
  default of the perceptual audit (off for single-layer, on for two-layer
  items) instead of a "guarantee".

# cogitem 0.1.0

* Initial release.
* 15 figural carriers x 13 relational rules (identity, distribution of two
  and three, two-axis progression, rotation, mirror, addition, subtraction,
  multiplication, AND, OR, XOR, XNOR); `supported_combinations()` lists the
  admissible pairs.
* Visual-uniqueness guarantee for response options (`visual_signature()`,
  `is_item_valid()`), seeded generation (`generate_items()`,
  `generate_mixed()`).
* Renderer-based perceptual audit (`perceptual_distance()`, `audit_item()`,
  `audit_items()`, `generate_items(perceptual = TRUE)`) that flags items
  whose options or matrix cells look alike although they are nominally
  different. Symbolic checks additionally reject imperceptible count or
  single-cell checkerboard differences between cells of the same row or
  column.
* `is_item_valid()` also verifies that the matrix instantiates its rule
  (identity, Latin square, progression axes, rotation steps, mirror,
  `a op b = c` for the arithmetic and bitwise rules), so hand-made items are
  checked too. Attributes a carrier does not draw (the fill of checkerboards,
  bars, sector pies and block letters, the stroke width of unfilled dot
  grids) are ignored by `visual_signature()` and never used for distractors.
* Symbolic confusability also covers size steps below 0.06, stroke-width steps
  below 2 units and rotations that leave every component of a carrier within
  a few cell units of a symmetric position (a nested heptagon/octagon rotated
  by 45 degrees); pattern carriers take their rotational symmetry from the
  pattern itself, and quarter turns are folded into that pattern, so a glyph
  turned by 180 degrees and its rotated twin, or a 2 x 3 dot grid turned by
  90 degrees and a 3 x 2 grid, share one signature. Fill distractors use a
  stronger tint.
* `q_matrix()` uses explicit indicator coding (one column per level, a
  missing attribute is 0), so it also works for single-level factors and
  rule-specific features; the perceptual audit falls back to the **png**
  package when **ragg** is not installed; the Shiny administration accepts
  exactly one answer per item (late clicks are ignored).
* Design-feature records for explanatory IRT / LLTM analyses
  (`item_features()`, `q_matrix()`).
* Two-layer items scale the inner layer and the disc under it to the clear
  interior of the narrowest outer carrier in the item (`meta$inner_scale`),
  so a triangle keeps its edges intact.
* Redesigned Shiny interface: grouped sidebar forms, an empty state, a
  statistics strip and a one-item-at-a-time viewer after generation (arrow
  keys page through the set), design features and
  audit result next to the browsed item, a test-mode administration with
  numbered options, keyboard shortcuts (1-8, Enter) and a results summary.
  Bundled typefaces (Fraunces, Manrope; SIL OFL), package logo in the header
  and as favicon; the display accent follows the logo (ink blue).
* Distribution of two is now admissible for box lines, nested polygons and
  sector pies as well (12 carriers, 84 shape-rule combinations); the rule is
  withheld only from carriers with sparse or empty states (grid dots,
  checkerboard, block letter).
* Sector pies are compared by what they draw: dividers between solid sectors
  are invisible, so an all-solid pie has one signature whatever its sector
  count, and dividers running through hatched or dotted sectors do not count
  as a difference. The side count of a small inner polygon (nested polygons)
  and the number of shallow or crowded spikes (spike rings) are treated as
  unreadable, calibrated with the perceptual audit.
* Two-layer items work with any option count from 2 to 8: each layer is
  generated with at most four options and the composed distractors are drawn
  from the combinations of the layers' wrong options (previously the outer
  layer had to supply all distractors itself and failed beyond five options).
  Stacked options and cells are compared with the inner layer at the scale it
  is drawn, so inner stroke-width or small size steps never serve as the only
  difference; the perceptual audit compares two-layer items layer by layer,
  each layer rendered at full tile size, because a blurred tile of the whole
  cell cannot resolve the inner carrier.
* Two-layer items (`generate_layered()`, `layer_shapes()`, `layer_rules()`):
  an outer and an inner carrier with their own rules superimposed cell by
  cell, as `com()` does in matRiks; distractors are wrong in the inner
  layer, in the outer layer or in both.
* grid-based rendering (`plot()`, `shape_grob()`,
  `matrix_grob()`, `options_grob()`), export to PNG/PDF/SVG and PDF booklets
  (`save_item()`, `save_items()`, `save_booklet()`).
* Shiny application for generation, browsing and computerised administration
  with response-time and hover logging (`run_app()`). Generation runs in a
  background R process (**callr**) with a progress bar and a Stop button
  that keeps the items produced so far; `generate_items(progress = )`
  exposes the same hook.
