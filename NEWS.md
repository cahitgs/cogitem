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
