# cogitem 0.1.0

* Initial release: R port of the 3x3 matrix-item core of the Cogitem web
  application.
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
* Design-feature records for explanatory IRT / LLTM analyses
  (`item_features()`, `q_matrix()`).
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
