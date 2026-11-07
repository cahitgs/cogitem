# cogitem 0.1.0

* Initial release: R port of the 3x3 matrix-item core of the Cogitem web
  application.
* 15 figural carriers x 13 relational rules (identity, distribution of two
  and three, two-axis progression, rotation, mirror, addition, subtraction,
  multiplication, AND, OR, XOR, XNOR); `supported_combinations()` lists the
  admissible pairs.
* Visual-uniqueness guarantee for response options (`visual_signature()`,
  `is_item_valid()`), seeded generation (`generate_items()`,
  `generate_mixed()`), calibrated a priori difficulty
  (`calibrate_difficulty()`).
* Design-feature records for explanatory IRT / LLTM analyses
  (`item_features()`, `q_matrix()`).
* grid-based rendering in light and dark themes (`plot()`, `shape_grob()`,
  `matrix_grob()`, `options_grob()`), export to PNG/PDF/SVG and PDF booklets
  (`save_item()`, `save_items()`, `save_booklet()`).
* JSON interchange with the web application (`write_items_json()`,
  `read_items_json()`).
* Shiny application for generation, browsing and computerised administration
  with response-time and hover logging (`run_app()`).
