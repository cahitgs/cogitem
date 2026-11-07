# cogitem

Rule-based automatic generation of Raven-like 3x3 matrix items in R.

`cogitem` is the R port of the matrix-item core of the
[Cogitem](https://cahitgs.github.io/itemgen1/) web application. It builds
figural matrix items from an explicit item model (a figural carrier crossed
with a relational rule), guarantees that the response options of every item
are visually distinct, records the design features of every item for
explanatory item response models, draws items with `grid`, exchanges items
with the web application through JSON, and ships a Shiny application for
computerised administration.

## Installation

```r
# install.packages("remotes")
remotes::install_github("cahitgs/cogitem")
```

## Quick start

```r
library(cogitem)

items <- generate_items("dice", "addition", n = 20, seed = 42)
items
#> <cog_item_set> 20 item(s) | dice x addition | seed 42

plot(items[[1]], show_answer = TRUE)

# design features (Q-matrix input) ------------------------------------
head(item_features(items))
Q <- q_matrix(items, ~ rule + shape)

# a test that samples every rule available for one carrier ----------
specs <- subset(supported_combinations(), shape == "sector-pie")
test <- generate_mixed(specs, n_each = 3, seed = 1)
save_booklet(test, "sector-pie-test.pdf")

# interoperability with the web application ---------------------------
write_items_json(test, "sector-pie-test.json")

# computerised administration -----------------------------------------
run_app(test)
```

## The item model

Every item is one **shape family** crossed with one **rule**. The rules
correspond to the relational taxonomy of Carpenter, Just and Shell (1990) for
the Raven Progressive Matrices, extended with bitwise logic.

| Rule | Family | Carriers |
| --- | --- | --- |
| `identity` | constant in a row | all 15 shapes |
| `dist-of-3` | distribution of three values | all 15 shapes |
| `dist-of-2` | distribution of two values + blank | 9 shapes with clear silhouettes |
| `progression` | quantitative pairwise progression on two axes | 14 shapes |
| `addition`, `subtraction` | figure addition / subtraction on a count | 7 count-based shapes |
| `multiplication` | figure multiplication | `dice`, `sector-pie` |
| `rotation` | sliding-window rotation | `arrow`, `hammer` |
| `mirror` | 180-degree flip of a row or column | `arrow`, `hammer` |
| `and`, `or`, `xor`, `xnor` | bitwise logic on a presence mask | `sector-pie`, `checkerboard` |

Shape families: annulus, dice, polygon, star, arrow, petals, spike ring,
hammer, bars, grid dots, checkerboard, box lines, nested polygon, sector pie,
block letter. `supported_combinations()` lists the 60+ admissible pairs.

### Option uniqueness

Each response option is compared with every other option through
`visual_signature()`, which normalises rotation by the shape's rotational
symmetry and strips imperceptible count differences (15 versus 16 spikes).
Distractors are drawn first from rule-derived candidates (the other
distribution variants, grid neighbours, operands, wrong operators, bit flips)
and then from an ordered pool of perturbations (count, colour, size, fill,
rotation, stroke width). Items that fail `is_item_valid()` are discarded and
regenerated.

### Design features and difficulty

`item_features()` returns, for every item, the rule and its family, the
carrier, the number of options, the calibrated a priori difficulty and its
components, how the distractors were constructed, and rule-specific
parameters (varying axis, progression axes, rotation step, operands, bit
count). `q_matrix()` turns these into a design matrix for linear logistic
test models (`eRm::LLTM()`), explanatory IRT models (`lme4::glmer()`,
`TAM`) or any other analysis that links design features to empirical item
difficulty.

## Interoperability with the web application

`write_items_json()` produces the same JSON layout as the web application's
"Download JSON" button (0-based `correctIndex`, `ShapeConfig` cells), and
`read_items_json()` reads such files. Items generated in R can therefore be
administered in the browser, and web-generated items can be analysed in R.

## Shiny application

`run_app()` opens an application with four panels: **Generate** (shape,
rule, n, seed, option count; gallery preview; JSON, feature-CSV and PDF
booklet downloads), **Browse** (single items with answer key and design
features), **Administer** (one item at a time, logging the chosen option,
response time and the hover trace of every option, with CSV export) and
**About**.

## Related software

* matRiks (Brancaccio, Epifania, Anselmi & de Chiusole, 2025), an R package
  generating Raven-like matrices with rule-based distractors.
* IMak (Blum & Holling, 2018), an R package generating figural analogies.
* Sandia Matrices (Matzen et al., 2010), the Java generator of Raven-like
  matrices.
* Corvus (Thimbleby), the browser generator that inspired the Cogitem web
  application.

## License

MIT. Copyright (c) 2026 Mehmet Cahit Marangoz.
