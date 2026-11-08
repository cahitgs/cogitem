# Constants shared across the package.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Stroke colours of the generator palette (designed for the web app's dark
# background; see PALETTE in draw.R for the display mapping).
STROKE_PALETTE <- c(
  "#e4e4e7", # light gray (default)
  "#a78bfa", # accent purple
  "#34d399", # green
  "#f59e0b", # amber
  "#60a5fa"  # blue
)

SIZE_BUCKETS <- c(0.55, 0.75, 0.92)
ROTATION_BUCKETS <- c(0, 45, 90, 135, 180)
DISTRACTOR_FILL <- "rgba(167, 139, 250, 0.45)" # light enough to keep the stroke legible, strong enough to be seen at cell size

# Figural carriers available for 3x3 matrix items.
SHAPES <- c(
  "annulus", "dice", "polygon", "star", "arrow", "petals", "spike-ring",
  "hammer", "bars", "grid-dots", "checkerboard", "box-lines",
  "nested-polygon", "sector-pie", "block-letter"
)

# Relational rules available for 3x3 matrix items.
RULES <- c(
  "identity", "dist-of-3", "dist-of-2", "progression", "rotation",
  "addition", "subtraction", "multiplication", "mirror",
  "and", "or", "xor", "xnor"
)

# Rules whose relation runs along rows only (column values are unrelated).
ROW_ONLY_RULES <- c("addition", "subtraction", "multiplication", "and", "or", "xor", "xnor")

RULE_FAMILY <- c(
  identity = "constant",
  "dist-of-3" = "distribution",
  "dist-of-2" = "distribution",
  progression = "progression",
  rotation = "spatial",
  mirror = "spatial",
  addition = "arithmetic",
  subtraction = "arithmetic",
  multiplication = "arithmetic",
  and = "logical", or = "logical", xor = "logical", xnor = "logical"
)

# Dice faces with 90-degree rotational symmetry vs. 180-degree only.
DICE_90_SYMMETRIC <- c(1, 4, 5, 8, 9)
DICE_180_SYMMETRIC <- c(2, 3, 6, 7)

# Each shape's primary integer ("count") parameter and its valid range.
# NULL means the shape has no single count axis (arithmetic rules excluded).
PRIMARY_PARAM <- list(
  annulus = list(name = "ringCount", min = 1, max = 4),
  dice = list(name = "dotCount", min = 1, max = 9),
  polygon = list(name = "sides", min = 3, max = 8),
  star = list(name = "points", min = 4, max = 10),
  petals = list(name = "petalCount", min = 3, max = 12),
  "spike-ring" = list(name = "spikeCount", min = 4, max = 16),
  arrow = NULL,
  hammer = NULL,
  bars = list(name = "barCount", min = 1, max = 6),
  "grid-dots" = list(name = "rows", min = 1, max = 4),
  checkerboard = NULL,
  "box-lines" = NULL,
  "nested-polygon" = list(name = "outerSides", min = 3, max = 8),
  "sector-pie" = list(name = "sectorCount", min = 2, max = 8),
  "block-letter" = list(name = "patternIndex", min = 0, max = 7)
)

# Secondary (row-wise) progression axes that are visually meaningful per shape.
SECONDARY_AXES <- list(
  annulus = c("size", "strokeWidth"),
  dice = "size",
  polygon = c("size", "strokeWidth"),
  star = c("size", "strokeWidth"),
  arrow = "size",
  hammer = "size",
  bars = c("size", "strokeWidth"),
  "grid-dots" = "size",
  checkerboard = "size",
  "box-lines" = c("size", "strokeWidth"),
  "nested-polygon" = c("size", "strokeWidth"),
  "sector-pie" = c("size", "strokeWidth"),
  petals = c("size", "strokeWidth"),
  "spike-ring" = c("size", "strokeWidth"),
  "block-letter" = "size"
)

# Upper count cap for arithmetic rules so results stay countable.
ARITH_PERCEPTIBLE_MAX <- c("spike-ring" = 8, petals = 8, star = 8)

# Shape eligibility per rule family ----------------------------------------

COUNT_PARAM_SHAPES <- c(
  "annulus", "dice", "polygon", "star", "petals", "spike-ring", "bars",
  "grid-dots", "nested-polygon", "sector-pie"
)
ARITH_SHAPES <- setdiff(COUNT_PARAM_SHAPES, c("spike-ring", "petals", "star"))
MULTIPLICATION_SHAPES <- c("dice", "sector-pie")
ROTATION_SHAPES <- c("arrow", "hammer")
# Distribution of two puts a blank cell in every row and column, so the carrier
# must never look nearly empty itself: carriers with sparse or empty states (a
# lattice reduced to a single dot, an empty checkerboard, a glyph without
# filled cells) are excluded.
DIST_OF_2_SHAPES <- c(
  "annulus", "dice", "polygon", "star", "arrow", "petals", "spike-ring",
  "hammer", "bars", "box-lines", "nested-polygon", "sector-pie"
)
MIRROR_SHAPES <- c("arrow", "hammer")
BOOL_SHAPES <- c("sector-pie", "checkerboard")
PROGRESSION_SHAPES <- setdiff(SHAPES, "block-letter")

# Human-readable labels used by print methods and the Shiny app.
SHAPE_LABELS <- c(
  annulus = "Annulus (concentric rings)",
  dice = "Dice (dot pattern)",
  polygon = "Polygon (regular n-gon)",
  star = "Star (n-pointed)",
  arrow = "Arrow (directional)",
  petals = "Petals (flower)",
  "spike-ring" = "Spike ring (gear)",
  hammer = "Hammer (asymmetric T + corner marker)",
  bars = "Bars (parallel lines)",
  "grid-dots" = "Grid dots (m x n lattice)",
  checkerboard = "Checkerboard (filled / empty cells)",
  "box-lines" = "Box lines (square + inner lines)",
  "nested-polygon" = "Nested polygon",
  "sector-pie" = "Sector pie (patterned sectors)",
  "block-letter" = "Block letter (asymmetric glyph)"
)

RULE_LABELS <- c(
  identity = "Identity - all cells identical",
  "dist-of-3" = "Distribution of three - Latin square",
  "dist-of-2" = "Distribution of two - two variants + blank cell",
  progression = "Progression - two-axis quantitative change",
  rotation = "Rotation - sliding-window pure rotation",
  addition = "Addition - col1 + col2 = col3",
  subtraction = "Subtraction - col1 - col2 = col3",
  multiplication = "Multiplication - col1 x col2 = col3",
  mirror = "Mirror - row 3 (or col 3) is the 180-degree flip",
  and = "AND - bitwise conjunction of col1 and col2",
  or = "OR - bitwise disjunction of col1 and col2",
  xor = "XOR - bitwise exclusive or of col1 and col2",
  xnor = "XNOR - bitwise equivalence of col1 and col2"
)

round2 <- function(x) round(x * 100) / 100

clamp <- function(v, lo, hi) pmax(lo, pmin(hi, v))
