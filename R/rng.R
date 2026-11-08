# Random helpers built on R's own RNG so that `set.seed()` (or the `seed`
# argument of generate_items()) makes every batch reproducible.

# Pick one element of a vector or list.
rpick <- function(x) x[[sample.int(length(x), 1L)]]

# Pick k distinct elements (without replacement), keeping the container type.
rsample <- function(x, k) {
  if (k > length(x)) stop("rsample: k > length(x)", call. = FALSE)
  x[sample.int(length(x), k)]
}

# Random integer in [min, max] inclusive (returned as double).
rint <- function(min, max) {
  if (max < min) stop("rint: max < min", call. = FALSE)
  as.numeric(min) + (sample.int(as.integer(max - min + 1), 1L) - 1)
}

# Uniform draw on [0, 1).
runif1 <- function() stats::runif(1)

# Fisher-Yates style shuffle returning both the result and the permutation.
rshuffle <- function(x) {
  perm <- sample.int(length(x))
  list(result = x[perm], perm = perm)
}

# A fresh random seed (used when the caller does not supply one).
random_seed <- function() sample.int(.Machine$integer.max, 1L)

# Evaluate `code` under a temporary RNG state seeded with `seed`, then restore
# the caller's RNG state so that package calls do not disturb user scripts.
# The RNG kind is fixed for the duration of the call (Mersenne-Twister,
# inversion, rejection sampling) and the item-id counter is restarted, so the
# same seed yields the same items, identifiers included, in any R session
# with the same package version, whatever RNG kind the caller has chosen.
with_seed <- function(seed, code) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) get(".Random.seed", envir = globalenv()) else NULL
  old_kind <- RNGkind()
  old_counter <- .cog_env$counter
  on.exit({
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    .cog_env$counter <- old_counter
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  .cog_env$counter <- 0L
  force(code)
}

# Item id counter (`puz-<base36>-<counter>`). The base36 part comes from the
# seeded RNG and the counter restarts in every with_seed() call, so generated
# ids are reproducible under a seed; hand-made items (cog_item()) continue the
# session counter.
.cog_env <- new.env(parent = emptyenv())
.cog_env$counter <- 0L

to_base36 <- function(n) {
  digits <- c(0:9, letters)
  n <- as.numeric(n)
  if (n <= 0) return("0")
  out <- character(0)
  while (n > 0) {
    out <- c(digits[(n %% 36) + 1], out)
    n <- n %/% 36
  }
  paste(out, collapse = "")
}

next_item_id <- function() {
  .cog_env$counter <- .cog_env$counter + 1L
  sprintf("puz-%s-%d", to_base36(floor(stats::runif(1) * 1e9)), .cog_env$counter)
}
