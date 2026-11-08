#' Figural carriers and relational rules
#'
#' `cog_shapes()` lists the 15 shape families, `cog_rules()` the 13 rules.
#' `supported_combinations()` returns every shape-rule pair that produces
#' solvable items; `is_supported()`, `supported_rules()` and
#' `supported_shapes()` query that table.
#'
#' @param shape,rule Shape or rule name.
#' @return Character vectors, a data frame, or a logical.
#' @examples
#' cog_rules()
#' supported_rules("arrow")
#' head(supported_combinations())
#' @name support
NULL

#' @rdname support
#' @export
cog_shapes <- function() SHAPES

#' @rdname support
#' @export
cog_rules <- function() RULES

#' @rdname support
#' @export
shape_labels <- function() SHAPE_LABELS

#' @rdname support
#' @export
rule_labels <- function() RULE_LABELS

SUPPORTED <- local({
  pairs <- list()
  add <- function(shapes, rules) {
    for (s in shapes) for (r in rules) pairs[[length(pairs) + 1]] <<- c(s, r)
  }
  add(SHAPES, c("identity", "dist-of-3"))
  add(PROGRESSION_SHAPES, "progression")
  add(ARITH_SHAPES, c("addition", "subtraction"))
  add(ROTATION_SHAPES, "rotation")
  add(DIST_OF_2_SHAPES, "dist-of-2")
  add(MIRROR_SHAPES, "mirror")
  add(MULTIPLICATION_SHAPES, "multiplication")
  add(BOOL_SHAPES, c("and", "or", "xor", "xnor"))
  m <- do.call(rbind, pairs)
  data.frame(shape = m[, 1], rule = m[, 2],
             family = unname(RULE_FAMILY[m[, 2]]), stringsAsFactors = FALSE)
})

#' @rdname support
#' @export
supported_combinations <- function() SUPPORTED

#' @rdname support
#' @export
is_supported <- function(shape, rule) {
  any(SUPPORTED$shape == shape & SUPPORTED$rule == rule)
}

#' @rdname support
#' @export
supported_rules <- function(shape) {
  shape <- match.arg(shape, SHAPES)
  RULES[RULES %in% SUPPORTED$rule[SUPPORTED$shape == shape]]
}

#' @rdname support
#' @export
supported_shapes <- function(rule) {
  rule <- match.arg(rule, RULES)
  SHAPES[SHAPES %in% SUPPORTED$shape[SUPPORTED$rule == rule]]
}

new_item_set <- function(items, meta = list()) {
  structure(items, class = "cog_item_set", meta = meta)
}

#' Generate Raven-like matrix items
#'
#' Produces `n` distinct, validated 3x3 matrix items for one shape-rule
#' combination. Generation is seeded, so the same `seed` always yields the
#' same items. Items whose options are not visually distinct (see
#' [is_item_valid()]) are discarded and regenerated; structural duplicates
#' (see [item_signature()]) are removed.
#'
#' @param shape One of [cog_shapes()].
#' @param rule One of [cog_rules()]; must be supported for `shape`, see
#'   [supported_rules()].
#' @param n Number of items requested.
#' @param seed Integer seed. `NULL` draws a random seed, which is stored in
#'   the result so the batch can be reproduced.
#' @param n_options Number of response options (2-8, default 4). One option
#'   is correct; the rest are distractors.
#' @param max_attempts Maximum number of generation attempts before giving up.
#' @param progress Optional function called after every accepted item and at
#'   least every 25 attempts with three arguments: the number of items
#'   generated so far, the number requested and the number of attempts. Return
#'   `FALSE` from it to stop early; the items produced so far are returned and
#'   `meta$stopped` is `TRUE`. The function must not draw random numbers.
#' @param perceptual Also run the renderer-based [audit_item()] on every
#'   candidate and discard items whose options or cells are perceptually
#'   confusable. Slower (each item is rasterised) and needs **ragg** or a
#'   cairo-capable R; the result is reproducible for a given seed on the same
#'   graphics stack.
#' @param perceptual_threshold Distance below which two renderings count as
#'   confusable, see [perceptual_distance()].
#' @return An object of class `cog_item_set`: a list of `cog_item` objects
#'   with a `meta` attribute (shape, rule, seed, counts of skipped
#'   duplicates, invalid and perceptually ambiguous attempts).
#' @examples
#' items <- generate_items("annulus", "dist-of-3", n = 5, seed = 42)
#' items
#' items[[1]]
#' plot(items[[1]])
#' @seealso [generate_item()], [generate_mixed()], [item_features()],
#'   [audit_items()], [run_app()]
#' @export
generate_items <- function(shape, rule, n = 10, seed = NULL, n_options = 4,
                           max_attempts = NULL, perceptual = FALSE,
                           perceptual_threshold = DEFAULT_PERCEPTUAL_THRESHOLD,
                           progress = NULL) {
  shape <- match.arg(shape, SHAPES)
  rule <- match.arg(rule, RULES)
  if (!is_supported(shape, rule)) {
    stop(sprintf("Rule '%s' is not supported for shape '%s'. See supported_rules('%s').",
                 rule, shape, shape), call. = FALSE)
  }
  stopifnot(length(n) == 1, n >= 1, n_options >= 2, n_options <= 8)
  n <- as.integer(n)
  n_options <- as.integer(n_options)
  seed <- as.integer(seed %||% random_seed())
  max_attempts <- as.integer(max_attempts %||% max(100, n * 8))
  if (!is.null(progress)) stopifnot(is.function(progress))
  report <- function(done, attempts) {
    if (is.null(progress)) return(TRUE)
    !isFALSE(progress(done, n, attempts))
  }

  res <- with_seed(seed, {
    items <- vector("list", n)
    n_items <- 0L
    seen <- new.env(hash = TRUE, parent = emptyenv())
    attempts <- 0L
    duplicates <- 0L
    invalid <- 0L
    ambiguous <- 0L
    consecutive_dup <- 0L
    last_error <- NULL
    stopped <- FALSE
    while (n_items < n && attempts < max_attempts) {
      if (attempts %% 25L == 0L && !report(n_items, attempts)) {
        stopped <- TRUE
        break
      }
      attempts <- attempts + 1L
      p <- tryCatch(generate_one(shape, rule, n_options), error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      })
      if (is.null(p) || !is_item_valid(p, n_options = n_options)) {
        invalid <- invalid + 1L
        next
      }
      sig <- item_signature(p)
      if (exists(sig, envir = seen, inherits = FALSE)) {
        duplicates <- duplicates + 1L
        consecutive_dup <- consecutive_dup + 1L
        if (consecutive_dup > 500L) break
        next
      }
      if (perceptual && audit_item(p, threshold = perceptual_threshold)$flagged) {
        ambiguous <- ambiguous + 1L
        next
      }
      assign(sig, TRUE, envir = seen)
      n_items <- n_items + 1L
      items[[n_items]] <- p
      consecutive_dup <- 0L
      if (!report(n_items, attempts)) {
        stopped <- TRUE
        break
      }
    }
    list(items = items[seq_len(n_items)], attempts = attempts,
         duplicates = duplicates, invalid = invalid, ambiguous = ambiguous,
         last_error = last_error, stopped = stopped)
  })

  if (length(res$items) < n && !res$stopped) {
    msg <- sprintf("Only %d of %d requested %s/%s items could be generated (%d attempts, %d duplicates, %d invalid, %d perceptually ambiguous).",
                   length(res$items), n, shape, rule, res$attempts, res$duplicates,
                   res$invalid, res$ambiguous)
    if (length(res$items) == 0 && !is.null(res$last_error)) {
      msg <- paste0(msg, " Last generator error: ", res$last_error)
    }
    warning(msg, call. = FALSE)
  }

  new_item_set(res$items, meta = list(
    shape = shape, rule = rule, n_requested = n, seed = seed,
    n_options = n_options, duplicates_skipped = res$duplicates,
    invalid_skipped = res$invalid, perceptual = perceptual,
    perceptual_threshold = if (perceptual) perceptual_threshold else NA_real_,
    perceptual_skipped = res$ambiguous, attempts = res$attempts,
    stopped = res$stopped,
    reached_ceiling = length(res$items) < n && !res$stopped
  ))
}

#' Generate a single item
#'
#' Convenience wrapper around [generate_items()] returning one `cog_item`.
#'
#' @inheritParams generate_items
#' @return A `cog_item`.
#' @examples
#' it <- generate_item("dice", "addition", seed = 7)
#' it
#' @export
generate_item <- function(shape, rule, seed = NULL, n_options = 4) {
  set <- generate_items(shape, rule, n = 1, seed = seed, n_options = n_options)
  if (length(set) == 0) stop("No valid item could be generated.", call. = FALSE)
  set[[1]]
}

#' Generate a mixed item set from several shape-rule specifications
#'
#' Draws `n_each` items
#' for every row of `specs` and combines them in one set (optionally
#' shuffled), for example to assemble a test that samples every rule.
#'
#' @param specs A data frame with columns `shape` and `rule`, for example a
#'   subset of [supported_combinations()].
#' @param n_each Items per specification (recycled).
#' @param seed Integer seed for the whole set.
#' @param n_options Number of response options.
#' @param shuffle Shuffle the combined item order?
#' @inheritParams generate_items
#' @return A `cog_item_set`.
#' @examples
#' specs <- subset(supported_combinations(), shape == "dice")
#' test <- generate_mixed(specs, n_each = 2, seed = 1)
#' table(item_features(test)$rule)
#' @export
generate_mixed <- function(specs, n_each = 5, seed = NULL, n_options = 4, shuffle = TRUE,
                           perceptual = FALSE, perceptual_threshold = DEFAULT_PERCEPTUAL_THRESHOLD) {
  stopifnot(is.data.frame(specs), all(c("shape", "rule") %in% names(specs)))
  seed <- as.integer(seed %||% random_seed())
  n_each <- rep_len(n_each, nrow(specs))
  items <- list()
  sources <- list()
  with_seed(seed, {
    for (i in seq_len(nrow(specs))) {
      sub_seed <- sample.int(.Machine$integer.max, 1L)
      set <- generate_items(specs$shape[i], specs$rule[i], n = n_each[i],
                            seed = sub_seed, n_options = n_options,
                            perceptual = perceptual,
                            perceptual_threshold = perceptual_threshold)
      items <- c(items, unclass(set))
      sources[[i]] <- list(shape = specs$shape[i], rule = specs$rule[i],
                           n = length(set), seed = sub_seed,
                           perceptual_skipped = attr(set, "meta")$perceptual_skipped)
    }
    if (shuffle && length(items) > 1) items <- items[sample.int(length(items))]
  })
  new_item_set(items, meta = list(shape = "mixed", rule = "mixed", seed = seed,
                                  n_options = n_options, perceptual = perceptual,
                                  perceptual_skipped = sum(vapply(sources, function(s) {
                                    s$perceptual_skipped %||% 0L
                                  }, numeric(1))),
                                  sources = sources))
}

#' Sample items from a set
#'
#' @param x A `cog_item_set`.
#' @param n Number of items to draw without replacement.
#' @param seed Optional seed.
#' @return A `cog_item_set`.
#' @export
sample_items <- function(x, n, seed = NULL) {
  stopifnot(inherits(x, "cog_item_set"), n <= length(x))
  idx <- if (is.null(seed)) sample.int(length(x), n) else with_seed(seed, sample.int(length(x), n))
  x[idx]
}

# S3 methods -------------------------------------------------------------------

#' @export
print.cog_item <- function(x, ...) {
  cat(sprintf("<cog_item %s> %s x %s | %d options | answer #%d\n",
              x$id, x$shape, x$rule, x$n_options, x$correct))
  invisible(x)
}

#' @export
print.cog_item_set <- function(x, ...) {
  m <- attr(x, "meta")
  cat(sprintf("<cog_item_set> %d item(s)", length(x)))
  if (!is.null(m$shape)) cat(sprintf(" | %s x %s", m$shape, m$rule))
  if (!is.null(m$seed)) cat(sprintf(" | seed %s", format(m$seed)))
  cat("\n")
  if (length(x) > 0) {
    f <- item_features(x)
    tab <- table(rule = f$rule, shape = f$shape)
    if (length(unique(f$rule)) > 1 || length(unique(f$shape)) > 1) {
      print(tab)
    }
  }
  if (!is.null(m$attempts)) {
    cat(sprintf("attempts %d | duplicates skipped %d | invalid skipped %d%s%s\n",
                m$attempts, m$duplicates_skipped, m$invalid_skipped,
                if (isTRUE(m$perceptual)) sprintf(" | perceptually ambiguous skipped %d",
                                                  m$perceptual_skipped) else "",
                if (isTRUE(m$reached_ceiling)) " | ceiling reached" else ""))
  }
  invisible(x)
}

#' @export
`[.cog_item_set` <- function(x, i) {
  new_item_set(unclass(x)[i], meta = attr(x, "meta"))
}

#' @export
c.cog_item_set <- function(...) {
  sets <- list(...)
  items <- do.call(c, lapply(sets, unclass))
  new_item_set(items, meta = list(shape = "mixed", rule = "mixed",
                                  sources = lapply(sets, attr, "meta")))
}

#' @export
as.data.frame.cog_item_set <- function(x, ...) item_features(x)

#' @export
summary.cog_item_set <- function(object, ...) {
  f <- item_features(object)
  structure(list(n = nrow(f), by_rule = table(f$rule), by_shape = table(f$shape),
                 meta = attr(object, "meta")), class = "summary.cog_item_set")
}

#' @export
print.summary.cog_item_set <- function(x, ...) {
  cat(sprintf("cog_item_set with %d items\n", x$n))
  cat("\nBy rule:\n"); print(x$by_rule)
  cat("\nBy shape:\n"); print(x$by_shape)
  invisible(x)
}
