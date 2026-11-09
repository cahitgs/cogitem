# Independent re-check of generated items. The items of the evaluation sweeps
# are regenerated from their seeds and every item is checked with code that
# shares nothing with the generator or with is_item_valid(): the rule relation
# is re-derived from the raw cell parameters, the key must equal the missing
# cell parameter by parameter, and no two options may be parameter-identical.
# Two-layer items are checked layer by layer and on the stacked options.
# usage: Rscript tools/validation/recheck-rules.R <outdir> <tag> [single counts] [layered counts] [n per layered set]
#   e.g. Rscript tools/validation/recheck-rules.R out recheck 2,3,4,5,6,7,8 4,6,8 3
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; tag <- args[2]
counts <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else 2:8
lcounts <- if (length(args) >= 4) as.integer(strsplit(args[4], ",")[[1]]) else c(4L, 6L, 8L)
n_layered <- if (length(args) >= 5) as.integer(args[5]) else 3L
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- independent helpers (no package internals) -----------------------------
ATTRS <- c("size", "strokeWidth", "rotation", "stroke", "fill")
COUNT_PARAM <- c(annulus = "ringCount", dice = "dotCount", polygon = "sides", star = "points",
                 petals = "petalCount", "spike-ring" = "spikeCount", bars = "barCount",
                 "grid-dots" = "rows", "nested-polygon" = "outerSides", "sector-pie" = "sectorCount")
num <- function(x) as.character(signif(as.numeric(x), 12))
canon <- function(s) {
  if (identical(s$kind, "stack")) return(paste0("stack[", canon(s$layers[[1]]), ";", canon(s$layers[[2]]), "]"))
  p <- s$params; p <- p[order(names(p))]
  pv <- vapply(seq_along(p), function(i) paste0(names(p)[i], "=", if (is.numeric(p[[i]])) num(p[[i]]) else as.character(p[[i]])), "")
  paste0(s$kind, "(", num(s$size), ",", num(s$rotation %% 360), ",", s$fill %||% "NA", ",", s$stroke, ",", num(s$strokeWidth), ";", paste(pv, collapse = ","), ")")
}
is_blank <- function(s) isTRUE(s$params$blank == 1)
get_attr <- function(s, name) if (name %in% ATTRS) s[[name]] else s$params[[name]]
set_attr <- function(s, name, value) { if (name %in% ATTRS) s[[name]] <- value else s$params[[name]] <- value; s }
neutral <- function(s, name) set_attr(s, name, if (name == "rotation") 0 else if (name %in% c("stroke", "fill")) "x" else 0)
same_after_neutralising <- function(shapes, name) length(unique(vapply(shapes, function(s) canon(neutral(s, name)), ""))) == 1
monotone <- function(v) all(diff(v) > 0) || all(diff(v) < 0)
const_step <- function(v) { d <- diff(v) %% 360; length(unique(round(d, 6))) == 1 && d[1] != 0 }
presence_mask <- function(s) {
  p <- s$params
  if (s$kind == "checkerboard") {
    bits <- round(p$rows %||% 3) * round(p$cols %||% 3)
    return(c(round(p$pattern %||% 0), bits))
  }
  if (s$kind == "sector-pie") {
    n <- round(p$sectorCount %||% 6)
    packed <- as.integer(round(p$sectorPatterns %||% 0))
    mask <- 0L
    for (i in seq_len(n)) if (bitwAnd(bitwShiftR(packed, as.integer(3 * (i - 1))), 7L) != 0) mask <- bitwOr(mask, bitwShiftL(1L, as.integer(i - 1)))
    return(c(mask, n))
  }
  NULL
}
bool_op <- function(op, a, b, full) switch(op, and = bitwAnd(bitwAnd(a, b), full), or = bitwAnd(bitwOr(a, b), full),
                                           xor = bitwAnd(bitwXor(a, b), full), xnor = bitwAnd(bitwNot(bitwXor(a, b)), full))
arith <- function(op, a, b) switch(op, addition = a + b, subtraction = a - b, multiplication = a * b)

## ---- rule relation from the cells ----------------------------------------------
rule_ok <- function(rule, shape, cells, meta) {
  at <- function(r, c) cells[[r]][[c]]
  rows <- lapply(1:3, function(r) cells[[r]])
  cols <- lapply(1:3, function(c) lapply(1:3, function(r) at(r, c)))
  cn <- function(shapes) vapply(shapes, canon, "")
  switch(rule,
    identity = length(unique(unlist(lapply(rows, cn)))) == 1,
    "dist-of-3" = {
      sets <- lapply(rows, function(r) sort(cn(r)))
      all(vapply(sets, function(s) length(unique(s)) == 3, TRUE)) && identical(sets[[1]], sets[[2]]) &&
        identical(sets[[1]], sets[[3]]) && all(vapply(cols, function(c) length(unique(cn(c))) == 3, TRUE))
    },
    "dist-of-2" = {
      vis <- lapply(rows, function(r) { b <- vapply(r, is_blank, TRUE); if (sum(b) != 1) return(NULL); sort(cn(r[!b])) })
      all(!vapply(vis, is.null, TRUE)) && all(lengths(vis) == 2) && all(vapply(vis, function(v) length(unique(v)) == 2, TRUE)) &&
        identical(vis[[1]], vis[[2]]) && identical(vis[[1]], vis[[3]])
    },
    progression = {
      pa <- meta$primary_axis; sa <- meta$secondary_axis
      if (is.null(pa) || is.null(sa)) return(FALSE)
      ok <- TRUE
      for (r in 1:3) {
        v <- lapply(rows[[r]], get_attr, name = pa)
        if (any(vapply(v, is.null, TRUE))) return(FALSE)
        v <- unlist(v)
        ok <- ok && length(unique(v)) == 3 && (if (pa == "rotation") const_step(v) else if (is.numeric(v) && pa != "patternIndex") monotone(v) else TRUE)
        ok <- ok && same_after_neutralising(rows[[r]], pa)
      }
      pv <- lapply(1:3, function(r) unlist(lapply(rows[[r]], get_attr, name = pa)))
      ok <- ok && identical(pv[[1]], pv[[2]]) && identical(pv[[1]], pv[[3]])
      for (c in 1:3) {
        v <- unlist(lapply(cols[[c]], get_attr, name = sa))
        ok <- ok && length(unique(v)) == 3 && (if (sa == "rotation") const_step(v) else if (is.numeric(v)) monotone(v) else TRUE)
        ok <- ok && same_after_neutralising(cols[[c]], sa)
      }
      sv <- lapply(1:3, function(c) unlist(lapply(cols[[c]], get_attr, name = sa)))
      ok && identical(sv[[1]], sv[[2]]) && identical(sv[[1]], sv[[3]])
    },
    rotation = {
      rot <- function(r, c) at(r, c)$rotation %% 360
      steps <- c(unlist(lapply(1:3, function(r) c(rot(r, 2) - rot(r, 1), rot(r, 3) - rot(r, 2)))),
                 unlist(lapply(1:3, function(c) c(rot(2, c) - rot(1, c), rot(3, c) - rot(2, c))))) %% 360
      length(unique(round(steps, 6))) == 1 && steps[1] != 0 && same_after_neutralising(unlist(rows, recursive = FALSE), "rotation")
    },
    mirror = {
      flip <- function(s) set_attr(s, "rotation", (s$rotation + 180) %% 360)
      if (identical(meta$mirror_axis, "horizontal")) {
        identical(cn(rows[[1]]), cn(rows[[2]])) && identical(cn(lapply(rows[[1]], flip)), cn(rows[[3]]))
      } else if (identical(meta$mirror_axis, "vertical")) {
        identical(cn(cols[[1]]), cn(cols[[2]])) && identical(cn(lapply(cols[[1]], flip)), cn(cols[[3]]))
      } else FALSE
    },
    addition = , subtraction = , multiplication = {
      pn <- COUNT_PARAM[shape]
      if (is.na(pn)) return(FALSE)
      all(vapply(1:3, function(r) {
        v <- unlist(lapply(rows[[r]], function(s) s$params[[pn]]))
        length(v) == 3 && length(unique(v)) == 3 && arith(rule, v[1], v[2]) == v[3] && same_after_neutralising(rows[[r]], pn)
      }, TRUE))
    },
    and = , or = , xor = , xnor = {
      all(vapply(1:3, function(r) {
        m <- lapply(rows[[r]], presence_mask)
        if (any(vapply(m, is.null, TRUE))) return(FALSE)
        bits <- m[[1]][2]
        if (m[[2]][2] != bits || m[[3]][2] != bits) return(FALSE)
        full <- 2^bits - 1
        pn <- if (shape == "checkerboard") "pattern" else "sectorPatterns"
        bool_op(rule, m[[1]][1], m[[2]][1], full) == m[[3]][1] && same_after_neutralising(rows[[r]], pn)
      }, TRUE))
    },
    NA)
}

check_item <- function(it) {
  opts <- it$options
  key_ok <- canon(opts[[it$correct]]) == canon(it$cells[[3]][[3]])
  distinct_ok <- anyDuplicated(vapply(opts, canon, "")) == 0
  if (identical(it$type, "layered")) {
    rules <- character(0); oks <- logical(0)
    for (k in 1:2) {
      lay <- if (k == 1) it$meta$outer else it$meta$inner
      cells <- lapply(it$cells, function(row) lapply(row, function(s) s$layers[[k]]))
      rules <- c(rules, lay$rule); oks <- c(oks, isTRUE(rule_ok(lay$rule, lay$shape, cells, lay)))
    }
    # the key must differ from every distractor in at least one layer, and
    # every distractor must be wrong in at least one layer (parameter-wise)
    key <- opts[[it$correct]]
    wrong_ok <- all(vapply(setdiff(seq_along(opts), it$correct), function(j) {
      canon(opts[[j]]$layers[[1]]) != canon(key$layers[[1]]) || canon(opts[[j]]$layers[[2]]) != canon(key$layers[[2]])
    }, TRUE))
    rule_res <- all(oks); note <- if (rule_res) "" else paste(rules[!oks], collapse = "+")
    return(data.frame(id = it$id, type = "layered", shape = it$shape, rule = it$rule, n_options = length(opts),
                      key_ok = key_ok, distinct_ok = distinct_ok && wrong_ok, rule_ok = rule_res, note = note, stringsAsFactors = FALSE))
  }
  r <- rule_ok(it$rule, it$shape, it$cells, it$meta %||% list())
  data.frame(id = it$id, type = "single", shape = it$shape, rule = it$rule, n_options = length(opts),
             key_ok = key_ok, distinct_ok = distinct_ok, rule_ok = isTRUE(r), note = if (is.na(r)) "rule not checked" else "", stringsAsFactors = FALSE)
}

## ---- regenerate the sweep items and check them -------------------------------------
t0 <- Sys.time(); rows <- list(); sets <- list()
note_set <- function(set, type, what, no) {
  m <- attr(set, "meta")
  sets[[length(sets) + 1]] <<- data.frame(type = type, spec = what, n_options = no, requested = m$n_requested,
                                          items = length(set), attempts = m$attempts, invalid = m$invalid_skipped,
                                          duplicates = m$duplicates_skipped, stringsAsFactors = FALSE)
}
combos <- supported_combinations()
for (no in counts) for (i in seq_len(nrow(combos))) for (seed in 1:2) {
  set <- suppressWarnings(generate_items(combos$shape[i], combos$rule[i], n = 8, seed = seed * 1000L + no + i, n_options = no))
  note_set(set, "single", paste(combos$shape[i], combos$rule[i], sep = "/"), no)
  for (it in set) rows[[length(rows) + 1]] <- check_item(it)
}
cat(sprintf("single-layer items checked: %d (%.0f s)\n", length(rows), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
outer <- do.call(rbind, lapply(layer_shapes("outer"), function(s) data.frame(shape = s, rule = layer_rules(s, "outer"), stringsAsFactors = FALSE)))
inner <- do.call(rbind, lapply(layer_shapes("inner"), function(s) data.frame(shape = s, rule = layer_rules(s, "inner"), stringsAsFactors = FALSE)))
pairs <- merge(outer, inner, by = NULL, suffixes = c("_o", "_i"))
n_single <- length(rows)
for (no in lcounts) for (k in seq_len(nrow(pairs))) {
  cb <- c(pairs$shape_o[k], pairs$rule_o[k], pairs$shape_i[k], pairs$rule_i[k])
  set <- suppressWarnings(generate_layered(cb[1:2], cb[3:4], n = n_layered, seed = 500L + no * 1000L + k, n_options = no))
  note_set(set, "layered", paste(cb[1], cb[2], cb[3], cb[4], sep = "/"), no)
  for (it in set) rows[[length(rows) + 1]] <- check_item(it)
}
res <- do.call(rbind, rows)
gen <- do.call(rbind, sets)
dir.create(out, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
utils::write.csv(gen, file.path(out, paste0(tag, "-sets.csv")), row.names = FALSE)
cat(sprintf("\nsets: single %d (items %d, attempts %d, max attempts per set %d), two-layer %d (items %d of %d requested, attempts %d, mean %.2f per set, max %d)\n",
            sum(gen$type == "single"), sum(gen$items[gen$type == "single"]), sum(gen$attempts[gen$type == "single"]), max(gen$attempts[gen$type == "single"]),
            sum(gen$type == "layered"), sum(gen$items[gen$type == "layered"]), sum(gen$requested[gen$type == "layered"]),
            sum(gen$attempts[gen$type == "layered"]), mean(gen$attempts[gen$type == "layered"]), max(gen$attempts[gen$type == "layered"])))
fail <- res[!(res$key_ok & res$distinct_ok & res$rule_ok), ]
utils::write.csv(fail, file.path(out, paste0(tag, "-failures.csv")), row.names = FALSE)
cat(sprintf("\nitems checked: %d (single %d, two-layer %d) | key = missing cell: %d | options parameter-distinct: %d | rule relation holds: %d | all three: %d\n",
            nrow(res), n_single, nrow(res) - n_single, sum(res$key_ok), sum(res$distinct_ok), sum(res$rule_ok), nrow(res) - nrow(fail)))
cat("by rule (items, all checks passed):\n")
print(do.call(rbind, lapply(split(res, res$rule), function(d) data.frame(items = nrow(d), passed = sum(d$key_ok & d$distinct_ok & d$rule_ok)))))
if (nrow(fail)) { cat("\nFAILURES:\n"); print(utils::head(fail, 30)) }
cat(sprintf("\nelapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
