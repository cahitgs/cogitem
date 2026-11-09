# Exhaustive two-layer sweep: every (outer shape, outer rule) x (inner shape,
# inner rule) pair admitted by layer_shapes()/layer_rules(), generated with the
# default settings (image-similarity screen off since 0.1.3), option pairs
# measured afterwards with the screen's distance.
# usage: Rscript sweep-layered-all.R <outdir> <tag> <counts> [n_per_pair]
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; tag <- args[2]
counts <- as.integer(strsplit(args[3], ",")[[1]])
n_per <- if (length(args) >= 4) as.integer(args[4]) else 3L
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
outer <- do.call(rbind, lapply(layer_shapes("outer"), function(s) data.frame(shape = s, rule = layer_rules(s, "outer"), stringsAsFactors = FALSE)))
inner <- do.call(rbind, lapply(layer_shapes("inner"), function(s) data.frame(shape = s, rule = layer_rules(s), stringsAsFactors = FALSE)))
pairs <- merge(outer, inner, by = NULL, suffixes = c("_o", "_i"))
cat(sprintf("outer specs %d x inner specs %d = %d pairs; counts %s; %d items per pair\n", nrow(outer), nrow(inner), nrow(pairs), paste(counts, collapse = ","), n_per))
t0 <- Sys.time()
rows <- list(); sets <- list()
for (no in counts) for (k in seq_len(nrow(pairs))) {
  cb <- c(pairs$shape_o[k], pairs$rule_o[k], pairs$shape_i[k], pairs$rule_i[k])
  set <- suppressWarnings(generate_layered(cb[1:2], cb[3:4], n = n_per, seed = 500L + no * 1000L + k, n_options = no))
  m <- attr(set, "meta"); if (is.null(m)) m <- set$meta
  sets[[length(sets) + 1]] <- data.frame(outer = paste(cb[1], cb[2], sep = "/"), inner = paste(cb[3], cb[4], sep = "/"), n_options = no,
                                         items = length(set), attempts = m$attempts, invalid = m$invalid_skipped, ambiguous = m$perceptual_skipped,
                                         stringsAsFactors = FALSE)
  for (it in set) {
    stopifnot(is_item_valid(it, n_options = no))
    a <- audit_item(it); D <- a$option_distances; kk <- it$correct
    rows[[length(rows) + 1]] <- data.frame(outer = paste(cb[1], cb[2], sep = "/"), inner = paste(cb[3], cb[4], sep = "/"), n_options = no, id = it$id,
                                           min_pair = min(D, na.rm = TRUE), min_vs_correct = min(D[kk, -kk]), min_cell = a$min_cell_distance,
                                           n_pairs = sum(!is.na(D[upper.tri(D)])), pairs_005 = sum(D[upper.tri(D)] < 0.05, na.rm = TRUE),
                                           pairs_015 = sum(D[upper.tri(D)] < 0.15, na.rm = TRUE),
                                           sig_dup = anyDuplicated(vapply(it$options, visual_signature, "")) > 0,
                                           tiers = paste(it$meta$distractor_tiers, collapse = ";"), stringsAsFactors = FALSE)
  }
  if (k %% 25 == 0) cat(sprintf("n_options %d: %d / %d pairs, %d items, %.0f s\n", no, k, nrow(pairs), length(rows), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
res <- do.call(rbind, rows); gen <- do.call(rbind, sets)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
utils::write.csv(gen, file.path(out, paste0(tag, "-sets.csv")), row.names = FALSE)
cat(sprintf("\nitems: %d | signature duplicates: %d | exact duplicates (0): %d | < 0.05: %d | < 0.15: %d\n",
            nrow(res), sum(res$sig_dup), sum(res$min_pair == 0), sum(res$min_pair < 0.05), sum(res$min_pair < 0.15)))
cat(sprintf("option pairs: %d | pairs < 0.05: %d | pairs < 0.15: %d | items with a pair < 0.15: %d\n",
            sum(res$n_pairs), sum(res$pairs_005), sum(res$pairs_015), sum(res$min_pair < 0.15)))
cat(sprintf("pairs with the correct answer: min %.3f | < 0.15: %d\n", min(res$min_vs_correct), sum(res$min_vs_correct < 0.15)))
cat(sprintf("sets short of %d items: %d of %d (sets with 0 items: %d)\n", n_per, sum(gen$items < n_per), nrow(gen), sum(gen$items == 0)))
short <- gen[gen$items < n_per, ]
if (nrow(short)) print(short[order(short$items), ], row.names = FALSE)
cat("\nby n_options:\n")
print(do.call(rbind, lapply(split(gen, gen$n_options), function(d) data.frame(n_options = d$n_options[1], pairs = nrow(d), items = sum(d$items), of = n_per * nrow(d), mean_attempts = round(mean(d$attempts), 1), max_attempts = max(d$attempts)))), row.names = FALSE)
print(do.call(rbind, lapply(split(res, res$n_options), function(d) data.frame(n_options = d$n_options[1], min_pair = round(min(d$min_pair), 3), min_vs_correct = round(min(d$min_vs_correct), 3), min_cell = round(min(d$min_cell, na.rm = TRUE), 3)))), row.names = FALSE)
cat("\nworst 10 items:\n")
print(head(res[order(res$min_pair), c("outer", "inner", "n_options", "min_pair", "min_vs_correct")], 10), row.names = FALSE)
cat(sprintf("\nelapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
