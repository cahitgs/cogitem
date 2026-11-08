# Sweep over option counts given on the command line: for every supported
# shape x rule combination, generate items WITHOUT the perceptual filter and
# measure the perceptual distance between every pair of options.
# usage: Rscript sweep-options.R <outdir> <tag> <counts e.g. 2,3,5,6,7> [layered]
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]
tag <- args[2]
counts <- as.integer(strsplit(args[3], ",")[[1]])
with_layered <- length(args) >= 4 && args[4] == "layered"
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)

combos <- supported_combinations()
rows <- list()
t0 <- Sys.time()
measure <- function(it, shape, rule, no, seed) {
  stopifnot(is_item_valid(it))
  a <- audit_item(it)
  D <- a$option_distances
  k <- it$correct
  data.frame(shape = shape, rule = rule, n_options = no, seed = seed, id = it$id,
             n_opt_actual = length(it$options),
             min_pair = min(D, na.rm = TRUE),
             n_pairs = sum(!is.na(D[upper.tri(D)])),
             pairs_005 = sum(D[upper.tri(D)] < 0.05, na.rm = TRUE),
             pairs_015 = sum(D[upper.tri(D)] < 0.15, na.rm = TRUE),
             min_vs_correct = min(D[k, -k]),
             min_cell = a$min_cell_distance,
             sig_dup = anyDuplicated(vapply(it$options, visual_signature, "")) > 0,
             tiers = paste(it$meta$distractor_tiers, collapse = ";"),
             stringsAsFactors = FALSE)
}
short <- list()
for (no in counts) {
  for (i in seq_len(nrow(combos))) {
    for (seed in 1:2) {
      set <- suppressWarnings(generate_items(combos$shape[i], combos$rule[i], n = 8,
                                             seed = seed * 1000L + no + i, n_options = no))
      if (length(set) < 8) short[[length(short) + 1]] <- sprintf("%s/%s n_options %d seed %d: %d items", combos$shape[i], combos$rule[i], no, seed, length(set))
      for (it in set) rows[[length(rows) + 1]] <- measure(it, combos$shape[i], combos$rule[i], no, seed)
    }
    if (i %% 10 == 0) cat(sprintf("n_options %d: %d / %d combos, %d items, %.0f s\n",
                                  no, i, nrow(combos), length(rows),
                                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
}
if (with_layered) {
  layered <- list(c("polygon", "progression", "dice", "dist-of-3"),
                  c("polygon", "dist-of-3", "checkerboard", "xor"),
                  c("spike-ring", "progression", "grid-dots", "addition"),
                  c("spike-ring", "identity", "arrow", "rotation"),
                  c("polygon", "addition", "box-lines", "dist-of-3"),
                  c("spike-ring", "dist-of-3", "block-letter", "identity"))
  for (no in counts) for (cb in layered) {
    set <- suppressWarnings(generate_layered(cb[1:2], cb[3:4], n = 6, seed = 77L + no, n_options = no))
    if (length(set) < 6) short[[length(short) + 1]] <- sprintf("layered %s+%s n_options %d: %d items", cb[1], cb[3], no, length(set))
    for (it in set) rows[[length(rows) + 1]] <- measure(it, it$shape, it$rule, no, 77L)
  }
}
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
cat(sprintf("\nitems: %d | signature duplicates: %d | exact duplicates (0): %d | < 0.05: %d | < 0.15: %d\n",
            nrow(res), sum(res$sig_dup), sum(res$min_pair == 0), sum(res$min_pair < 0.05),
            sum(res$min_pair < 0.15)))
cat(sprintf("option pairs: %d | pairs < 0.05: %d | pairs < 0.15: %d | items with a pair < 0.15: %d\n",
            sum(res$n_pairs), sum(res$pairs_005), sum(res$pairs_015), sum(res$min_pair < 0.15)))
cat(sprintf("pairs with the correct answer: min %.3f | < 0.05: %d | < 0.15: %d\n",
            min(res$min_vs_correct), sum(res$min_vs_correct < 0.05), sum(res$min_vs_correct < 0.15)))
cat(sprintf("items with fewer options than requested: %d\n", sum(res$n_opt_actual != res$n_options)))
cat("\nsets short of the requested item count:\n")
if (length(short)) cat(paste(short, collapse = "\n"), "\n") else cat("none\n")
cat("\nby n_options (items, min pair distance, min vs correct, < 0.15):\n")
print(do.call(rbind, lapply(split(res, res$n_options), function(d) data.frame(n_options = d$n_options[1], items = nrow(d), min_pair = round(min(d$min_pair), 3), min_vs_correct = round(min(d$min_vs_correct), 3), below_015 = sum(d$min_pair < 0.15), fallback_items = sum(grepl("fallback", d$tiers))))), row.names = FALSE)
cat("\nworst 15 items by option distance:\n")
print(head(res[order(res$min_pair), c("shape", "rule", "n_options", "min_pair", "min_vs_correct", "tiers")], 15), row.names = FALSE)
cat(sprintf("\nelapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
