# Two-layer sweep over option counts: for every outer/inner pair below, generate
# items with the default settings and measure the image-similarity
# distance between every pair of options; report items that could
# not be generated as well.
# usage: Rscript sweep-layered.R <outdir> <tag> <counts e.g. 2,3,4,5,6,7,8>
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]
tag <- args[2]
counts <- as.integer(strsplit(args[3], ",")[[1]])
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
t0 <- Sys.time()
pairs <- list(c("polygon", "progression", "dice", "dist-of-3"),
              c("polygon", "dist-of-3", "checkerboard", "xor"),
              c("spike-ring", "progression", "grid-dots", "addition"),
              c("spike-ring", "identity", "arrow", "rotation"),
              c("polygon", "dist-of-3", "box-lines", "dist-of-3"),
              c("spike-ring", "dist-of-3", "block-letter", "identity"),
              c("polygon", "identity", "star", "progression"),
              c("spike-ring", "progression", "polygon", "progression"),
              c("polygon", "progression", "hammer", "mirror"),
              c("spike-ring", "identity", "annulus", "dist-of-3"),
              c("polygon", "dist-of-3", "bars", "progression"),
              c("spike-ring", "dist-of-3", "dice", "addition"))
rows <- list()
sets <- list()
for (no in counts) for (cb in pairs) {
  w <- NULL
  set <- withCallingHandlers(generate_layered(cb[1:2], cb[3:4], n = 6, seed = 77L + no, n_options = no),
                             warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  m <- attr(set, "meta"); if (is.null(m)) m <- set$meta
  sets[[length(sets) + 1]] <- data.frame(pair = paste(cb[1], cb[3], sep = "+"), rules = paste(cb[2], cb[4], sep = "+"),
                                         n_options = no, items = length(set), attempts = m$attempts,
                                         invalid = m$invalid_skipped, ambiguous = m$perceptual_skipped,
                                         stringsAsFactors = FALSE)
  for (it in set) {
    stopifnot(is_item_valid(it, n_options = no))
    a <- audit_item(it)
    D <- a$option_distances
    k <- it$correct
    rows[[length(rows) + 1]] <- data.frame(pair = paste(cb[1], cb[3], sep = "+"), n_options = no, id = it$id,
                                           min_pair = min(D, na.rm = TRUE), min_vs_correct = min(D[k, -k]),
                                           min_cell = a$min_cell_distance,
                                           sig_dup = anyDuplicated(vapply(it$options, visual_signature, "")) > 0,
                                           tiers = paste(it$meta$distractor_tiers, collapse = ";"),
                                           stringsAsFactors = FALSE)
  }
  cat(sprintf("n_options %d %-26s items %d/6 attempts %3d invalid %3d ambiguous %3d  %.0f s\n", no, paste(cb[1], cb[3], sep = "+"),
              length(set), m$attempts, m$invalid_skipped, m$perceptual_skipped, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
res <- do.call(rbind, rows)
gen <- do.call(rbind, sets)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
utils::write.csv(gen, file.path(out, paste0(tag, "-sets.csv")), row.names = FALSE)
cat(sprintf("\nitems: %d | signature duplicates: %d | exact duplicates (0): %d | < 0.05: %d | < 0.15: %d\n",
            nrow(res), sum(res$sig_dup), sum(res$min_pair == 0), sum(res$min_pair < 0.05), sum(res$min_pair < 0.15)))
cat(sprintf("pairs with the correct answer: min %.3f | < 0.15: %d\n", min(res$min_vs_correct), sum(res$min_vs_correct < 0.15)))
cat(sprintf("sets short of 6 items: %d of %d\n", sum(gen$items < 6), nrow(gen)))
print(gen[gen$items < 6, ], row.names = FALSE)
cat("\nby n_options:\n")
print(do.call(rbind, lapply(split(gen, gen$n_options), function(d) data.frame(n_options = d$n_options[1], sets = nrow(d), items = sum(d$items), of = 6 * nrow(d), mean_attempts = round(mean(d$attempts), 1)))), row.names = FALSE)
print(do.call(rbind, lapply(split(res, res$n_options), function(d) data.frame(n_options = d$n_options[1], min_pair = round(min(d$min_pair), 3), min_vs_correct = round(min(d$min_vs_correct), 3), min_cell = round(min(d$min_cell, na.rm = TRUE), 3)))), row.names = FALSE)
cat("\ntier mix with 8 options:\n")
t8 <- unlist(strsplit(res$tiers[res$n_options == 8], ";"))
print(table(sub(":.*", "", t8)))
cat(sprintf("\nelapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
