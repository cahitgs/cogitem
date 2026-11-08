# Generation statistics per set: for every supported shape x rule combination
# and option count, regenerate the sets of sweep-options.R (same seeds, no
# audit, no perceptual measurement) and record attempts, invalid candidates
# and structural duplicates, so that rejection rates can be reported by rule
# family and option count.
# usage: Rscript sweep-attempts.R <outdir> <tag> <counts e.g. 2,3,4,5,6,7,8>
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; tag <- args[2]
counts <- as.integer(strsplit(args[3], ",")[[1]])
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
`%||%` <- function(a, b) if (is.null(a)) b else a
combos <- supported_combinations()
rows <- list(); t0 <- Sys.time()
for (no in counts) for (i in seq_len(nrow(combos))) for (seed in 1:2) {
  set <- suppressWarnings(generate_items(combos$shape[i], combos$rule[i], n = 8, seed = seed * 1000L + no + i, n_options = no))
  m <- attr(set, "meta")
  rows[[length(rows) + 1]] <- data.frame(shape = combos$shape[i], rule = combos$rule[i], family = combos$family[i],
                                         n_options = no, seed = seed, items = length(set),
                                         attempts = m$attempts %||% NA, invalid = m$invalid_skipped %||% NA,
                                         duplicates = m$duplicates_skipped %||% m$duplicates %||% NA,
                                         stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
cat("meta fields:", paste(names(attr(set, "meta")), collapse = ", "), "\n")
cat(sprintf("sets: %d | items: %d | attempts: %d | invalid: %d | duplicates: %d | elapsed %.0f s\n",
            nrow(res), sum(res$items), sum(res$attempts), sum(res$invalid, na.rm = TRUE), sum(res$duplicates, na.rm = TRUE),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat("\nby rule family (attempts per accepted item, share of invalid candidates):\n")
print(do.call(rbind, lapply(split(res, res$family), function(d) data.frame(
  family = d$family[1], items = sum(d$items), attempts_per_item = round(sum(d$attempts) / sum(d$items), 2),
  invalid_share = round(sum(d$invalid, na.rm = TRUE) / sum(d$attempts), 3),
  duplicate_share = round(sum(d$duplicates, na.rm = TRUE) / sum(d$attempts), 3)))), row.names = FALSE)
cat("\nby option count:\n")
print(do.call(rbind, lapply(split(res, res$n_options), function(d) data.frame(
  n_options = d$n_options[1], items = sum(d$items), attempts_per_item = round(sum(d$attempts) / sum(d$items), 2),
  invalid_share = round(sum(d$invalid, na.rm = TRUE) / sum(d$attempts), 3)))), row.names = FALSE)
cat("\ncombinations with the most attempts per item:\n")
agg <- aggregate(cbind(attempts, items) ~ shape + rule, res, sum); agg$per_item <- round(agg$attempts / agg$items, 2)
print(head(agg[order(-agg$per_item), ], 10), row.names = FALSE)
