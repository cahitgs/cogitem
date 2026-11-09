# Why are two-layer candidates rejected? Replays the two-layer sets of the
# evaluation (the seeds of recheck-rules.R) and records, for every rejected
# candidate, the reason: no admissible outer layer or inner layer within the
# allowed draws (a layer must not rely on size, the outer layer must not rely
# on fill and must leave room for the inner layer), a composition without a
# distractor that is wrong in only one layer, or a composed item that fails
# the design constraints of is_item_valid().
# usage: Rscript tools/validation/layered-rejections.R <outdir> <tag> [counts] [n per set]
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; tag <- args[2]
counts <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else c(4L, 6L, 8L)
n_per <- if (length(args) >= 4) as.integer(args[4]) else 3L
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
ns <- asNamespace("cogitem")
orig <- get("make_layered_one", ns)
reasons <- character(0)
wrapped <- function(...) tryCatch(orig(...), error = function(e) { reasons <<- c(reasons, conditionMessage(e)); stop(e) })
if (bindingIsLocked("make_layered_one", ns)) unlockBinding("make_layered_one", ns)
assign("make_layered_one", wrapped, envir = ns)

outer <- do.call(rbind, lapply(layer_shapes("outer"), function(s) data.frame(shape = s, rule = layer_rules(s, "outer"), stringsAsFactors = FALSE)))
inner <- do.call(rbind, lapply(layer_shapes("inner"), function(s) data.frame(shape = s, rule = layer_rules(s, "inner"), stringsAsFactors = FALSE)))
pairs <- merge(outer, inner, by = NULL, suffixes = c("_o", "_i"))
rows <- list(); t0 <- Sys.time()
for (no in counts) for (k in seq_len(nrow(pairs))) {
  cb <- c(pairs$shape_o[k], pairs$rule_o[k], pairs$shape_i[k], pairs$rule_i[k])
  reasons <- character(0)
  set <- suppressWarnings(generate_layered(cb[1:2], cb[3:4], n = n_per, seed = 500L + no * 1000L + k, n_options = no))
  m <- attr(set, "meta")
  known <- reasons %in% c("outer layer rejected", "inner layer rejected") | grepl("composition lacks", reasons, fixed = TRUE)
  rows[[length(rows) + 1]] <- data.frame(
    outer = paste(cb[1], cb[2], sep = "/"), inner = paste(cb[3], cb[4], sep = "/"), n_options = no,
    items = length(set), attempts = m$attempts, invalid = m$invalid_skipped, duplicates = m$duplicates_skipped,
    outer_layer = sum(reasons == "outer layer rejected"), inner_layer = sum(reasons == "inner layer rejected"),
    composition = sum(grepl("composition lacks", reasons, fixed = TRUE)), other_error = sum(!known),
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows)
res$invalid_item <- res$invalid - res$outer_layer - res$inner_layer - res$composition - res$other_error
dir.create(out, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
tot <- colSums(res[, c("items", "attempts", "invalid", "duplicates", "outer_layer", "inner_layer", "composition", "other_error", "invalid_item")])
print(tot)
pct <- function(x) 100 * tot[[x]] / tot[["attempts"]]
cat(sprintf("share of candidates: no admissible outer layer %.1f%% | no admissible inner layer %.1f%% | composition without a single-layer distractor %.1f%% | other errors %.1f%% | composed item fails the design constraints %.1f%% | duplicates %.2f%% | accepted %.1f%%\n",
            pct("outer_layer"), pct("inner_layer"), pct("composition"), pct("other_error"), pct("invalid_item"), pct("duplicates"), pct("items")))
cat("by outer specification (attempts per set; shares of candidates):\n")
print(do.call(rbind, lapply(split(res, res$outer), function(d) data.frame(
  sets = nrow(d), attempts_per_set = round(mean(d$attempts), 2), outer_layer = round(100 * sum(d$outer_layer) / sum(d$attempts), 1),
  composition = round(100 * sum(d$composition) / sum(d$attempts), 1), invalid_item = round(100 * sum(d$invalid_item) / sum(d$attempts), 1)))))
cat(sprintf("elapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
