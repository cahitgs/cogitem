# Composite-cell audit of two-layer items at display size. The package audits
# two-layer items layer by layer (each layer rendered alone at full tile
# size). This script measures, for one item per outer x inner pair, the
# perceptual distance between the response options rendered as COMPOSITE
# cells (outer carrier, disc and scaled inner carrier together) at the
# default 96-px tile and at a larger tile that approximates the size at which
# the application displays a cell, and compares them with the layer-wise
# result. usage: Rscript audit-display-size.R <outdir> <tag> <counts> [px]
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; tag <- args[2]
counts <- as.integer(strsplit(args[3], ",")[[1]])
px_large <- if (length(args) >= 4) as.integer(args[4]) else 192L
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
prepare_tiles <- get("prepare_tiles", asNamespace("cogitem"))
tile_distance <- get("tile_distance", asNamespace("cogitem"))
composite <- function(shapes, px) {
  tiles <- prepare_tiles(shapes, px, 3)
  n <- length(tiles); D <- matrix(NA_real_, n, n)
  for (i in seq_len(n - 1)) for (j in (i + 1):n) D[i, j] <- D[j, i] <- tile_distance(tiles[[i]], tiles[[j]])
  D
}
outer <- do.call(rbind, lapply(layer_shapes("outer"), function(s) data.frame(shape = s, rule = layer_rules(s, "outer"), stringsAsFactors = FALSE)))
inner <- do.call(rbind, lapply(layer_shapes("inner"), function(s) data.frame(shape = s, rule = layer_rules(s), stringsAsFactors = FALSE)))
pairs <- merge(outer, inner, by = NULL, suffixes = c("_o", "_i"))
rows <- list(); t0 <- Sys.time()
for (no in counts) for (k in seq_len(nrow(pairs))) {
  cb <- c(pairs$shape_o[k], pairs$rule_o[k], pairs$shape_i[k], pairs$rule_i[k])
  set <- suppressWarnings(generate_layered(cb[1:2], cb[3:4], n = 1, seed = 500L + no * 1000L + k, n_options = no))
  if (!length(set)) next
  it <- set[[1]]; key <- it$correct
  tiers <- it$meta$distractor_tiers
  a <- audit_item(it)                      # layer-wise, 96 px
  D96 <- composite(it$options, 96L)
  Dlg <- composite(it$options, px_large)
  inner_only <- which(startsWith(tiers, "inner:"))
  outer_only <- which(startsWith(tiers, "outer:"))
  rows[[length(rows) + 1]] <- data.frame(
    outer = paste(cb[1], cb[2], sep = "/"), inner = paste(cb[3], cb[4], sep = "/"), n_options = no, id = it$id,
    inner_scale = it$meta$inner_scale,
    layerwise_min = min(a$option_distances, na.rm = TRUE),
    comp96_min = min(D96, na.rm = TRUE), complarge_min = min(Dlg, na.rm = TRUE),
    comp96_key_inner = if (length(inner_only)) min(D96[key, inner_only]) else NA_real_,
    complarge_key_inner = if (length(inner_only)) min(Dlg[key, inner_only]) else NA_real_,
    comp96_key_outer = if (length(outer_only)) min(D96[key, outer_only]) else NA_real_,
    complarge_key_outer = if (length(outer_only)) min(Dlg[key, outer_only]) else NA_real_,
    stringsAsFactors = FALSE)
  if (k %% 50 == 0) cat(sprintf("n_options %d: %d / %d pairs, %.0f s\n", no, k, nrow(pairs), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(out, paste0(tag, ".csv")), row.names = FALSE)
cat(sprintf("\nitems: %d | tile %d px\n", nrow(res), px_large))
cat(sprintf("layer-wise 96 px: min %.3f | below 0.15: %d\n", min(res$layerwise_min), sum(res$layerwise_min < 0.15)))
cat(sprintf("composite 96 px: min %.3f | below 0.15: %d | key vs inner-only below 0.15: %d of %d\n",
            min(res$comp96_min), sum(res$comp96_min < 0.15), sum(res$comp96_key_inner < 0.15, na.rm = TRUE), sum(!is.na(res$comp96_key_inner))))
cat(sprintf("composite %d px: min %.3f | below 0.15: %d | key vs inner-only below 0.15: %d of %d\n", px_large,
            min(res$complarge_min), sum(res$complarge_min < 0.15), sum(res$complarge_key_inner < 0.15, na.rm = TRUE), sum(!is.na(res$complarge_key_inner))))
cat("\nquantiles of key vs inner-only distance (composite, large tile):\n"); print(round(quantile(res$complarge_key_inner, c(0, .01, .05, .25, .5), na.rm = TRUE), 3))
cat("quantiles of key vs inner-only distance (composite, 96 px):\n"); print(round(quantile(res$comp96_key_inner, c(0, .01, .05, .25, .5), na.rm = TRUE), 3))
cat(sprintf("\nelapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
