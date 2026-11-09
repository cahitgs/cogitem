# Cross-platform reproducibility check. Generates a reference item set (every
# supported single-layer combination with 2 items and 4 options, plus twelve
# two-layer pairs with 2 items and 6 options, all with fixed seeds), writes a
# canonical text dump of the items' structure (identifiers, cells, options,
# keys, distractor tiers) and the MD5 hash of that dump. The GitHub Actions
# workflow "reproducibility" runs this script on Ubuntu, Windows and macOS and
# fails when the hashes differ; locally, run it twice (or on two machines) and
# compare hash.txt.
# usage: Rscript tools/validation/reproducibility.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "repro"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
`%||%` <- function(a, b) if (is.null(a)) b else a

num <- function(x) as.character(signif(as.numeric(x), 12))
canon <- function(s) {
  if (identical(s$kind, "stack")) {
    return(paste0("stack[", canon(s$layers[[1]]), ";", canon(s$layers[[2]]), ";", num(s$inner_scale %||% NA), "]"))
  }
  p <- s$params
  p <- p[order(names(p))]
  pv <- vapply(seq_along(p), function(i) paste0(names(p)[i], "=", if (is.numeric(p[[i]])) num(p[[i]]) else as.character(p[[i]])), "")
  paste0(s$kind, "(", num(s$size), ",", num(s$rotation), ",", s$fill %||% "NA", ",", s$stroke, ",", num(s$strokeWidth), ";", paste(pv, collapse = ","), ")")
}
item_text <- function(it) {
  cells <- unlist(it$cells, recursive = FALSE)
  paste(it$id, it$type %||% "single", it$rule, it$shape, it$correct, it$n_options %||% length(it$options),
        paste(vapply(cells, canon, ""), collapse = "|"),
        paste(vapply(it$options, canon, ""), collapse = "|"),
        paste(it$meta$distractor_tiers %||% "", collapse = ","),
        sep = "\t")
}

t0 <- Sys.time()
lines <- character(0)
combos <- supported_combinations()
for (i in seq_len(nrow(combos))) {
  set <- suppressWarnings(generate_items(combos$shape[i], combos$rule[i], n = 2, seed = 1000L + i, n_options = 4))
  lines <- c(lines, vapply(set, item_text, ""))
}
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
for (k in seq_along(pairs)) {
  set <- suppressWarnings(generate_layered(pairs[[k]][1:2], pairs[[k]][3:4], n = 2, seed = 2000L + k, n_options = 6))
  lines <- c(lines, vapply(set, item_text, ""))
}
# binary connection: identical bytes on every platform (no CRLF translation)
con <- file(file.path(out, "items.txt"), "wb"); writeLines(lines, con, sep = "\n", useBytes = TRUE); close(con)
h <- unname(tools::md5sum(file.path(out, "items.txt")))
info <- sprintf("%s %d-items %s R-%s cogitem-%s", h, length(lines), R.version$platform, getRversion(),
                as.character(utils::packageVersion("cogitem")))
con <- file(file.path(out, "hash.txt"), "wb"); writeLines(info, con, sep = "\n"); close(con)
cat(info, "\n")
cat(sprintf("elapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
