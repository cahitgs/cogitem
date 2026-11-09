# Scripted administration session. Starts the application in a background R
# process, generates a known item set, answers every item through a headless
# browser with known choices, known hover events and known delays, downloads
# the results CSV and compares it with what was scripted and with the keys of
# the same item set regenerated in this process. Needs the suggested packages
# shiny, bslib, callr and chromote (and a Chrome or Edge installation).
# usage: Rscript tools/validation/check-log.R <outdir> [port]
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "check-log"
port <- if (length(args) >= 2) as.integer(args[2]) else 7011L
dir.create(out, showWarnings = FALSE, recursive = TRUE)
root <- normalizePath(".")
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(cogitem)
for (pkg in c("callr", "chromote", "shiny", "bslib")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("package '", pkg, "' is needed for this script")
}

## ---- the scripted session ------------------------------------------------------
n_items <- 6L; seed <- 11L; n_options <- 4L
participant <- "check-log"
picks <- c(2L, 4L, 1L, 3L, 2L, 1L)                 # option clicked per item (1-based)
delays <- c(1200L, 1800L, 900L, 2400L, 1500L, 1100L) # ms between item display and click
hovers <- list(c(1L, 3L, 2L), c(4L), integer(0), c(3L, 3L), c(2L, 1L), c(1L))
ref <- generate_items("annulus", "dist-of-3", n = n_items, seed = seed, n_options = n_options)
keys <- vapply(ref, function(it) it$correct, integer(1))

## ---- start the application -----------------------------------------------------
app <- callr::r_bg(function(root, port) {
  if (file.exists(file.path(root, "DESCRIPTION"))) pkgload::load_all(root, quiet = TRUE) else library(cogitem)
  cogitem::run_app(port = port, host = "127.0.0.1", launch.browser = FALSE)
}, args = list(root = root, port = port), stdout = file.path(out, "app.log"), stderr = file.path(out, "app.log"))
on.exit(try(app$kill(), silent = TRUE), add = TRUE)
url <- sprintf("http://127.0.0.1:%d", port)
t0 <- Sys.time()
repeat {
  ok <- tryCatch({ con <- url(url); on.exit(close(con), add = TRUE); length(readLines(con, n = 1, warn = FALSE)) > 0 }, error = function(e) FALSE)
  if (ok) break
  if (!app$is_alive()) stop("the application process died; see ", file.path(out, "app.log"))
  if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 90) stop("the application did not start within 90 s")
  Sys.sleep(0.5)
}

## ---- drive it ------------------------------------------------------------------
b <- chromote::ChromoteSession$new(width = 1366, height = 900)
on.exit(try(b$close(), silent = TRUE), add = TRUE)
js <- function(code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
run <- function(code) invisible(b$Runtime$evaluate(paste0("(function(){", code, " return true;})()"), returnByValue = TRUE))
wait_for <- function(code, timeout = 60) {
  t <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t, units = "secs")) < timeout) {
    if (isTRUE(tryCatch(js(code), error = function(e) FALSE))) return(invisible(TRUE))
    Sys.sleep(0.25)
  }
  stop("timeout waiting for: ", code)
}
b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(out))
b$Page$navigate(url)
wait_for("document.querySelector('#generate') !== null")
Sys.sleep(1.5)
run(sprintf("$('#n').val(%d).trigger('change'); $('#seed').val('%d').trigger('change');", n_items, seed))
Sys.sleep(0.8)
run("document.getElementById('generate').click();")
wait_for("document.querySelectorAll('.cog-viewer').length > 0", timeout = 120)
Sys.sleep(1)
run("var a=[...document.querySelectorAll('.navbar .nav-link')].find(x=>x.textContent.trim()==='Administer'); if(a){a.click();}")
Sys.sleep(1.2)
run(sprintf("$('#participant').val('%s').trigger('change');", participant))
Sys.sleep(0.6)
run("document.getElementById('start').click();")
wait_for("document.querySelectorAll('.cog-opt').length > 0")
started <- Sys.time()
for (k in seq_len(n_items)) {
  Sys.sleep(0.6) # the item is on screen; cogStart was set when the server sent it
  for (h in hovers[[k]]) {
    run(sprintf("var o=document.querySelectorAll('.cog-opt'); if(o.length>=%d){o[%d].dispatchEvent(new MouseEvent('mouseenter',{bubbles:false}));}", h, h - 1L))
    Sys.sleep(0.15)
  }
  elapsed <- as.numeric(js("Date.now() - window.cogStart"))
  Sys.sleep(max(0, (delays[k] - elapsed) / 1000))
  run(sprintf("var o=document.querySelectorAll('.cog-opt'); if(o.length>=%d){o[%d].click();}", picks[k], picks[k] - 1L))
  wait_for("document.getElementById('admin_next') !== null")
  Sys.sleep(0.4)
  run("document.getElementById('admin_next').click();")
  if (k < n_items) wait_for("document.getElementById('admin_next') === null && document.querySelectorAll('.cog-opt').length > 0")
}
wait_for("document.getElementById('dl_results') !== null")
Sys.sleep(0.8)
before <- list.files(out, pattern = "^cogitem-results-.*[.]csv$", full.names = TRUE)
run("document.getElementById('dl_results').click();")
t <- Sys.time()
repeat {
  files <- setdiff(list.files(out, pattern = "^cogitem-results-.*[.]csv$", full.names = TRUE), before)
  if (length(files) && file.size(files[1]) > 0) break
  if (as.numeric(difftime(Sys.time(), t, units = "secs")) > 30) stop("the results CSV was not downloaded")
  Sys.sleep(0.3)
}
Sys.sleep(0.5)
log <- utils::read.csv(files[1], stringsAsFactors = FALSE)

## ---- compare -------------------------------------------------------------------
checks <- data.frame(
  check = c("one row per item, in order", "participant id recorded", "item ids are those of the regenerated set",
            "keys equal the regenerated keys", "chosen options equal the scripted clicks", "correct = chosen equals key",
            "no item marked skipped", "response times within [delay, delay + 2500 ms]", "hover counts equal the scripted hover events",
            "hover traces name the hovered options in order"),
  passed = c(
    nrow(log) == n_items && identical(log$item_number, seq_len(n_items)),
    all(log$participant == participant),
    identical(log$item_id, vapply(ref, function(it) it$id, "")),
    identical(as.integer(log$correct_option), keys),
    identical(as.integer(log$chosen_option), picks),
    identical(as.logical(log$correct), picks == keys),
    !any(as.logical(log$skipped)),
    all(log$rt_ms >= delays & log$rt_ms <= delays + 2500),
    identical(as.integer(log$hover_count), lengths(hovers)),
    identical(vapply(strsplit(ifelse(is.na(log$hover_trace), "", log$hover_trace), ";", fixed = TRUE), function(x) paste(sub("@.*", "", x), collapse = ","), ""),
              vapply(hovers, function(h) paste(h, collapse = ","), ""))
  ), stringsAsFactors = FALSE)
print(checks, row.names = FALSE)
utils::write.csv(cbind(log, scripted_pick = picks, scripted_delay = delays, scripted_hovers = lengths(hovers)),
                 file.path(out, "log-vs-script.csv"), row.names = FALSE)
utils::write.csv(checks, file.path(out, "checks.csv"), row.names = FALSE)
cat(sprintf("\n%d of %d checks passed; log: %s\n", sum(checks$passed), nrow(checks), basename(files[1])))
cat(sprintf("response times (ms): scripted %s | logged %s\n", paste(delays, collapse = " "), paste(log$rt_ms, collapse = " ")))
if (!all(checks$passed)) quit(status = 1)
