scripts <- list.files("data-raw", pattern = "\\.R$", full.names = TRUE)
scripts <- scripts[!grepl("_helpers\\.R$|run-all\\.R$", scripts)]
for (script in scripts) {
  message("Running ", script)
  source(script, local = new.env(parent = globalenv()))
}
