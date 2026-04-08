## Copy package namespace bindings into the test environment so tests can call
## unexported helpers (replaces eval-parse into .GlobalEnv; CRAN policy).
ns <- asNamespace("CausalMixGPD")
for (nm in ls(ns, all.names = TRUE)) {
  if (!nzchar(nm) || startsWith(nm, ".__")) {
    next
  }
  if (!exists(nm, envir = ns, inherits = FALSE)) {
    next
  }
  assign(nm, get(nm, envir = ns, inherits = FALSE))
}
