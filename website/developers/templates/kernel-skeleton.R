# Kernel skeleton (developer template)
# Replace <NAME> with your kernel name, and fill in parameterization.

d<NAME> <- function(x, ..., log = FALSE) {
  # 1) enforce support
  # 2) compute density
  # 3) return log or density
  stop("TODO: implement d<NAME>")
}

p<NAME> <- function(q, ..., lower.tail = TRUE, log.p = FALSE) {
  stop("TODO: implement p<NAME>")
}

q<NAME> <- function(p, ..., lower.tail = TRUE, log.p = FALSE) {
  stop("TODO: implement q<NAME>")
}

r<NAME> <- function(n, ...) {
  stop("TODO: implement r<NAME>")
}

# If this kernel is used with spliced GPD tails, implement MixGPD wrappers too:
# d<NAME>MixGpd(), p<NAME>MixGpd(), q<NAME>MixGpd(), r<NAME>MixGpd()
