set_max_fails(Inf)
options(testthat.reporter = "summary")
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", "1"))
