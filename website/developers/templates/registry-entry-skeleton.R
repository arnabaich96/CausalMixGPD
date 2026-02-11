# Registry entry skeleton (developer template)
# Add into R/00-kernel-registry.R in the appropriate place.

new_kernel_entry <- list(
  name = "<NAME>",
  support = "<positive|real>",
  backends = c("crp", "sb"),
  gpd_allowed = TRUE,
  params = list(
    # list parameter names and constraints
  ),
  fns = list(
    d = d<NAME>,
    p = p<NAME>,
    q = q<NAME>,
    r = r<NAME>
    # optionally MixGPD wrappers
  )
)
