## Common pitfalls

- **Registry mismatch:** the registry advertises a kernel, but a required d/p/q/r function (or MixGPD wrapper) isn’t implemented.
- **Support mismatch:** kernel claims positive support but returns density for negative inputs (or vice versa).
- **Tail discontinuity:** the bulk and GPD pieces don’t match at the splice point (visible as a “kink” in log-density or CDF).
- **Backend drift:** SB and CRP paths diverge in parameter naming or dimensions, causing subtle posterior differences.
- **Docs drift:** website kernel page says one parameterization but code uses another.
