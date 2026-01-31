# Summary: Distribution & Backend Reference

## Summary: Distribution & Backend Reference

This section provides a compact reference for the distributions
supported by **DPmixGPD** and the corresponding user-facing function
families.  
The **CRP backend** uses base (single-kernel) functions, while the **SB
backend** uses mixture (multi-component) functions. The “Type” column
indicates whether each backend expects **scalar** parameters (single
value per parameter) or **vectors** indexed by mixture component.

[TABLE]

### Notes

- **CRP Backend** uses base (single-kernel) functions; parameters are
  scalar.

- **SB Backend** uses mixture/spliced mixture functions;
  component-specific parameters are typically vectors indexed by
  component $`j`$; mixture weights `w` are required as vector.

- For **GPD-only**, SB entries are **NA** because there is no
  mixture-only GPD wrapper in the SB family.
