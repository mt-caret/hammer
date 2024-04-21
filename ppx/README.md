# ppx_hammer

Derive `Hammer.Sampler.t` off of type definitions via ppx_deriving.

## features to implement

- [ ] types with parameters
- [ ] recursive types
  - [ ] simple case
  - [ ] mutually recursive case
- [ ] inline derivation of sampler via something like `[%hammer.sampler: [`A | `B of int]]`