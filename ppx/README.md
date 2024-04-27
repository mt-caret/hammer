# ppx_hammer

Derive `Hammer.Sampler.t` off of type definitions via ppx_deriving.

## features to implement

- [x] types with parameters
- [x] recursive types
  - [x] simple case
  - [x] mutually recursive case
- [ ] inline derivation of sampler via something like `[%hammer.sampler: [`A | `B of int]]`