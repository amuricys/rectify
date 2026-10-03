# Haskell implementations

- [native](native/): the existing optimization server and surface/TSP modules. The root `cabal.project` selects this package.
- [kernels](kernels/): a base-only geometry comparison library; no dependency on the server or Clash.
- [thc](thc/README.md): a separate Cabal project consuming that library and a native/THC probe.
- [Clash](../../hardware/clash/): the separate reservoir hardware project, with its own compiler constraints.

The first compiler experiment targets proper segment intersections and oriented polygon area. Its conventions are explicit. It does not fix, replace, or formally verify the existing surface implementation.

Longer term, extract representations, objectives, and proposal operations from `native/src/SimulatedAnnealing/` while retaining a reproducible behavior baseline. Evaluate dependencies such as sized vectors, typechecker plugins, and Effectful under each intended toolchain before expanding shared code.
