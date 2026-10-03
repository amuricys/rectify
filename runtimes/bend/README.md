# Bend geometry exploration

Status: proposed experiment; no Bend implementation or toolchain is installed by this repository.

The motivating question is whether runtime-managed parallel evaluation is a useful execution model for surface processing. A first candidate is segment-intersection candidate generation using spatial partitioning, compared with the Haskell quadratic baseline in `../haskell/kernels/`.

Specify treatment of cell boundaries, duplicate pairs, endpoint contact, collinear overlap, and floating-point tolerances before comparing outputs. The current Haskell probe counts proper crossings only, not all polygon invalidities. Measure actual work and speedup rather than assuming partitioning or parallel execution is always beneficial.

Keep this implementation independent of the Haskell and Lean build plans. Add toolchain pins, a minimal runnable example, and common fixtures when the experiment starts. No proof transfer from Lean to Bend is assumed.
