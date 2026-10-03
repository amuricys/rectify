# THC geometry experiment

Status: compatibility scaffold. The probe can be compiled with native GHC; THC execution must be validated separately. The application server is not configured to run under THC.

[THC upstream](https://github.com/ekmett/thc) currently documents GHC 9.14.1, Cabal, and a specific GraalVM/JDK toolchain. Check that guide before installation and pin the tested THC commit. THC needs executable Core for dependencies; successful Core acquisition does not establish that an application runs. This project keeps the initial dependency set to `base` and our kernel library.

From the repository root:

```bash
python3 scripts/haskell_probe.py ghc

# Inside nix develop, with a THC source checkout:
export RECTIFY_THC_ROOT=/absolute/path/to/thc
rectify-thc-build
python3 scripts/haskell_probe.py thc
```

The THC runner invokes the documented `thc run` path for `rectify-thc-probe:exe:geometry-probe` with this directory as its project. It does not clone, install, or modify THC. Record the checkout revision, compiler versions, platform, and actual result when evaluating it.

The same Main module checks area orientation, proper crossings, excluded contacts/overlaps, degenerate segments, and unordered-pair counting. Passing the probe is not a benchmark. Subsequent measurements should vary and consume inputs, compare native GHC, and separate startup from warmed execution.

Possible follow-ups specific to Rectify:

- Repeated evaluation of surface energy and deformation kernels.
- Compare quadratic intersection scans with spatial partitioning.
- Investigate whether repeated use of a selected objective benefits from specialization.
- Grow the dependency set only after the small kernel runs correctly.

These are research hypotheses; this scaffold claims neither a speedup nor full server compatibility.

The default Nix shell supplies GraalVM and wrappers for `thc` and `rectify-thc-build`. These wrappers select GHC 9.14 independently of the native/Clash compiler. The THC source itself is not fetched or built on shell entry. `nix develop .#thc` provides the focused environment.
