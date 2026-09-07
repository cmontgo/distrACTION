# ---------------------------------------------------------------------------
# Load the package's own source into the test environment.
#
# The suite runs via testthat::test_dir() without installing the package (it
# needs jmvcore, which is not on CRAN), so the R/ files are sourced directly.
# Only the parts that do not need jmvcore are loaded: the spec table, the core,
# and the ggplot2 compatibility shim. The .b.R files define R6 classes against
# jmvcore base classes and are exercised by test-analysis-smoke.R instead.
# ---------------------------------------------------------------------------

local({
    root <- testthat::test_path("..", "..")
    for (f in c("ggplot_compat.R", "distribution-specs.R", "distribution-core.R")) {
        path <- file.path(root, "R", f)
        if (file.exists(path)) sys.source(path, envir = globalenv())
    }
})

core_loaded <- exists("distributionResults", envir = globalenv())
