# ---------------------------------------------------------------------------
# Make the package's own code visible to the tests, in either way they run.
#
# From a source checkout (testthat::test_dir(), and CI), the R/ files are
# sourced directly, so the fast suite needs no install. Only the parts that
# stand alone are loaded: the spec table, the core, and the ggplot2 shim. The
# .b.R and .h.R files define R6 classes against jmvcore base classes and need a
# real install; test-analysis-smoke.R covers those.
#
# Under R CMD check - and test_check(), which tests/testthat.R calls - there is
# no source tree. The tests run against the installed package instead, and the
# test environment already inherits from its namespace, so nothing is sourced.
# Earlier versions of this file assumed a checkout, which made R CMD check fail
# outright and quietly hollowed out two other test files.
# ---------------------------------------------------------------------------

local({
    src <- testthat::test_path("..", "..", "R")
    if (file.exists(file.path(src, "distribution-core.R")))
        for (f in c("ggplot_compat.R", "distribution-specs.R", "distribution-core.R"))
            sys.source(file.path(src, f), envir = globalenv())
})

# Evaluated here, with inheritance, so it answers for whichever of the two
# routes above actually supplied the code.
if (!exists("distributionResults") || !exists(".distSpecs"))
    stop("the distrACTION core is not available - neither a source checkout ",
         "nor the installed package supplied distributionResults()")
