# ---------------------------------------------------------------------------
# The package version is declared in three places that have to agree.
#
# DESCRIPTION is what R installs, jamovi/0000.yaml is what the jamovi library
# reads for update detection, and the README badge is what a person sees first.
# Nothing keeps them in step on its own, and the release before this one shipped
# with eight generated headers already out of line with the YAML they came from,
# so drift here is not hypothetical.
# ---------------------------------------------------------------------------

repoFile <- function(...) testthat::test_path("..", "..", ...)

# These files belong to the repository, not to the installed package: under
# R CMD check the README, the changelog and the jamovi manifest are not there to
# read. CI runs this file from a checkout, and fails if anything skips, so the
# checks are still enforced where they matter.
inCheckout <- function()
    all(file.exists(c(repoFile("DESCRIPTION"), repoFile("README.md"),
                      repoFile("NEWS.md"), repoFile("jamovi", "0000.yaml"))))

descriptionVersion <- function() {
    line <- grep("^Version:", readLines(repoFile("DESCRIPTION"), warn = FALSE), value = TRUE)
    trimws(sub("^Version:", "", line[1]))
}

moduleVersion <- function() {
    line <- grep("^version:", readLines(repoFile("jamovi", "0000.yaml"), warn = FALSE), value = TRUE)
    trimws(sub("^version:", "", line[1]))
}

readmeVersion <- function() {
    readme <- readLines(repoFile("README.md"), warn = FALSE)
    hit <- regmatches(readme, regexpr("badge/version-[0-9][0-9.]*-", readme))
    if (!length(hit)) return(NA_character_)
    sub("-$", "", sub("^badge/version-", "", hit[1]))
}

test_that("DESCRIPTION, the jamovi manifest and the README agree on the version", {
    skip_if_not(inCheckout(), "not run from a source checkout")
    desc <- descriptionVersion()
    expect_match(desc, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
    expect_identical(moduleVersion(), desc,
                     label = "jamovi/0000.yaml version")
    expect_identical(readmeVersion(), desc,
                     label = "README version badge")
})

test_that("the changelog documents the version being shipped", {
    skip_if_not(inCheckout(), "not run from a source checkout")
    news <- readLines(repoFile("NEWS.md"), warn = FALSE)
    headings <- grep("^# distrACTION ", news, value = TRUE)
    expect_gt(length(headings), 0)
    # the newest entry has to be the version in DESCRIPTION
    expect_identical(sub("^# distrACTION ", "", headings[1]), descriptionVersion())
})
