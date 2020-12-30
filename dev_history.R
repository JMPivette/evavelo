usethis::use_build_ignore("dev_history.R")
devtools::document()

# Document functions and dependencies
attachment::att_to_description()
# Check the package
devtools::check()

usethis::use_vignette("aa-exploration")

usethis::use_github()
usethis::use_test()

usethis::use_vignette("ba-using_package")
usethis::use_coverage()

# Update code coverage
covr::codecov(token=Sys.getenv("COVR_TOKEN"))
