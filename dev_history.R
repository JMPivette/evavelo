usethis::use_build_ignore("dev_history.R")

# Document functions and dependencies
attachment::att_to_description()
# Check the package
devtools::check()

usethis::use_vignette("aa-exploration")

usethis::use_github()
