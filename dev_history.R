usethis::use_build_ignore("dev_history.R")
devtools::document()

usethis::use_vignette("aa-exploration")

usethis::use_github()
usethis::use_test()

usethis::use_vignette("ba-using_package")
usethis::use_coverage()



# Document functions and dependencies
##attachment::att_to_description()
attachment::att_amend_desc()
# Check the package
devtools::check()
# don't forget to commit
# Update code coverage
covr::codecov()
