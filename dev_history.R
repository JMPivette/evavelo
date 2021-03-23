usethis::use_build_ignore("dev_history.R")
devtools::document()

usethis::use_vignette("aa-exploration")

usethis::use_github()
usethis::use_test()

usethis::use_vignette("ba-using_package")
usethis::use_coverage()

usethis::use_news_md()
## Pkgdown
usethis::use_pkgdown()
usethis::use_github_action("pkgdown")

# Document functions and dependencies
##attachment::att_to_description()
attachment::att_amend_desc()
# Check the package
devtools::check()
# don't forget to commit
# Update code coverage
covr::codecov()


#Update version
usethis::use_version()


## Pull Requests
# Create a PR
usethis::pr_push()
# After PR approval to clean locally.
usethis::pr_finish()

# Run to build the website
pkgdown::build_site()
