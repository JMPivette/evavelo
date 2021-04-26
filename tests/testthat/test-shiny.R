test_that(
  "app launches",{
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(interactive())
    x <- processx::process$new(
      "R",
      c(
        "-e",
        "pkgload::load_all(here::here());app_run()"
      )
    )
    Sys.sleep(5)
    expect_true(x$is_alive())
    x$kill()
  }
)
