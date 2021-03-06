

test_that("check_evavelo works", {
  expect_type(check <- check_evavelo(evavelo_example),
              "list")

  expect_false(check$error)

  ## Remove lines from calendrier:
  modified_example <- evavelo_example
  modified_example$calendrier <- tail(modified_example$calendrier,
                                      n =3)

  expect_type(check <- check_evavelo(modified_example),
              "list")

  expect_true(check$error)

})

test_that("Missing id_quest are detected", {
  check_mismatch <- check_evavelo(quest_mismatch_example)
  expect_true(check_mismatch$error)

})


