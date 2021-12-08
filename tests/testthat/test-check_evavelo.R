

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

test_that("Missing variables are detected", {
  ## Remove categorie_visuelle variable from comptage data.frame
  modified_example <- evavelo_example
  modified_comptage <- modified_example$comptage %>%
    dplyr::select(-categorie_visuelle)
  modified_example$comptage <- modified_comptage

  expect_true(check_evavelo(modified_example)$error)


  modified_comptage_init <- modified_example$comptage_init %>%
    dplyr::select(-categorie_visuelle)
  modified_example$comptage_init <- modified_comptage_init

  expect_true(check_evavelo(modified_example)$error)

})


