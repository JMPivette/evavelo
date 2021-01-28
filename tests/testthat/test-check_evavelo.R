

test_that("check_evavelo works", {
  expect_type(check <- check_evavelo(calendrier = evavelo_example$calendrier,
                                      comptage = evavelo_example$comptage,
                                     enquete = evavelo_example$enquete,
                                     comptage_init = evavelo_example$comptage_init,
                                     enquete_init = evavelo_example$enquete_init),
              "list")

  expect_false(check$error)

  ## Remove lines from calendrier:
  expect_type(check <- check_evavelo(calendrier = tail(evavelo_example$calendrier,
                                                       n =3),
                                     comptage = evavelo_example$comptage,
                                     enquete = evavelo_example$enquete,
                                     comptage_init = evavelo_example$comptage_init,
                                     enquete_init = evavelo_example$enquete_init),
              "list")

  expect_true(check$error)


})


