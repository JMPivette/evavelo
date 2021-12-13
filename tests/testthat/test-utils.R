library(tibble)

test_that("df_compares works:", {
  df1 <- tibble(
    a = 1:10,
    b = LETTERS[1:10],
    c = rep(c(0L,NA),time = 5)
  )

  df2 <- tibble(
    a = c(1:9,5L),
    b = LETTERS[1:10],
    c = 0L
  )

  df3 <- tibble(
    d = 1:10,
    b = 1:10,
    c = rep(c(0L,NA),time = 5)
  )

  expect_s3_class(df_compare(df1, df1),
                  "data.frame")

  expect_warning(df_compare(df1, df2))

})

test_that("equal_with_na() works",{
  vec_A <- c(1:5, NA)
  vec_B <- c(1:4, 6L, NA)

  expect_equal(equal_with_na(vec_A,
                             vec_A),
               rep(TRUE, 6))

  expect_equal(equal_with_na(vec_A,
                             vec_B),
               c(rep(TRUE,4), FALSE, TRUE))



})

test_that("compare_init_post() works:", {
  df_init <- tibble(
    a = 1:10,
    b = LETTERS[1:10],
    c = 0L
  )

  df_post_modif <- tibble(
    a = c(1:9,5L),
    b = LETTERS[1:10],
    c = rep(c(0L,NA),time = 5)
  )

  df_post_newline <- tibble(
    a = 1:11,
    b = LETTERS[1:11],
    c = 0L
  )

  df_post_missingline <- tibble(
    a = 1:9,
    b = LETTERS[1:9],
    c = 0L
  )

  df_post_reordered <- tibble(
    a = 10:1,
    b = LETTERS[10:1],
    c = 0L
  )

  df_post_newcol <- df_init %>%
    dplyr::bind_cols(d = "new")

  df_init_dupl_key <- df_init %>%
    dplyr::add_row(
      head(df_init, 1)
    )

  df_post_dupl_key <- df_post_newcol %>%
    dplyr::add_row(
      tail(df_post_newcol, 1)
    )

  ## Changes
  expect_warning(
    compare_init_post(init = df_init, post = df_post_modif),
    "Les colonnes suivantes ne sont pas identiques"
  )

  ## New lines
  expect_warning(
    compare_init_post(init = df_init, post = df_post_newline),
    "Le nombre de ligne"
  )
  expect_warning(
    compare_init_post(init = df_init, post = df_post_newline, key = "a"),
    "Le nombre de ligne"
  )

  ## Missing lines
  expect_warning(
    compare_init_post(init = df_init, post = df_post_missingline),
    "Le nombre de ligne"
  )
  compare_init_post(init = df_init, post = df_post_missingline, key = "a") %>%
    expect_warning("Le nombre de ligne") %>%
    expect_warning("Les colonnes suivantes ne sont pas identiques")

  ## Reorder
  compare_init_post(init = df_init, post = df_post_reordered) %>%
    expect_warning("Les colonnes suivantes ne sont pas identiques")

  compare_init_post(init = df_init, post = df_post_reordered, key = "a") %>%
    expect_warning(NA)

  ## New col

  compare_init_post(init = df_init, post = df_post_newcol) %>%
    expect_warning(NA)

  ## duplicated keys...
    ## . inside init
  compare_init_post(init = df_init_dupl_key, post = df_post_newcol) %>%
    expect_warning("Le nombre de ligne")
  compare_init_post(init = df_init_dupl_key, post = df_post_newcol, key = "a") %>%
    expect_warning("Le nombre de ligne") %>%
    expect_warning("Il y a des doublons")

  ## . inside post
  compare_init_post(init = df_init, post = df_post_dupl_key, key = "a") %>%
    expect_warning("Le nombre de ligne") %>%
    expect_warning("Il y a des doublons")
  ## . inside both but different
  compare_init_post(init = df_init_dupl_key, post = df_post_dupl_key, key = "a") %>%
    expect_warning("Il y a des doublons")


})
