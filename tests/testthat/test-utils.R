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
