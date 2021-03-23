check_similar_enquete(evavelo_example$enquete)

test_that("check_similar_enquete gives a results", {
  check <- check_similar_enquete(evavelo_example$enquete)

  expect_type(check,
              "list")

  expect_equal(names(check),
               c("simil_enq", "log"))

  expect_s3_class(check$simil_enq,
                  "data.frame")


})
