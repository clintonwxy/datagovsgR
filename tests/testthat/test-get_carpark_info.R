test_that("get_carpark_info() returns data.frame", {
  expect_type(get_carpark_info(), "list")
})

test_that("get_carpark_info() returns all the data", {
  output <- get_carpark_info()
  max <- max(output$X_id)
  nr <- nrow(output)

  expect_equal(nr, max)
})
