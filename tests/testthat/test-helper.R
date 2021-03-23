#context("Ensure helper functions work properly")

test_that("helper function", {
  set.seed(277)
  df1 <- data.frame("id" = rep(1:4, 4),
                    "product" = sample(1:20, 16, replace = TRUE),
                    price = sample(1:150, 16, replace = TRUE))

  df2 <- data.frame("id" = rep(1:3, 3),
                    "product" = sample(1:15, 9, replace = TRUE),
                    price = sample(1:100, 9, replace = TRUE))

  g1 <- graph_data(df1, id = "id", product_id = "product")

  g2 <- graph_data(df2, id = "id", product_id = "product")

  expect_true(check_inputs(df1, id = "id", product_id = "product"))
  expect_true(check_inputs(df2, id = "id", product_id = "product"))

  expect_error(check_inputs())
  expect_error(check_inputs(df1))
  expect_error(check_inputs(df1, id = "id"))
})
