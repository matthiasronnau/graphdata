#context("Ensure correct output of clean data")

test_that("check output type", {
  set.seed(277)
  df1 <- data.frame("id" = rep(1:4, 4),
                    "product" = sample(1:20, 16, replace = TRUE),
                    price = sample(1:150, 16, replace = TRUE))

  df2 <- data.frame("id" = rep(1:3, 3),
                    "product" = sample(1:15, 9, replace = TRUE),
                    price = sample(1:100, 9, replace = TRUE))

  g1 <- graph_data(df1, id = "id", product_id = "product")

  g2 <- graph_data(df2, id = "id", product_id = "product")

  expect_true(typeof(g1) == "list")
  expect_true(typeof(g2) == "list")
})

test_that("graph_data fails with invalid inputs", {
  set.seed(277)
  df1 <- data.frame("id" = rep(1:4, 4),
                    "product" = sample(1:20, 16, replace = TRUE),
                    price = sample(1:150, 16, replace = TRUE))

  df2 <- data.frame("id" = rep(1:3, 3),
                    "product" = sample(1:15, 9, replace = TRUE),
                    price = sample(1:100, 9, replace = TRUE))

  g1 <- graph_data(df1, id = "id", product_id = "product")

  g2 <- graph_data(df2, id = "id", product_id = "product")

  expect_error(graph_data(id = "id", product_id = "product"))
  expect_error(graph_data(data = df1))
  expect_error(graph_data(data = df1, id = "id"))
  expect_error(graph_data(data = df1, product_id = "product"))
  expect_error(graph_data(id = "id", product_id = "product"))
  expect_error(graph_data(data = df2))
  expect_error(graph_data(data = df2, id = "id"))
  expect_error(graph_data(data = df2, product_id = "product"))
})
