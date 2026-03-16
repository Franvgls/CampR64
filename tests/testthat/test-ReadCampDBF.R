test_that("readCampDBF lee especies en cant/local", {
  df <- readCampDBF("especies", zona = "cant", dns = "local")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
})