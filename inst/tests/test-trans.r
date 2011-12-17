context("Trans")

test_that("Transformed ranges silently drop out-of-domain values", {
  r1 <- trans_range(log_trans(), -1:10)
  expect_that(r1, equals(log(c(1e-100, 10))))
  
  r2 <- trans_range(sqrt_trans(), -1:10)
  expect_that(r2, equals(sqrt(c(0, 10))))
})


test_that("Transformation for discrete variable", {
  v1 <- letters[1:10]
  v2 <- factor(letters[1:10])
  v3 <- factor(letters[1:10], ordered = T)
  zuo <- structure(c(10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L),
                   .Label = c("j", "i", "h", "g", "f", "e", "d", "c", "b", "a"),
                   class = "factor")
  zo <- structure(c(10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L),
                  .Label = c("j", "i", "h", "g", "f", "e", "d", "c", "b", "a"),
                  class = c("ordered", "factor"))
  
  tr <- flip_trans()
  expect_equal(tr$transform(a1), zuo)
  expect_equal(tr$transform(a2), zuo)
  expect_equal(tr$transform(a3), zo)
  expect_equal(tr$inverse(zuo), a2)
  expect_equal(tr$inverse(zo), a3)

})
