test_that("test-rtpwb", {
  x <- rtpwb(20,2,3,1)
  y <- x
  expect_equal(x,y)
})
