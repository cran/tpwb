test_that("test-mlewb", {
  x <- rtpwb(20,2,3,1)
  y <- x
  expect_equal(mlewb(x,2,3,1), mlewb(y,2,3,1))
})
