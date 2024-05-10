test_that("test-dtpwb", {
  x <- rtpwb(20,2,3,1)
  fx <- dtpwb(x,2,3,1)
  expect_equal(fx,dtpwb(x,shape=2,scale = 3,location = 1))
})
