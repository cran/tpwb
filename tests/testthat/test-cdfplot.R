test_that("test-cdfplot", {
  x <- rtpwb(20,2,3,1)
  expect_equal(cdfplot(x,2,3,1),cdfplot(x,shape=2,scale=3,location=1))
})
