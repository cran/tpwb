test_that("test-ptpwb", {
  x <- rtpwb(20,2,3,1)
  p1 <- ptpwb(x,2,3,1)
  p2 <- ptpwb(x,shape=2,scale=3,location=1)
  expect_equal(p1, p2)
})
