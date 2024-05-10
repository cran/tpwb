test_that("test-qtpwb", {
  x <- rtpwb(20,2,3,1)
  p <- ptpwb(x,2,3,1)
  q <- qtpwb(p,2,3,1)
  expect_equal(x,q)
})
