test_that("test-pdfplot", {
  x <- rtpwb(20,2,3,1)
  expect_equal(pdfplot(x,2,3,1),pdfplot(x,shape=2,scale=3,location=1))
})
