test_that("test-tpwb", {
  x <- rtpwb(20,2,3,1)
  expect_equal(dtpwb(x,shape=2,scale=3,location = 1), dtpwb(x,2,3,1))
  expect_equal(ptpwb(x,shape=2,scale=3,location = 1), ptpwb(x,2,3,1))
})
