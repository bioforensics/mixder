test_that("Check AF file format and format if necessary", {
  outdir = tempdir()
  test_af = test_path("testdata", "freq_EAS.csv")
  expect_equal(length(checking_af(test_af, outdir)), 10039)
  test_af_notformatted = test_path("testdata", "AF_notformatted.csv")
  expect_equal(length(checking_af(test_af_notformatted, outdir)), 5)
})
