test_that("Check AF file format and format if necessary", {
  tmpdir = tempdir()
  file.copy(test_path("testdata", "freq_EAS.csv"), tmpdir)
  expect_equal(length(checking_af(paste0(tmpdir, "/freq_EAS.csv"))), 10039)
  file.copy(test_path("testdata", "AF_notformatted.csv"), tmpdir)
  expect_equal(length(checking_af(paste0(tmpdir, "/AF_notformatted.csv"))), 5)
})
