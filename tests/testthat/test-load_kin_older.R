test_that("Loading Sample Reports created with UAS version <2.5", {
  file = test_path("testdata", "Sample01a_Sample_Report_2021_11_01_16_56_38.xlsx")
  expect_equal(ncol(load_kin_older(file)), 4)
  expect_equal(nrow(load_kin_older(file)), 20084)
})
