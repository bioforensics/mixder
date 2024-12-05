test_that("Loading Sample Reports created with UAS version 2.5", {
  file = test_path("testdata", "NA24385_Sample_Report_2023_09_07_15_11_11.xlsx")
  expect_equal(ncol(load_kin_uas25(file)), 4)
  expect_equal(nrow(load_kin_uas25(file)), 20081)
})
