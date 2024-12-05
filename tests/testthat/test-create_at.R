test_that("Create AT table for subset of SNPs", {
  evidData_wreps = create_evid("Sample01a_set5", "Sample01b_rep_set5", test_path("testdata"))
  attable = process_kinreport("Sample01a", "Sample01b", test_path("testdata"), 0.015, 10)
  at_tablereps = create_at(evidData_wreps, "Sample01a_set5", "Sample01b_set5", attable)
  expect_equal(length(at_tablereps), 1005)
  expect_equal(at_tablereps[["RS977299"]], 11)
})
