test_that("Creating the replicate SNP files", {
  outpath = tempdir()
  input = test_path("testdata", "Sample01a_set1.tsv")
  alla = test_path("testdata", "Sample01a_snpsetscombined_evidence.tsv")
  allb = test_path("testdata", "Sample01b_snpsetscombined_evidence.tsv")
  file.copy(c(input, alla, allb), outpath)
  expect_equal(create_replicatedf(outpath, "Sample01a", "Sample01b", 1), list("Sample01a", "Sample01b_rep"))
})
