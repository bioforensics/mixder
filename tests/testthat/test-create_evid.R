test_that("Creating evidence data frame for EFM", {
  sampleid = "Sample01a_set1"
  repid = "Sample01b_rep_set1"
  evidrep = create_evid(sampleid, repid, test_path("testdata"))
  expect_equal(length(evidrep), 2)
  expect_equal(length(evidrep[[sampleid]]), 1005)
  expect_equal(length(evidrep[["Sample01b_set1"]]), 1005)
  evidnorep = create_evid(sampleid, "", test_path("testdata"))
  expect_equal(length(evidnorep), 1)
})
