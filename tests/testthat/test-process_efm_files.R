test_that("Processing EFM output files", {
  testdf = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "C2", "C2"), Locus = c("RS10941232", "RS10941232", "RS10941232", "RS10941232", "RS11946874", "RS11946874", "RS11946874"), Allele = c("A", "G", "A", "T", "G", "C", 99), Probability = c(1, 0.999, 1, 0.5, 1, 0.99, 0.5))
  expect_equal(nrow(process_efm_files(testdf, "C2", "ref", 1, 0.99, 1, 0.5, 0.5, metrics=FALSE)), 2)
  expect_equal(nrow(process_efm_files(testdf, "C1", "ref", 1, 0.99, 1, 0.5, 0.5, metrics=FALSE)), 2)
  expect_equal(nrow(process_efm_files(testdf, "C2", "ref", 1, 0.99, 1, 0.5, 0.5, metrics=FALSE, filter_missing=TRUE)), 2)
})
