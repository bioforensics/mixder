test_that("Filtering alleles based on specified thresholds", {
  testdf = data.frame(Locus = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1 = c("A", "T", "T", "T"), A2 = c("A", "A", "A", "C"), A1_Prob = c(1, 0.99, 0.9, 0.6), A2_Prob = c(1, 0.9, 0.99, 0.5))
  expect_equal(nrow(filter_alleles(testdf, "major", 2, 0.95, 0.5, FALSE, FALSE)), 2)
  expect_equal(ncol(filter_alleles(testdf, "major", 2, 0.95, 0.5, FALSE, FALSE)), 5)
  expect_equal(nrow(filter_alleles(testdf, "major", 2, 0.90, 0.5, FALSE, FALSE)), 3)
  expect_equal(nrow(filter_alleles(testdf, "major", 3, 0.95, 0.5, FALSE, FALSE)), 3)
  expect_equal(nrow(filter_alleles(testdf, "minor", 3, 0.95, 0.5, FALSE, FALSE)), 3)
  expect_equal(nrow(filter_alleles(testdf, "minor", 3, 0.5, 0.5, FALSE, FALSE)), 3)
  expect_equal(nrow(filter_alleles(testdf, "minor", 3, 0.5, 0.5, TRUE, FALSE)), 4)
  testdf2 = data.frame(Locus = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1 = c("A", "T", "T", "T"), A2 = c("A", "A", 99, 99), A1_Prob = c(1, 0.99, 0.9, 0.6), A2_Prob = c(1, 0.9, 0.6, 0.5))
  expect_equal(nrow(filter_alleles(testdf2, "major", 2, 0.85, 0.65, FALSE, FALSE)), 3)
  expect_equal(nrow(filter_alleles(testdf2, "major", 2, 0.85, 0.65, FALSE, TRUE)), 2)
})
