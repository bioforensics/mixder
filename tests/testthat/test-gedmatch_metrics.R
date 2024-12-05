test_that("gedmatch metrics", {
  testdf = data.frame(Locus = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1 = c("A", "T", "T", "T"), A2 = c("A", "A", "A", "C"), A1_Prob = c(1, 0.99, 0.9, 0.6), A2_Prob = c(1, 0.9, 0.99, 0.5))
  expected_output = data.frame(Allele1_Threshold_Applied=c("No", "Yes", "Minimum # of SNPs Used"), Allele2_Threshold_Applied=c("Yes", "Yes", "Yes"), Total_SNPs=c(4, 2, 3), Mean_A1_Prob=c(0.8725, 0.995, 0.9633), Median_A1_Prob=c(0.945, 0.995, 0.99), SD_A1_Prob=c(0.1871, 0.0071, 0.0551), Heterozygosity=c(0.5, 0.5, 0.6667))
  expect_equal(gedmatch_metrics(testdf, 0.99, 0.6, 3)[[1]], expected_output)
  testdf2 = data.frame(Locus = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1 = c("A", "T", "T", "T"), A2 = c("A", 99, "A", "C"), A1_Prob = c(1, 0.99, 0.9, 0.6), A2_Prob = c(1, 0.9, 0.99, 0.5))
  expected_output2 = data.frame(Allele1_Threshold_Applied=c("No", "Yes", "Minimum # of SNPs Used"), Allele2_Threshold_Applied=c("Yes", "Yes", "Yes"), Total_SNPs=c(4, 2, 3), Mean_A1_Prob=c(0.8725, 0.995, 0.9633), Median_A1_Prob=c(0.945, 0.995, 0.99), SD_A1_Prob=c(0.1871, 0.0071, 0.0551), Heterozygosity=c(0.25, 0, 0.3333))
  expect_equal(gedmatch_metrics(testdf2, 0.99, 0.6, 3)[[1]], expected_output2)
})
