test_that("Formatting AF files for use in EFM", {
  input_af = data.frame(SNP = c("rs6690515", "rs28635343", "rs16824588", "rs424079"), Ref = c("G", "C", "T", "T"), Alt = c("A", "T", "C", "A"), Ref_AF = c(0.461, 0.240, 0.387, 0), Alt_AF = c(0.539, 0.760, 0.613, 1))
  exp_out = data.frame(Allele = c("A", "C", "G", "T"), rs6690515 = c(0.539, NA, 0.461, NA), rs28635343 = c(NA, 0.240, NA, 0.760), rs16824588 = c(NA, 0.613, NA, 0.387), rs424079 = c(0.9999, NA, NA, 0.0001))
  expect_equal(format_af(input_af), exp_out)
})
