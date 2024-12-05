test_that("Performing reverse comp on SNPs reported on reverse strand", {
  testdf = data.frame(Marker = c("rs7554936", "rs7554936", "rs1294331", "rs1294331"), Allele = c("T", "C", "G", "A"), Typed = c("Yes", "No", "Yes", "Yes"), Reads = c(370, 0, 704, 136))
  exp_df = data.frame(Marker = c("rs7554936", "rs7554936", "rs1294331", "rs1294331"), Allele = c("T", "C", "C", "T"), Typed = c("Yes", "No", "Yes", "Yes"), Reads = c(370, 0, 704, 136))
  expect_equal(reverse_comp(testdf), exp_df)
})
