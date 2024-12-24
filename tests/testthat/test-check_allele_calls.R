test_that("Checking allele calls for dataset with same alleles for C1 and C2", {
  notsame = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "A", "T", "", "99", ""), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
  expect_false(check_allele_calls(notsame))
  same = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "T", ""), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
  expect_true(check_allele_calls(same))
  same_99 = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c(99, 99, 99, 99, 99, 99, 99, 99), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
  expect_true(check_allele_calls(same_99))
})
