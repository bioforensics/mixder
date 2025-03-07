test_that("Checking allele probabilities for errors", {
  efm_v = getNamespaceVersion("euroformix")[["version"]]
  sameprob = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "C", "", "99", ""), Probability = c("1.000", "0.9264", "1.000", "0.9264", "1", "", "1", ""))
  expect_false(check_allele_probabilities(sameprob, 1))
  samealleles = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "T", ""), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
  expect_false(check_allele_probabilities(samealleles, 1))
  if (substr(efm_v, 1,3)!="4.0" & substr(efm_v, 1,2) != "3.") {
    notsame = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "99", ""), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
    expect_false(check_allele_probabilities(notsame, 1))
    probswitch = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "99", ""), Probability = c("0.9998", "0.9264", "1.000", "0.9845", "1", "", "1", ""))
    expect_true(check_allele_probabilities(probswitch, 1))
  } else {
    notsame = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "99", ""), Probability = c("1.000", "0.9264", "0.9998", "0.9845", "1", "", "1", ""))
    expect_true(check_allele_probabilities(notsame, 1))
    probswitch = data.frame(Contr. = c("C1", "C1", "C2", "C2", "C1", "", "C2", ""), Locus = c("RS12446319", "RS12446319", "RS12446319", "RS12446319", "RS1458043", "", "RS1458043", ""), Allele = c("A", "G", "A", "G", "T", "", "99", ""), Probability = c("0.9998", "0.9264", "1.000", "0.9845", "1", "", "1", ""))
    expect_false(check_allele_probabilities(probswitch, 1))
  }
})
