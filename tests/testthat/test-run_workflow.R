test_that("Running MixDeR workflow", {
  tmpdir = tempdir()
  file.copy(test_path("testdata", "Sample01a_Sample_Report_2021_11_01_16_56_38.xlsx"), tmpdir)
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  refdir=paste0(tmpdir, "/ref")
  dir.create(refdir)
  file.copy(test_path("testdata", "EFM_references.csv"), refdir)
  refData = euroformix::sample_tableToList(euroformix::tableReader(test_path("testdata", "EFM_references.csv")))
  snppos = mixder::kintelligence_snp_positions
  popFreq = list(mixder::popFreq_1000G, mixder::popFreq_1000G)
  out_path = glue("{tmpdir}/snp_sets/output/")
  ## calculating metrics
  create_config(date, FALSE, "Global - 1000G", NULL, NULL, refdir, NULL, "Sample01a", NULL, out_path, run_mixdeconv=TRUE, software="EFM", unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, tmpdir, 0.015, 10, 6000, NULL, NULL, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2", filter_missing=FALSE, runancestry=FALSE, NULL, NULL, "kintelligence", NULL)
  run_workflow(date, "Sample01a", "", FALSE, popFreq, refData, refdir, out_path, run_mixdeconv=TRUE, unconditioned=TRUE, software="EFM", cond=NULL, "Calculate Metrics", 1, tmpdir, 0.015, 10, 6000, NULL, NULL, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2", minor_threshold=FALSE, keep_bins=TRUE, filter_missing=FALSE, runancestry=FALSE, NULL, NULL, assay="kintelligence", positions=snppos)
  expect_true(file.exists(glue("{out_path}/config_log_files/{date}/config_settings_run_{date}.txt")))
  expect_true(file.exists(glue("{out_path}/Single/Sample01a/unconditioned/Ref1_metrics_table.tsv")))
  run_workflow(date, "Sample01a", "", FALSE, popFreq, refData, refdir, out_path, run_mixdeconv=TRUE, unconditioned=TRUE, software="EFMmps", cond=NULL, "Create GEDmatch PRO Report", 1, tmpdir, 0.015, 10, 6000, 0.99, 0.6, NULL,NULL,NULL,NULL, "Ref1", "Ref2", minor_threshold=FALSE, keep_bins=TRUE, filter_missing=FALSE, runancestry=FALSE, NULL, NULL, assay="custom", positions=snppos)
  expect_true(file.exists(glue("{out_path}/Single/GEDMatchPROReports/Sample01a_uncond_major_Single_GEDmatchPROReport.txt")))
})

