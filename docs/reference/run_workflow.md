# Run MixDeR workflow

Runs workflow

## Usage

``` r
run_workflow(
  date,
  id,
  replicate_id,
  twofreqs,
  popFreq,
  refData,
  refs,
  out_path,
  run_mixdeconv,
  unconditioned,
  cond,
  method,
  sets,
  kinpath,
  dynamicAT,
  staticAT,
  minimum_snps,
  A1_threshold,
  A2_threshold,
  A1min,
  A1max,
  A2min,
  A2max,
  major,
  minor,
  minor_threshold,
  keep_bins,
  filter_missing,
  skipancestry,
  ancestrysnps,
  pcagroups,
  assay,
  positions,
  threads = 0,
  parallel = FALSE
)
```

## Arguments

- date:

  date and time of run

- id:

  Sample ID

- replicate_id:

  Sample ID of replicate, if specified

- twofreqs:

  TRUE if using separate AF data for major and minor contributors

- popFreq:

  List of properly formatted allele frequency data

- refData:

  Reference data (if available)

- refs:

  Path of reference genotype(s) file

- run_mixdeconv:

  TRUE if running EFM mixture deconvolution

- unconditioned:

  TRUE if running unconditioned mixture deconvolution

- cond:

  Sample IDs to condition on

- method:

  Calculate Metrics or Create GEDmatch PRO Report

- sets:

  Number of SNP sets

- kinpath:

  Path of Kintelligence Sample Reports

- dynamicAT:

  Dynamic AT

- staticAT:

  Static AT

- minimum_snps:

  Minimum number of SNPs for creating GEDmatch PRO reports

- A1_threshold:

  Allele 1 probability threshold for creating GEDmatch PRO reports

- A2_threshold:

  Allele 2 probability threshold for creating GEDmatch PRO reports

- A1min:

  Minimum value for allele 1 probability thresholds for calculating
  metrics

- A1max:

  Maximum value for allele 1 probability thresholds for calculating
  metrics

- A2min:

  Minimum value for allele 2 probability thresholds for calculating
  metrics

- A2max:

  Maximum value for allele 2 probability thresholds for calculating
  metrics

- major:

  Major contributor ID

- minor:

  Minor contributor ID

- minor_threshold:

  If apply the allele 1 probability threshold to the minor contributor

- keep_bins:

  To use existing SNP bins or create new bins (and files)

- filter_missing:

  TRUE/FALSE whether to filter SNPs with either allele missing

- skipancestry:

  TRUE/FALSE whether to skip ancestry prediction

- ancestrysnps:

  SNPs to use for ancestry prediction (either ancestry only or all SNPs)

- pcagroups:

  How to color PCA plots (superpopulations and/or subpopulations)

- assay:

  assay used (kintelligence or custom)

- positions:

  SNP chromosomal positions

- threads:

  number of threads to use for EFM

- output:

  Name of output directory
