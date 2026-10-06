# run MixDeR using the CLI; ancestry prediction

run MixDeR using the CLI; ancestry prediction

## Usage

``` r
run_mixder_ancestry(
  sample_manifest = NULL,
  sample = NULL,
  replicate = NULL,
  sample_reports = getwd(),
  output = "output",
  refpath = NULL,
  refs = NULL,
  mixdeconv = TRUE,
  uncond = TRUE,
  cond = FALSE,
  sets = 10,
  dynamicAT = 0.015,
  staticAT = 10,
  minimum_snps = 6000,
  A1_threshold = 0.99,
  A2_threshold = 0.6,
  minor_contrib_threshold = FALSE,
  keep_bins = TRUE,
  snps,
  pcagroups
)
```

## Arguments

- sample_manifest:

  path to sample manifest file (default=NULL)

- sample:

  If running individual sample, specify the sample ID; requires
  sample_manifest=NULL (default=NULL)

- replicate:

  If running individual sample along with a replicate, specify the
  replicate ID; requires sample_manifest=NULL (default=NULL)

- sample_reports:

  Path to directory of Sample Reports or CSV file of mixture genotypes
  (default=current directory)

- output:

  name of output directory, will be outputted into the sample reports
  directory (default="output")

- refpath:

  Path to directory containing reference genotypes (default=NULL)

- refs:

  list of sample IDs to use as reference for conditioning, can specify
  more than one (default=NULL, example: list("Ref1", "Ref2"))

- mixdeconv:

  whether to run mixture deconvolution (default=TRUE)

- uncond:

  run (or use) an unconditioned mixture deconvolution (default=TRUE)

- cond:

  run (or use) a conditioned mixture deconvolution (default=FALSE)

- sets:

  number of bins to divide SNPs into (default=10)

- dynamicAT:

  dynamic analytical threshold applied to data (default=0.015)

- staticAT:

  static analytical threshold applied to data (default=10)

- minimum_snps:

  minimum number of SNPs used to generate profile (default=6000)

- A1_threshold:

  Allele 1 probability threshold (default=0.99)

- A2_threshold:

  Allele 2 probability threshold (default=0.60)

- minor_contrib_threshold:

  Whether to apply the allele 1 probability threshold to the minor
  contributor, regardless of the minimum number of SNPs (default=FALSE)

- keep_bins:

  Use existing binned SNP data, if exists (default=TRUE)

- snps:

  SNPs to use for ancestry prediction (either ancestry only or all SNPs)

- pcagroups:

  How to color PCA plots (superpopulations and/or subpopulations)
