# run MixDeR using the CLI; create GEDmatch PRO report(s)

run MixDeR using the CLI; create GEDmatch PRO report(s)

## Usage

``` r
run_mixder_report(
  sample_manifest = NULL,
  sample = NULL,
  replicate = NULL,
  sample_reports = getwd(),
  output = "output",
  twofreqs = FALSE,
  freq_both = "global_1000g",
  freq_major = NULL,
  freq_minor = NULL,
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
  filter_missing = FALSE,
  minor_contrib_threshold = FALSE,
  keep_bins = TRUE
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

- twofreqs:

  TRUE if using different allele frequency data for each contributor
  (default=FALSE)

- freq_both:

  frequency data for both contributors; requires twofreqs=FALSE
  (default=1000G global)

- freq_major:

  frequency data for major contributor; requires twofreqs=TRUE
  (default=NULL)

- freq_minor:

  frequency data for minor contributor; requires twofreqs=TRUE
  (default=NULL)

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

- filter_missing:

  Whether to remove SNPs with a missing value for the allele 2
  (default=FALSE)

- minor_contrib_threshold:

  Whether to apply the allele 1 probability threshold to the minor
  contributor, regardless of the minimum number of SNPs (default=FALSE)

- keep_bins:

  Use existing binned SNP data, if exists (default=TRUE)
