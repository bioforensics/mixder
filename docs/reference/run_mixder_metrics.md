# run MixDeR using the CLI; calculate validation metrics

run MixDeR using the CLI; calculate validation metrics

## Usage

``` r
run_mixder_metrics(
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
  A1min = 0.95,
  A1max = 0.99,
  A2min = 0.5,
  A2max = 0.65,
  major = NULL,
  minor = NULL,
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

- A1min:

  when calculating metrics, the start of the range of the allele 1
  probability threshold (default=0.95)

- A1max:

  when calculating metrics, the end of the range of the allele 1
  probability threshold (default=0.99)

- A2min:

  when calculating metrics, the start of the range of the allele 2
  probability threshold (default=0.50)

- A2max:

  when calculating metrics, the end of the range of the allele 2
  probability threshold (default=0.65)

- major:

  for comparing references to generated single source profiles, the
  assumed major contributor sample ID (default=NULL)

- minor:

  for comparing references to generated single source profiles, the
  assumed minor contributor sample ID (default=NULL)

- filter_missing:

  Whether to remove SNPs with a missing value for the allele 2
  (default=FALSE)

- minor_contrib_threshold:

  Whether to apply the allele 1 probability threshold to the minor
  contributor, regardless of the minimum number of SNPs (default=FALSE)

- keep_bins:

  Use existing binned SNP data, if exists (default=TRUE)
