# Create configuration file containing all settings for a specific run

Create configuration file containing all settings for a specific run

## Usage

``` r
create_config(
  date,
  twofreqs,
  freq_all,
  freq_major,
  freq_minor,
  refs,
  sample_manifest,
  sample,
  replicate,
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
  filter_missing,
  skipancestry,
  pcasnps,
  pcagroups
)
```

## Arguments

- date:

  date and time of run

- twofreqs:

  TRUE if using separate AF data for major and minor contributors

- freq_all:

  Path (or name) of allele frequency data if using same data for both

- freq_major:

  Path (or name) of allele frequency data for major contributor

- freq_minor:

  Path (or name) of allele frequency data for minor contributor

- refs:

  path to reference folder

- sample_manifest:

  path to sample manifest

- sample:

  sample ID if sample_manifest=NULL

- replicate:

  replicate ID for running a single sample if sample_manifest=NULL

- out_path:

  name of output folder

- run_mixdeconv:

  if running mixture deconvolution

- unconditioned:

  if running an unconditioned deconvolution

- cond:

  if running a conditioned deconvolution, which references to condition
  on

- method:

  downstream method (calculate metrics/create GEDmatch PRO report)

- sets:

  number of SNP sets

- kinpath:

  path to mixture (evidence) files

- dynamicAT:

  dynamic AT

- staticAT:

  static AT

- minimum_snps:

  minimum number of SNPs

- A1_threshold:

  Allele 1 probability threshold for GEDmatch PRO report creation

- A2_threshold:

  Allele 2 probability threshold for GEDmatch PRO report creation

- A1min:

  minimum allele 1 probability threshold in range for calculating
  metrics

- A1max:

  maximum allele 1 probability threshold in range for calculating
  metrics

- A2min:

  minimum allele 2 probability threshold in range for calculating
  metrics

- A2max:

  maximum allele 2 probability threshold in range for calculating
  metrics

- major:

  assumed major contributor of mixture

- minor:

  assumed minor contributor of mixture

- filter_missing:

  TRUE/FALSE whether to filter SNPs if second allele is missing (99)

- skipancestry:

  TRUE/FALSE whether to skip ancestry prediction step

- pcasnps:

  SNPs used for PCA (ancestry prediction)

- pcagroups:

  Groups used for PCA (ancestry prediction), either Superpopulations or
  Subpopulations
