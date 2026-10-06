# Ancestry prediction using PCA

Ancestry prediction using PCA

## Usage

``` r
ancestry_prediction(
  report,
  path,
  id,
  analysis_type,
  contrib_status,
  testsnps,
  groups
)
```

## Arguments

- report:

  inferred genotypes

- path:

  write path

- id:

  sample ID

- analysis_type:

  mixure deconvolution type (conditioned vs. unconditioned)

- contrib_status:

  contributor status (major vs. minor)

- testsnps:

  SNP set to use for PCA (All autosomal SNPs or only ancestry SNPs)

- groups:

  How to color PCA plots (superpopulations and/or subpopulations)

## Value

NA
