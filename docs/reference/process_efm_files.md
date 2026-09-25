# Process files created by EFM

Subsets the EFM files by contributor (C1/C2)

## Usage

``` r
process_efm_files(
  x,
  contrib,
  ref,
  minimum_snps,
  A1min,
  A1max,
  A2min,
  A2max,
  metrics = TRUE,
  filter_missing = FALSE
)
```

## Arguments

- x:

  EFM file

- contrib:

  C1/C2

- ref:

  Reference genotype file

- minimum_snps:

  Minimum number of SNPs required

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

- metrics:

  TRUE/FALSE to calculate validation metrics

- filter_missing:

  TRUE/FALSE to filter SNPs with missing allele 2 values

## Value

Data frame containing either calculated metrics (if specified) or data
frame containing genotype calls
