# Calculating metrics for a single set of allele 1 and allele 2 probability thresholds

Calculating metrics for a single set of allele 1 and allele 2
probability thresholds

## Usage

``` r
calc_metrics(x, A1_thresh, A2_thresh, ref, minimum_snps, filter_missing)
```

## Arguments

- x:

  Data frame of allele probabilities and genotype calls

- A1_thresh:

  Allele 1 probability threshold

- A2_thresh:

  Allele 2 probability threshold

- ref:

  Data frame of reference genotypes

- minimum_snps:

  Minimum number of SNPs required

- filter_missing:

  TRUE/FALSE to filter SNPs with missing allele 2 values

## Value

return row of metrics for specified thresholds
