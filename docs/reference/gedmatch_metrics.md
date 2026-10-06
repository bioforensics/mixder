# Calculate various metrics on the inferred genotype dataset to assist user in evaluating the final dataset

Calculate various metrics on the inferred genotype dataset to assist
user in evaluating the final dataset

## Usage

``` r
gedmatch_metrics(report, A1_threshold, A2_threshold, min_num, path)
```

## Arguments

- report:

  data frame of the inferred single source genotypes

- A1_threshold:

  Allele 1 probability threshold

- A2_threshold:

  Allele 2 probability threshold

- min_num:

  Minimum number of SNPs

- path:

  output path

## Value

data frame of compiled metrics
