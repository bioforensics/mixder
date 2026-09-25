# Filters the called genotypes based on the allele 1 and allele 2 probability thresholds

Filters the called genotypes based on the allele 1 and allele 2
probability thresholds

## Usage

``` r
filter_alleles(
  all_files,
  contrib_status,
  minimum_snps,
  A1_threshold,
  A2_threshold,
  minor_threshold,
  filter_missing
)
```

## Arguments

- all_files:

  Data frame containing the called genotypes

- contrib_status:

  Major/Minor

- minimum_snps:

  Minimum number of SNPs required

- A1_threshold:

  Allele 1 probability threshold

- A2_threshold:

  Allele 2 probability threshold

- minor_threshold:

  If apply allele 1 probability threshold to minor contributor

- filter_missing:

  TRUE/FALSE to filter SNPs with missing allele 2 values

## Value

Data frame with filtered allele calls
