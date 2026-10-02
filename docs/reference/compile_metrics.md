# Compiling metrics for the range of allele 1 and allele 2 probablity thresholds

Compiling metrics for the range of allele 1 and allele 2 probablity
thresholds

## Usage

``` r
compile_metrics(
  x,
  ref,
  minimum_snps,
  A1min,
  A1max,
  A2min,
  A2max,
  filter_missing
)
```

## Arguments

- x:

  Data frame of allele probabilities and genotype calls

- ref:

  Data frame of reference genotypes

- minimum_snps:

  Minimum number of SNPs required

- A1min:

  Minimum value of allele 1 probability threshold

- A1max:

  Maximum value of allele 1 probability threshold

- A2min:

  Minimum value of allele 2 probability threshold

- A2max:

  Maximum value of allele 2 probability threshold

- filter_missing:

  TRUE/FALSE to filter SNPs with missing allele 2 values

## Value

List of data frames compiled using the min/max values of the allele
probability thresholds and the minimum number of SNPs
