# Function to determine how to create the AT file

This function determines if there is a replicate sample, it will
calculate the AT for all SNPs using each sample, then for each SNP will
use the higher threshold for the final AT file.

## Usage

``` r
process_kinreport(sample, rep_sample, kinpath, dynamicAT, staticAT)
```

## Arguments

- sample:

  Sample ID

- rep_sample:

  Replicate ID

- kinpath:

  path of the Kintelligence Sample Reports

- dynamicAT:

  Dynamic analytical threshold

- staticAT:

  Static analytical threshold

## Value

Data frame containing the AT for each SNP
