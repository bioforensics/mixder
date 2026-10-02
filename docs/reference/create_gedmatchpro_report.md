# Create GEDmatch PRO report

Create GEDmatch PRO report

## Usage

``` r
create_gedmatchpro_report(
  path,
  x,
  contrib,
  contrib_status,
  minimum_snps,
  A1,
  A2,
  A1min,
  A1max,
  A2min,
  A2max,
  minor_threshold,
  filter_missing
)
```

## Arguments

- path:

  Write path

- x:

  Data frame of EFM predicted genotypes

- contrib:

  Contributor number from the EFM output file (C1/C2)

- contrib_status:

  Major/Minor

- minimum_snps:

  Minimum number of SNPs required

- A1:

  Allele 1 probability threshold for report

- A2:

  Allele 2 probability threshold for report

- A1min:

  Minimum value for allele 1 probability threshold

- A1max:

  Maximum value for allele 1 probability threshold

- A2min:

  Minimum value for allele 2 probability threshold

- A2max:

  Maximum value for allele 2 probability threshold

- minor_threshold:

  If apply allele 1 probability to minor contributor

- filter_missing:

  TRUE/FALSE to filter SNPs with missing allele 2 values

## Value

list of two data frames, the GEDmatch PRO report and a data frame of
metrics calculated for that report
