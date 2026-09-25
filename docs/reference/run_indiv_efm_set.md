# Running individual EFM SNP sets in parallel

Running individual EFM SNP sets in parallel

## Usage

``` r
run_indiv_efm_set(
  i,
  ids,
  snps_input,
  popFreq,
  refData,
  id,
  replicate_id,
  write_path,
  attable,
  cond = NULL,
  uncond = TRUE
)
```

## Arguments

- i:

  Set number

- ids:

  dataframe of replicate data

- snps_input:

  Path of SNP sets

- popFreq:

  Allele Frequency file

- refData:

  Reference(s) file

- id:

  Sample ID

- replicate_id:

  Replicate ID, if provided

- write_path:

  Path to write output to

- attable:

  AT table

- cond:

  Condition on specific references

- uncond:

  TRUE/FALSE if performing unconditioned analyses

## Value

list of mixture ratios for each analysis
