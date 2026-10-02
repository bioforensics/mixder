# Creates replicate df

This function reads in both sample files, determines which contains the
larger number of SNPs and creates the replicate files, with ensuring the
same SNPs are in each SNP set for both samples.

## Usage

``` r
create_replicatedf(inpath, id, replicate_id, nsets)
```

## Arguments

- inpath:

  Path of SNP sets

- id:

  Sample ID

- replicate_id:

  Replicate ID

- nsets:

  Number of SNP sets

## Value

List of IDs, identifying the replicate sample
