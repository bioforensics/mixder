# Running mixture deconvolution in EuroForMix

Running mixture deconvolution in EuroForMix

## Usage

``` r
run_efm(
  date,
  popFreq,
  refData,
  id,
  replicate_id,
  inpath,
  out_path,
  attable,
  nsets,
  ancestry,
  cond = NULL,
  uncond = TRUE,
  keep_bins = TRUE
)
```

## Arguments

- date:

  Date and time of MixDeR run

- popFreq:

  Allele frequency file

- refData:

  Reference genotypes file

- id:

  Sample ID

- replicate_id:

  Replicate ID

- inpath:

  Path to SNP sets

- out_path:

  Output path

- attable:

  AT table

- nsets:

  Number of SNP sets

- ancestry:

  if not skipping ancestry prediction

- cond:

  Sample IDs to condition on

- uncond:

  TRUE/FALSE if performing unconditioned analysis

- keep_bins:

  To use existing SNP bins or create new bins (and files)
