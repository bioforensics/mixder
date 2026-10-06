# SNP information json file

Provides information on which SNPs need to be reverse complemented

## Usage

``` r
snpinfo
```

## Format

A list containing a list of 172 SNPs:

- SNP:

  SNP rsID

- Type:

  Type of SNP; identity/ancestry/phenotype

- Alleles:

  Possible alleles

- ReverseCompNeeded:

  If reverse complementation is necessary

- Coord:

  STRait Razor coordinate to identify SNP within sequence string

## Source

lusSTR Python package (https://github.com/bioforensics/lusSTR)
