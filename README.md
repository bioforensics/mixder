
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MixDeR - Current Version: 0.7.1

<!-- badges: start -->
<!-- badges: end -->

MixDeR (**Mix**ture **De**convolution in **R**) is a workflow (with a
Shiny app) for performing mixture deconvolution of ForenSeq
Kintelligence SNP data for two-person mixtures using
[EuroForMix](https://github.com/oyvble/euroformix/) and creating
GEDmatch PRO reports for the individual contributor SNP profiles.

This method requires extensive validation of the settings. MixDeR
provides the option of calculating various metrics for evaluating the
accuracy of the deduced SNP genotypes. This is extremely useful when
determining settings, specifically the allele 1 probability threshold,
the allele 2 probability threshold, and the minimum number of SNPs.

*Note: MixDeR (and EFM) assume the mixture samples are composed of two
contributors. MixDeR is able to identify and alert the user to samples
that may be potentially either single source or consist of a mixture
with a large mixture ratio (i.e. \> 1:100 ratio between contributors).
In these scenarios, the user is warned to be cautious with the minor
contributor inferred genotyping results.*

The preprint of the manuscript on MixDeR is available
[here](https://www.preprints.org/manuscript/202407.1705/v1).

## Installation

For any installation, EuroForMix must be installed. Please follow the
instructions from the [EuroForMix GitHub
page](https://github.com/oyvble/euroformix/). For this version of
MixDeR, EFM version 4.0.8 or earlier is required.

If installing from GitHub:  
The R package `devtools` is required to install from GitHub:

    install.packages("devtools")
    devtools::install_github("bioforensics/mixder")

If installing from source, first install the following R packages:

    install.packages(c( "dplyr", "glue", "prompter","readxl", "rlang", shiny", "shinyFiles", "shinyjs", "tibble", "tidyr"))

To install MixDeR from source (i.e. the `mixder_0.1.0.tar.gz` file):

    install.packages("/path/to/mixder_0.1.0.tar.gz", repos = NULL, type="source")

For example, if the `mixder_0.1.0.tar.gz` file is located in your
Documents folder:

    install.packages("~/Documents/mixder_0.1.0.tar.gz", repos = NULL, type="source")

## Usage

To launch the shiny app:

    library(mixder)
    mixder()

## Required files

1.  **Mixture Kintelligence SNP profiles**  
    This can be in the form of the UAS Sample Report or a TSV file with
    the below format:

| Sample.Name |   Marker   | Allele.1 | Allele.2 | Height.1 | Height.2 |
|:-----------:|:----------:|:--------:|:--------:|:--------:|:--------:|
|  Sample01   | rs12615742 |    T     |    C     |    0     |   134    |
|  Sample01   | rs16885694 |    G     |    A     |    43    |    63    |

The files should be tab delimited and should be named as a `.tsv` file,
such as: `SampleID_set1.tsv`.

If using the Shiny app, you must specify the folder containing these SNP
files. Multiple samples (with multiple files each) can be in the same
folder. Additional files may be present in the folder and will be
ignored by MixDeR.

MixDeR will divide the entire Kintelligence dataset into more manageable
sets (organized by total SNP read depth) to run through EFM (ideal for
best performance). The user may specify how many sets the program will
use (see below); the default is 10 sets. If providing TSV files, the SNP
should be divided into multiple sets and named accordingly
(i.e. `SampleID_set1.tsv`). The user must then specify how many sets are
provided so MixDeR knows how many files to process per sample.

The default is for MixDeR to use previously-created SNP sets, if present
in the specified input folder. If this option is unselected, MixDeR will
create new SNP sets files, overwriting any previously made files.

2.  **The sample manifest**  
    This file lists the Sample IDs of the files to run. The `SampleID`
    is extracted from the `Sample Name` field in the Sample Report. The
    columns names do not matter, just the order and both columns
    **must** be present even if no replicates are included. If a single
    sample is run, only the single ID needs to be in the first column,
    the second column should be left blank. If a second sample is to be
    run in replicate, the replicate ID should be listed in the second
    column.

| SampleID  | ReplicateID |
|:---------:|:-----------:|
| Sample01a |             |
| Sample01a |  Sample01b  |

## Other files which may be required

**Allele frequency file**

MixDeR provides general population allele frequencies for Kintelligence
SNPs from either [1000 Genomes Phase 3
dataset](https://www.internationalgenome.org/home) or the [gnomAD v4
dataset](https://gnomad.broadinstitute.org/downloads#v4). However, it is
ideal to use allele frequencies derived from the population that closely
matches the contributor(s) of interest. Therefore, MixDeR provides the
user the opportunity to upload a different allele frequency file.
EuroForMix requires the below format for allele frequency files, for all
SNPs with each SNP as its own column:

| Allele | rs6690515 | rs424079 | rs2055204 |
|:------:|:---------:|:--------:|:---------:|
|   A    | 0.122837  | 0.64677  | 0.501441  |
|   C    |    NA     | 0.35323  |    NA     |
|   G    | 0.877163  |    NA    | 0.498559  |
|   T    |    NA     |    NA    |    NA     |

Given the difficulty of formatting the data as such, MixDeR will create
this format for the user if the user provides the frequency data in a
CSV file with the following format (NOTE: the column names MUST match
below; the order of columns and additional columns will not affect it).

|    SNP    | Ref | Alt |  Ref_AF  |  Alt_AF  |
|:---------:|:---:|:---:|:--------:|:--------:|
| rs6690515 |  A  |  C  | 0.35323  | 0.64677  |
| rs424079  |  T  |  C  | 0.122837 | 0.877163 |
| rs2055204 |  G  |  A  | 0.501441 | 0.498559 |

MixDeR provides the option to use the same allele frequency file for
both the major and minor contributor or select different allele
frequency files for the major and minor contributor. If two different
frequency files are selected, MixDeR will run EFM twice, once using the
allele frequency file for the major contributor (and extracting the
inferred genotypes for the major contributor from those results) and
once using the allele frequency file for the minor contributor (and
extracting the inferred genotypes for the minor contributor from those
results).

**Reference Genotypes**

If calculating validation metrics or performing a conditioned
deconvolution, the reference genotypes are required.

There are two options for providing reference genotypes. MixDeR accepts
the UAS Sample Report, stored in a separate folder. A second option is
to provide a single CSV file containing all references named
`EFM_references.csv` with the following format:

| Sample.Name |   Marker   | Allele1 | Allele2 |
|:-----------:|:----------:|:-------:|:-------:|
|  Sample01   | rs12615742 |    T    |    C    |
|  Sample01   | rs16885694 |    G    |    G    |

The user must provide the folder containing either the Sample Reports or
the CSV reference file.  
NOTE: MixDeR first searches for the CSV file in the provided folder. If
there are additional wanted references not contained in this file,
please remove the CSV file and run MixDeR again. MixDeR creates the CSV
file containing genotypes from the Sample Reports within the provided
folder.  
\_\_\_\_\_\_\_\_

## Details

MixDeR has three modules:  
1. EFM mixture deconvolution  
2. Calculate validation metrics  
3. Create GEDmatch PRO reports

EFM mixture deconvolution must be run at least once. If it’s been run
previously, the other modules can be run using the existing
deconvolution data.

MixDeR can either calculate validation metrics OR create the GEDmatch
PRO reports during a single run, not both.

NOTE about the allele probability thresholds:  
This workflow utilizes individual probabilities for each allele call
from EFM. The reported allele 1 is the allele with the higher
probability; allele 2 is the allele with the lower probability. When
applying the allele 1 probability threshold, any SNP with a probability
below the threshold will be removed completely from the dataset. When
applying the allele 2 probability threshold to the remaining SNPs, if
the probability is below the threshold, allele 2 is reported as the same
allele as allele 1. If it is above, the allele will be reported as
called. For example, if the genotype for SNP rs12615742 is C,T but the
allele 2 probability is below the threshold, the SNP genotype will be
reported as C,C. If it is above the threshold, the SNP genotype will be
reported as C,T.

## Running mixture deconvolution using EuroForMix

There are several options/settings to run EFM mixture deconvolution:  
The type of mixture deconvolution analysis to perform (one or both can
be selected at once):  
\* Unconditioned analysis  
\* Conditioned analysis  
**Allele Frequency Data**: The user must choose which allele frequency
data to use: 1000Genomes Phase 3 data, gnomAD v4 data, or upload a
custom file. See above for more details about the format for uploading a
custom AF file.  
**References to Condition on**: IF running a conditioned analysis, once
the reference folder has been uploaded, this dropdown menu will
auto-populate containing the reference sample IDs. The user must select
which references to condition on. **As of now, MixDeR can only condition
on a single reference sample**. However, multiple references can be
selected for conditioning; the conditioned analyses will be run
separately.  
**Number of SNP Bins**: The number of SNP sets for each sample. The
default is 10.  
**Static Analytical Threshold**: The minimum number of reads required
for a SNP to be included (default = 10).  
**Dynamic Analytical Threshold**: The percent of total SNP reads
required for a SNP to. be included (default = 0.015 or 1.5%). (A quick
note on the ATs: both the static and dynamic ATs are applied and the one
producing the higher AT will be used in the EFM software).  
**Output Folder Name**: The name of the folder created within the folder
containing the original SNP files to store the generated output for the
entire workflow.  
**Minimum Number of SNPs**: The minimum number of SNPs

## Calculating validation metrics

There are several options/settings to calculate the validation
metrics:  
**Minimum Allele 1 Probability Threshold** and **Maximum Allele 1
Probability Threshold**: The range of allele 1 probability thresholds
for calculating the validation metrics, increasing in increments of 0.01
(i.e. if minimum is set to 0.5 and maximum is set to 1, will calculate
metrics using a threshold of 0.5, 0.51, 0.52, 0.53, up to 1).  
**Minimum Allele 2 Probability Threshold** and **Maximum Allele 2
Probability Threshold**: The range of allele 2 probability thresholds
for calculating the validation metrics, increasing in increments of 0.01
(i.e. if minimum is set to 0.5 and maximum is set to 1, will calculate
metrics using a threshold of 0.5, 0.51, 0.52, 0.53, up to 1).  
**Major Contributor Sample ID**: The ID of the major contributor of the
mixture. Once the reference folder is uploaded, this dropdown menu will
auto-populate with the reference sample IDs.  
**Minor Contributor Sample ID**: The ID of the minor contributor of the
mixture. Once the reference folder is uploaded, this dropdown menu will
auto-populate with the reference sample IDs.  
**Remove SNPs If Missing Either Allele?**: If an allele 1 is inferred to
be missing (reported as `99`), that SNP will be automatically dropped
from the final dataset. By default, if an allele 2 is inferred to be
missing and the allele 2 probability is above the allele 2 probability
threshold, the SNP is reported as homozygous for allele 1. However,
selecting this option will result in dropping the SNP if the allele 2
probability of the missing allele 2 is above the allele 2 probability
threshold, instead of reporting the SNP as homozygous for allele 1.

If calculating validation metrics, reference genotypes are required to
calculate genotype accuracy. MixDerR will calculate the metrics for the
range of allele 1 probability thresholds and allele 2 probability
thresholds specified by the user. The final output file looks as such:

| A1 cutoff | A2 cutoff | Total SNPs | N No Ref | N SNPs tested | N Genotypes Correct | Genotype Accuracy | Heterozygosity |
|:---------:|:---------:|:----------:|:--------:|:-------------:|:-------------------:|:-----------------:|:--------------:|
|   0.99    |   0.01    |    9735    |    8     |     9727      |        9549         |      0.9817       |     0.456      |
|   0.99    |   0.02    |    9735    |    8     |     9727      |        9548         |      0.9815       |     0.456      |
|   0.99    |   0.03    |    9735    |    8     |     9727      |        9548         |      0.9815       |     0.456      |

## Creating GEDmatch PRO Reports

**Allele 1 Probability Threshold to create GEDmatch PRO Report** and
**Allele 2 Probability Threshold to create GEDmatch PRO Report**: The
allele 1 and allele 2 probability thresholds. If the contributor is the
major contributor to the mixture, MixDeR will apply the allele 1 and
allele 2 probability thresholds UNLESS this results in a profile with
fewer SNPs than the specified minimum (6000 is the default). If it does
not meet this minimum, the top 6,000 SNPs (or whatever the user
specifies as the minimum) with the highest allele 1 probabilities are
used and then the allele 2 probability threshold is applied. For minor
contributors, the default setting is that the minimum number of SNPs is
automatically used and then the allele 2 probability threshold is
applied. The option to apply the allele 1 probability threshold (similar
in manner to the major contributor) can be applied.  
**Remove SNPs If Missing Either Allele?**: If an allele 1 is inferred to
be missing (reported as `99`), that SNP will be automatically dropped
from the final dataset. By default, if an allele 2 is inferred to be
missing and the allele 2 probability is above the allele 2 probability
threshold, the SNP is reported as homozygous for allele 1. However,
selecting this option will result in dropping the SNP if the allele 2
probability of the missing allele 2 is above the allele 2 probability
threshold, instead of reporting the SNP as homozygous for allele 1.

As a way to assist the analyst in evaluating the inferred genotypes of a
mixture of unknown contributors, several metrics are calculated in this
step for three different scenarios: (1) only the allele 2 probability
threshold applied; (2) the allele 1 and allele 2 probability thresholds
applied; and (3) the minimum number of SNPs used and the allele 2
probability threshold applied. For each dataset, the follow metrics are
calculated: number of SNPs, mean allele 1 probabilities, median allele 1
probabilities, standard deviation (SD) of the allele 1 probabilities and
heterozygosity. Below is an example of the table created by MixDeR in
this step. A density plot of allele 1 probability thresholds is also
created. In general, the more SNPs with higher allele 1 probabilities,
the higher the accuracy of the inferred genotypes. However, determining
exactly what qualifies as acceptable should be determined by individual
labs.

| Allele1 Threshold Applied | Allele2 Threshold Applied | Total SNPs | Mean A1 Prob | Median A1 Prob | SD A1 Prob | Heterozygosity |
|:-------------------------:|:-------------------------:|:----------:|:------------:|:--------------:|:----------:|:--------------:|
|            No             |            Yes            |   10024    |    0.9984    |       1        |   0.0096   |     0.4626     |
|            Yes            |            Yes            |    9718    |    0.9998    |       1        |  9.00E-04  |     0.4569     |
|    Minimum \# of SNPs     |            Yes            |    6000    |      1       |       1        |     0      |     0.4495     |
