
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MixDeR - Current Version: 0.7.4

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

## Installation

For any installation, EuroForMix must be installed. Please follow the
instructions from the [EuroForMix GitHub
page](https://github.com/oyvble/euroformix/). EuroForMix version 4.2.3
and earlier have been tested and are compatible with MixDeR. If using a
newer version, please be aware it has not been tested and errors may
occur!

If installing from GitHub:  
The R package `devtools` is required to install from GitHub:

    install.packages("devtools")
    devtools::install_github("bioforensics/mixder")

If installing from source, first install the following R packages:

    install.packages(c("dplyr", "ggplot2", "glue", "prompter", "readxl", "rlang", "shiny", "shinyFiles", "shinyjs", "tibble", "tidyr"))

To install MixDeR from source (i.e. the `mixder_0.1.0.tar.gz` file):

    install.packages("/path/to/mixder_0.1.0.tar.gz", repos = NULL, type="source")

For example, if the `mixder_0.1.0.tar.gz` file is located in your
Documents folder:

    install.packages("~/Documents/mixder_0.1.0.tar.gz", repos = NULL, type="source")

## Usage

To launch the shiny app:

    library(mixder)
    mixder()

## Citation

Please cite the following paper:

    Mitchell, R., Peck, M., Gorden, E., & Just, R. (2025). MixDeR: A SNP mixture 
    deconvolution workflow for forensic genetic genealogy. Forensic Science 
    International: Genetics, 76, 103224, doi: 10.1016/j.fsigen.2025.103224
