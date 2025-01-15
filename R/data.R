# -------------------------------------------------------------------------------------------------
# Copyright (c) 2024, DHS.
#
# This file is part of MixDeR and is licensed under the BSD license: see LICENSE.
#
# This software was prepared for the Department of Homeland Security (DHS) by the Battelle National
# Biodefense Institute, LLC (BNBI) as part of contract HSHQDC-15-C-00064 to manage and operate the
# National Biodefense Analysis and Countermeasures Center (NBACC), a Federally Funded Research and
# Development Center.
# -------------------------------------------------------------------------------------------------
#' Kintelligence SNP positions file
#'
#' Contains the hg19 SNP positions for 10,230 SNPs necessary
#'    for creating GEDmatch PRO reports.
#'
#' @format A data frame containing 10,230 rows and 3 variables:
#'  \describe{
#'      \item{rsid}{SNP rsID}
#'      \item{chromosome}{Chromosome}
#'      \item{position}{Position}
#'      }
#'
#' @source Verogen UAS Kintelligence Sample Report
#'
"kintelligence_snp_positions"

#' Allele Frequency file using gnomADv4 dataset
#'
#' Allele frequencies for 10,039 SNPs
#'
#' @format A list containing a 10039 elements (SNPs) with 4 rows:
#'  \describe{
#'      \item{SNP}{SNP rsID}
#'      \item{Allele}{Allele (A/C/G/T)}
#'      \item{Probability}{Allele Probability}
#'      }
#'
#' @source gnomAD v4 genomes (https://gnomad.broadinstitute.org/downloads#v4)
#'
"popFreq_gnomad"

#' Allele Frequency file using 1000G Phase 3 dataset
#'
#' Allele frequencies for 10,039 SNPs
#'
#' @format A list containing a 10039 elements (SNPs) with 4 rows:
#'  \describe{
#'      \item{SNP}{SNP rsID}
#'      \item{Allele}{Allele (A/C/G/T)}
#'      \item{Probability}{Allele Probability}
#'      }
#'
#' @source 1000 Genomes (https://www.internationalgenome.org/data)
#'
"popFreq_1000G"

#' SNP information json file
#'
#' Provides information on which SNPs need to be reverse complemented
#'
#' @format A list containing a list of 172 SNPs:
#'  \describe{
#'      \item{SNP}{SNP rsID}
#'      \item{Type}{Type of SNP; identity/ancestry/phenotype}
#'      \item{Alleles}{Possible alleles}
#'      \item{ReverseCompNeeded}{If reverse complementation is necessary}
#'      \item{Coord}{STRait Razor coordinate to identify SNP within sequence string}
#'      }
#'
#' @source lusSTR Python package (https://github.com/bioforensics/lusSTR)
#'
"snpinfo"

#' 1000 Genomes genotypes for 54 ancestry SNPs
#'
#' Used to run PCA for ancestry prediction
#'
#' @format A dataframe containing 60 columns (each SNP) and 2,157 rows (each individual):
#'  \describe{
#'      \item{SNP}{SNP rsID with reference allele}
#'      \item{ID}{Sample ID of individual}
#'      }
#'
#' @source 1000 Genomes (https://www.internationalgenome.org/data)
#'
"ancestry_1000G"
