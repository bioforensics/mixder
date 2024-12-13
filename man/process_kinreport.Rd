% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_kinreport.R
\name{process_kinreport}
\alias{process_kinreport}
\title{Function to determine how to create the AT file}
\usage{
process_kinreport(sample, rep_sample, kinpath, dynamicAT, staticAT)
}
\arguments{
\item{sample}{Sample ID}

\item{rep_sample}{Replicate ID}

\item{kinpath}{path of the Kintelligence Sample Reports}

\item{dynamicAT}{Dynamic analytical threshold}

\item{staticAT}{Static analytical threshold}
}
\value{
Data frame containing the AT for each SNP
}
\description{
This function determines if there is a replicate sample, it will
   calculate the AT for all SNPs using each sample, then for each SNP will
   use the higher threshold for the final AT file.
}
