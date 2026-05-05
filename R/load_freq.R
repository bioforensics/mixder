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
#' Load frequency data
#'
#' @param twofreqs If two different allele frequency files are to be used
#' @param freq_both_input Allele frequency file, if only using one
#' @param freq_major_input Allele frequency file for major contributor
#' @param freq_minor_input Allele frequency file for minor contributor
#'
#' @return list of major AF data data frame and minor AF data data frame
#' @export
load_freq = function(out_path, twofreqs, freq_both_input, freq_major_input, freq_minor_input) {
  freqs_allowed = c("Global - 1000G", "Global - gnomAD", "AFR - 1000G", "AMR - 1000G", "EAS - 1000G", "EUR - 1000G", "SAS - 1000G")
  if (!twofreqs) {
    if (file.exists(freq_both_input)) {
      freq_minor = checking_af(freq_both_input)
      freq_major = freq_minor
    } else {
      freq_both = decode_freq_name(tolower(freq_both_input))
      if (!freq_both %in% freqs_allowed) {
        stop("Please check allele frequency file specified!")
      }
      if (freq_both == "Global - 1000G") {
        freq_minor = mixder::popFreq_1000G
        freq_major = mixder::popFreq_1000G
      } else if (freq_both == "Global - gnomAD"){
        freq_minor = mixder::popFreq_gnomad
        freq_major = mixder::popFreq_gnomad
      } else if (freq_both == "AFR - 1000G") {
        freq_minor = mixder::popFreq_AFR
        freq_major = mixder::popFreq_AFR
      } else if (freq_both == "AMR - 1000G") {
        freq_minor = mixder::popFreq_AMR
        freq_major = mixder::popFreq_AMR
      } else if (freq_both == "EAS - 1000G") {
        freq_minor = mixder::popFreq_EAS
        freq_major = mixder::popFreq_EAS
      } else if (freq_both == "EUR - 1000G") {
        freq_minor = mixder::popFreq_EUR
        freq_major = mixder::popFreq_EUR
      } else if (freq_both == "SAS - 1000G") {
        freq_minor = mixder::popFreq_SAS
        freq_major = mixder::popFreq_SAS
      } else {
        message("Check frequency data selection")
      }
    }
  } else {
    if (file.exists(freq_major_input)) {
      freq_major = checking_af(freq_major_input)
    } else {
      freq_major = decode_freq_name(tolower(freq_major_input))
      if (!freq_major %in% freqs_allowed) {
        stop("Please check allele frequency file specified!")
      }
      if (freq_major == "Global - 1000G") {
        freq_major = mixder::popFreq_1000G
      } else if (freq_major == "Global - gnomAD") {
        freq_major = mixder::popFreq_gnomad
      } else if (freq_major == "AFR - 1000G") {
        freq_major = mixder::popFreq_AFR
      } else if (freq_major == "AMR - 1000G") {
        freq_major = mixder::popFreq_AMR
      } else if (freq_major == "EAS - 1000G") {
        freq_major = mixder::popFreq_EAS
      } else if (freq_major == "EUR - 1000G") {
        freq_major = mixder::popFreq_EUR
      } else if (freq_major == "SAS - 1000G") {
        freq_major = mixder::popFreq_SAS
      } else {
        message("Check frequency data selection")
      }
    }
    if (file.exists(freq_minor_input)) {
      freq_minor = checking_af(freq_minor_input)
    } else {
      freq_minor = decode_freq_name(tolower(freq_minor_input))
      if (!freq_minor %in% freqs_allowed) {
        stop("Please check allele frequency file specified!")
      }
       if (freq_minor == "Global - 1000G") {
        freq_minor = mixder::popFreq_1000G
      } else if (freq_minor == "Global - gnomAD") {
        freq_minor = mixder::popFreq_gnomad
      } else if (freq_minor == "AFR - 1000G") {
        freq_minor = mixder::popFreq_AFR
      } else if (freq_minor == "AMR - 1000G") {
        freq_minor = mixder::popFreq_AMR
      } else if (freq_minor == "EAS - 1000G") {
        freq_minor = mixder::popFreq_EAS
      } else if (freq_minor == "EUR - 1000G") {
        freq_minor = mixder::popFreq_EUR
      } else if (freq_minor == "SAS - 1000G") {
        freq_minor = mixder::popFreq_SAS
      } else {
        message("Check frequency data selection")
      }
    }
  }
  return(list(freq_major, freq_minor))
}
