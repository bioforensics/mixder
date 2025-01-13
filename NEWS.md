
### 0.7.2: December 24, 2024
- Fixed bug with compiling metrics test script
- Changed labels of 'General' allele frequencies to 'Global' allele frequencies

### 0.7.1: November 14, 2024
- Added option to remove SNPs with missing allele 2 if the allele 2 probability is above the allele 2 probability threshold (default is to report SNP as homozygous for allele 1 if allele 2 is missing)

### 0.7.0: November 7, 2024
- Improved run time by:
  - Parallelized running EFM SNP sets for up to 10 cores (10 sets) at once
  - Loads reference data once initially instead of with every sample in the sample manifest

### 0.6.4: October 30, 2024
- Added in metric calculations for the inferred genotype profile(s) in the GEDmatch PRO report(s).
- Now creates density plot(s) of the allele 1 probabilities for the inferred genotypes
- Changed how GEDmatch PRO reports for conditioned analyses are named.
- Fixed bug with EFM sometimes changing "T" alleles to "TRUE" when >2 alleles are present in the Sample Report at a particular SNP.
- Added MixDeR version to the config_settings_run document

### 0.6.3: October 16, 2024
- Fixed bug with reporting the missing allele ("99") in the GEDmatch PRO Report for allele 2. If "99" is inferred by the software for allele 2, MixDeR will now report the SNP as homozygous for allele 1 (assuming the allele 1 probability passes the allele 1 probability threshold).

### 0.6.2: October 15, 2024
- Added in mixture ratios check to alert user to potential issues with the mixture, either the sample could be single source or the mixture could have a large mixture ratio (e.g. >1:100). User is alerted to proceed with caution with the minor contributor's inferred genotypes.

### 0.6.1: October 7, 2024
- Fixed handling of initial SNP set contains only SNPs with 0 reads. MixDeR will bypass the SNP set to avoid an error in EuroForMix. 

### 0.6.0: October 2, 2024
- Added ability to specify different allele frequency files for the major and minor contributor. If two separate files are used, MixDeR will run EFM twice, using each AF file separately.
- Rearranged Shiny app interface
- Fixed bug in loading Sample Reports generated using UAS version 2.5 or later
- Added option to always remake SNP bins (default is set to use previously created SNP bins, if present)

### 0.5.1: July 30, 2024
- Fixed bug with applying allele 1 probability threshold to minor contributor.

### 0.5.0: July 30, 2024
- Added option to apply allele 1 probability threshold to minor contributor if applying the allele 1 probability threshold results in a profile containing at least the minimum number of SNPs. This is in contract to the default which always uses the minimum number of SNPs for the minor contributor.
- Made improvements to how validation metrics are calculated to significantly reduce the run time

### 0.4.0: June 18, 2024
- Added creation of log files for each run (labeled as the date and time), including a file containing all the settings and a file containing the verbose MixDeR output.
- Added ability to input Sample Reports from UAS version 2.6.0
- Added check for correct column name in reference sample CSV file (if provided).
- Fixed bug in which marker N29INSA was not handled properly when comparing to reference genotypes

### 0.3.0: June 7, 2024
- Fixed bug with reading in previously created EFM data
- Improved method for calculating validation metrics- instead of only reporting metrics for profiles containing at least the minimum number of SNPs, MixDeR will report all specified allele 1 and allele 2 probability threshold combinations.

### 0.2.0: May 3, 2024
- Major improvements to the Shiny app:
  - Changed to conditional panels (i.e. whichever settings are required for specific methods are auto-populated when those methods are selected)
  - References auto-populate sample IDs for performing conditioned analyses and specifying the major and minor contributors when validation metrics are calculated when the reference sample report folder is specified.
- Added built-in allele frequency data:
	- Average allele frequencies for all populations of 1000 Genomes v3 dataset
	- Average allele frequencies for all populations using gnomAD v4 dataset
- Added option to upload custom allele frequency data, either in the EFM-required format or in a simpler format (one SNP per row, providing the alternate/reference alleles and their corresponding frequencies) and MixDeR formats the data for EFM.
- Added ability to input Sample Reports from UAS version 2.5
- Fixed bug in creating SNP sets

### 0.1.0: April 22, 2024
 - Initial release
