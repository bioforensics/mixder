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
#' @title Run MixDeR
#'
#' @export
#'
#' @import prompter
#' @import shiny
#' @import shinyFiles
#'
mixder = function() {
  ui = fluidPage(
    # App title
    titlePanel("MixDeR: A Mixture Deconvolution Workflow for FGG"),
    sidebarPanel(width=6,
      use_prompt(),
      checkboxInput("skip_ancestry", tags$span("Skip Ancestry Prediction Step", tags$span(
        icon(
          name = "question-circle",
        )
      ) |>
        add_prompt(message = "Check box to skip ancestry prediction step and move to mixture deconvolution.", position = "right")
      ), value = FALSE),
      conditionalPanel(condition = "input.skip_ancestry == 0", uiOutput("ancestry_text"), uiOutput("pcagroups"), uiOutput("ancestry_snps")),
      checkboxInput("uncond", tags$span("Unconditioned Analysis", tags$span(icon(
        name = "question-circle",
      )
      ) |>
        add_prompt(message = "An unconditioned analysis assumes no known contributor to the mixture and therefore does not require known genotypes to be provided.", position = "right")
      ), value = FALSE),
      checkboxInput("cond", tags$span("Conditioned Analysis", tags$span(icon(
        name = "question-circle",
      )
      ) |>
        add_prompt(message = "A conditioned analysis assumes a single known contributor to the mixture. User will select which reference sample to condition on after providing the Reference Sample Report Folder.", position = "right")
      ), value = FALSE),
      shinyFilesButton("sample_GetFile", "Select a Sample Manifest File", "Select a sample manifest", multiple = FALSE,
                       buttonType = "default", class = NULL), tags$span(icon(
                         name = "question-circle",
                       )
                       ) |>
        add_prompt(message = "A tab-delimited file containing the list of samples to run MixDeR. <br> It must contain two columns (SampleID and ReplicateID). Each row contains the ID of a single sample or the IDs of both the sample and replicate.", position = "right"),
      textOutput("sample_file"),
      shinyDirButton("kin_prefix", "Select Folder containing Mixture Sample Reports", "Please select a folder containing Mixture Sample Reports",
                     buttonType = "default", class = NULL), tags$span(icon(
                       name = "question-circle",
                     )
                     ) |>
        add_prompt(message = "Select a folder containing the Mixture Sample Reports.", position = "right"),
      textOutput("kin_inpath"),

      conditionalPanel(condition = "input.skip_ancestry == 1", uiOutput("runmd"), uiOutput("twofreqs"), uiOutput("method")),
      conditionalPanel(condition = "input.skip_ancestry == 1 & input.twofreqs == 0", uiOutput("freqselect")),
      conditionalPanel(condition = "input.twofreqs == 1", uiOutput("freqselect_major"), uiOutput("freqselect_minor")),
      conditionalPanel(condition = "input.skip_ancestry == 1 & input.twofreqs == 0 & input.uploadfreq == 'Upload Custom'", uiOutput("freq_GetFile"), uiOutput("freq_text")),
      conditionalPanel(condition = "input.twofreqs == 1 & input.uploadfreq_major == 'Upload Custom'", uiOutput("freq_GetFile_major"), uiOutput("freq_text_major")),
      conditionalPanel(condition = "input.twofreqs == 1 & input.uploadfreq_minor == 'Upload Custom'", uiOutput("freq_GetFile_minor"), uiOutput("freq_text_minor")),
      conditionalPanel(condition = "input.method == 'Calculate Metrics' | input.cond == 1", uiOutput("ref_GetFile"), uiOutput("ref_text")),
      conditionalPanel(condition = "input.cond == 1", uiOutput("ref_selector")),
      conditionalPanel(condition = "input.method == 'Calculate Metrics' | input.method == 'Create GEDmatch PRO Report'", uiOutput("filter_missing")),
      conditionalPanel(condition = "input.method == 'Calculate Metrics'", uiOutput("major_selector"), uiOutput("minor_selector"), uiOutput("metrics_A1min"), uiOutput("metrics_A1max"), uiOutput("metrics_A2min"), uiOutput("metrics_A2max")),
      conditionalPanel(condition = "input.skip_ancestry == 0 | input.method == 'Create GEDmatch PRO Report'", uiOutput("min_cont_prob"), uiOutput("report_A1"), uiOutput("report_A2")),
      conditionalPanel(condition = "input.skip_ancestry == 0 | input.run_mixdeconv == 1", uiOutput("sets"), uiOutput("keep_bins"), uiOutput("staticAT"), uiOutput("dynamicAT")),
      numericInput("minimum_snps", tags$span("Minimum Number of SNPs",  tags$span(
        icon(
          name = "question-circle",
        )
      ) |>
        add_prompt(message = "The minimum number of SNPs to retain either for calculating metrics or for creating the GEDmatch PRO report.", position = "right")
      ), value=6000),
      textInput("output", tags$span("Output Folder Name", tags$span(icon(
        name = "question-circle",
      )
      ) |>
        add_prompt(message = "This folder will be created in the specified SNP files folder to store generated output.
                   If not running EFM, it is required to specify the name of the folder containing previously generated EFM output.", position = "right")
      ), "output"),
    shinyjs::useShinyjs(),
    actionButton("Submit", "Run MixDeR")
    ),
    mainPanel(width=15,
    textOutput("text")
  ),
)

# Define server
server = function(input, output, session) {
  output$runmd = renderUI({
    checkboxInput("run_mixdeconv", tags$span("Run EFM Mixture Deconvolution", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "Check box to run mixture deconvolution using EuroForMix. Not required if run previously using MixDeR.", position = "right")
    ), value = TRUE)
  })
  output$method = renderUI({
    selectInput("method", tags$span("Method to run after mixture deconvolution?", tags$span(icon(
    name = "question-circle",
  )
  ) |>
    add_prompt(message = "Optional- if selecting Calculate Metrics, must include reference genotypes.", position = "right")
  ), c("", "Calculate Metrics", "Create GEDmatch PRO Report"))
  })
  output$twofreqs = renderUI({
    checkboxInput("twofreqs", tags$span("Use Different Allele Frequency Files For Each Contributor?", tags$span(icon(
    name = "question-circle",
  )
  ) |>
    add_prompt(message = "Will allow user to select or upload different allele frequency data for each contributor.", position = "right")
  ), value = FALSE)
  })
  output$freq_GetFile = renderUI({
                fluidRow(column(10,shinyFilesButton("freq_GetFile", "Select an Allele Frequency file" ,
                     title = "Please select an allele frequency file:", multiple = FALSE,
                     buttonType = "default", class = NULL),
                             tags$span(
                       icon(
                         name = "question-circle",
                       )
                     ) |>
    add_prompt(message = "Allele frequency file.", position = "right")
                ))
    })
  output$freq_text = renderUI({
    textOutput("freq_file")
  })
  output$freq_GetFile_major = renderUI({
    fluidRow(column(10,shinyFilesButton("freq_GetFile_major", "Select an Allele Frequency file for the Major Contributor" ,
                                        title = "Please select an allele frequency file:", multiple = FALSE,
                                        buttonType = "default", class = NULL),
    ))
  })
  output$freq_text_major = renderUI({
    textOutput("freq_file_major")
  })
  output$freq_GetFile_minor = renderUI({
    fluidRow(column(10,shinyFilesButton("freq_GetFile_minor", "Select an Allele Frequency file for the Minor Contributor" ,
                                        title = "Please select an allele frequency file:", multiple = FALSE,
                                        buttonType = "default", class = NULL),
    ))
  })
  output$freq_text_minor = renderUI({
    textOutput("freq_file_minor")
  })

  output$ref_GetFile = renderUI({
        fluidRow(
          column(10,
                 shinyDirButton("ref_GetFile", "Select Folder containing References" ,
                     title = "Please select a folder containing reference genotypes:", multiple = FALSE,
                     buttonType = "default", class = NULL),
                 tags$span(
                   icon(
                     name = "question-circle",
                   )
                 ) |>
                   add_prompt(message = "A folder containing the reference genotypes in either the UAS Sample Report format or in a CSV file (see README for specific formatting). If both are present, MixDeR will use the CSV file.", position = "right")
          ))
  })
  output$ref_text = renderUI({
    textOutput("refs_file")
  })
  output$ref_selector = renderUI({
    if (isTruthy(refs())) {
      sampleids = get_ids(refs())
      selectInput("ref_selector",
                label = tags$span("Select Reference(s) to Condition on:", tags$span(
                  icon(
                    name = "question-circle",
                  )
                ) |>
                  add_prompt(message = "Select one or more references to condition on. If more than one selected, the mixture(s) will be conditioned on each reference separately.", position = "right")
                ),
                choices = sampleids, multiple = TRUE)
    }
  })

  output$sample_file_text = renderUI({

  })
  output$ancestry_text = renderUI({
    HTML("<b>Optional: Ancestry Prediction Tool using PCA</b> <br/> Use this tool to assist in predicting the ancestry of each contributor. The population-specific allele frequency file can then be used in the next mixture deconvolution step. Select the above box to skip this step and move forward to mixture deconvolution.<br/>See the README for more information.<br/><br/>")
  })
  output$ancestry_snps = renderUI({
    selectInput("ancestry_snps", tags$span("SNPs to Use for Ancestry Prediction:", tags$span(icon(
      name = "question-circle")
    ) |>
      add_prompt(message = "Select whether to use all autosomal SNPs or only ancestry SNPs for ancestry prediction. Warning: using all SNPs may result in longer analysis time!", position = "right")
    ), c("Ancestry SNPs Only", "All Autosomal SNPs"))
  })
  output$pcagroups = renderUI({
    checkboxGroupInput("pcagroups", tags$span("Select Groups for Ancestry Prediction:", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "PCA plots are colored by the selected grouping to identify ancestry. Both may be selected. Subpopulations include 26 subgroups making up the stated superpopulations.", position = "right")
    ), choices = list("Superpopulations (AFR/AMR/EAS/EUR/SAS Only)", "Subpopulations"), selected = "Superpopulations (AFR/AMR/EAS/EUR/SAS Only)")
  })
  output$major_selector = renderUI({
    if (isTruthy(refs())) {
      sampleids = get_ids(refs())
      selectInput("major_selector",
                label = tags$span("Select major contributor:", tags$span(
                  icon(
                    name = "question-circle",
                  )
                ) |>
                  add_prompt(message = "When calculating metrics for an unconditioned analysis, the major contributor sample ID is required for calculating the genotyping accuracy. Please select the correct sample from the Dropdown menu.", position = "right")
                ),
                choices = sampleids)
    }
  })
  output$minor_selector = renderUI({
    if (isTruthy(refs())) {
      sampleids = get_ids(refs())
      selectInput("minor_selector",
                label = tags$span("Select minor contributor:",tags$span(
                  icon(
                    name = "question-circle",
                  )
                ) |>
                  add_prompt(message = "When calculating metrics for an unconditioned analysis, the minor contributor sample ID is required for calculating the genotyping accuracy. Please select the correct sample from the Dropdown menu.", position = "right")
                ),
                choices = sampleids)
    }
  })
  output$sets = renderUI({
    numericInput("sets", tags$span("Number of SNP Bins", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "The number of SNP bins a mixture SNP profile is divided into. This tells MixDeR how many SNP files to process for each mixture. The default is 10.", position = "right")
    ), value=10)
  })
  output$keep_bins = renderUI({
    checkboxInput("keep_bins", tags$span("Use Previously Created Bins, if Present?", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "Check to use previously created bins, if detected by MixDeR. Otherwise, will create new SNP bins (and files) and write over any existing files.", position = "right")
    ), value=TRUE)
  })
  output$staticAT = renderUI({
    numericInput("staticAT", tags$span("Static Analytical Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "The static analytical threshold indicates the minimum number of reads required to include a called allele it in the deconvolution at a particular SNP. The default is 10 reads.", position = "right")
    ), value=10)
  })
  output$dynamicAT = renderUI({
    numericInput("dynamicAT", tags$span("Dynamic Analytical Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "The dynamic analytical thresholds indicates the percentage of number of total reads to set the minimum number of reads required to include a called allele it in the deconvolution at a particular SNP. For example, if a SNP has 100 total reads, a 10% dynamic AT would require an allele to have at least 10 reads to be included.", position = "right")
    ), value=0.015)
  })
  output$min_cont_prob = renderUI({
    checkboxInput("min_cont_prob", tags$span("Apply Allele 1 Probability Threshold to Minor Contributor", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "By default, the SNP profile for the minor contributor contains the specified minimum number of SNPs (i.e. the top 6,000 SNPs ordered by allele 1 probability). This will instead apply the allele 1 probability threshold for the created SNP profile for the minor contributor, assuming a SNP profile can be generated that meets the specified minimum number of SNPs.", position = "right")
    ))
  })
  output$freqselect = renderUI({
    selectInput("uploadfreq", tags$span("Select Allele Frequency Data", tags$span(icon(
      name = "question-circle",
    )
    ) |>
      add_prompt(message = "Select allele frequency data. Options are global population datasets (1000G Phase 3 or gnomAD v4) or upload your own file. See README for more details.", position = "right")
    ), choices = c("Global - 1000G", "Global - gnomAD", "Upload Custom"))
  })
  output$freqselect_major = renderUI({
    selectInput("uploadfreq_major", tags$span("Select Allele Frequency Data for the Major Contributor", tags$span(icon(
      name = "question-circle",
    )
    ) |>
      add_prompt(message = "Select allele frequency data for the major contributor. Options are global population datasets (1000G Phase 3 or gnomAD v4) or upload your own file. See README for more details.", position = "right")
    ), choices = c("Global - 1000G", "Global - gnomAD", "Upload Custom"))
  })
  output$freqselect_minor = renderUI({
    selectInput("uploadfreq_minor", tags$span("Select Allele Frequency Data for the Minor Contributor", tags$span(icon(
      name = "question-circle",
    )
    ) |>
      add_prompt(message = "Select allele frequency data for the minor contributor. Options are global population datasets (1000G Phase 3 or gnomAD v4) or upload your own file. See README for more details.", position = "right")
    ), choices = c("Global - 1000G", "Global - gnomAD", "Upload Custom"))
  })
  output$report_A1 = renderUI({
    numericInput("A1_threshold", tags$span("Allele 1 Probability Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "This sets the allele 1 probability threshold for creating the GEDmatch PRO report. Any SNP with an allele 1 probability below the threshold will be removed from the report.", position = "right")
    ), value=0.99, min = 0, max = 1)
  })
  output$report_A2 = renderUI({
    numericInput("A2_threshold", tags$span("Allele 2 Probability Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "This sets the allele 2 probability threshold for creating the GEDmatch PRO report. Any SNP with an allele 2 probability below the threshold will be removed from the report.", position = "right")
    ), value=0.60, min = 0, max = 1)
  })
  output$filter_missing = renderUI({
    checkboxInput("filter_missing", tags$span("Remove SNPs If Missing Either Allele?", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "This will remove a SNP if either of its alleles are missing. If not checked, a SNP will be removed if allele 1 is missing or will report the SNP as homozygous for allele 1 if allele 2 is missing.", position = "right")
    ))
  })
  output$metrics_A1min = renderUI({
    numericInput("A1_threshmin_metrics", tags$span("Minimum Allele 1 Probability Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "When calculating metrics, a range of allele 1 probability thresholds can be used to calculate the metrics at each combination of allele 1 and allele 2 probability thresholds. This sets the minimum allele 1 probability threshold. The threshold increases in increments of 0.01.", position = "right")
    ), value=0, min = 0, max = 1)
  })
  output$metrics_A1max = renderUI({
    numericInput("A1_threshmax_metrics", tags$span("Maximum Allele 1 Probability Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "When calculating metrics, a range of allele 1 probability thresholds can be used to calculate the metrics at each combination of allele 1 and allele 2 probability thresholds. This sets the maximum allele 1 probability threshold. The threshold increases in increments of 0.01.", position = "right")
    ), value=1, min = 0, max = 1)
  })
  output$metrics_A2min = renderUI({
    numericInput("A2_threshmin_metrics", tags$span("Minimum Allele 2 Probability Threshold",  tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "When calculating metrics, a range of allele 2 probability thresholds can be used to calculate the metrics at each combination of allele 1 and allele 2 probability thresholds. This sets the minimum allele 2 probability threshold. The threshold increases in increments of 0.01.", position = "right")
    ), value=0, min = 0, max = 1)
  })
  output$metrics_A2max = renderUI({
    numericInput("A2_threshmax_metrics", tags$span("Maximum Allele 2 Probability Threshold", tags$span(
      icon(
        name = "question-circle",
      )
    ) |>
      add_prompt(message = "When calculating metrics, a range of allele 2 probability thresholds can be used to calculate the metrics at each combination of allele 1 and allele 2 probability thresholds. This sets the maximum allele 2 probability threshold. The threshold increases in increments of 0.01.", position = "right")
    ), value=1, min = 0, max = 1)
  })
  volumes = getVolumes()
  ## sample file
  shinyFileChoose(input, "sample_GetFile", roots=volumes, session=session)
  samplefile = reactive({parseFilePaths(volumes, input$sample_GetFile)})
  observe({
    if(!is.null(samplefile)){
      output$sample_file = renderText({if(input$Submit==0){as.character(samplefile()$datapath)} else {return()}})
    }
  })

  ## freq files
  shinyFileChoose(input, "freq_GetFile", roots=volumes, session=session)
  freq = reactive({parseFilePaths(volumes, input$freq_GetFile)})
  if (!is.null(freq)) {
    observe({
      output$freq_file = renderText({if(input$Submit==0){as.character(freq()$datapath)} else {return()}})
    })
  }

  shinyFileChoose(input, "freq_GetFile_major", roots=volumes, session=session)
  freq_major = reactive({parseFilePaths(volumes, input$freq_GetFile_major)})
  if (!is.null(freq_major)) {
    observe({
      output$freq_file_major = renderText({if(input$Submit==0){as.character(freq_major()$datapath)} else {return()}})
    })
  }

  shinyFileChoose(input, "freq_GetFile_minor", roots=volumes, session=session)
  freq_minor = reactive({parseFilePaths(volumes, input$freq_GetFile_minor)})
  if (!is.null(freq_minor)) {
    observe({
      output$freq_file_minor = renderText({if(input$Submit==0){as.character(freq_minor()$datapath)} else {return()}})
    })
  }

  ## refs
  shinyDirChoose(input, "ref_GetFile", roots=volumes(), session=session)
  refs = reactive({parseDirPath(volumes, input$ref_GetFile)})
  if (!is.null(refs)) {
    observe({
      output$refs_file = renderText({if(input$Submit==0){as.character(refs())} else {return()}})
    })
  }

  shinyDirChoose(input, "kin_prefix", roots=volumes(), session=session)
  kin_inpath = reactive({parseDirPath(volumes, input$kin_prefix)})
  observe({
    if(!is.null(kin_inpath)){
      output$kin_inpath = renderText({if(input$Submit==0){as.character(kin_inpath())} else {return()}})
    }
  })


## Input the sample manifest and run the workflow on each line (sample)
  observeEvent(input$Submit, {
    sample_list = read.table(samplefile()$datapath, sep="\t", header=T)
    date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
    create_config(date, input$twofreqs, ifelse(!isTruthy(freq()$datapath),  input$uploadfreq, freq()$datapath), ifelse(!isTruthy(freq_major()$datapath), input$uploadfreq_major, freq_major()$datapath), ifelse(!isTruthy(freq_minor()$datapath), input$uploadfreq_minor, freq_minor()$datapath), refs(), samplefile()$datapath, input$output, input$run_mixdeconv, input$uncond, input$ref_selector, input$method, input$sets, kin_inpath(), input$dynamicAT, input$staticAT, input$minimum_snps, input$A1_threshold, input$A2_threshold, input$A1_threshmin_metrics, input$A1_threshmax_metrics, input$A2_threshmin_metrics, input$A2_threshmax_metrics, input$major_selector, input$minor_selector, input$filter_missing, input$skip_ancestry)
    if (isTruthy(refs())) {
      withProgress(message = "Loading References", value = 0, {
        if (!file.exists(glue("{refs()}/EFM_references.csv"))) {
          refData = euroformix::sample_tableToList(data.frame(processing_ref_sample_reports(refs())))
        } else {
          refData = euroformix::sample_tableToList(euroformix::tableReader(glue("{refs()}/EFM_references.csv")))
        }
      })
    } else if(input$method == "Calculate Metrics" | isTruthy(input$cond)) {
      stop("No references provided but selected conditioned analyses or calculating metrics. Please re-run!")
    } else {
      refData = NULL
    }
    withProgress(message = "Running Samples", value = 0, {
      n = nrow(sample_list)
      for (row in 1:n) {
        id = sample_list[row, 1]
        replicate_id = ifelse(is.na(sample_list[row, 2]), "", sample_list[row, 2])
        incProgress((row-1)/n, detail = glue("On Sample {row} of {n}"))
          withCallingHandlers({
            shinyjs::html(id = "text", html = "")
            run_workflow(date, id, replicate_id, input$twofreqs, ifelse(!isTruthy(freq()$datapath), input$uploadfreq, freq()$datapath),ifelse(!isTruthy(freq_major()$datapath), input$uploadfreq_major, freq_major()$datapath), ifelse(!isTruthy(freq_minor()$datapath), input$uploadfreq_minor, freq_minor()$datapath), refData, refs(), samplefile()$datapath, input$output, input$run_mixdeconv, input$uncond, input$ref_selector, input$method, input$sets, kin_inpath(), input$dynamicAT, input$staticAT, input$minimum_snps, input$A1_threshold, input$A2_threshold, input$A1_threshmin_metrics, input$A1_threshmax_metrics, input$A2_threshmin_metrics, input$A2_threshmax_metrics, input$major_selector, input$minor_selector, input$min_cont_prob, input$keep_bins, input$filter_missing, input$skip_ancestry, input$ancestry_snps, input$pcagroups)
          },
          message = function(m) {
            shinyjs::html(id = "text", html = m$message, add = TRUE)
          })
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
}
