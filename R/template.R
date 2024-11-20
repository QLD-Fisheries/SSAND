# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Copy an example report file into your working directory (or other directory of choosing)
#' Template can be customised to the model, presence of MCMC and data types, and jurisdiction style guides
#'
#' @param dir Directory where template will be saved. Defaults to Desktop.
#' @param species Name of species, to populate template
#' @param filename Name of species, as one word with underscores, to populate file name and Makefile
#' @param stock Name of stock, to populate template
#' @param model "SS" or "DD" for Stock Synthesis or DDUST respectively. Will populate the relevant content
#' @param MCMC TRUE or FALSE if MCMC was used. Will populate the relevant content
#' @param scenarios A vector of scenario numbers used. Will populate the appendix.
#' @param length TRUE or FALSE if length compositions were used. Will populate the relevant content
#' @param age TRUE or FALSE if age compositions were used. Will populate the relevant content
#' @param caal TRUE or FALSE if conditional age-at-length compositions were used. Will populate the relevant content
#' @param discard TRUE or FALSE if discard inputs were used. Will populate the relevant content
#' @param class_dir Directory of folder containing class file, macros and images.
#'
#' @return A folder containing an Rtex file containing a report template, and the associated files, copied to your directory of choice
#' @export
#'
#' @examples
#' \dontrun{
#' template(dir=paste0(getwd(),"/ssandfish/"),
#'          species = "ssandfish", stock = "east coast", model = "SS",
#'          MCMC = TRUE, scenarios = 1:2,
#'          length = TRUE, age = FALSE, caal = TRUE, discard = FALSE)
#' }
template <- function(dir = NULL,
                     species = "ssand fish",
                     filename = "ssand_fish",
                     stock = "east coast",
                     model = "SS",
                     MCMC = TRUE,
                     scenarios = 1:2,
                     length = TRUE,
                     age = FALSE,
                     caal = TRUE,
                     discard = FALSE,
                     class_dir = NULL
) {

  if (!length && !age && !caal && !discard) {stop("Please set at least one of the following to TRUE: length, age, caal, discard. You can always delete these sections of the report later.")}

  # ____________________________________
  # Customise the template
  # ____________________________________

  # Find and replace species and stock name
  file_path <- paste0(system.file("template/", package = "SSAND"),'/template.Rtex') # Specify the file path
  file_content <- readLines(file_path) # Read the content of the file
  file_content <- gsub("SPECIES", species, file_content) # Replace "SPECIES" with specified species
  file_content <- gsub("STOCK", stock, file_content) # Replace "STOCK" with specified stock

  # Remove unneeded blocks of code based on customization
  remove_content_blocks <- function(content, start, end) {
    remove_block <- FALSE # Create a flag to keep track of whether we're within a block to remove
    content_cleaned <- character() # Initialize an empty vector to store cleaned lines
    # Loop through each line in the vector
    for (line in content) {
      if (line == start) {
        remove_block <- TRUE  # Start removing from start
      } else if (line == end) {
        remove_block <- FALSE  # Stop removing at end
      } else if (!remove_block) {
        content_cleaned <- c(content_cleaned, line)  # Append line if not in block to remove
      }
    }
    content <- content_cleaned
    return(content)
  }

  if (model=="DD") {file_content <- remove_content_blocks(file_content, "%begin_SS","%end_SS")}
  if (model=="SS") {file_content <- remove_content_blocks(file_content, "%begin_DD","%end_DD")}
  if (!MCMC) {file_content <- remove_content_blocks(file_content, "%begin_MCMC","%end_MCMC")}
  if (MCMC) {file_content <- remove_content_blocks(file_content, "%begin_MLE","%end_MLE")}

  if (!length) {file_content <- remove_content_blocks(file_content, "%begin_length","%begin_length")}
  if (!age) {file_content <- remove_content_blocks(file_content, "%begin_age","%end_age")}
  if (!caal) {file_content <- remove_content_blocks(file_content, "%begin_caal","%end_caal")}
  if (!discard) {file_content <- remove_content_blocks(file_content, "%begin_discard","%end_discard")}


  # Replicate scenarios appendix for appropriate number of scenarios
  begin <- which(file_content=="%begin_scenarios")
  end <- which(file_content=="%end_scenarios")

  scenario_section <- file_content[(begin+1):(end-1)]
  modified_scenario_section <- c()
  for (i in scenarios) {
    tmp <- gsub("XXX", i, scenario_section)
    modified_scenario_section <- c(modified_scenario_section,tmp)
  }
  file_content <- c(file_content[1:(begin-1)], modified_scenario_section, file_content[(end+1):length(file_content)])

  # Remove any lingering %begin or %end
  file_content <- file_content[!grepl(paste0("^", "%begin_"), file_content)]
  file_content <- file_content[!grepl(paste0("^", "%end_"), file_content)]

  # Write template as fish filename
  writeLines(file_content, paste0(system.file("template/", package = "SSAND"),'/',filename,'.Rtex')) # Write the modified content back to the file

  # ____________________________________
  # Copy files to desired destination
  # ____________________________________
  # Default to desktop
  if (missing(dir)) {
    if (!dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Desktop/template"))) {
      dir.create(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Desktop/"))
    }
    dir <- paste0("C:/Users/",Sys.getenv("USERNAME"),"/Desktop/template/")
  }

  # Create folders for template if they don't already exist
  if (!dir.exists(dir)) {dir.create(dir)}
  if (!dir.exists(paste0(dir,"/bib"))) {dir.create(paste0(dir,"/bib"))}
  if (!dir.exists(paste0(dir,"/class"))) {dir.create(paste0(dir,"/class"))}

  # Function to copy files from SSAND package to desired directory
  copy <- function(filename) {
    file.copy(from = paste0(system.file("template/", package = "SSAND"),'/',filename), to = paste0(dir,'/',filename))
  }

  # Copy files
  copy(paste0(filename,".Rtex"))
  copy("bib/references.bib")
  copy("bib/biber.conf")
  copy("class/SATclass.cls")
  copy("class/SATmacros.sty")
  copy("class/SATpresentation.cls")
  copy("class/report_cover.png")
  copy("Makefile")

  magick::image_write(magick::image_read(system.file("fig/report_cover.png", package = "SSAND")),
                      file.path(dir, "class/report_cover.png"))

  # If specified, overwrite class files with custom files
  if (!missing(class_dir)) {
    file.copy(from = paste0(class_dir,"SATclass.cls"),
              to = paste0(dir,"class/SATclass.cls"),
              overwrite = TRUE)

    file.copy(from = paste0(class_dir,"SATmacros.sty"),
              to = paste0(dir,"class/SATmacros.sty"),
              overwrite = TRUE)

    magick::image_write(magick::image_read(system.file("fig/qg_logo.png", package = "SSAND")),
                        file.path(dir, "class/qg_logo.png"))

    magick::image_write(magick::image_read(system.file("fig/copyright_logo.jpg", package = "SSAND")),
                        file.path(dir, "class/copyright_logo.jpg"))

    magick::image_write(magick::image_read(system.file("fig/interpreter.jpg", package = "SSAND")),
                        file.path(dir, "class/interpreter.jpg"))
  }

  # ____________________________________
  # Edit Makefile to match fish filename
  # ____________________________________
  Makefile_path <- paste0(dir,'Makefile')
  Makefile_content <- gsub("template", filename, readLines(Makefile_path))
  writeLines(Makefile_content, Makefile_path) # Write the modified content back to the file
}


#' Generate a detailed model output appendix
#' This code produced a text file that contains code that can be pasted directly into the "Detailed model outputs" section of a stock assessment report
#' Code can be customised for the number of scenarios, model type and output type.
#'
#' @param filename Directory and filename where text file will be saved. Defaults to working directory
#' @param model "SS" or "DD" for Stock Synthesis or DDUST respectively. Will populate the relevant content
#' @param MCMC TRUE or FALSE if MCMC was used. Will populate the relevant content
#' @param scenarios A vector of scenario numbers used. Will populate the appendix.
#' @param length TRUE or FALSE if length compositions were used. Will populate the relevant content
#' @param age TRUE or FALSE if age compositions were used. Will populate the relevant content
#' @param caal TRUE or FALSE if conditional age-at-length compositions were used. Will populate the relevant content
#' @param discard TRUE or FALSE if discard inputs were used. Will populate the relevant content
#'
#' @return A text file that contains code that can be pasted directly into the "Detailed model outputs" section of a stock assessment report
#' @export
#'
#' @examples
#' \dontrun{
#' add_scenarios()
#' add_scenarios(model="SS", MCMC=FALSE, scenarios=1:12, discard=TRUE)
#' }
add_scenarios <- function(filename = paste0(getwd(),"/scenarios_appendix.txt"),
                          model = "SS",
                          MCMC = TRUE,
                          scenarios = 1:2,
                          length = TRUE,
                          age = FALSE,
                          caal = TRUE,
                          discard = FALSE) {

  write("\\chapter{Detailed model outputs} \\label{sec:appdetailedoutputs}", filename, append=FALSE)
  write("\\thispagestyle{fancy}", filename, append=TRUE)
  write("\n", filename, append=TRUE)


  for (scenario in scenarios) {
    write("\\section{Scenario XXX}", filename, append=TRUE)
    write("\n", filename, append=TRUE)

    if (model=="SS") {
      write("\\rowcolors{2}{white}{light-gray}", filename, append=TRUE)
      write("<<table_mleXXX, results='asis', echo=FALSE>>=", filename, append=TRUE)
      write("parameters <- extract_SS_parameters(ss_mle)[2:4,]", filename, append=TRUE)
      write("data <- parametertable_prep_SS(ss_mle=ss_mle, parameters=parameters, scenario=XXX)", filename, append=TRUE)
      write("parametertable(data, label='tab:paramXXX_mle')", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("\\rowcolors{2}{white}{light-gray}", filename, append=TRUE)
        write("<<table_mcmcXXX, results='asis', echo=FALSE>>=", filename, append=TRUE)
        write("parameters <- extract_SS_parameters(ss_mle)[2:4,]", filename, append=TRUE)
        write("data <- parametertable_prep_SS(ss_mcmc=ss_mcmc, parameters=parameters, scenario=XXX)", filename, append=TRUE)
        write("parametertable(data, label='tab:paramXXX_mcmc')", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }


      write("<<cpue_fitsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Model predictions (blue line) to standardised catch rates for SPECIES in the STOCK, based on maximum likelihood estimation---grey line and error bars represent the model input and associated uncertainty'>>=", filename, append=TRUE)
      write("data <- cpueplot_prep_SS(ss_mle)", filename, append=TRUE)
      write("cpueplot(data, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)



      if (MCMC) {
        write("<<recdevsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Recruitment deviations---whiskers represent 95% credible intervals, boxes represent 50% credible intervals, horizontal bars represent medians, and the points represent outliers'>>=", filename, append=TRUE)
        write("data <- recdevplot_prep_SS(ss_mle,ss_mcmc)", filename, append=TRUE)
        write("recdevplot(data, scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)

      } else {
        write("<<recdevsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Recruitment deviations'>>=", filename, append=TRUE)
        write("data <- recdevplot_prep_SS(ss_mle)", filename, append=TRUE)
        write("recdevplot(data, scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      if (length) {
        write("<<length_outputsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=8,out.width='0.85textwidth',fig.align='center',fig.cap='Weight--length relationship for SPECIES in the STOCK'>>=", filename, append=TRUE)
        write("data <- lengthplot_prep_SS(ss_mle, scenarios=XXX)", filename, append=TRUE)
        write("lengthplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      if (age) {
        write("<<age_outputsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=8,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Fits to age structures, based on maximum likelihood estimation---grey bars represent input data and black line and points represent model fits'>>=", filename, append=TRUE)
        write("data <- caal_agefitplot_prep_SS(ss_mle, sex_code=1, scenarios=XXX)", filename, append=TRUE)
        write("caal_agefitplot(data,show_fits=FALSE, scenario=XXX)", filename, append=TRUE)
        write("# data <- ageplot_prep_SS(ss_mle)", filename, append=TRUE)
        write("# ageplot(data, show_fits=FALSE)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      if (caal) {
        write("<<caal_outputsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=8,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Pearson residuals for age-at-length compositions, based on maximum likelihood estimation---circle size represents the magnitude of the Pearson residual'>>=", filename, append=TRUE)
        write("data <- conditionalageatlengthplot_prep_SS(ss_mle,sex_code=1, scenarios=XXX)", filename, append=TRUE)
        write("conditionalageatlengthplot(data, show_fits=FALSE)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      write("<<biomass_mleXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Predicted spawning biomass trajectory relative to unfished for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- biomassplot_prep_SS(ss_mle)", filename, append=TRUE)
      write("biomassplot(data, mcmc_style = 'banded', show_median = c('annual_biomass','trajectory'), scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("<<biomass_mcmcXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Predicted spawning biomass trajectory relative to unfished for SPECIES in the STOCK, based on MCMC'>>=", filename, append=TRUE)
        write("data <- biomassplot_prep_SS(ss_mle, ss_mcmc)", filename, append=TRUE)
        write("biomassplot(data, mcmc_style = 'banded', show_median = c('annual_biomass','trajectory'), scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      write("<<phaseXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=8,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Stock status indicator trajectory for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- phaseplot_prep_SS(ss_mle)", filename, append=TRUE)
      write("phaseplot(data, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      write("<<yieldXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Equilibrium dead catch curve for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- yieldplot_prep_SS(ss_mle)", filename, append=TRUE)
      write("yieldplot(data, show_msy_line=TRUE, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("<<pdfXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=10,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Posterior density of MCMC iterations. \"Median\" line shows median parameter value for MCMC iterations  \"Optimised\" shows the parameter value found from maximum likelihood estimates.'>>=", filename, append=TRUE)
        write("parameters <- extract_SS_parameters(ss_mcmc)[c(2:10,449),]", filename, append=TRUE)
        write("data <- mcmc_posteriordensityplot_prep_SS(ss_mle, ss_mcmc, scenario = XXX, parameters)", filename, append=TRUE)
        write("mcmc_posteriordensityplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)

        write("<<traceXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=10,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Trace plot of MCMC iterations---\"Optimised\" shows the parameter value found from maximum likelihood estimates'>>=", filename, append=TRUE)
        write("parameters <- extract_SS_parameters(ss_mcmc)[c(2:9),]", filename, append=TRUE)
        write("data <- mcmc_posteriordensityplot_prep_SS(ss_mle,", filename, append=TRUE)
        write("                                          ss_mcmc,", filename, append=TRUE)
        write("                                          scenario = XXX,", filename, append=TRUE)
        write("                                          parameters,", filename, append=TRUE)
        write("                                          show_objective_function=TRUE)", filename, append=TRUE)
        write("mcmc_traceplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)

        write("<<mcmc_correlation_XXX, echo=FALSE, fig.pos='H', fig.width=12,fig.height=12,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Correlation plot of MCMC iterations'>>=", filename, append=TRUE)
        write("parameters <- extract_SS_parameters(ss_mcmc)[c(2:9),]", filename, append=TRUE)
        write("data <- correlationplot_prep_SS(ss_mle, ss_mcmc, scenario = XXX, parameters = parameters)", filename, append=TRUE)
        write("correlationplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      if (!MCMC) {
        write("<<mcmc_correlation_XXX, echo=FALSE, fig.pos='H', fig.width=12,fig.height=12,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Correlation plot of MCMC iterations'>>=", filename, append=TRUE)
        write("parameters <- extract_SS_parameters(ss_mle)[c(2,3,4,26),]", filename, append=TRUE)
        write("data <- correlationplot_prep_SS(ss_mle, scenario = 1, parameters = parameters)", filename, append=TRUE)
        write("correlationplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }
    }

    if (model=="DD") {
      write("\\rowcolors{2}{white}{light-gray}", filename, append=TRUE)
      write("<<table_mleXXX, results='asis', echo=FALSE>>=", filename, append=TRUE)
      write("data <- parametertable_prep_DD(dd_mle=dd_mle, scenario=XXX)", filename, append=TRUE)
      write("parametertable(data, label='tab:paramXXX_mle')", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("\\rowcolors{2}{white}{light-gray}", filename, append=TRUE)
        write("<<table_mcmcXXX, results='asis', echo=FALSE>>=", filename, append=TRUE)
        write("# dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc", filename, append=TRUE)
        write("data <- parametertable_prep_DD(dd_mcmc=dd_mcmc, scenario=XXX)", filename, append=TRUE)
        write("parametertable(data, label='tab:paramXXX_mcmc')", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      write("<<cpue_fitsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Model predictions (blue line) to standardised catch rates for SPECIES in the STOCK, based on maximum likelihood estimation---grey line and error bars represent the model input and associated uncertainty'>>=", filename, append=TRUE)
      write("data <- cpueplot_prep_DD(dd_mle)", filename, append=TRUE)
      write("cpueplot(data, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("<<recdevsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Recruitment deviations---whiskers represent 95% credible intervals, boxes represent 50% credible intervals, horizontal bars represent medians, and the points represent outliers'>>=", filename, append=TRUE)
        write("data <- recdevplot_prep_DD(dd_mle,dd_mcmc,dd_sim)", filename, append=TRUE)
        write("recdevplot(data, scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      } else {
        write("<<recdevsXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Recruitment deviations'>>=", filename, append=TRUE)
        write("data <- recdevplot_prep_DD(dd_mle)", filename, append=TRUE)
        write("recdevplot(data, scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      write("<<biomass_mleXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Predicted spawning biomass trajectory relative to unfished for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- biomassplot_prep_DD(dd_mle)", filename, append=TRUE)
      write("biomassplot(data, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC){
        write("<<biomass_mcmcXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Predicted spawning biomass trajectory relative to unfished for SPECIES in the STOCK, based on MCMC'>>=", filename, append=TRUE)
        write("data <- biomassplot_prep_DD(dd_mle, dd_mcmc, dd_sim)", filename, append=TRUE)
        write("biomassplot(data, mcmc_style = 'banded', show_median = c('annual_biomass','trajectory'), scenarios=XXX)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      write("<<phaseXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=8,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Stock status indicator trajectory for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- phaseplot_prep_DD(dd_mle)", filename, append=TRUE)
      write("phaseplot(data, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      write("<<yieldXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=4,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Equilibrium dead catch curve for SPECIES in the STOCK, based on maximum likelihood estimation'>>=", filename, append=TRUE)
      write("data <- yieldplot_prep_DD(dd_mle)", filename, append=TRUE)
      write("yieldplot(data, show_msy_line=TRUE, scenarios=XXX)", filename, append=TRUE)
      write("@", filename, append=TRUE)
      write("\n", filename, append=TRUE)

      if (MCMC) {
        write("<<pdfXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=10,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Posterior density of MCMC iterations. \"Median\" line shows median parameter value for MCMC iterations  \"Optimised\" shows the parameter value found from maximum likelihood estimates.'>>=", filename, append=TRUE)
        write("data <- mcmc_posteriordensityplot_prep_DD(dd_mle, dd_mcmc, dd_sim, scenario = XXX)", filename, append=TRUE)
        write("mcmc_posteriordensityplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)

        write("<<traceXXX, echo=FALSE, fig.pos='H', fig.width=8,fig.height=10,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Trace plot of MCMC iterations---\"Optimised\" shows the parameter value found from maximum likelihood estimates'>>=", filename, append=TRUE)
        write("data <- mcmc_posteriordensityplot_prep_DD(dd_mle, dd_mcmc, dd_sim, scenario = XXX)", filename, append=TRUE)
        write("mcmc_traceplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)

        write("<<mcmc_correlation_XXX, echo=FALSE, fig.pos='H', fig.width=12,fig.height=12,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Correlation plot of MCMC iterations'>>=", filename, append=TRUE)
        write("# dd_sdr <- TMB::sdreport(dd_mle[[XXX]]$model)", filename, append=TRUE)
        write("# data <- correlationplot_prep_DD(dd_mle, dd_mcmc, dd_sim, scenario = XXX)", filename, append=TRUE)
        write("# correlationplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }

      if (!MCMC) {
        write("<<mcmc_correlation_XXX, echo=FALSE, fig.pos='H', fig.width=12,fig.height=12,out.width='0.85textwidth',fig.align='center',fig.cap='Scenario XXX: Correlation plot of MCMC iterations'>>=", filename, append=TRUE)
        write("# data <- correlationplot_prep_DD(dd_mle, scenario = 1)", filename, append=TRUE)
        write("# correlationplot(data)", filename, append=TRUE)
        write("@", filename, append=TRUE)
        write("\n", filename, append=TRUE)
      }
    }
    # Find and replace XXX with scenario number
    file  <- readLines(filename)
    file <- gsub(pattern = "XXX", replacement = scenario, x = file)
    writeLines(file, con=filename)
  }

  # Adjust escape characters
  file  <- readLines(filename)
  file <- gsub(pattern = "rowcolours", replacement = "\\rowcolours", x = file)
  file <- gsub(pattern = "textwidth", replacement = "\\\\\\\\textwidth", x = file)
  file <- gsub(pattern = "%", replacement = "\\\\\\\\%", x = file)
  writeLines(file, con=filename)
}
