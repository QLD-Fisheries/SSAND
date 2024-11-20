# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Investigate raw and worked-up biological data
#' Produces a document (Rmd, complies to a HTML) that shows various data summaries and investigations.
#' Plots and tables are saved to a folder called "investigate_biology" within the specified directory.
#' Sections can be toggled on and off in order to save compilation time.
#'
#' @param data A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.
#' @param dir Directory of file outputs (.Rmd, .html, and plots and tables). Default is working directory.
#' @param render Set to TRUE to render report. Set to FALSE to only produce Rmd file.
#' @param year_var The name of the year variable used. Default is 'year'
#' @param month_var  The name of the month variable used. Default is 'month'
#' @param length_var  The name of the length variable used. Default is 'length'
#' @param length_label The label for the length variable. Default is "Fork length (cm)"
#' @param age_var  The name of the age variable used. Default is 'age'
#' @param age_label The label for the age variable. Default is "Age group (years)"
#' @param region_var  The name of the region variable used. Default is 'region'
#' @param sex_var  The name of the sex variable used. Default is 'sex'
#' @param method_var  The name of the fishing method variable used. Default is 'method'
#' @param sector_var  The name of the sector variable used. Default is 'sector'
#' @param MLS Optional. The value of the minimum legal size. Numeric
#' @param show_length Logical. Set to TRUE to show length plots. Default is TRUE.
#' @param show_age Logical. Set to TRUE to show length plots. Default is TRUE.
#' @param show_age_at_length Logical. Set to TRUE to show length plots. Default is TRUE.
#' @param male Label given to male fish in the sex_var column (default is "Male")
#' @param female Label given to female fish in the sex_var column (default is "Female")
#' @param unknown_sex Label given to fish of unknown sex in the sex_var column (default is = "Unknown")
#'
#' @return If render==FALSE, returns investigate.Rmd at the specified directory. If render==TRUE, this Rmd is compiled and investigation.html and a folder called "investigate", containing all produced plots and tables, are generated in the specified directory.
#' @export
#'
#'@examples
#' \dontrun{
#' investigate_biology(biological_data, render=TRUE)
#' }
investigate_biology <- function(data,
                                year_var = 'year',
                                month_var = 'month',
                                length_var = 'length',
                                length_label = "Fork length (cm)",
                                age_var = 'age',
                                age_label = "Age group (years)",
                                region_var = 'region',
                                sex_var = 'sex',
                                method_var = 'method',
                                sector_var = 'sector',
                                MLS = NULL,
                                show_length = TRUE,
                                show_age = TRUE,
                                show_age_at_length = TRUE,
                                male = "Male",
                                female = "Female",
                                unknown_sex = "Unknown",
                                dir = getwd(),
                                render = FALSE) {


  # ____________ ----
  # DATA AND FUNCTION SET UP----
  add_header <- function(string, rmd_file_name = "investigation_biology.Rmd") {
    write(paste0("\n## ",string," {.tabset .tabset-fade .tabset-pills}"), rmd_file_name, append=TRUE)
  }

  add_plot <- function (string, name="", width=8, height=4,rmd_file_name = "investigation_biology.Rmd") {
    new_chunk <- c("\n",
                   paste0("```{r ",name, ", echo=TRUE, fig.path='",dir,"/investigate_biology/', fig.keep='all', fig.width=",width,", fig.height=",height,"}"),
                   string,
                   "```")
    write(new_chunk, rmd_file_name, append=TRUE)
  }

  add_text <- function(string, rmd_file_name = "investigation_biology.Rmd") {
    write(paste0("\n",string), rmd_file_name, append=TRUE)
  }

  add_table <- function(string, caption = "", name="", rmd_file_name = "investigation_biology.Rmd") {
    new_table <- c("\n",
                   paste0("```{r ",name,", echo=TRUE, results='asis'}"),
                   paste0("DT::datatable(",string,", caption = '",caption,"', rownames=FALSE)"),
                   "```")

    if (!dir.exists(paste0(dir,"/investigate_biology/"))) {dir.create(paste0(dir,"/investigate_biology/"))}

    utils::write.csv(eval(parse(text=string)), file=paste0(dir,"/investigate_biology/",name,".csv"))
    write(new_table, rmd_file_name, append=TRUE)
  }


  # ____________ ----
  # YAML         ----
  rmd_file_name <- "investigation_biology.Rmd"

  yaml_header <- "---
title: 'Biological data at a glance'
output:
  html_document:
---"

  write(yaml_header, rmd_file_name, append=FALSE)
  write("```{r, echo=TRUE, results='asis'}\nlibrary(SSAND)\n```\n", rmd_file_name, append=TRUE)
  write("# {.tabset}", rmd_file_name, append=TRUE)



  # ___________----
  # LENGTH----
  if (show_length) {
    add_header("Length")

    # Summary
    add_text("### Summary {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_boxplot(data, x_var = '",year_var,"', xlab='Year', y_var = '",length_var,"', ylab = '",length_label,"')"), width=8, height=4, name="length_boxplot")
    add_plot(paste0("rawbiological_boxplot(data, x_var = '",year_var,"', xlab='Year', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = '",region_var,"')"), width=8, height=8,name="length_box_region1")
    add_plot(paste0("rawbiological_boxplot(data, x_var = '",year_var,"', xlab='Year', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = '",sex_var,"')"), width=8, height=4 ,name="length_box_sex1")
    add_plot(paste0("rawbiological_boxplot(data, x_var = '",year_var,"', xlab='Year', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = '",sector_var,"')"), width=8, height=4 ,name="length_box_sector1")

    # By year
    add_text("### By year {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = '",year_var,"')"), width=8, height=20 ,name="length_freq_year")

    # By region
    add_text("### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'n', ylab = 'Number of samples', axis_angle = 90)"), width=8, height=4, name="length_nsamp_region")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",region_var,"')"), width=8, height=4, name="length_nsamp_region_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",region_var,"')"), width=8, height=4, name="length_psamp_region_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",region_var,"', MLS_x = ",MLS,")"), width=8, height=4, name="length_freq_region")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",region_var,"', dodge=TRUE, MLS_x = ",MLS,")"), width=8, height=4, name="length_nsamp_region_dodge")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = '",region_var,"')"), width=8, height=8, name="length_nsamp_region_facet")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = c('",year_var,"','",region_var,"'))"), height=24,width=12, name="length_freq_region_year")

    # By sector
    add_text("### By sector {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",sector_var,"', xlab='Region', y_var = 'n', ylab = 'Number of samples')"), width=8, height=4, name="length_nsamp_sector")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="length_nsamp_sector_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="length_psamp_sector_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sector_var,"', MLS_x = ",MLS,")"), width=8, height=4, name="length_freq_sector")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sector_var,"', dodge=TRUE, MLS_x = ",MLS,")"), width=8, height=4, name="length_nsamp_sector_dodge")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = '",sector_var,"')"), width=8, height=4, name="length_freq_sector_facet")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = c('",year_var,"','",sector_var,"'))"), height=24,width=12, name="length_freq_sector_year")

    # By region and sector
    add_text("### By region and sector{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sector_var,"', axis_angle = 90)"), width=8, height=4, name="length_nsamp_region_sector")

    # By fishing method
    add_text("### By fishing method {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'n', ylab = 'Number of samples', fill_var = '",method_var,"', axis_angle = 90)"), width=8, height=4, name="length_nsamp_region_fishingmethod")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",method_var,"', axis_angle = 90)"), width=8, height=4, name="length_psamp_region_fishingmethod")

    # By sex
    add_text("### By sex {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",sex_var,"', xlab='Sex', y_var = 'n', ylab = 'Number of samples')"), width=8, height=4, name="length_nsamp_sex")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"')"), width=8, height=4, name="length_nsamp_sex_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sex_var,"')"), width=8, height=4, name="length_psamp_sex_year")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', MLS_x = ",MLS,")"), width=8, height=4, name="length_freq_sex")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', dodge=TRUE, MLS_x = ",MLS,")"), width=8, height=4, name="length_freq_sex_year_dodge")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = '",sex_var,"')"), width=8, height=4, name="length_freq_sex_facet")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = c('",year_var,"','",sex_var,"'))"), height=24,width=12, name="length_freq_sex_year_grid")

    add_plot(paste0("rawbiological_barplot(data, x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', facet_var='",year_var,"', dodge=TRUE, ncol=2)"), height=24,width=12, name="length_freq_sex_year")
    add_plot(paste0("rawbiological_barplot(data |> dplyr::filter(",sex_var,"=='",male,"'), x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', facet_var='",year_var,"', dodge=TRUE, ncol=2, colours='#9E0E2D')"), height=24,width=12, name="length_freq_sex_year_male")
    add_plot(paste0("rawbiological_barplot(data |> dplyr::filter(",sex_var,"=='",female,"'), x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', facet_var='",year_var,"', dodge=TRUE, ncol=2, colours='#FFC000')"), height=24,width=12, name="length_freq_sex_year_female")
    add_plot(paste0("rawbiological_barplot(data |> dplyr::filter(",sex_var,"=='",unknown_sex,"'), x_var = '",length_var,"', xlab='",length_label,"', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"', facet_var='",year_var,"', dodge=TRUE, ncol=2, colours='#70AD47')"), height=24,width=12, name="length_freq_sex_year_unknown")

    # By sex and region
    add_text("### By sex and region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sex_var,"')"), height=4,width=8, name="length_sex_region_p")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",region_var,"', xlab='Region', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"')"), height=4,width=8, name="length_sex_region_n")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', facet_var = '",region_var,"', fill_var = '",sex_var,"')"), height=8,width=8, name="length_sex_region_n_facet")

    # By sex and sector
    add_text("### By sex and sector {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",sector_var,"', xlab='Sector', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sex_var,"')"), height=4,width=8, name="length_psamp_sector_sex")

    # By month
    add_text("### By month {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",month_var,"', xlab='Month', y_var = 'n', ylab = 'Number of samples', fill_var = '",region_var,"')"), width=8, height=4, name="length_nsamp_month_region")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",month_var,"', xlab='Month', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",region_var,"')"), width=8, height=4, name="length_psamp_month_region")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",month_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', facet_var = '",region_var,"')"), width=8, height=4, name="length_nsamp_month_region_facet")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",month_var,"', xlab='Month', y_var = 'n', ylab = 'Number of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="length_nsamp_month_sector")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",month_var,"', xlab='Month', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="length_psamp_month_sector")

    # Heat plots
    add_text("### Heat plots {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_heatplot(data, x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",region_var,"')"), name = "length_heat_region", height=20)
    add_plot(paste0("rawbiological_heatplot(data, x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",sector_var,"')"), name = "length_heat_sector", height=20)
    add_plot(paste0("rawbiological_heatplot(data, x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",sex_var,"')"), name = "length_heat_sex", height=20)
    add_plot(paste0("rawbiological_heatplot(data, x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",method_var,"')"), name = "length_heat_method", height=20)
  }



  # ____________ ----
  # AGE ----
  if (show_age) {
    add_header("Age")

    add_text("### Summary {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = '",age_var,"', ylab = '",age_label,"')"), width=8, height=4, name="age_box")

    # By year
    add_text("### By age {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data, x_var = '",age_var,"', xlab='",age_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = '",year_var,"')"), width=8, height=12, name="age_freq_year")

    # By region
    add_text("### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",region_var,"')"), width=8, height=4, name="age_nsamp_year_region")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",region_var,"')"), width=8, height=4, name="age_psamp_year_region")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = c('",year_var,"','",region_var,"'))"), width=12, height=24, name="age_freq_year_region")

    # By sector
    add_text("### By sector {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="age_nsamp_year_sector")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sector_var,"')"), width=8, height=4, name="age_psamp_year_sector")

    # By fishing method
    add_text("### By fishing method {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",method_var,"')"), width=8, height=4, name="age_nsamp_year_fishingmethod")

    # By sex
    add_text("### By sex {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'n', ylab = 'Number of samples', fill_var = '",sex_var,"')"),width=8, height=4, name="age_nsamp_year_sex",)
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",year_var,"', xlab='Year', y_var = 'p', ylab = 'Proportion of samples', fill_var = '",sex_var,"')"), width=8, height=4, name="age_psamp_year_sex")
    add_plot(paste0("rawbiological_barplot(data = data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = 'n', ylab = 'Number of samples', facet_var = c('",year_var,"','",sex_var,"'))"), width=12, height=24, name="age_freq_year_sex")

    # Heat plots
    add_text("### Heat plots {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_heatplot(data |> dplyr::filter(!is.na('",age_var,"')), x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",region_var,"')"), name="age_heat_region", height=20)
    add_plot(paste0("rawbiological_heatplot(data |> dplyr::filter(!is.na('",age_var,"')), x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",sector_var,"')"), name="age_heat_sector", height=20)
    add_plot(paste0("rawbiological_heatplot(data |> dplyr::filter(!is.na('",age_var,"')), x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",sex_var,"')"), name="age_heat_sex", height=20)
    add_plot(paste0("rawbiological_heatplot(data |> dplyr::filter(!is.na('",age_var,"')), x_var = '",month_var,"', xlab='Month', y_var = '",year_var,"', ylab = 'Year', facet_var = '",method_var,"')"), name="age_heat_method", height=20)
  }


  # ____________ ----
  # AGE AT LENGTH ----
  if (show_age_at_length) {
    add_header("Age and length")

    add_text("### Age-at-length {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",length_var,")), x_var = '",length_var,"', xlab='",length_label,"', y_var = '",age_var,"', ylab = '",age_label,"', MLS_x=",MLS,")"), width=8, height=4, name="aal_box")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",length_var,")), x_var = '",length_var,"', xlab='",length_label,"', y_var = '",age_var,"', ylab = '",age_label,"', facet_var = '",region_var,"', MLS_x=",MLS,")"), width=8, height=8, name="aal_box_region")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",length_var,")), x_var = '",length_var,"', xlab='",length_label,"', y_var = '",age_var,"', ylab = '",age_label,"', facet_var = '",sex_var,"', MLS_x=",MLS,")"), width=8, height=4, name="aal_box_sex")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",length_var,")), x_var = '",length_var,"', xlab='",length_label,"', y_var = '",age_var,"', ylab = '",age_label,"', facet_var = c('",region_var,"','",sex_var,"'), MLS_x=",MLS,")"), width=12, height=24, name="aal_box_sex_region")

    add_text("### Length-at-age {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', MLS_y=",MLS,")"), width=8, height=4, name="laa_box")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = '",region_var,"', MLS_y=",MLS,")"), width=8, height=8, name="laa_box_region")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = '",sex_var,"', MLS_y=",MLS,")"), width=8, height=4, name="laa_box_sex")
    add_plot(paste0("rawbiological_boxplot(data |> dplyr::filter(!is.na(",age_var,")), x_var = '",age_var,"', xlab='",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var = c('",region_var,"','",sex_var,"'), MLS_y=",MLS,")"), width=12, height=24, name="laa_box_sex_region")

    add_text("### von Bertalanffy curves {.tabset .tabset-fade .tabset-pills}")
    add_text("The following plots can show von Bertalanffy curves fitted to the data if show_vonBert is set to TRUE. This feature is in development and may not always work. This is not an output of a stock assessment model and is to be used as an indication of trend only. ")
    add_plot(paste0("# rawbiological_scatterplot(data, x_var = '",age_var,"', xlab = '",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var='",region_var,"', show_vonBert=FALSE) \n# rawbiological_scatterplot(data, x_var = '",age_var,"', xlab = '",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var='",region_var,"', show_vonBert=FALSE, show_vonBert_only=FALSE, legend_position='top')"), width=8, height=16, name="ageatlength_vonBert_region")
    # add_plot(paste0("rawbiological_scatterplot(data, x_var = '",age_var,"', xlab = '",age_label,"', y_var = '",length_var,"', ylab = '",length_label,"', facet_var='",region_var,"', show_vonBert=FALSE, show_vonBert_only=FALSE, legend_position='top')"), width=12, height=6, name="ageatlength_vonBert_region_combined")
  }

  # ____________ ----
  # MAP ----
  # add_header("Map")
  # add_plot(paste0("map(show_monitoring_regions = TRUE, show_latbands = TRUE, latbands_level = 'top')"), width=14,height=14, name="map")


  # ____________ ----
  # COMPILE ----
  if (render) {
    rmarkdown::render(input = rmd_file_name,
                      output_file = "investigation_biology.html",
                      output_dir = dir,
                      clean = TRUE)

    system2("open",paste0(dir,"/investigation_biology.html"))
  }
}


