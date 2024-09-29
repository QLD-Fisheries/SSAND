# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Generate a catalogue of all available plots
#'
#' @param dir Directory where catalogue will be saved. Defaults to working directory.
#'
#' @return A html file containing a catalogue of all available plots
#' @export
#'
#' @examples
#' \dontrun{
#' catalogue()
#' }
catalogue <- function(dir = getwd()) {

  input <- system.file("rmd/catalogue.Rmd", package = "SSAND")

  rmarkdown::render(input = input,
                    output_file = "catalogue.html",
                    output_dir = dir,
                    clean = TRUE)

  system2("open",paste0(dir,"/catalogue.html"))
  print(paste0("Catalogue file created in ",dir, "/"))
}
