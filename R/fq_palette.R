# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Complete list of palettes
#'
#' Use \code{\link{fq_palette}} to construct palettes of desired length.
#'
#' @export
fq_palettes <- list(
  DAF = c("#31B7C3", "#005572", "#0085B8", "#006A4B", "#AD3D25", "#F3753A", "#FCC17E", "#00925B"),
  # adjust_shade <- function(hex_color, factor) {
  #   rgb_vals <- grDevices::col2rgb(hex_color) / 255
  #   rgb_adjusted <- pmax(pmin(rgb_vals * factor, 1), 0)
  #   adjusted_hex <- grDevices::rgb(rgb_adjusted[1], rgb_adjusted[2], rgb_adjusted[3], maxColorValue = 1)
  #   return(adjusted_hex)
  # }
  #
  # alisecolours <- c("#FFC000","#9E0E2D","#70AD47","#7CC8FC","#ED7D31","#9D9D9D","#01917C","#915CA0","#3A619C")
  # for (i in 1:9) {alisecolours <- c(alisecolours,adjust_shade(alisecolours[i],1.25))}
  # for (i in 1:9) {alisecolours <- c(alisecolours,adjust_shade(alisecolours[i],0.75))}
  # for (i in 1:9) {alisecolours <- c(alisecolours,adjust_shade(alisecolours[i],0.5))}
  # for (i in 1:9) {alisecolours <- c(alisecolours,adjust_shade(alisecolours[i],0.35))}
  alisecolours = c("#FFC000", "#9E0E2D", "#70AD47", "#7CC8FC", "#ED7D31", "#9D9D9D", "#01917C", "#915CA0", "#3A619C",
                    "#FFF000", "#C61238", "#8CD859", "#9BFAFF", "#FF9C3D", "#C4C4C4", "#01B59B", "#B573C8", "#4979C3",
                    "#BF9000", "#770B22", "#548235", "#5D96BD", "#B25E25", "#767676", "#016D5D", "#6D4578", "#2C4975",
                    "#806000", "#4F0717", "#385724", "#3E647E", "#773F19", "#4F4F4F", "#01493E", "#492E50", "#1D314E",
                    "#594300", "#370510", "#273D19", "#2B4658", "#532C11", "#373737", "#00332B", "#332038", "#142237"),

  fishsex = c("#F4BB48","#248BB7","#6BA357"),
  biomass = c("#127B06", "#CF3B16","#000000"),
  cols = c("#88CCEE","#CC6677","#DDCC77",   # Colorblind-friendly scheme "safe" of CARTOColors
           "#117733","#332288","#AA4499",
           "#44AA99","#999933","#882255",
           "#661100","#6699CC","#888888",
           "#E58606","#5D69B1","#52BCA3",
           "#99C945","#CC61B0","#A5AA99",
           "#88CCEE","#CC6677","#DDCC77",
           "#117733","#332288","#AA4499",
           "#44AA99","#999933","#882255",
           "#661100","#6699CC","#888888",
           "#E58606","#5D69B1","#52BCA3", # The last six colors are from "vivid"
           "#99C945","#CC61B0","#A5AA99",
           "#88CCEE","#CC6677","#DDCC77",
           "#117733","#332288","#AA4499",
           "#44AA99","#999933","#882255",
           "#661100","#6699CC","#888888",
           "#E58606","#5D69B1","#52BCA3", # The last six colors are from "vivid"
           "#99C945","#CC61B0","#A5AA99")
)


#' A Fisheries Queensland palette generator
#'
#' Code adapted from karthik/wesanderson
#'
#' @param n Number of colors desired
#' @param name Name of desired palette. Choices are:
#'   \code{DAF}, \code{alisecolours},  \code{fishsex},
#'   \code{biomass}, \code{cols}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours
#' @export
#' @keywords colors
#' @examples
#' fq_palette("DAF", 4)
#' fq_palette("alisecolours")
#' fq_palette("biomass")
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- fq_palette(21, name = "DAF", type = "continuous")
#' image(volcano, col = pal)
fq_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- fq_palettes[[name]]
  # if (type == "continuous" && name == "Zissou1") {
  #   pal <- wes_palettes[["Zissou1Continuous"]]
  # }

  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
