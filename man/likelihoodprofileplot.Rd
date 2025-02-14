% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/likelihoodprofileplot.R
\name{likelihoodprofileplot}
\alias{likelihoodprofileplot}
\title{Likelihood profile plot
View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.}
\usage{
likelihoodprofileplot(
  data,
  xlab = NULL,
  ylab = "Change in -log-likelihood",
  xlim = NULL,
  ylim = NULL,
  colours = c(SSAND::fq_palette("alisecolours"), SSAND::fq_palette("cols")),
  shapes = c(16, 17, 18, 15, 1, 2, 5, 0, 19, 20, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14),
  component_names = NULL,
  legend_position = "top"
)
}
\arguments{
\item{data}{Output from likelihoodprofileplot_prep(). A dataframe with columns x_vector, component and likelihood}

\item{xlab}{Label for x-axis (character). # Default is \code{expression(log(italic(R)[0]))}.}

\item{ylab}{Label for y-axis (character). Default is "Change in -log-likelihood".}

\item{xlim}{A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).}

\item{ylim}{A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).}

\item{colours}{A vector of colours used (character).}

\item{shapes}{A vector of shapes used (character).}

\item{component_names}{A vector of customised component names for legend (character).}

\item{legend_position}{Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".}
}
\value{
Likelihood profile plot
}
\description{
Likelihood profile plot
View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.
}
\examples{
\dontrun{
DDUST:
# Run DDUST model:
data <- dd_mle[[1]]$data
parameters <- dd_mle[[1]]$parameters
map <- dd_mle[[1]]$map
dd_out <- DDUST::run_DDUST(data, parameters, map, MCMC = FALSE)

# Set up parameter profile:
Rinit <- seq(11,12,by = 0.05)

# Simulate model over every value of parameter
profile <- c()
for (i in 1:length(Rinit)){
  simulation_parameters <- parameters
  for (item in names(map)){
    simulation_parameters[item] <- NULL
  }
  simulation_parameters$Rinit <- Rinit[i]
  sim <- dd_out$dd_mle$model$simulate(unlist(simulation_parameters))

  profile <- rbind(profile,
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$LL, component = 'Total'),
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$biomassLL, component = 'Biomass survey'),
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$cpueLL, component = 'Index'),
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$RecDevLL, component = 'Recruitment'),
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$penLL1, component = 'Catch penalty'),
                   data.frame(x_vector = Rinit[i],
                              likelihood = sim$penLL2, component = 'Recruits penalty'))
}
likelihoodprofileplot(profile)

Stock Synthesis:
r4ss::profile(dir = '.', # directory of 4 SS files
              oldctlfile = "control.ctl",
              newctlfile = "control.ctl",
              string = "steep",
              profilevec = c(0.4,0.5,0.6),
              exe = "C:/stocksynthesis/ss_3.30.22.exe")

profile_input <- r4ss::SSsummarize(
  r4ss::SSgetoutput(dirvec = ".",
                    keyvec = 1:3, # 1:length(profilevec)
                    getcovar = FALSE,
                    getcomp = FALSE))

data <- likelihoodprofileplot_prep_SS(profile_input, parameter="SR_BH_steep")
likelihoodprofileplot(data)
}
}
