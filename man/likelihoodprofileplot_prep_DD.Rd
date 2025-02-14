% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/likelihoodprofileplot_prep_DD.R
\name{likelihoodprofileplot_prep_DD}
\alias{likelihoodprofileplot_prep_DD}
\title{Prepare DDUST data for likelihoodprofileplot()
View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.}
\usage{
likelihoodprofileplot_prep_DD(null)
}
\arguments{
\item{null}{placeholder}
}
\value{
placeholder
}
\description{
Prepare DDUST data for likelihoodprofileplot()
View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.
}
\examples{
\dontrun{
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
}
}
