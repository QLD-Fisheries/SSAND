# Contributing to SSAND

## Hello and thanks

Thank you for showing interest in contributing to SSAND! We aim to provide tools that can be adapted for multiple stock assessment models and potentially multiple jurisdictions, so it's people like you who make our package so adaptable.

Please read these guidelines to see how your feedback or contribution can best be incorporated into SSAND.

## Reporting a bug

Please submit an [issue](https://github.com/QLD-Fisheries/SSAND/issues) through GitHub, ideally using an appropriate label. It is often incredibly helpful to provide the data set and sample code that produced the bug.

Please do not open up a GitHub issue if the bug is a security vulnerability, and instead contact [fisheriesassessment\@daf.qld.gov.au](mailto:fisheriesassessment@daf.qld.gov.au) directly.

## Suggesting a feature or enhancement

We would love to hear from you, either through a GitHub issue or by emailing [fisheriesassessment\@daf.qld.gov.au](mailto:fisheriesassessment@daf.qld.gov.au).

Please consider how this might fit into or extend the current [SSAND workflow](https://github.com/QLD-Fisheries/SSAND?tab=readme-ov-file#suggested-ssand-workflow).

You don't need to write the code for the feature or enhancement in order to suggest it, but if you have written something please include that in your issue/email.

## Pull requests

At the moment, pull requests are closed to community members outside of the Queensland Department of Agriculture and Fisheries.

If you have code you'd like to contribute, email us at [fisheriesassessment\@daf.qld.gov.au](mailto:fisheriesassessment@daf.qld.gov.au).

## Code conventions

There are some simple style conventions for our code that we follow at SSAND.

### General structure

Typically, each plot requires two functions: the `prep()` function and the `plot()` function.

-   All data preparation decisions should be declared in the `prep()` file, and all plotting decisions are declared in the `plot()` function.
-   The `prep()` functions have a suffix to denote the model the data is prepared for. "DD" means delay-difference (namely, DDUST) and "SS" means Stock Synthesis.
-   The output of the `prep()` functions will look the same, regardless of the model used.
-   The first argument of a `prep_SS()` function will be a list of outputs from `r4ss::SS_outputs()` with one element per scenario. This is referred to as `ss_mle`
-   The first argument of a `prep_DD()` function will be a list of outputs from `DDUST::DDUST_output()` with one element per scenario. This is referred to as `dd_mle`.
-   If using an MCMC model, the second argument of a `prep_SS()` function will be a list of outputs from `r4ss::SSgetMCMC()` with one element per scenario. This is referred to as `ss_mcmc`.
-   If using an MCMC model, the second argument of a `prep_DD()` function will be a list of model fits from `tmbstan::tmbstan()` with one list element per scenario. This is referred to as `dd_mcmc`.
-   If using an MCMC model, the third argument of a `prep_DD()` function will *often* be a list of outputs from DDUST::simulate_DD() with one list element per scenario. This is referred to as `dd_sim`.
-   The presence of `ss_mcmc` and `dd_mcmc` informs the SSAND function that an MCMC model was used.

The scenarios listed in `ss_mle` and `ss_mcmc` should align, similarly for `dd_mle`, `dd_mcmc` and `dd_sim`. If you want to plot a subset of scenarios in these lists, this can be specified using the `scenarios` argument which is available in the `prep()` function (and a second chance available in the `plot()` function). Scenario names can be specified in the `plot()` functions using the `scenario_labels` argument.

### Variable names

There are several function arguments that are used across multiple functions (e.g. `colours`, `scenarios`). Please ensure that you are following these naming conventions wherever possible.

-   All variable names are to be in lower case with the following exceptions: `B`, `F`, `MSY`, `LL`, `Linf`, `W_r`.
-   Multi-word variable names are spelled in full and separated by an underscore with the following exceptions: `xlab`, `xbreaks`, `xlim`, `ylab`, `ybreaks`, `ylim` and `ncol`.
-   Logical variables that include/exclude features have a prefix of `show_`(e.g.`show_median`)
-   Data entered into the `plot()`function should be called `data`. Incremental and final ggplot objects should be called `p`.

We indent using 2 spaces and regularly use Ctl+I in Posit/RStudio to indent our code.
