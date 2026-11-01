# Modeling Momentum in Tennis

This base-R codebase  asks whether a
first-order Markov model improves on an independent Bernoulli model for service
points after controlling for the server and match.

The included `2024_Wimbledon_featured_matches.csv` contains point records from
31 featured men's matches at Wimbledon 2023 and was distributed with the 2024
Mathematical Contest in Modeling Problem C materials. 

## Scripts

- `00_data_preparation.R` excludes tiebreaks, validates ordinary service games,
  constructs binary outcomes and within-game transitions, and records the
  observed game roster.
- `01_bernoulli_markov.R` fits the homogeneous Bernoulli and first-order Markov
  models to the Wimbledon transitions.
- `02_heterogeneity.R` illustrates the pseudodependence mixture identity using
  the observed match--server probabilities.
- `03_match_server_adjustment.R` fits the match--server-adjusted models and
  calculates the clustered interval and fitted probability contrast.
- `04_monte_carlo.R` defines the optional estimator-matched legal-game Monte
  Carlo demonstration.
- `05_scoring_amplification.R` reproduces the manuscript's scoring appendix for
  a hypothetical constant point-win probability `p = 0.54`.

## Running in RStudio

Set the working directory to this folder and source the scripts in order:

```r
source("00_data_preparation.R")
source("01_bernoulli_markov.R")
source("02_heterogeneity.R")
source("03_match_server_adjustment.R")
source("04_monte_carlo.R")
source("05_scoring_amplification.R")
```

Scripts 00--03 and 05 print their results when sourced. Script 04 only defines
the simulation functions. Run its default demonstration explicitly:

```r
mc_demo <- run_wimbledon_monte_carlo(B = 49, seed = 46001)
```

The 49-replicate run demonstrates the algorithm; it is not expected to
reproduce the manuscript's saved 999-replicate Monte Carlo value. The nominal
chi-square calculation, match-clustered interval, and estimator-matched Monte
Carlo diagnostic have different interpretations, and none proves that all
service points are independent.
