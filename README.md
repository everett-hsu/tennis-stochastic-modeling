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

