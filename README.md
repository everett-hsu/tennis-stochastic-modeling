# Tennis Momentum — MATH 460 Project 2

R scripts for analyzing Wimbledon point data using Bernoulli and Markov models.

## Run in RStudio

Open `Tennis_Momentum.Rproj`. Run scripts from the project directory; no additional packages are needed.

1. **`R/01_load_data.R` — Load data.** Imports the point-level matches and data dictionary.
2. **`R/02_first_look.R` — Inspect data.** Displays match information and overall service-point outcomes.
3. **`R/03_prepare_games.R` — Prepare games.** Identifies service-game boundaries and previous-point outcomes.
4. **`R/04_stats323_basics.R` — Probability basics.** Explores binomial probabilities, simulation, and conditional point outcomes.
5. **`R/05_deuce_vs_ad_side_win_rates.R` — Ignore.** Earlier exploratory experiment; not part of the current analysis.
6. **`R/06_markov_likelihood_test.R` — Bernoulli vs. Markov.** Estimates transition probabilities and runs a pooled likelihood-ratio test.
7. **`R/07_player_specific_null.R` — Monte Carlo null model.** Simulates independent service games using player-specific win probabilities and compares transition differences. Runs script 06 automatically.

The raw CSV files remain unchanged. Scripts 06–07 are preliminary analyses, not final conclusions about point-to-point dependence.
