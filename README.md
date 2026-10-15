# Tennis Momentum — MATH 460 Project 2

R scripts for exploring Wimbledon point data and building introductory probability models.

## Run in RStudio

Open `Tennis_Momentum.Rproj`, then run the scripts in order from the project directory. No additional packages are needed.

1. **`R/01_load_data.R` — Load the data.** Imports the point-by-point match data and data dictionary. Prints the number of points, columns, matches, and dictionary entries.
2. **`R/02_first_look.R` — Inspect the data.** Displays example points and matchups, counts outcomes by server, and calculates the overall server-win rate.
3. **`R/03_prepare_games.R` — Prepare game-level observations.** Separates game points from tie-break points, sorts them by match and point number, assigns each game an ID, and records whether the server won the current and preceding point within the game.
4. **`R/04_stats323_basics.R` — Explore probability and dependence.** Calculates binomial probabilities, checks one with Monte Carlo simulation, and compares server-win frequencies conditional on the preceding point's outcome.

The original CSV files remain unchanged. These scripts are introductory calculations and descriptive exploration, not a completed statistical model.
