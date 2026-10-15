# MATH 460 Project 2: Tennis momentum
# Load the original Wimbledon point data and its data dictionary.
# Run this script from the Tennis_Momentum.Rproj project directory.
# No packages required.

points_path <- file.path("data", "raw", "2024_Wimbledon_featured_matches.csv")
dictionary_path <- file.path("data", "raw", "2024_data_dictionary.csv")

points <- read.csv(points_path)
data_dictionary <- read.csv(dictionary_path)

cat("Point records:", nrow(points), "\n")
cat("Variables:", ncol(points), "\n")
cat("Matches:", length(unique(points$match_id)), "\n")
cat("Dictionary entries:", nrow(data_dictionary), "\n")
