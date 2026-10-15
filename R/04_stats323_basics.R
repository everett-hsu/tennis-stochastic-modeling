# MATH 460 Project 2: Bernoulli probability and point transitions
# Run 03_prepare_games.R first.
# These are descriptive calculations, not a test of momentum.

# Independent-point model: a fixed block of ten hypothetical points.
p <- mean(ordinary_points$server_won)
n <- 10
k <- 7

cat("\nServer-win rate in ordinary games:", round(p, 3), "\n")
cat("Expected wins in ten independent points:", round(n * p, 2), "\n")
cat("P(exactly seven wins):", round(dbinom(k, n, p), 4), "\n")
cat("P(at least seven wins):", round(1 - pbinom(k - 1, n, p), 4), "\n")

# Check the binomial probability with a simple simulation.
set.seed(460)
simulated_wins <- rbinom(5000, size = n, prob = p)
cat("Simulated P(at least seven wins):",
    round(mean(simulated_wins >= k), 4), "\n")

# Compare outcomes after a server loss (0) and server win (1).
# First points are excluded so transitions stay within a game.
paired_points <- ordinary_points[
  !is.na(ordinary_points$server_won_last_point), ]

transition_counts <- table(
  previous = paired_points$server_won_last_point,
  current = paired_points$server_won)
transition_rates <- prop.table(transition_counts, 1)

cat("\nPoint-transition counts:\n")
print(transition_counts)
cat("\nConditional server-win rates:\n")
print(round(transition_rates, 3))

cat("Difference (after win minus after loss):",
    round(transition_rates["1", "1"] - transition_rates["0", "1"], 3), "\n")
