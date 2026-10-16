
# MATH 460 — Player-specific independent Bernoulli null

# Reproduce the previous analysis.
source("R/06_markov_likelihood_test.R")

# Identify the server for every point.
points$server_name <- ifelse(
  points$server == 1,
  points$player1,
  points$player2
)

# Estimate each player's service-point win probability.
p_by_server <- tapply(
  points$server_won,
  points$server_name,
  mean
)

# Preserve the number of observed service games per player.
game_servers <- points$server_name[
  !duplicated(points$game_id)
]

# Verify that every game has only one server.
consistent <- tapply(
  points$server_name,
  points$game_id,
  function(x) length(unique(x)) == 1
)

stopifnot(all(consistent))

p_games <- unname(p_by_server[game_servers])
stopifnot(!anyNA(p_games))

# Simulate one complete tennis service game.
simulate_game <- function(p) {
  
  score <- c(0L, 0L)
  counts <- integer(4)  # n00, n01, n10, n11
  previous <- NA_integer_
  
  repeat {
    
    current <- as.integer(runif(1) < p)
    
    if (!is.na(previous)) {
      index <- 2L * previous + current + 1L
      counts[index] <- counts[index] + 1L
    }
    
    score[current + 1L] <- score[current + 1L] + 1L
    previous <- current
    
    if (max(score) >= 4L &&
        abs(score[1] - score[2]) >= 2L) {
      break
    }
  }
  
  counts
}

# Calculate the transition difference in one simulated dataset.
simulate_dataset <- function() {
  
  transition_counts <- rowSums(
    vapply(p_games, simulate_game, integer(4))
  )
  
  n00 <- transition_counts[1]
  n01 <- transition_counts[2]
  n10 <- transition_counts[3]
  n11 <- transition_counts[4]
  
  simulated_p01 <- n01 / (n00 + n01)
  simulated_p11 <- n11 / (n10 + n11)
  
  simulated_p11 - simulated_p01
}

# Monte Carlo experiment
set.seed(460)
B <- 1000

simulated_differences <- replicate(
  B, simulate_dataset()
)

observed_difference <- as.numeric(p11 - p01)
null_mean <- mean(simulated_differences)

# Two-sided simulation test, centered on the null mean.
p_mc <- (
  1 + sum(
    abs(simulated_differences - null_mean) >=
      abs(observed_difference - null_mean)
  )
) / (B + 1)

cat("\nObserved difference:", observed_difference)
cat("\nSimulated null mean:", null_mean)
cat("\nSimulated null SD:", sd(simulated_differences))
cat("\nMonte Carlo p-value:", p_mc, "\n")

hist(
  simulated_differences,
  breaks = 35,
  main = "Transition Difference Under Player-Specific Independence",
  xlab = "p11 - p01",
  col = "gray85",
  border = "white"
)

abline(v = observed_difference, col = "red", lwd = 2)
abline(v = null_mean, col = "blue", lwd = 2)
