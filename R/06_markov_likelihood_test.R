
# MATH 460 — Bernoulli vs. first-order Markov model

points <- read.csv("data/raw/2024_Wimbledon_featured_matches.csv")

# Sort points chronologically and exclude tie-breaks.
points <- points[order(points$match_id, points$point_no), ]
points <- subset(points, game_no != 13)

# Server wins: 1; server loses: 0.
points$server_won <- as.integer(
  points$server == points$point_victor
)

# Unique game identifier.
points$game_id <- interaction(
  points$match_id, points$set_no, points$game_no,
  drop = TRUE
)

# Keep only consecutive points in the same game.
i <- which(
  points$game_id[-1] ==
    points$game_id[-nrow(points)]
) + 1L

previous <- points$server_won[i - 1L]
current  <- points$server_won[i]

# Transition counts.
counts <- table(
  previous = factor(previous, levels = 0:1),
  current  = factor(current, levels = 0:1)
)

print(counts)

n00 <- counts[1, 1]
n01 <- counts[1, 2]
n10 <- counts[2, 1]
n11 <- counts[2, 2]

# Maximum likelihood estimates.
p01 <- n01 / (n00 + n01)
p11 <- n11 / (n10 + n11)
p_null <- (n01 + n11) / sum(counts)

# Log-likelihood under independence.
ll_null <- (n01 + n11) * log(p_null) +
  (n00 + n10) * log(1 - p_null)

# Log-likelihood under the Markov model.
ll_markov <- n01 * log(p01) +
  n00 * log(1 - p01) +
  n11 * log(p11) +
  n10 * log(1 - p11)

# Likelihood-ratio test.
LR <- 2 * (ll_markov - ll_null)
p_value <- pchisq(LR, df = 1, lower.tail = FALSE)

cat("\nPooled Bernoulli estimate:", p_null)
cat("\nMarkov p01:", p01)
cat("\nMarkov p11:", p11)
cat("\nDifference:", p11 - p01)
cat("\nLikelihood-ratio statistic:", LR)
cat("\np-value:", p_value, "\n")
