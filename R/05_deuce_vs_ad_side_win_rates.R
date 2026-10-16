# MATH 460 Project 2: Deuce- and ad-court service-point win rates
# Run from the Tennis_Momentum.Rproj project directory.
# Uses base R only.

# 1. Prepare point outcomes and service-court labels --------------------------

source("R/01_load_data.R")

points <- points[order(points$match_id, points$point_no), ]

game_id <- paste(points$match_id, points$set_no, points$game_no)
point_in_game <- ave(points$point_no, game_id, FUN = seq_along)

court <- ifelse(point_in_game %% 2 == 1, "Deuce", "Ad")
server_won <- points$server == points$point_victor
server_name <- ifelse(points$server == 1, points$player1, points$player2)

# 2. Estimate each player's win rate by court ---------------------------------

player_court_summary <- aggregate(
  server_won,
  by = list(player = server_name, court = court),
  FUN = mean
)

player_court_table <- reshape(
  player_court_summary,
  idvar = "player",
  timevar = "court",
  direction = "wide"
)

player_court_table$Difference_pp <- round(
  100 * (player_court_table$x.Deuce - player_court_table$x.Ad), 2
)

player_court_table <- player_court_table[
  , c("player", "x.Deuce", "x.Ad", "Difference_pp")
]

print(player_court_table)

# The mean absolute difference ignores which court has the higher win rate.
absolute_difference <- abs(player_court_table$x.Deuce - player_court_table$x.Ad)
observed <- mean(absolute_difference)

cat("Mean absolute difference:", observed * 100, "pp\n")
cat("SD of absolute differences:", sd(absolute_difference) * 100, "pp\n")

# 3. Monte Carlo test: no court difference within each player -----------------

n_court <- table(server_name, court)
wins_court <- table(server_name[server_won], court[server_won])

# Under the null model, each player has one win probability across both courts.
p_player <- rowSums(wins_court) / rowSums(n_court)

set.seed(460)
simulated <- replicate(5000, {
  deuce <- rbinom(nrow(n_court), n_court[, "Deuce"], p_player)
  ad <- rbinom(nrow(n_court), n_court[, "Ad"], p_player)

  mean(abs(
    deuce / n_court[, "Deuce"] - ad / n_court[, "Ad"]
  ))
})

cat("Observed difference:", observed * 100, "pp\n")
cat("Mean simulated difference:", mean(simulated) * 100, "pp\n")
cat("Monte Carlo p-value:", mean(simulated >= observed), "\n")

hist(
  simulated * 100,
  main = "Average Absolute Court Difference Under Null Model",
  xlab = "Average difference (percentage points)"
)
abline(v = observed * 100, col = "red", lwd = 2)

# 4. Holdout comparison: player-only vs. player-and-court models --------------

model_data <- data.frame(
  player = server_name,
  court = court,
  won = as.integer(server_won)
)

set.seed(460)
train_rows <- sample(nrow(model_data), 0.8 * nrow(model_data))
train <- model_data[train_rows, ]
test <- model_data[-train_rows, ]

# Model A: one service-point win probability per player.
model_A <- aggregate(won ~ player, data = train, FUN = mean)

# Model B: separate deuce- and ad-court probabilities per player.
model_B <- aggregate(won ~ player + court, data = train, FUN = mean)

pred_A <- model_A$won[match(test$player, model_A$player)]

key_B <- paste(model_B$player, model_B$court)
test_key <- paste(test$player, test$court)
pred_B <- model_B$won[match(test_key, key_B)]

# Lower Brier score means better probabilistic predictions.
score_A <- mean((test$won - pred_A)^2)
score_B <- mean((test$won - pred_B)^2)

cat("Model A Brier score:", score_A, "\n")
cat("Model B Brier score:", score_B, "\n")
cat("Improvement (A - B):", score_A - score_B, "\n")



# 5. Repeated holdout validation --------------------------------------------

set.seed(461)

results <- replicate(100, {
  
  # New random 80/20 split
  train_rows <- sample(nrow(model_data), 0.8 * nrow(model_data))
  train <- model_data[train_rows, ]
  test <- model_data[-train_rows, ]
  
  # Fit both models
  model_A <- aggregate(won ~ player, data = train, FUN = mean)
  model_B <- aggregate(won ~ player + court, data = train, FUN = mean)
  
  # Predict test outcomes
  pred_A <- model_A$won[match(test$player, model_A$player)]
  
  key_B <- paste(model_B$player, model_B$court)
  test_key <- paste(test$player, test$court)
  pred_B <- model_B$won[match(test_key, key_B)]
  
  # Calculate Brier scores
  score_A <- mean((test$won - pred_A)^2)
  score_B <- mean((test$won - pred_B)^2)
  
  c(A = score_A, B = score_B, improvement = score_A - score_B)
})

cat("Mean Model A Brier score:", mean(results["A", ]), "\n")
cat("Mean Model B Brier score:", mean(results["B", ]), "\n")
cat("Mean improvement (A - B):",
    mean(results["improvement", ]), "\n")
cat("SD of improvement:",
    sd(results["improvement", ]), "\n")
cat("Fraction of splits favoring Model B:",
    mean(results["improvement", ] > 0), "\n")

hist(
  results["improvement", ],
  main = "Predictive Improvement from Adding Service Court",
  xlab = "Brier score difference (A - B)"
)

abline(v = 0, col = "red", lwd = 2)
