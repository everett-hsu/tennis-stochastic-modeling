# MATH 460 Project 2: Prepare ordinary service games
# Run 01_load_data.R first.
# Tie-breaks are game 13 in this dataset; we exclude them here.

ordinary_points <- points[points$game_no != 13, ]
ordinary_points <- ordinary_points[
  order(ordinary_points$match_id, ordinary_points$point_no), ]
row.names(ordinary_points) <- NULL

# Identify each game by match, set, and game number.
ordinary_points$game_id <- paste(ordinary_points$match_id,
                                 ordinary_points$set_no,
                                 ordinary_points$game_no, sep = "_")

ordinary_points$server_won <- as.integer(
  ordinary_points$server == ordinary_points$point_victor)

# The previous-point outcome is missing at the start of each game.
ordinary_points$server_won_last_point <- c(
  NA, head(ordinary_points$server_won, -1))
first_point <- !duplicated(ordinary_points$game_id)
ordinary_points$server_won_last_point[first_point] <- NA

cat("Ordinary-game points:", nrow(ordinary_points), "\n")
cat("Ordinary games:", length(unique(ordinary_points$game_id)), "\n")
cat("Tie-break points excluded:", sum(points$game_no == 13), "\n")
