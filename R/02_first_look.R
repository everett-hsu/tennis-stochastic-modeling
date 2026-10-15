# MATH 460 Project 2: First look at the Wimbledon data
# Run 01_load_data.R first.

cat("\nFirst six points:\n")
print(head(points[c("match_id", "set_no", "game_no", "point_no",
                    "server", "point_victor")]))

cat("\nFirst six matchups:\n")
print(head(unique(points[c("match_id", "player1", "player2")])))

cat("\nPoint winners by server:\n")
print(table(server = points$server, winner = points$point_victor))

server_won <- points$server == points$point_victor
cat("\nOverall server-win rate:", round(mean(server_won), 3), "\n")
