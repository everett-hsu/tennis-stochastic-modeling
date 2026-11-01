# Wimbledon 2023 data preparation
# --------------------------------
# A point outcome is 1 when the server wins and 0 when the server loses.
# Tiebreaks are excluded because their server rotation and stopping rule differ
# from ordinary service games. The supplied point_no runs through each match,
# so within-game positions are derived after constructing game identifiers.

make_within_game_transitions <- function(point_data) {
  same_game <- c(
    FALSE,
    point_data$game_id[-1L] == point_data$game_id[-nrow(point_data)]
  )
  previous <- c(NA_integer_, head(point_data$outcome, -1L))

  data.frame(
    match_id = point_data$match_id[same_game],
    game_id = point_data$game_id[same_game],
    context = point_data$context[same_game],
    previous = previous[same_game],
    outcome = point_data$outcome[same_game],
    stringsAsFactors = FALSE
  )
}

raw_points <- read.csv(
  file.path("data", "raw", "2024_Wimbledon_featured_matches.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
raw_points <- raw_points[
  order(raw_points$match_id, raw_points$point_no), , drop = FALSE
]
rownames(raw_points) <- NULL

raw_points$game_id <- paste(
  raw_points$match_id, raw_points$set_no, raw_points$game_no, sep = "::"
)
raw_points$is_tiebreak <- raw_points$game_no == 13L &
  raw_points$p1_games == 6L & raw_points$p2_games == 6L
tiebreak_games <- length(unique(raw_points$game_id[raw_points$is_tiebreak]))
tiebreak_points <- sum(raw_points$is_tiebreak)

points <- raw_points[!raw_points$is_tiebreak, , drop = FALSE]
points$server_player <- ifelse(
  points$server == 1L, points$player1, points$player2
)
points$outcome <- as.integer(points$server == points$point_victor)
points$context <- paste(points$match_id, points$server, sep = "|S")
points$point_in_game <- ave(
  seq_len(nrow(points)), points$game_id, FUN = seq_along
)

# Each retained game must have one server and must stop at its first score with
# at least four points won and a lead of at least two points.
game_rows <- split(seq_len(nrow(points)), points$game_id)
one_server <- vapply(game_rows, function(rows) {
  length(unique(points$server[rows])) == 1L
}, logical(1L))
first_legal_endpoint <- vapply(game_rows, function(rows) {
  outcome <- points$outcome[rows]
  server_points <- cumsum(outcome)
  returner_points <- cumsum(1L - outcome)
  endpoint <- which(
    pmax(server_points, returner_points) >= 4L &
      abs(server_points - returner_points) >= 2L
  )
  length(endpoint) == 1L && endpoint == length(rows)
}, logical(1L))
if (!all(one_server) || !all(first_legal_endpoint)) {
  stop("The retained data contain an invalid ordinary service game.")
}

transitions <- make_within_game_transitions(points)
game_roster <- points[!duplicated(points$game_id), c(
  "match_id", "game_id", "context", "server_player"
)]
rownames(game_roster) <- NULL

sample_summary <- c(
  matches = length(unique(points$match_id)),
  tiebreak_games_removed = tiebreak_games,
  tiebreak_points_removed = tiebreak_points,
  ordinary_games = nrow(game_roster),
  ordinary_points = nrow(points),
  transitions = nrow(transitions),
  contexts = length(unique(points$context))
)

cat("\n00. WIMBLEDON ANALYSIS SAMPLE\n")
print(data.frame(
  quantity = names(sample_summary), value = unname(sample_summary),
  row.names = NULL
), row.names = FALSE)
