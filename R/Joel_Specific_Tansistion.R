# Player-Specific Transition Probabilities

# Wimbledon data
points <- read.csv("data/raw/2024_Wimbledon_featured_matches.csv")

# inspect data
head(points)
names(points)
str(points)
nrow(points)

# sort points in order
points <- points[order(points$match_id, points$point_no), ]

# remove tie breakers
points <- subset(points, game_no != 13) # omit tie-breaks

# server win=1, server loss=0
points$server_won <- as.integer(
  points$server == points$point_victor
)

# identify the server
points$server_name <- ifelse(
  points$server == 1, points$player1, points$player2)

# identify each game
points$game_id <- interaction(
  points$match_id, points$set_no, points$game_no,
  drop = TRUE)



# i indexes each point that has a predecessor in the same game.
i <- which(
  points$game_id[-1] == points$game_id[-nrow(points)]
) + 1L

# building transition data
transitions <- data.frame(
  server_name = points$server_name[i],
  previous = points$server_won[i - 1L],
  current = points$server_won[i]
)

# count transitions
tab <- table(
  previous = factor(transitions$previous, levels = 0:1),
  current = factor(transitions$current, levels = 0:1)
)
tab

# transition probability
p01 <- tab[1, 2] / sum(tab[1, ])
p11 <- tab[2, 2] / sum(tab[2, ])
p11 - p01

# group by player and quantify uncertainty

#group transition by server
by_server <- split(transitions, transitions$server_name)
names(by_server)
length(by_server)

# Inspect one server's subset:
one <- by_server[[1]]
nrow(one)
table(one$previous, one$current)

# Iterate over groups:
results <- lapply(by_server, function(d) {
  tab <- table(
    factor(d$previous, levels = 0:1),
    factor(d$current, levels = 0:1)
  )
  
  n00 <- tab[1, 1]
  n01 <- tab[1, 2]
  n10 <- tab[2, 1]
  n11 <- tab[2, 2]
  
  
  n <- n00 + n01 + n10 + n11
  
  
p01 <- tab[1, 2] / sum(tab[1, ])
p11 <- tab[2, 2] / sum(tab[2, ])

difference <- p11 - p01

data.frame(
  n00 = n00,
  n01 = n01,
  n10 = n10,
  n11 = n11,
  n = n,
  p01 = p01,
  p11 = p11,
  difference = difference
)
  })

# Combine player results
results_table <- do.call(rbind, results)