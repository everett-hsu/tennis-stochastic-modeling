# Scoring amplification from points to games, sets, and matches
# ---------------------------------------------------------------------------
# This optional appendix calculation assumes
#
#   Z_j ~ iid Bernoulli(p),
#
# where Z_j=1 means that the focal player wins point j. The same p is used on
# serve and return. This deliberately isolates amplification by the scoring
# rules; it is not a player-specific forecast and does not assume Markov
# dependence between consecutive points.

# Standard game --------------------------------------------------------------

game_win_probability <- function(player_points, opponent_points, p) {
  # Let W(a,b;p) be the probability of eventually winning a standard game
  # from score (a,b). At a nonterminal score,
  #
  #   W(a,b;p) = p W(a+1,b;p) + (1-p) W(a,b+1;p).
  #
  # At deuce, D = p^2 + 2p(1-p)D, and hence
  #
  #   D(p) = p^2 / {p^2 + (1-p)^2}.
  q <- 1 - p
  deuce_probability <- p^2 / (p^2 + q^2)

  W <- function(a, b) {
    if (a >= 4 && a - b >= 2) return(1)
    if (b >= 4 && b - a >= 2) return(0)

    # Beyond 3--3, only deuce and the two advantage states are possible before
    # the next legal endpoint. Their exact formulas prevent infinite recursion.
    if (a >= 3 && b >= 3) {
      if (a == b) return(deuce_probability)
      if (a == b + 1) return(p + q * deuce_probability)
      if (b == a + 1) return(p * deuce_probability)
    }

    p * W(a + 1, b) + q * W(a, b + 1)
  }

  W(player_points, opponent_points)
}

# Seven-point, win-by-two tiebreak ------------------------------------------

tiebreak_win_probability <- function(p) {
  q <- 1 - p
  win_from_six_all <- p^2 / (p^2 + q^2)

  # A pre-6--6 win has seven won points and k=0,...,5 lost points. The final
  # winning point is fixed last, leaving choose(6+k,k) earlier arrangements:
  #
  #   T(p) = sum_{k=0}^5 C(6+k,k)p^7q^k
  #          + C(12,6)p^6q^6 D(p).
  won_before_six_all <- sum(vapply(0:5, function(k) {
    choose(6 + k, k) * p^7 * q^k
  }, numeric(1L)))
  reach_six_all <- choose(12, 6) * p^6 * q^6

  won_before_six_all + reach_six_all * win_from_six_all
}

# Conventional tiebreak set --------------------------------------------------

set_win_probability <- function(p) {
  game_probability <- game_win_probability(0, 0, p)
  tiebreak_probability <- tiebreak_win_probability(p)

  # Let F(a,b) be the probability of winning the set from a games won and b
  # games lost. Before a terminal score or 6--6,
  #
  #   F(a,b) = G(p)F(a+1,b) + {1-G(p)}F(a,b+1).
  F <- function(a, b) {
    if (a >= 6 && a - b >= 2) return(1)
    if (b >= 6 && b - a >= 2) return(0)
    if (a == 6 && b == 6) return(tiebreak_probability)

    game_probability * F(a + 1, b) +
      (1 - game_probability) * F(a, b + 1)
  }

  F(0, 0)
}

# Best-of-three and best-of-five matches ------------------------------------

best_of_three_win_probability <- function(set_probability) {
  # M_3 = s^2 + 2s^2(1-s) = s^2(3-2s).
  set_probability^2 * (3 - 2 * set_probability)
}

best_of_five_win_probability <- function(set_probability) {
  # M_5 = s^3{1+3(1-s)+6(1-s)^2}
  #     = s^3(10-15s+6s^2).
  set_probability^3 *
    (10 - 15 * set_probability + 6 * set_probability^2)
}

# Federer appendix calculation ----------------------------------------------

p <- 0.54
q <- 1 - p
deuce_probability <- p^2 / (p^2 + q^2)
game_probability <- game_win_probability(0, 0, p)
tiebreak_probability <- tiebreak_win_probability(p)
set_probability <- set_win_probability(p)
best_of_three_probability <- best_of_three_win_probability(set_probability)
best_of_five_probability <- best_of_five_win_probability(set_probability)

amplification_progression <- data.frame(
  level = c("Point", "Game", "Tiebreak", "Set", "Best of 3", "Best of 5"),
  win_probability = c(
    p, game_probability, tiebreak_probability, set_probability,
    best_of_three_probability, best_of_five_probability
  ),
  stringsAsFactors = FALSE
)

cat("\n05. OPTIONAL APPENDIX: TENNIS SCORING AMPLIFICATION\n")
cat("====================================================\n")
cat(
  "Assumption: iid point outcomes with the same p on serve and return.\n",
  "Point-win probability p = ", sprintf("%.3f", p), "\n",
  "Exact probability of winning from deuce = ",
  sprintf("%.5f", deuce_probability), "\n\n",
  sep = ""
)
print(data.frame(
  level = amplification_progression$level,
  win_probability = sprintf("%.3f", amplification_progression$win_probability),
  stringsAsFactors = FALSE
), row.names = FALSE)
cat(
  "\nInterpretation: a four-percentage-point advantage over an even point\n",
  "contest is amplified by tennis's nested scoring rules. At p=0.54, the\n",
  "model gives an 85.9% best-of-three win probability without any serial\n",
  "dependence between points.\n",
  sep = ""
)
