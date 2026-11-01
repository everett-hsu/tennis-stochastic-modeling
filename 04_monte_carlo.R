# Estimator-matched legal-game Monte Carlo demonstration
# ------------------------------------------------------
# Under B_H, context h has a fixed service-win probability p_h and consecutive
# points are independent. Each replicate uses the observed game roster and the
# observed all-point estimate of p_h, simulates every game to its first legal
# endpoint, reconstructs transitions, and refits B_H and M_H.
#
# With observed statistic G_H,obs^2 and B simulated statistics, the plus-one
# upper-tail diagnostic is
#
#   p_MC = [1 + #{b: G_H,b^2 >= G_H,obs^2}] / (B + 1).
#
# The function is defined when this script is sourced; simulation begins only
# when run_wimbledon_monte_carlo() is called explicitly.

if (!exists("fit_adjusted_models", inherits = FALSE)) {
  source("03_match_server_adjustment.R")
}

is_legal_game_end <- function(server_points, returner_points) {
  max(server_points, returner_points) >= 4L &&
    abs(server_points - returner_points) >= 2L
}

simulate_service_game <- function(service_win_probability) {
  outcome <- integer()
  server_points <- returner_points <- 0L

  repeat {
    next_outcome <- rbinom(1L, size = 1L, prob = service_win_probability)
    outcome <- c(outcome, next_outcome)
    server_points <- server_points + next_outcome
    returner_points <- returner_points + 1L - next_outcome
    if (is_legal_game_end(server_points, returner_points)) break
  }
  outcome
}

simulate_game_roster <- function(roster, context_probability) {
  games <- lapply(seq_len(nrow(roster)), function(g) {
    outcome <- simulate_service_game(
      context_probability[roster$context[g]]
    )
    data.frame(
      match_id = roster$match_id[g],
      game_id = roster$game_id[g],
      context = roster$context[g],
      outcome = outcome,
      stringsAsFactors = FALSE
    )
  })
  answer <- do.call(rbind, games)
  rownames(answer) <- NULL
  answer
}

context_probability <- tapply(points$outcome, points$context, mean)

run_wimbledon_monte_carlo <- function(B = 49L, seed = 46001L) {
  B <- as.integer(B)
  simulation <- data.frame(
    replicate = seq_len(B),
    seed = seed + seq_len(B) - 1L,
    beta = NA_real_,
    G2 = NA_real_
  )

  for (b in seq_len(B)) {
    set.seed(simulation$seed[b])
    simulated_points <- simulate_game_roster(
      game_roster, context_probability
    )
    simulated_transitions <- make_within_game_transitions(simulated_points)
    fit <- try(fit_adjusted_models(simulated_transitions), silent = TRUE)

    if (inherits(fit, "try-error") ||
        !fit$B_H$converged || !fit$M_H$converged || !is.finite(fit$G2)) {
      stop("A Monte Carlo model fit failed; no p-value was calculated.")
    }
    simulation$beta[b] <- fit$beta
    simulation$G2[b] <- fit$G2
  }

  tail_count <- sum(simulation$G2 >= adjusted_G2)
  monte_carlo_p <- (1 + tail_count) / (B + 1)

  cat("\n04. LEGAL-GAME MONTE CARLO DEMONSTRATION\n")
  cat(
    "Replicates:", B,
    "  Observed G_H^2:", sprintf("%.6f", adjusted_G2),
    "  Upper-tail count:", tail_count,
    "  Plus-one p_MC:", sprintf("%.4f", monte_carlo_p), "\n"
  )
  cat(
    "This short run demonstrates the method; the manuscript's p_MC=0.560\n",
    "uses 999 saved replicates.\n"
  )

  list(
    observed_G2 = adjusted_G2,
    replicates = simulation,
    tail_count = tail_count,
    monte_carlo_p = monte_carlo_p
  )
}
mc <- run_wimbledon_monte_carlo()