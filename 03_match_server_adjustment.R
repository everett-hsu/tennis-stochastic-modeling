# Match--server-adjusted Bernoulli and Markov models
# -------------------------------------------------
# Let h(g) identify the server in the match containing game g, and let
# X_(g,t)=Y_(g,t-1) be the preceding outcome in the same game. We compare
#
#   B_H: logit P(Y_(g,t)=1) = alpha_h,
#   M_H: logit P(Y_(g,t)=1) = alpha_h + beta X_(g,t).
#
# Every match--server context receives its own baseline alpha_h. The common
# coefficient beta therefore measures previous-point association within
# contexts rather than differences between stronger and weaker servers.

if (!exists("pooled_beta", inherits = FALSE)) source("01_bernoulli_markov.R")

# This is the single adjusted-model implementation used here and by script 04.
fit_adjusted_models <- function(data) {
  data$context <- factor(data$context)
  B_H <- glm(
    outcome ~ 0 + context, family = binomial(), data = data
  )
  M_H <- glm(
    outcome ~ 0 + context + previous, family = binomial(), data = data
  )
  loglik_BH <- as.numeric(logLik(B_H))
  loglik_MH <- as.numeric(logLik(M_H))

  list(
    data = data, B_H = B_H, M_H = M_H,
    beta = unname(coef(M_H)["previous"]),
    loglik_BH = loglik_BH,
    loglik_MH = loglik_MH,
    G2 = 2 * (loglik_MH - loglik_BH),
    delta_AIC = AIC(M_H) - AIC(B_H)
  )
}

match_clustered_vcov <- function(model, match_id) {
  # Observation i has logistic score s_i=x_i(y_i-p_i). Summing these scores
  # within match m gives S_m. The match-clustered sandwich covariance is
  #
  #   I^(-1) [sum_m S_m S_m'] I^(-1),
  #
  # where I=X'WX and W_i=p_i(1-p_i). The two final factors are the finite-
  # sample corrections used in the manuscript calculation.
  X <- model.matrix(model)
  y <- model.response(model.frame(model))
  probability <- fitted(model)
  match_score <- rowsum(
    X * as.numeric(y - probability), match_id, reorder = FALSE
  )
  information <- crossprod(
    X, X * as.numeric(probability * (1 - probability))
  )
  bread <- solve(information)
  M <- nrow(match_score)
  N <- nrow(X)
  K <- ncol(X)

  bread %*% crossprod(match_score) %*% bread *
    M / (M - 1) * (N - 1) / (N - K)
}

transition_weighted_contrast <- function(model, data) {
  # A log-odds coefficient implies different probability changes at different
  # baselines. For each context, compare fitted probabilities after a previous
  # win and loss, then weight the contrast by its observed transition count.
  context_levels <- levels(data$context)
  prediction_data <- data.frame(
    context = factor(rep(context_levels, 2), levels = context_levels),
    previous = rep(0:1, each = length(context_levels))
  )
  probability <- matrix(
    predict(model, prediction_data, type = "response"), ncol = 2
  )
  context_transitions <- table(data$context)

  sum(context_transitions * (probability[, 2L] - probability[, 1L])) /
    nrow(data)
}

adjusted_fit <- fit_adjusted_models(transitions)
adjusted_beta <- adjusted_fit$beta
adjusted_G2 <- adjusted_fit$G2
adjusted_delta_AIC <- adjusted_fit$delta_AIC
adjusted_nominal_p <- pchisq(adjusted_G2, df = 1, lower.tail = FALSE)

cluster_vcov <- match_clustered_vcov(
  adjusted_fit$M_H, adjusted_fit$data$match_id
)
cluster_se <- sqrt(cluster_vcov["previous", "previous"])
match_df <- length(unique(adjusted_fit$data$match_id)) - 1L
critical_value <- qt(0.975, df = match_df)
cluster_interval <- adjusted_beta + c(-1, 1) * critical_value * cluster_se
fitted_probability_contrast <- transition_weighted_contrast(
  adjusted_fit$M_H, adjusted_fit$data
)

cat("\n03. WIMBLEDON MATCH--SERVER ADJUSTMENT\n")
print(data.frame(
  quantity = c(
    "Pooled previous-point coefficient",
    "Adjusted previous-point coefficient",
    "Adjusted odds ratio",
    "Adjusted likelihood-ratio G_H^2",
    "Nominal chi-square p-value",
    "AIC(M_H) - AIC(B_H)",
    "Match-clustered standard error",
    "Clustered 95% interval lower",
    "Clustered 95% interval upper",
    "Transition-weighted probability contrast"
  ),
  value = c(
    pooled_beta, adjusted_beta, exp(adjusted_beta), adjusted_G2,
    adjusted_nominal_p, adjusted_delta_AIC, cluster_se,
    cluster_interval, fitted_probability_contrast
  ),
  row.names = NULL
), digits = 11, row.names = FALSE)
cat(
  "Match clustering allows within-match association but does not remove\n",
  "finite-context bias from estimating many short fixed effects.\n"
)
