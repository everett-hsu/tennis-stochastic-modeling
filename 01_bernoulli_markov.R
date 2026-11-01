# Homogeneous Bernoulli and first-order Markov models
# ---------------------------------------------------
# Let Y_t=1 when the server wins a transition-destination point and Y_t=0
# otherwise. The Bernoulli model assumes one common probability p:
#
#   L_B(p) = product_t p^Y_t (1-p)^(1-Y_t),     p_hat = mean(Y_t).
#
# The first-order Markov model conditions on the preceding outcome:
#
#   p_ij = P(Y_t=j | Y_(t-1)=i),       i,j in {0,1},
#
#             current outcome
#                0      1
#   previous 0  p_00   p_01
#            1  p_10   p_11
#
# Its two free probabilities have row-wise maximum-likelihood estimates
#
#   p01_hat = n_01/(n_00+n_01),    p11_hat = n_11/(n_10+n_11).
#
# Both likelihoods below use the same current outcomes: the 5,752 non-opening
# points in ordinary Wimbledon service games.

if (!exists("transitions", inherits = FALSE)) source("00_data_preparation.R")

transition_counts <- table(
  previous = factor(transitions$previous, levels = 0:1),
  current = factor(transitions$outcome, levels = 0:1)
)

p_hat <- mean(transitions$outcome)
p01_hat <- mean(transitions$outcome[transitions$previous == 0])
p11_hat <- mean(transitions$outcome[transitions$previous == 1])

bernoulli_loglik <- sum(dbinom(
  transitions$outcome, size = 1, prob = p_hat, log = TRUE
))
markov_probability <- ifelse(
  transitions$previous == 0, p01_hat, p11_hat
)
markov_loglik <- sum(dbinom(
  transitions$outcome, size = 1, prob = markov_probability, log = TRUE
))

# H0: p_01=p_11. The Markov model adds one parameter, so the usual nominal
# reference is chi-square with one degree of freedom.
pooled_G2 <- 2 * (markov_loglik - bernoulli_loglik)
pooled_nominal_p <- pchisq(pooled_G2, df = 1, lower.tail = FALSE)
pooled_beta <- qlogis(p11_hat) - qlogis(p01_hat)

cat("\n01. POOLED BERNOULLI--MARKOV ANALYSIS\n")
cat("Transition counts (previous outcome by current outcome):\n")
print(transition_counts)
print(data.frame(
  quantity = c(
    "Bernoulli p_hat", "P(win | previous loss)",
    "P(win | previous win)", "Probability difference",
    "Previous-point log-odds coefficient", "Bernoulli log-likelihood",
    "Markov log-likelihood", "Likelihood-ratio G^2",
    "Nominal chi-square p-value"
  ),
  value = c(
    p_hat, p01_hat, p11_hat, p11_hat - p01_hat, pooled_beta,
    bernoulli_loglik, markov_loglik, pooled_G2, pooled_nominal_p
  ),
  row.names = NULL
), digits = 10, row.names = FALSE)
