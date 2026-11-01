# Heterogeneity and apparent previous-point dependence
# ----------------------------------------------------
# Let C identify a match--server context. Conditional on C, suppose adjacent
# outcomes are independent Bernoulli trials with probability P_C. With
# transition-opportunity weighting, write
#
#   mu = E(P_C),       sigma^2 = Var(P_C).
#
# A previous win disproportionately selects contexts with larger P_C:
#
#   P(Y_t=1 | Y_(t-1)=1) = mu + sigma^2/mu,
#   P(Y_t=1 | Y_(t-1)=0) = mu - sigma^2/(1-mu).
#
# Therefore the pooled probability contrast induced by mixing contexts is
#
#   Delta_mix = sigma^2 / {mu(1-mu)} >= 0.
#
# This is pseudodependence: unequal baseline probabilities can create pooled
# association even when outcomes are independent within every context.

if (!exists("p01_hat", inherits = FALSE)) source("01_bernoulli_markov.R")

context_probability <- tapply(points$outcome, points$context, mean)
opportunities <- table(transitions$context)
context_probability <- context_probability[names(opportunities)]
weight <- as.numeric(opportunities) / sum(opportunities)

mixture_mean <- sum(weight * context_probability)
mixture_variance <- sum(
  weight * (context_probability - mixture_mean)^2
)
mixture_contrast <- mixture_variance /
  (mixture_mean * (1 - mixture_mean))
observed_pooled_contrast <- p11_hat - p01_hat

cat("\n02. WIMBLEDON HETEROGENEITY ILLUSTRATION\n")
print(data.frame(
  quantity = c(
    "Transition-weighted mean context probability",
    "Transition-weighted variance",
    "Mixture-identity contrast",
    "Observed pooled contrast"
  ),
  value = c(
    mixture_mean, mixture_variance,
    mixture_contrast, observed_pooled_contrast
  ),
  row.names = NULL
), digits = 8, row.names = FALSE)
cat(
  "The mixture value is a descriptive plug-in illustration, not a causal\n",
  "decomposition or statistical test.\n"
)
