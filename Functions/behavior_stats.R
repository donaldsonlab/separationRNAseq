BehavStats <- function(aov.result) {
  summary <- summary(aov.result)
  eta <- eta_squared(aov.result)
  post_hoc <- TukeyHSD(aov.result)
  
  full_summary <- list(summary = summary,
                       eta = eta,
                       Tukey_posthoc = post_hoc)
}
