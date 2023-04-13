mod_sep_PPTs_huddles <- function(sep_df, timepoint) {
  sep_timepoint <- sep_df %>% filter(Timepoint == timepoint)
  sep_timepoint_huddles <- sep_timepoint %>% select(c(1:7))
  sep_timepoint_huddles <- pivot_longer(sep_timepoint_huddles, cols = c("abspHuddle", "absnHuddle"), names_to = "stimulus", values_to = "raw_huddle_time") %>% mutate(stimulus = if_else(stimulus == "abspHuddle", "partner", stimulus)) %>% mutate(stimulus = if_else(stimulus == "absnHuddle", "novel", stimulus))
  sep_timepoint_huddles$stimulus <- factor(sep_timepoint_huddles$stimulus, levels = c("partner", "novel"))
  return(sep_timepoint_huddles)
}