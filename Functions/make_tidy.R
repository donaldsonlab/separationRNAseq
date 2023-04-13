huddle_make_tidy <- function(wide_df, timepoint) {
  df <- wide_df %>% filter(Timepoint == timepoint)
  df_huddles <- df %>% select(Animal, Cohort, PairingType, Timepoint, Condition, abspHuddle, absnHuddle) #selects for all animal information and also the huddle columns
  df_huddles <- pivot_longer(df_huddles, cols = c("abspHuddle", "absnHuddle"), names_to = "stimulus", values_to = "raw_huddle_time") %>% mutate(stimulus = if_else(stimulus == "abspHuddle", "partner", stimulus)) %>% mutate(stimulus = if_else(stimulus == "absnHuddle", "novel", stimulus)) #more intuative re-naming
  df_huddles$stimulus <- factor(df_huddles$stimulus, levels = c("partner", "novel")) #refactors so that partner is plotted on the left
  return(df_huddles)
}