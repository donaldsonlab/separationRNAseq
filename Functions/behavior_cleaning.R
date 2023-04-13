behavior_cleaning_ggplot <- function(ss_ggplot, os_ggplot) {
  both_ggplot <- rbind(ss_ggplot, os_ggplot)
  both_ggplot <- mutate_at(both_ggplot, vars(Animal, PairingType, PairingTime, Cohort, Timepoint, Stimulus), as.factor)
  #refactor the Timepoints so that they plot in the order done in the experiment
  both_ggplot$Timepoint <- factor(both_ggplot$Timepoint, levels = c("Baseline", "Acute", "Chronic"))
  ss_ggplot$Timepoint <- factor(ss_ggplot$Timepoint, levels = c("Baseline", "Acute", "Chronic"))
  os_ggplot$Timepoint <- factor(os_ggplot$Timepoint, levels = c("Baseline", "Acute", "Chronic"))
  
  #make dfs for both stimulus animal types
  partner_both_ggplot <- both_ggplot %>% filter(Stimulus == "Partner")
  novel_both_ggplot <- both_ggplot %>% filter(Stimulus == "Novel")

  #partner and novel ggplot dfs for just ss and making mean df for plotting
  partner_ss_ggplot <- ss_ggplot %>% filter(Stimulus == "Partner")
  novel_ss_ggplot <- ss_ggplot %>% filter(Stimulus == "Novel")

  #partner and novel ggplot dfs for just os and making mean df for plotting
  partner_os_ggplot <- os_ggplot %>% filter(Stimulus == "Partner")
  novel_os_ggplot <- os_ggplot %>% filter(Stimulus == "Novel")
  
  #make mean dfs for plotting
  mean.ppref_ss <- partner_ss_ggplot %>% group_by(Timepoint) %>% summarise(mean = mean(Huddle), mean_se = sd(Huddle)/sqrt(n()))
  mean.ppref_os <- partner_os_ggplot %>% group_by(Timepoint) %>% summarise(mean = mean(Huddle), mean_se = sd(Huddle)/sqrt(n()))
  
  cleaned_data <- list(both_ggplot = both_ggplot,
                       partner_both_ggplot = partner_both_ggplot,
                       novel_both_ggplot = novel_both_ggplot,
                       partner_os_ggplot = partner_os_ggplot,
                       novel_os_ggplot = novel_os_ggplot,
                       mean.ppref_os = mean.ppref_os,
                       partner_ss_ggplot = partner_ss_ggplot,
                       novel_ss_ggplot = novel_ss_ggplot,
                       mean.ppref_ss = mean.ppref_ss)
  return(cleaned_data)
  }

behavior_cleaning_matrix <- function(ss_matrix, os_matrix) {
  ss_matrix <- mutate_at(ss_matrix, vars(Animal), as.factor)
  os_matrix <- mutate_at(ss_matrix, vars(Animal), as.factor)
  both_matrix <- rbind(os_matrix, ss_matrix)
  return(both_matrix)
}

behavior_cleaning_changeScores <- function(ss_changescores, os_changescores) {
  ss_changescores <- mutate_at(ss_changescores, vars(Animal, PairingType, PairingTime, ChangeTime, Cohort, Stimulus), as.factor)
  os_changescores <- mutate_at(os_changescores, vars(Animal, PairingType, PairingTime, ChangeTime, Cohort, Stimulus), as.factor)
  both_changescores <- rbind(os_changescores, ss_changescores)
  return(both_changescores)
}
  
matrix_locomotion <- function(ss_matrix, os_matrix) { 
  os_names <- as.data.frame(rep(c("OS"), nrow(os_matrix))) %>% dplyr::rename("PairingType" = 1)
  ss_names <- as.data.frame(rep(c("SS"), nrow(ss_matrix))) %>% dplyr::rename("PairingType" = 1)
  os_loc <- os_matrix %>% column_to_rownames("Animal") %>% select(contains("Locomotion"))
  os_loc <- cbind(os_names, os_loc)
  ss_loc <- ss_matrix %>% column_to_rownames("Animal") %>% select(contains("Locomotion"))
  ss_loc <- cbind(ss_names, ss_loc)
  both_loc <- rbind(os_loc, ss_loc)
  
  locomotion_list <- list(os_loc = os_loc,
                          ss_loc = ss_loc,
                          both_loc = both_loc)
  return(locomotion_list)
}

ggplot_locomotion <- function(both_loc) {
  both_loc.df <- as.data.frame(both_loc)
  both_loc_ggplot <- both_loc.df %>% rownames_to_column("Animal")
  both_loc_ggplot <- both_loc_ggplot %>% pivot_longer(!c(Animal, PairingType), names_to = "Timepoint", values_to = "Distance")
  both_loc_ggplot <- both_loc_ggplot %>% mutate(Distance = Distance/1000) %>% mutate(Timepoint = str_replace(Timepoint, "Locomotion", ""))
  return(both_loc_ggplot)
}
