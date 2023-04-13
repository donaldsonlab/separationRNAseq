shuffle_column <- function(metrics.df, column, number) {
  all_shuffles <- replicate(number, sample(metrics.df[[column]]))
  #now substitue shuffled column in the original "metrics" column for DESeq
  shuffled_metrics <- metrics.df
  dds.list <- list()
  for (i in 1:ncol(all_shuffles)) {
    shuffled_metrics[[column]] <- all_shuffles[, i]
    dds <- DESeqDataSetFromMatrix(countData = inputs, colData = shuffled_metrics, design = ~ParentCode + PairingAgeCat + Cohort)
    dds <- DESeq(dds)
    dds.list[[i]] <- dds
    print(i)
  }
  return(dds.list)
}

#run the DESeq contrasts for the 48hrs separated and 4wks separated conditions for all shuffles
contrast_48I <- function(dds.list) { 
  list_48I <- list()
  for (i in 1:length(dds.list)) {
    res_temp <- as.data.frame(results(dds.list[[i]], contrast = c("Cohort", "OS48I", "SS48I"), alpha = 0.05, pAdjustMethod = "fdr")) %>% rownames_to_column("genes") %>% dplyr::select(c(1,3)) %>% dplyr::rename("48IL2FC" = 2)
    list_48I[[i]] <- res_temp
  }
  return(list_48I)
  }

contrast_4I <- function(dds.list) { 
  temp_4I <- list()
  for (i in 1:length(dds.list)) {
    res_temp <- as.data.frame(results(dds.list[[i]], contrast = c("Cohort", "OS4I", "SS4I"), alpha = 0.05, pAdjustMethod = "fdr")) %>% rownames_to_column("genes") %>% dplyr::select(c(1,3)) %>% dplyr::rename("4IL2FC" = 2)
    temp_4I[[i]] <- res_temp
  }
  return(temp_4I)
}

#all 48I and 4I shuffles are compared to the true combined pair bond differential expression. "results_colPBond" runs the combined pair bond DESeq with the true data.
results_colPBond <- function(number) {
  dds_colPBond <- DESeqDataSetFromMatrix(countData = inputs, colData = metrics, design = ~ParentCode + PairingAgeCat + RPCollapsed)
  dds_colPBond <- DESeq(dds_colPBond)
  res_colPBond <- as.data.frame(results(dds_colPBond, contrast = c("RPCollapsed", "OSP", "SSP"), alpha = 0.05, pAdjustMethod = "fdr")) %>% rownames_to_column("genes") %>% dplyr::select(c(1,3)) %>% dplyr::rename("colPBondL2FC" = 2)
  temp_colPBond <- replicate(number, res_colPBond, simplify = FALSE)
  return(temp_colPBond)
}

#merge differential expression lists of the true combined pair bond and the shuffled 48I and 4I
merge_results <- function(list_48I, list_4I, list_colPBond) { 
  merged_list <- list()
  for (i in 1:length(list_colPBond)) {
    #temp[[i]] <- left_join(sample(temp_colPBond[[i]], number, replace = TRUE), sample(list_48I[[i]], replace = TRUE), by = "genes") %>% left_join(., sample(list_4I[[i]], number, replace = TRUE), by = "genes")
    merged_list[[i]] <- left_join(list_colPBond[[i]], list_48I[[i]], by = "genes") %>% left_join(., list_4I[[i]], by = "genes")
    }
  return(merged_list)
  }

#run spearman's correlations on the true combined pair bond and shuffled 48I differential expression. Returns a dataframe of Rho and pvalues
correlations_48I <- function(merged_list) {
  rho_list <- list()
  for (i in 1:length(merged_list)) { 
    rho_list[[i]] <- cor.test(merged_list[[i]][["colPBondL2FC"]], merged_list[[i]][["48IL2FC"]], method = "spearman")[["estimate"]][["rho"]]
  }
  rho_df <- do.call(rbind.data.frame, rho_list) %>% dplyr::rename("rho" = 1)
  #return(rho_df)
  pvalue_list <- list()
  for (i in 1:length(merged_list)) {
    pvalue_list[[i]] <- cor.test(merged_list[[i]][["colPBondL2FC"]], merged_list[[i]][["48IL2FC"]], method = "spearman")[["p.value"]]
  }
  pvalue_df <- do.call(rbind.data.frame, pvalue_list) %>% dplyr::rename("pvalue" = 1)
  pvalue_df <- pvalue_df %>% mutate(pvalue = -log10(pvalue))
  #return(pvalue_df)

  corr_df <- cbind(rho_df, pvalue_df)
  return(corr_df)
}

#run spearman's correlations on the true combined pair bond and shuffled 4I differential expression. Returns a dataframe of Rho and pvalues
correlations_4I <- function(merged_list) {
  rho_list <- list()
  for (i in 1:length(merged_list)) { 
    rho_list[[i]] <- cor.test(merged_list[[i]][["colPBondL2FC"]], merged_list[[i]][["4IL2FC"]], method = "spearman")[["estimate"]][["rho"]]
  }
  rho_df <- do.call(rbind.data.frame, rho_list) %>% dplyr::rename("rho" = 1)
  #return(rho_df)
  pvalue_list <- list()
  for (i in 1:length(merged_list)) {
    pvalue_list[[i]] <- cor.test(merged_list[[i]][["colPBondL2FC"]], merged_list[[i]][["4IL2FC"]], method = "spearman")[["p.value"]]
  }
  pvalue_df <- do.call(rbind.data.frame, pvalue_list) %>% dplyr::rename("pvalue" = 1)
  pvalue_df <- pvalue_df %>% mutate(pvalue = -log10(pvalue))
  #return(pvalue_df)
  
  corr_df <- cbind(rho_df, pvalue_df)
  return(corr_df)
}