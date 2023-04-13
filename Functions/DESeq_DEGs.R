deg_padj <- function(results.df) {
  upDEGS <- results.df %>% dplyr::filter(log2FoldChange>0.30) %>% dplyr::filter(padj < 0.05)
  downDEGS <- results.df %>% dplyr::filter(log2FoldChange<(-0.30)) %>% dplyr::filter(padj < 0.05)
  allDEGS <- rbind(upDEGS, downDEGS)
  
  upDEGS_genesonly <- as.data.frame(upDEGS$genes) %>% dplyr::rename("genes" = 1)
  downDEGS_genesonly <- as.data.frame(downDEGS$genes) %>% dplyr::rename("genes" = 1)
  allDEGS_genesonly <- rbind(upDEGS_genesonly, downDEGS_genesonly)
  
  DEGs <- list(upregulated_DEGs = upDEGS, 
               downregulated_DEGs = downDEGS,
               up.downregulated_DEGs = allDEGS,
               upreg_DEGs_genesonly = upDEGS_genesonly,
               downreg_DEGs_genesonly = downDEGS_genesonly,
               up.downregulated_DEGs_genesonly = allDEGS_genesonly)
  return(DEGs)
}

savedegs <- function(deg.list, directory) {
  upreg.deg.results <- as.data.frame(stri_list2matrix(deg.list[["upregulated_DEGs"]])) %>% dplyr::rename("genes" = 1, "baseMean" = 2, "log2FoldChange" = 3, "lfcSE" = 4, "stat" = 5, "pvalue" = 6, "padj" = 7)
  downreg.deg.results <- as.data.frame(stri_list2matrix(deg.list[["downregulated_DEGs"]])) %>% dplyr::rename("genes" = 1, "baseMean" = 2, "log2FoldChange" = 3, "lfcSE" = 4, "stat" = 5, "pvalue" = 6, "padj" = 7)
  all.deg.results <- as.data.frame(stri_list2matrix(deg.list[["up.downregulated_DEGs"]])) %>% dplyr::rename("genes" = 1, "baseMean" = 2, "log2FoldChange" = 3, "lfcSE" = 4, "stat" = 5, "pvalue" = 6, "padj" = 7)
  upreg.deg.genesOnly <- as.data.frame(deg.list[["upreg_DEGs_genesonly"]])
  dowreg.deg.genesOnly <- as.data.frame(deg.list[["downreg_DEGs_genesonly"]])
  all.deg.genesOnly <- as.data.frame(deg.list[["up.downregulated_DEGs_genesonly"]])
  
  setwd(directory)
  
  write_csv(upreg.deg.results, file = paste(deparse(substitute(deg.list)), "_upregDEGs_results", ".txt"))
  write_csv(downreg.deg.results, file = paste(deparse(substitute(deg.list)), "_downDEGs_results", ".txt"))
  write_csv(all.deg.results, file = paste(deparse(substitute(deg.list)), "_allDEGs_results", ".txt"))
  write_csv(upreg.deg.genesOnly, file = paste(deparse(substitute(deg.list)), "_upregDEGs_genesOnly", ".txt"))
  write_csv(dowreg.deg.genesOnly, file = paste(deparse(substitute(deg.list)), "_downDEGs_genesOnly", ".txt"))
  write_csv(all.deg.genesOnly, file = paste(deparse(substitute(deg.list)), "allDEGs_genesOnly", ".txt"))
}
  
L2FCdeg <- function(results.df) {
  upDEGS <- results.df %>% dplyr::filter(log2FoldChange>0.30) 
  downDEGS <- results.df %>% dplyr::filter(log2FoldChange<(-0.30))
  allDEGS <- rbind(upDEGS, downDEGS)
  
  upDEGS_genesonly <- as.data.frame(upDEGS$genes) %>% dplyr::rename("genes" = 1)
  downDEGS_genesonly <- as.data.frame(downDEGS$genes) %>% dplyr::rename("genes" = 1)
  allDEGS_genesonly <- rbind(upDEGS_genesonly, downDEGS_genesonly)
  
  DEGs <- list(upregulated_DEGs = upDEGS, 
               downregulated_DEGs = downDEGS,
               up.downregulated_DEGs = allDEGS,
               upreg_DEGs_genesonly = upDEGS_genesonly,
               downreg_DEGs_genesonly = downDEGS_genesonly,
               up.downregulated_DEGs_genesonly = allDEGS_genesonly)
  return(DEGs)
} 
  
