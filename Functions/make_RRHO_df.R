RRHO_math <- function(deseqDF) {
  #this part does the math to get the rankValue used for RRHO
  rrho_ranks <- deseqDF[, c(1,3,6)] %>% mutate(rankValue = -1 * log10(pvalue) * sign(log2FoldChange)) %>% arrange(desc(rankValue))
  
  #make dfs that only have gene name and rankValue
  rrho_output_df <- rrho_ranks[,c(1,4)]
  return(rrho_output_df)
}