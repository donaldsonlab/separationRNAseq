morpheus <- function(reference.res.df, comparison.res.df) {
  reference <- reference.res.df[,c(1,3)] %>% dplyr::rename("L2FC.ref" = 2)
  comparison <- comparison.res.df[,c(1,3)] %>% dplyr::rename("L2FC.comparison" = 2)
  
  final <- left_join(reference, comparison, by = "genes") %>% arrange(desc(L2FC.ref))
  return(final)
}
