rrho_quad_genes <- function(rrhoPlot, newname) {
  newname.uu <- as.data.frame(stri_list2matrix(rrhoPlot[["genelist_uu"]])) %>% dplyr::rename("list1_uu" = 1, "list2_uu" = 2, "overlap_uu" = 3)
  newname.dd <- as.data.frame(stri_list2matrix(rrhoPlot[["genelist_dd"]])) %>% dplyr::rename("list1_dd" = 1, "list2_dd" = 2, "overlap_dd" = 3)
  newname.du <- as.data.frame(stri_list2matrix(rrhoPlot[["genelist_du"]])) %>% dplyr::rename("list1_du" = 1, "list2_du" = 2, "overlap_du" = 3)
  newname.ud <- as.data.frame(stri_list2matrix(rrhoPlot[["genelist_ud"]])) %>% dplyr::rename("list1_ud" = 1, "list2_ud" = 2, "overlap_ud" = 3)
  
  output <- list(newname.uu, newname.dd, newname.du, newname.ud)
  names(output) <- c("UU genes", "DD genes", "DU genes", "UD genes")
  return(output)
}