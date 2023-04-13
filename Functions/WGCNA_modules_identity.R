wgcna_color <- function(module, color) {
  name_list <- rep(c("color"), nrow(module))
  df <- cbind(module, name_list)
  return(df)
}
