corr_plotting <- function(matrix, save, filename) {
  corr_of_matrix <- round(cor(matrix), 2)
  p.mat_of_matrix <- ggcorrplot::cor_pmat(corr_of_matrix)
  
  corrplot(corr_of_matrix, method = "color", type = "lower",
           diag = FALSE, tl.col = "black",
           addCoef.col = "black", number.cex = .5,
           sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
           insig = "label_sig", pch.col = "black")
  
  sig_filename <-  paste0(filename, sep = "_", "sigOnly.")
  num_filename <-  paste0(filename, sep = "_", "numOnly.")
  
  if(save == "yes") {
    pdf(file = paste0(filename, sep = "_", "sigOnly", sep = ".", "pdf"), height = 7, width = 8)
    corrplot(corr_of_matrix, p.mat = p.mat_of_matrix, method = "color", type = "lower",
             diag = FALSE, tl.col = "black",
             sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
             insig = "label_sig", pch.col = "black")
    dev.off()
    
    pdf(file = paste0(filename, sep = "_", "numOnly", sep = ".", "pdf"), height = 7, width = 8)
    corrplot(corr_of_matrix, method = "color", type = "lower",
             diag = FALSE, tl.col = "black",
             addCoef.col = "black", number.cex = .5)
    dev.off()} else {print("Not saved")
    }

}