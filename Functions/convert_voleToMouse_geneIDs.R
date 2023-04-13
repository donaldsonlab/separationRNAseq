#load Mouse gene names
mart_export_PVEnsembl_mouseEnsembl_mousegeneName <- read.csv("~/R/Thesis/RNAseq/Data/mart_export_PVEnsembl_mouseEnsembl_mousegeneName.txt", stringsAsFactors=FALSE)
#getting a biomart df that only contains rows that have mouse stable IDs 
biomart_mouseE <- filter(mart_export_PVEnsembl_mouseEnsembl_mousegeneName, trimws(Mouse.gene.stable.ID) !="")
library(org.Mm.eg.db)

vole_to_mouse <- function(RRHO.df) {
    genes <- as.data.frame(RRHO.df[grep("^ENSMO",RRHO.df$genes), ])
    genes <- genes %>% dplyr::rename("Gene.stable.ID" = 1)
    mouse_joined_genes <- inner_join(biomart_mouseE, genes, by = "Gene.stable.ID")
    mouseE_joined_genes <- mouse_joined_genes[,(-c(4))] %>% dplyr::rename("mouse.genes" = "Mouse.gene.stable.ID")
  
    genes <- as.data.frame(RRHO.df[-grep("^ENSMO", RRHO.df$genes), ])
    genes <- genes %>% dplyr::rename("Gene.name" = 1)
    mouse_joined_genes <- inner_join(biomart_mouseE, genes, by = "Gene.name")
    mouseGS_joined_genes <- mouse_joined_genes[,(-c(4))] %>% dplyr::rename("mouse.genes" = "Mouse.gene.stable.ID")
    
  final <- rbind(mouseE_joined_genes, mouseGS_joined_genes)
  
  final.convert <- bitr(final$mouse.genes, fromType = "ENSEMBL", toType = c("ENTREZID"), OrgDb = org.Mm.eg.db)
  return(final.convert)
}
