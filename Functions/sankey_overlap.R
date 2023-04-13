sankey_overlap <- function(neuronalDf, bulkDf, clusterColor) {
  n_cluster <- neuronalDf %>% filter(cluster_color == clusterColor)
  overlap_cluster <- bulkDf %>% filter(genes %in% n_cluster$genes)
  sum_cluster <- table(unlist(overlap_cluster$cluster_color))
  return(sum_cluster)
}
