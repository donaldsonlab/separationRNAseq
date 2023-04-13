sankey_overlap_wDEGs <- function(neuronalDf, bulkDf, clusterColor) {
  n_cluster <- neuronalDf %>% filter(cluster_color == clusterColor)
  overlap_cluster <- bulkDf %>% filter(genes %in% n_cluster$genes)
  sum_cluster <- nrow(overlap_cluster)
  return(sum_cluster)
}
