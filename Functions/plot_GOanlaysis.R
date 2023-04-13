custGO_barplot <- function(enrichGO.plot) {
  plot <- ggplot(enrichGO.plot) +
    geom_bar(stat = "identity", aes(fill = p.adjust, x = Count, y = reorder(Description, -p.adjust)), color = "black") + 
    scale_fill_gradient(low = "#005F73", high = "whitesmoke", na.value = NA, guide = guide_colorbar(reverse = TRUE)) + 
    scale_y_discrete(labels = wrap_format(35)) + 
    theme_minimal() + 
    ylab("") + 
    xlab("Genes") +
    labs(fill = "Adj. p-value") +
    theme(axis.text.y = element_text(size = 13))
}
