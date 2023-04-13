filtered_GOterms <- function(GO.plot.up.ref, rows, GO.plot.up) {
ref_upreg <- GO.plot.up.ref[c(rows), ]
ref_upreg_terms <- as.data.frame(ref_upreg$ID) %>% rename("terms" = 1)

select_upreg_df <- GO.plot.up@result %>% filter(ID %in% ref_upreg_terms$terms)

forM_upreg_df <- select_upreg_df %>% select(c(2,6))  %>% mutate(p.adjust = -log10(p.adjust))

return(forM_upreg_df)}
