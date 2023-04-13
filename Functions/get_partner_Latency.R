partner_Latencies <- function(filename, tibble = FALSE) {
  #read in files to get the animal names
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  
  #extract animal names from each df
  animal_names <- c(paste0("Animal", sep = "_", x[[1]]$...3[3]), paste0("Animal", sep = "_", x[[2]]$...3[3]), paste0("Animal", sep = "_", x[[3]]$...3[3]), paste0("Animal", sep = "_", x[[4]]$...3[3]), paste0("Animal", sep = "_", x[[5]]$...3[3]), paste0("Animal", sep = "_", x[[6]]$...3[3]), paste0("Animal", sep = "_", x[[7]]$...3[3]), paste0("Animal", sep = "_", x[[8]]$...3[3]))
  
  #extract the side of the partner from each df for filtering on line 19
  partner_sides <- c(x[[1]]$...4[3], x[[2]]$...4[3], x[[3]]$...4[3], x[[4]]$...4[3], x[[5]]$...4[3], x[[6]]$...4[3], x[[7]]$...4[3], x[[8]]$...4[3])

  #read files in again and remove first 5 rows that are just junk header
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, skip = 5))
  if(!tibble) x <- lapply(x, as.data.frame)

  #filter dfs to only include instances of partner huddle
  for (i in 1:length(x)) {
    x[[i]] <- if(partner_sides[[i]] == "right") {
      x[[i]] %>% filter(Event == "Social Contact [ 1 with 3 ] in Area right while Joint Motion < 0.030")} else {
        x[[i]] %>% filter(Event == "Social Contact [ 1 with 2 ] in Area left while Joint Motion < 0.030") 
      }
  }
  
  #rename modified objects with animal names
  names(x) <- animal_names
  x
}