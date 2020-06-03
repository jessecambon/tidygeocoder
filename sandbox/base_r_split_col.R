
df <- data.frame(coords = c("4.6,1.3", "-53,145", '154,0', '', NA))

split_coords <- function(input) as.numeric(strsplit(input,",", fixed = TRUE))


coord_df <- do.call(rbind, lapply(strsplit(df$coords,",", fixed = TRUE),as.numeric))
