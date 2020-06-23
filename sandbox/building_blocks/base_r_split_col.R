
df <- data.frame(coords = c("4.6,1.3", "-53,145", '154,0', '', NA, ''))

split_coords <- function(input) {
  # input is a single character value. 
  # output is an unnamed numeric list with 2 elements: lat, long
  # if comma contained in input then split it. otherwise return NA list
  if (grepl(',', input, fixed = TRUE)) {
    split <- as.list(unlist(strsplit(input,",", fixed = TRUE)))
  }
  else split <- (list('',''))
  
  return(as.numeric(split))
}

coord_df <- tibble::as_tibble(do.call(rbind, lapply(df$coords, split_coords)))

#coord_df <- do.call(rbind, lapply(strsplit(df$coords,",", fixed = TRUE), as.numeric))

colnames(coord_df) <- c('long', 'lat')

coord_df <- coord_df[c('lat','long')] # put in right order
