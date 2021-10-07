# number duplicates in query_names (for geocode_cascade())

query_names <- c('osm', 'census', 'osm', 'arcgis', 'census', 'census')



for (name in unique(query_names)) {
  # if the given name occurs more than once in query_names then iterate through and add numbers
  if ((sum(query_names == name)) > 1) {
    i <- 1
    dup_num <- 1
    for (i in 1:length(query_names)) {
      if (query_names[[i]] == name) {
        query_names[[i]] <- paste0(query_names[[i]], as.character(dup_num), collapse = '')
        dup_num <- dup_num + 1
      }
    }
  }
  print(paste0("name = ", name))
  print(query_names)
}

print(query_names)