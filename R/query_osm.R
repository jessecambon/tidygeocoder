#' query_osm
#'
#' Query OSM for an address's latatude and longitude coordinates.
#' Adapted from tmaptools::geocode_OSM. 
#'
#' @references \url{https://github.com/mtennekes/tmaptools}
#'
#' @param address - single line address.
#' @param server - url to OSM nominatim.
#' @return list containing latitude and longitude coordinates.
#'
#' @importFrom XML xmlTreeParse xmlChildren xmlRoot xmlAttrs

query_osm <- function(address,server="http://nominatim.openstreetmap.org") {

	# Build search query.
	query <- gsub(" ", "+", enc2utf8(address), fixed = TRUE)
	url <- paste0(server, "/search?q=", query, 
		       "&format=xml&polygon=0&addressdetails=0")

	# Download search results.
	tmpfile <- tempfile()
	suppressWarnings(download.file(url, destfile = tmpfile, 
				       mode= "wb", quiet = TRUE))

	# Parse search results.
	doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
	unlink(tmpfile)
	res <- xmlChildren(xmlRoot(doc))

	# If no results found, return NULL.
	if (length(res)==0) {
		message(paste("No results found for \"", address, "\".", sep=""))
		return(NULL)
	}

	# Extract latitude and longitude coordinates.
	sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", 
		      "display_name", "class", "type", "importance", "icon")
	search_result <- xmlAttrs(res[[1]])
	search_result_id <- search_result[sn_names]
	names(search_result_id) <- sn_names # in case of missings
	Encoding(search_result_id) <- "UTF-8"
	lat_lon <- search_result[c("lat","lon")]
	coords <- as.numeric(lat_lon)
	names(coords) <- c("lat", "lon")
	result <- list("query"=query,"coords"=coords)
	return(result)
}
