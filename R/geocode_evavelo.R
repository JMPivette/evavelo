#' Geocode a eva_data object
#'
#' @param data an evadata object obtained with read_evavelo() or an xlsx file, Workbook object or URL to xlsx file
#'
#' @return a geocoded eva_data object
#' @export

geocode_evavelo <- function(data){
  if(is.evadata(data) == FALSE)
    data <- read_evavelo(data)

  message("V\u00e9rification des noms de communes")
  message("---------------------------------")
  data$table_communes <- data$table_communes %>%
    geocode_table_communes()

  data$enquete <- data$enquete %>%
    geocode_cities(ville_heb) %>%
    geocode_cities(iti_depart_itineraire) %>%
    geocode_cities(iti_arrivee_itineraire) %>%
    geocode_cities(nom_site_enq) %>%
    geocode_cities_cp(ville_res,
                      cp_col = cp_res,
                      country_col = pays_res)

  attr(data, "geocoded") <- TRUE

  return(data)

}
