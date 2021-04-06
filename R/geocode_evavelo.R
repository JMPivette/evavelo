#' Geocode an evadata object
#'
#' @param data an evadata object obtained with read_evavelo() or an xlsx file, Workbook object or URL to xlsx file
#'
#' @return a geocoded evadata object
#' @export

geocode_evavelo <- function(data){
  if(is.evadata(data) == FALSE)
    data <- read_evavelo(data)

  message("V\u00e9rification des noms de communes")
  message("---------------------------------")
  data$table_communes <- data$table_communes %>%
    geocode_table_communes()

  data$enquete <- data$enquete %>%
    geocode_cities_cp(ville_res,
                      cp_col = cp_res,
                      country_col = pays_res) %>%
    geocode_cities(ville_heb) %>%
    geocode_cities(iti_depart_itineraire) %>%
    geocode_cities(iti_arrivee_itineraire) %>%
    geocode_cities(nom_site_enq)


  attr(data, "geocoded") <- TRUE

  return(data)

}


#' Find all cities that failed to geocode when using geocode_evavelo()
#'
#' @param data a geocoded eva_data object
#'
#' @return a list of data.frames with all the cities that couldn't be geocoded
#' @export

find_wrong_geocoding <- function(data){
  if(attr(data, "geocoded") == FALSE)
    stop("data must be geocoded before getting geocoding errors")

  geocoded_fields <- c(ville_heb = "ville_heb",
                       iti_depart_itineraire = "iti_depart_itineraire",
                       iti_arrivee_itineraire = "iti_arrivee_itineraire",
                       nom_site_enq = "nom_site_enq")


  ## Check tables communes (with cog)
  tables_communes <- data$table_communes %>%
    dplyr::filter(!is.na(.data$nom_commune) & is.na(.data$latitude)) %>%
    dplyr::select(.data$nom_commune, .data$cog)

  ## Check ville_res (with cp)
  ville_res <- find_wrong_cities("ville_res",
                                 data$enquete,
                                 fields_to_keep = c("id_quest", "pays_res", "cp_res"))

  return(
    c(list(tables_communes = tables_communes,
           ville_res = ville_res),
      purrr::map(geocoded_fields,
                 find_wrong_cities,
                 enquete = data$enquete))
  )

}

#' Find cities that couldn't be geocoded by geocode_cities()
#'
#' @param var_name string corresponding to the city name variable to check
#' @param enquete data.frame containing the var_name variable and an associated *_cog
#' @param fields_to_keep fields that will be exported along with var_name
#'
#' @return a data.frame with cities that couldn't be geocoded
#' @keywords internal

find_wrong_cities <- function(var_name, enquete, fields_to_keep = "id_quest"){
  lon <- paste0(var_name, "_lon")
  enquete %>%
    dplyr::filter(!is.na(get(var_name)) & is.na(get(lon))) %>%
    dplyr::select(dplyr::all_of(fields_to_keep),
           dplyr::all_of(var_name))
}
