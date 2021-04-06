
#' Computes distance (3.1.19-3.1.21)
#'
#' This function does the following actions
#' * Assign COG codes to ville_heb_cog_lau and ville_res_cog_lau
#' * Fill  iti_dep_iti_valide and iti_arr_iti_valide with the closest city on itinary
#' * Compute distances distance_domicile_enq distance_dom_enq_reelle distance_heb_enq and distance_heb_enq_reelle
#' @param eva_data an evadata object obtained using evavelo::read_evavelo()
#' @param max_dist distances greater than this value in kilometers will be discarded when looking for iti_dep_iti and iti_arr_iti and result will have NAs
#'
#' @return a list of data.frames with modified columns from the original xlsx file
#' @keywords internal
#'
calc_distance <- function(eva_data, max_dist = 30){



  if(attr(eva_data, "geocoded") == FALSE)
    stop("Cannot calculate distance on evadata that is not geocoded")

  message("Calcul des distances...")
  enquete <- eva_data$enquete

  ## Update COG values-------------------------
  enquete_cog <- enquete %>% dplyr::select(
    ville_heb_cog_lau = .data$ville_heb_cog,
    ville_res_cog_lau = .data$ville_res_cog
  )

  ##Remplir les champs [iti_dep_iti_valide] et [iti_arr_iti_valide] avec la ville d'itinéraire la plus proche----------------------

  iti_depart <- enquete %>%
    tidyr::drop_na(.data$iti_depart_itineraire_lon, .data$iti_depart_itineraire_lat) %>%
    dplyr::transmute(
      .data$id_quest, ## used as a key for later joins
      iti_dep_iti_valide = get_closest_point(
        lon_a = .data$iti_depart_itineraire_lon,
        lat_a = .data$iti_depart_itineraire_lat,
        lon_b = eva_data$table_communes$longitude,
        lat_b = eva_data$table_communes$latitude,
        id_b = eva_data$table_communes$nom_commune,
        max_dist = max_dist)
    ) %>%
    dplyr::left_join(
      dplyr::select(eva_data$table_communes,
                    .data$nom_commune, .data$id_section),
      by = c("iti_dep_iti_valide" = "nom_commune")
    ) %>%
    dplyr::rename(
      id_section_origine = .data$id_section
    )
  ## message on iti_dep too far from iti
  excluded_dep <- iti_depart %>%
    dplyr::filter(is.na(.data$iti_dep_iti_valide)) %>%
    dplyr::left_join(dplyr::select(enquete,
                                   .data$id_quest, .data$iti_depart_itineraire),
                     by = "id_quest") %>%
    dplyr::transmute(excluded = paste0(.data$iti_depart_itineraire, " (",.data$id_quest,")"))
  if(nrow(excluded_dep) !=0)
    message(
      "Les villes de d\u00e9part d\'itin\u00e9raire suivantes sont trop \u00e9loign\u00e9es de l\'itin\u00e9raire (>",
      max_dist,"km):\n\t",
      paste0(excluded_dep$excluded), collapse = "\n\t"
    )

  iti_arrivee <- enquete %>%
    tidyr::drop_na(.data$iti_arrivee_itineraire_lon, .data$iti_arrivee_itineraire_lat) %>%
    dplyr::transmute(
      .data$id_quest, ## used as a key for later joins
      iti_arr_iti_valide = get_closest_point(
        lon_a = .data$iti_arrivee_itineraire_lon,
        lat_a = .data$iti_arrivee_itineraire_lat,
        lon_b = eva_data$table_communes$longitude,
        lat_b = eva_data$table_communes$latitude,
        id_b = eva_data$table_communes$nom_commune,
        max_dist = max_dist)
    ) %>%
    dplyr::left_join(
      dplyr::select(eva_data$table_communes,
                    .data$nom_commune, .data$id_section),
      by = c("iti_arr_iti_valide" = "nom_commune")
    ) %>%
    dplyr::rename(
      id_section_dest = .data$id_section
    )

  ## message on iti_arr too far from iti
  excluded_arr <- iti_arrivee %>%
    dplyr::filter(is.na(.data$iti_arr_iti_valide)) %>%
    dplyr::left_join(dplyr::select(enquete,
                                   .data$id_quest, .data$iti_arrivee_itineraire),
                     by = "id_quest") %>%
    dplyr::transmute(excluded = paste0(.data$iti_arrivee_itineraire, " (",.data$id_quest,")"))
  if(nrow(excluded_arr) !=0)
    message(
      "Les villes d\'arriv\u00e9e d\'itin\u00e9raire suivantes sont trop \u00e9loign\u00e9es de l\'itin\u00e9raire (>",
      max_dist,"km):\n\t",
      paste0(excluded_arr$excluded), collapse = "\n\t"
    )


  ## Compute distance from Point_enquete---------------------
  ## Create an object with all regions from itinerary

  region_union_shape <- unify_regions(eva_data)

  ## Define all_points
  dist_point_enq <- enquete %>%
    dplyr::select(
      .data$id_quest,
      dplyr::starts_with(c("ville_res", "nom_site_enq", "ville_heb"))
    ) %>%
    intersection_traj_reg(region_shape = region_union_shape,
                          output_prefix = "cross_region") %>%
    intersection_traj_reg(region_shape = evavelo:::france_shape,
                          output_prefix = "cross_france")

  ## Comput all distances
  dist_point_enq <- dist_point_enq %>%
    dplyr::transmute(
      .data$id_quest,
      #Measure  distances
      distance_domicile_enq = measure_dist_vec(
        x1 = .data$ville_res_lon, y1 = .data$ville_res_lat,
        x2 = .data$nom_site_enq_lon, y2 = .data$nom_site_enq_lat
      ),
      distance_dom_enq_reelle_regions = measure_dist_vec(
        x1 = .data$cross_region_lon, y1 = .data$cross_region_lat,
        x2 = .data$nom_site_enq_lon, y2 = .data$nom_site_enq_lat
      ),
      distance_dom_enq_reelle_france = measure_dist_vec(
        x1 = .data$cross_france_lon, y1 = .data$cross_france_lat,
        x2 = .data$nom_site_enq_lon, y2 = .data$nom_site_enq_lat
        ) - .data$distance_dom_enq_reelle_regions,
      distance_heb_enq = measure_dist_vec(
        x1 = .data$ville_heb_lon, y1 = .data$ville_heb_lat,
        x2 = .data$nom_site_enq_lon, y2 = .data$nom_site_enq_lat,
      )
    ) %>%
    dplyr::mutate(
      ## Convert to "real" distance
      distance_dom_enq_reelle = real_distance(.data$distance_domicile_enq),
      distance_dom_enq_reelle_regions = real_distance(.data$distance_dom_enq_reelle_regions),
      distance_dom_enq_reelle_france = real_distance(.data$distance_dom_enq_reelle_france),
      distance_heb_enq_reelle = real_distance(.data$distance_heb_enq)
    ) %>%
    dplyr::mutate(
      ## end repartition
      distance_dom_enq_reelle_regions = dplyr::coalesce(
        .data$distance_dom_enq_reelle_regions,
        .data$distance_dom_enq_reelle
      ),
      distance_dom_enq_reelle_france = dplyr::case_when(
        is.na(distance_dom_enq_reelle_france) ~ .data$distance_dom_enq_reelle - .data$distance_dom_enq_reelle_regions,
        TRUE ~ .data$distance_dom_enq_reelle_france
      ),
      distance_dom_enq_reelle_etranger =
        .data$distance_dom_enq_reelle -
        .data$distance_dom_enq_reelle_regions -
        .data$distance_dom_enq_reelle_france
    )

  ##aggregate all information together------------------
  enquete_out <-  dist_point_enq %>%
    dplyr::left_join(iti_depart, by = "id_quest") %>%
    dplyr::left_join(iti_arrivee, by = "id_quest") %>%
    dplyr::select(-dplyr::any_of("id_quest")) %>%
    cbind(enquete_cog)


  list(enquetes_post_traitement = enquete_out)
}

#' measure distance using haversine
#'
#' Wrapper around geodist::geodist_vec that forces haversine method and paired = TRUE
#' Result is also returned in km instead of m
#'
#' @param ... argument to pass to geodist::geodist_vec()
#'
#' @return a numeric vector of distances
#' @keywords internal

measure_dist_vec <- function(...){
  geodist::geodist_vec(..., paired = TRUE, measure = "haversine")/1000
}


#' Get closest point b from points a
#'
#' Output a vector with the id of points b that is the closest to points a
#'
#'
#' @param lon_a points a longitude
#' @param lat_a points a latitude
#' @param lon_b points b longitude
#' @param lat_b points b latitude
#' @param id_b b ids used in the output. Should have the same size as lon_b and lat_b
#' @param max_dist distances greater than this value in kilometers will be discarded and result will have NAs
#' @param ... additional arguments passed to geodist::geodist_vec()
#'
#' @return a vector the same size as lon_a and lat_a
#' @keywords internal

get_closest_point <- function(lon_a, lat_a,
                              lon_b, lat_b, id_b,
                              max_dist = 30,
                              ...){
  dist_matrix <- geodist::geodist_vec(lon_a, lat_a, lon_b, lat_b, ...) %>%
    suppressMessages()
  dist_matrix[dist_matrix > max_dist*1000] <- NA ## remove distances exceeding max_dist
  idx_min <- apply(dist_matrix, 1, which_min)
  id_b[idx_min]
}



#' Compute real distance based on Indiggo formula
#'
#' @param dist a vector of distances to be updated
#'
#' @return a vector the same size as dist with computed value
#' @keywords internal
real_distance <- function(dist){
  dist * (1.1 + 0.3* exp(-dist/20))
}

#' Combined studied regions together
#'
#' Based on "tables_communes", this function lists all regions on the itinerary and create a new sf object with the union of these regions
#'
#' @param evadata a geocoded eva_data object that contains tables_communes
#'
#' @return
#' @keywords internal

unify_regions <- function(evadata){
  regions <- evadata$table_communes %>%
    tidyr::drop_na(.data$longitude, .data$latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326) %>%
    sf::st_transform(2154) %>%
    sf::st_intersects(evavelo:::regions_shape,
                      sparse = FALSE) %>%
    apply(2, any) %>%
    evavelo:::regions_shape[.,]

  message("Liste des r\u00e9gions de l\'itin\u00e9raire \u00e0 partir de tables_communes:\n\t",
          paste(regions$name, collapse = ", "))

  regions %>%
    sf::st_union()
}

#' Find the entrance point inside a defined region
#'
#' If both arrival and departure are inside region, we return NA
#'
#' @param .enquete enquete part of a geocoded eva_data object
#' @param region_shape sf object. A Polygon of the region to study
#' @param arrival names of the arrival point. the corresponding variable _lon _lat must exist in enquete
#' We assume that all arrival points are in the region
#' @param departure names of the departure point. the corresponding variable _lon _lat must exist in enquete
#' @param output_prefix prefix used for the output columns
#'
#' @return a data.frame the same length as enquete with 3 variables id_quest result_lon result_lat
#' @keywords internal
intersection_traj_reg <- function(.enquete,
                                  region_shape,
                                  arrival = "nom_site_enq",
                                  departure = "ville_res",
                                  output_prefix = "result"){


  region_boundary <- sf::st_boundary(region_shape)


  all_geom_points <- .enquete %>%
    dplyr::select(
      .data$id_quest,
      dplyr::starts_with(c(arrival, departure)) &
        dplyr::ends_with(c("_lon", "_lat"))
    ) %>%
    tidyr::pivot_longer(
      dplyr::matches("lon|lat|geocoded"),
      names_to = c("city", ".value"),
      names_pattern = "([[:graph:]]+)_([[:alnum:]]+)") %>%
    dplyr::mutate(is_geocoded = !is.na(.data$lon)) %>%
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326,
                 na.fail = FALSE) %>%
    sf::st_transform(2154) %>%
    dplyr::mutate(
      in_region = sf::st_intersects(., region_shape, sparse = FALSE)[,1]
    )

  ## Find crossing point with region

  cross_region <- all_geom_points %>%
    dplyr::group_by(.data$id_quest) %>%
    dplyr::filter(all(.data$is_geocoded), any(.data$in_region ==FALSE)) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_intersection(region_boundary) %>%
    suppressWarnings() %>%
    as.data.frame() %>%
    dplyr::left_join(
      all_geom_points %>%
        dplyr::filter(.data$city == departure) %>%
        dplyr::select(.data$id_quest) %>%
        as.data.frame(),
      by = "id_quest") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      cross_point = sf::st_cast(sf::st_nearest_points(.data$geometry.x,
                                                      .data$geometry.y),
                                "POINT")[[1]] %>%
        list()
    ) %>%
    dplyr::select(.data$id_quest, .data$cross_point) %>%
    sf::st_as_sf(crs = 2154) %>%
    sf::st_transform(4326)

  ## Format output

  result <- sf::st_coordinates(cross_region) %>%
    as.data.frame() %>%
    setNames(paste(output_prefix,
                   c("lon", "lat"),
                   sep = "_")) %>%
    dplyr::bind_cols(id_quest = cross_region$id_quest, .) %>%
    dplyr::left_join(dplyr::select(.enquete,
                                   .data$id_quest),
                     .,
                     by = "id_quest") %>%
    dplyr::select(-dplyr::any_of("id_quest"))


  cbind(.enquete, result)

}
