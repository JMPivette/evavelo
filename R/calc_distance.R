
#' Computes distance (3.1.19-3.1.21)
#'
#' This function does the following actions
#' * Assign COG codes to ville_heb_cog_lau and ville_res_cog_lau
#' * Fill  iti_dep_iti_valide and iti_arr_iti_valide with the closest city on itinary
#' * Compute distances distance_domicile_enq distance_dom_enq_reelle distance_heb_enq and distance_heb_enq_reelle
#' @param eva_data an evadata object obtained using evavelo::read_evavelo()
#'
#' @return a list of data.frames with modified columns from the original xlsx file
#' @export
#'
calc_distance <- function(eva_data){

  if(attr(eva_data, "geocoded") == FALSE)
    stop("Cannot calculate distance on evadata that is not geocoded")

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
      )
    ) %>%
    dplyr::left_join(
      select(eva_data$table_communes,
             .data$nom_commune, .data$id_section),
      by = c("iti_dep_iti_valide" = "nom_commune")
    ) %>%
    dplyr::rename(
      id_section_origine = .data$id_section
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
      )
    ) %>%
    dplyr::left_join(
      select(eva_data$table_communes,
             .data$nom_commune, .data$id_section),
      by = c("iti_arr_iti_valide" = "nom_commune")
    ) %>%
    dplyr::rename(
      id_section_dest = .data$id_section
    )


  ## Compute distance from Point_enquete---------------------
  dist_point_enq <- enquete %>%
    dplyr::transmute(
      .data$id_quest,
      distance_domicile_enq = geodist::geodist_vec(
        x1 = .data$ville_res_lon,
        y1 = .data$ville_res_lat,
        x2 = .data$nom_site_enq_lon,
        y2 = .data$nom_site_enq_lat,
        paired = TRUE,
        measure = "haversine"
      ) / 1000,
      distance_dom_enq_reelle = real_distance(.data$distance_domicile_enq),
      distance_heb_enq = geodist::geodist_vec(
        x1 = .data$ville_heb_lon,
        y1 = .data$ville_heb_lat,
        x2 = .data$nom_site_enq_lon,
        y2 = .data$nom_site_enq_lat,
        paired = TRUE,
        measure = "haversine"
      ) / 1000,
      distance_heb_enq_reelle = real_distance(.data$distance_heb_enq)
    )

  ##aggregate all information together------------------
  enquete_out <-  dist_point_enq %>%
    dplyr::left_join(iti_depart, by = "id_quest") %>%
    dplyr::left_join(iti_arrivee, by = "id_quest") %>%
    select(-dplyr::any_of("id_quest")) %>%
    cbind(enquete_cog)


  list(enquetes_post_traitement = enquete_out)
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
real_distance <- function(dist){
  dist * (1.1 + 0.3* exp(-dist/20))
}

