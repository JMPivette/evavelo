## code to prepare `DATASET` dataset goes here
library(evavelo)

## Dataset example for tests
xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
evavelo_example <- read_evavelo(xlsx_path)
evavelo_example_geocoded <- geocode_evavelo(evavelo_example)

## id_quest_mismatch
mismatch_path <- system.file("example-data/03_id_quest_mismatch.xlsx", package = "evavelo")
quest_mismatch_example <- read_evavelo(mismatch_path)

## All lines have an associated enquete
all_enquete_path <- system.file("example-data/04_enquete_everywhere.xlsx", package = "evavelo")
all_enquete_example <- read_evavelo(all_enquete_path)

## Minimal expected column names for each worksheet
comptage_colnames <- c(
  "id_quest", "categorie_visuelle",
  "categorie_visuelle_cycliste", "categorie_breve",
  "id_site_enq", "date_enq", "volume_manuel")

enquete_colnames <- c(
  "id_quest", "id_site_enq", "date_enq",
  "categorie", "categorie_corrige",
  "type_sortie", "dms", "km_sortie", "type_trajet",
  "nb_vae", "nb_total_velo", "activites", "activite_motiv", "activites_aucune","id_site_enq", "date_enq",
  "iti_km_voyage", "iti_depart_initial", "iti_depart_itineraire", "iti_arrivee_itineraire",
  "iti_arrivee_final", "iti_experience",
  "distance_domicile_enq", "distance_heb_enq","distance_dom_enq_reelle", "distance_heb_enq_reelle",
  "mode_transp_jour", "dist_transp_jour",
  "iti_dep_iti_valide", "iti_arr_iti_valide",
  "ville_heb_cog_lau", "ville_res_cog_lau",
  "ville_res", "cp_res", "pays_res", "ville_heb",
  "id_section_origine", "id_section_dest", "taille_totale_groupe",
  "mode_heb_regroupe", "revenu",
  "tour_dep_alim", "tour_dep_activites", "tour_dep_souvenirs", "tour_dep_location", "tour_dep_autres",
  "tour_dep_to_jour", "tour_dep_heb")

calendrier_colnames <- c("id_site_enq", "date_enq")


## SF data
## Region
regions_shape <- raster::getData(name="GADM", country="FRA", level=1) %>%
  sf::st_as_sf() %>% #Convert to sf
  sf::st_transform(crs = 2154) %>%  # Project in Lambert 93
  sf::st_simplify(dTolerance = 1000) %>% # reduce size
  dplyr::transmute(id = row_number(),name = NAME_1, geometry) # keep relevant information


## France
france_shape <- raster::getData(name="GADM", country="FRA", level=0) %>%
  sf::st_as_sf() %>% #Convert to sf
  sf::st_transform(france, crs = 2154) %>%  # Project in Lambert 93
  sf::st_simplify(dTolerance = 1000)

## All cities World
world_cities <- maps::world.cities %>%
  dplyr::mutate(country = countrycode::countryname(.data$country.etc)) %>%
  tidyr::drop_na(country) %>%
  dplyr::transmute(
    city = .data$name,
    country = countrycode::countryname(.data$country.etc),
    lon = .data$long,
    .data$lat,
    .data$pop
  )

## All cities France
france_cities <- system.file("extdata", "laposte_hexasmal.csv", package = "evavelo") %>%
  data.table::fread(colClasses = "character") %>%
  filter(coordonnees_gps != "")%>%
  tidyr::separate(coordonnees_gps,
                  into = c("lat", "lon"),
                  sep = ",",
                  convert = TRUE) %>%
  dplyr::filter(stringr::str_detect(Code_postal, "^97|98", negate = TRUE)) ## Remove Dom-Tom

france_cities <- france_cities %>%
  dplyr::bind_rows(
    france_cities %>%
      dplyr::filter(Nom_commune != Libellé_d_acheminement) %>%
      dplyr::mutate(Nom_commune = Libellé_d_acheminement)
  )%>%
  dplyr::select(city = Nom_commune,
                cp = Code_postal,
                cog = Code_commune_INSEE,
                lon, lat) %>%
  dplyr::distinct()

## France cities unique names (used when searching for cities without having a code)
## Cities with the same name are removed expect if in same departement (ex: Paris)
france_cities_unique_names <- france_cities %>%
  mutate(dep = stringr::str_sub(cp, end = 2)) %>%
  group_by(city) %>%
  mutate(n_dupl = length(unique(dep))) %>%
  filter(n_dupl == 1) %>%
  slice_head() %>%
  ungroup() %>%
  select(city, lon, lat, cog)


usethis::use_data(evavelo_example, evavelo_example_geocoded,
                  quest_mismatch_example, all_enquete_example,
                  comptage_colnames, enquete_colnames, calendrier_colnames,
                  regions_shape, france_shape,
                  world_cities, france_cities, france_cities_unique_names,
                  overwrite = TRUE, internal = TRUE)
