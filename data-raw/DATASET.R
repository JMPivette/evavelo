## code to prepare `DATASET` dataset goes here


## Dataset example for tests
xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
evavelo_example <- read_evavelo(xlsx_path)


## Minimal expected column names for each worksheet
comptage_colnames <- c(
  "id_quest", "categorie_visuelle",
  "categorie_visuelle_cycliste", "categorie_breve",
  "id_site_enq", "date_enq")

enquete_colnames <- c(
  "id_quest", "categorie", "categorie_corrige",
  "type_sortie", "dms", "km_sortie", "type_trajet",
  "nb_vae", "nb_total_velo", "activites", "activite_motiv", "id_site_enq", "date_enq",
  "iti_km_voyage", "iti_depart_initial", "iti_depart_itineraire", "iti_arrivee_itineraire",
  "iti_arrivee_final", "iti_experience",
  "distance_domicile_enq", "distance_heb_enq","distance_dom_enq_relle", "distance_heb_enq_reelle",
  "iti_dep_iti_valide", "iti_arr_iti_valide",
  "ville_heb_cog_lau", "ville_res_cog_lau")

calendrier_colnames <- c("id_site_enq", "date_enq")


usethis::use_data(evavelo_example, comptage_colnames, enquete_colnames, calendrier_colnames,
                  overwrite = TRUE, internal = TRUE)
