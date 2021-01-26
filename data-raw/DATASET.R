## code to prepare `DATASET` dataset goes here


## Dataset example for tests
xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
evavelo_example <- list(comptage = evavelo::read_comptage(xlsx_path),
                        enquete = evavelo::read_enquete(xlsx_path),
                        calendrier = evavelo::read_enquete(xlsx_path))

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
  "iti_arrivee_final", "iti_experience")

calendrier_colnames <- c("id_site_enq", "date_enq")


usethis::use_data(evavelo_example, comptage_colnames, enquete_colnames, calendrier_colnames,
                  overwrite = TRUE, internal = TRUE)
