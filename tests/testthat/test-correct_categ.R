library(dplyr)
# Test Global function ------------------------------------------------------------------------

test_that("wrong column names gives an error", {
  ## No errors:
  expect_type(out <- correct_categ(comptage = evavelo_example$comptage,
                                   enquete = evavelo_example$enquete),
              "list")

  # Check that number of rows hasn't changed and that id_quest are in the same order
  expect_equal(out$comptage$id_quest,
               evavelo_example$comptage$id_quest)
  expect_equal(out$enquete$id_quest,
               evavelo_example$enquete$id_quest)


  ## Lots of missing columns
  expect_error(correct_categ(comptage = data.frame(id_quest = 1:3),
                             enquete = data.frame(id_quest = 1:2)))

  ## Warning if categorie_corrige are different in the same group
  enquete_modified <- evavelo_example$enquete %>%
    mutate(
      activite_motiv = if_else(id_quest == "106aA16-2",
                               "Cette activité est le but de ma randonnée", # Will change from Loisir to Utilitaire
                               activite_motiv))

  expect_warning(correct_categ(comptage = evavelo_example$comptage,
                               enquete = enquete_modified))

  ## Test that inital "categorie_corrige" from input file is not taken in account (Issue #22)
  # empty columns `categorie_corrige`(normal situation)
  out_blank <- correct_categ(comptage = evavelo_example$comptage,
                             enquete = evavelo_example$enquete %>%
                               mutate(categorie_corrige = NA_character_))
  # full columns `categorie_corrige`(if passing an already processed file)
  out_loisir <- correct_categ(comptage = evavelo_example$comptage,
                              enquete = evavelo_example$enquete %>%
                                mutate(categorie_corrige = "Loisir"))

  expect_equal(out_blank, out)
  expect_equal(out_loisir, out)

})

test_that("Answers are similar in enquete and comptage", {
  ## Example data where each comptage has a unique enquete
  out <- correct_categ(all_enquete_example$comptage,
                       all_enquete_example$enquete)
  expect_equal(out$comptages_man_post_traitements$categorie_visuelle_cycliste_corrige,
               out$enquetes_post_traitement$categorie_corrige)

})




# Test Itinerant category ---------------------------------------------------------------------
test_that("correct_itinerant works", {
  ## Input data.frame
  df <- tibble::tribble(
    ~id_quest, ~categorie, ~categorie_visuelle_cycliste, ~categorie_corrige, ~type_sortie, ~dms, ~iti_km_voyage, ~iti_experience, ~iti_dep_iti_valide, ~iti_arr_iti_valide, ~iti_depart_initial, ~iti_arrivee_final,
    "1","Itinérant", "Sportif", "empty", "Plusieurs jours", 1, NA, NA, NA, NA, NA, NA,  ## Sportif
    "2","Itinérant", "Loisir", "empty", "Plusieurs jours", 1, 120, "answer", "answer", "answer", "answer", "answer",   ## Itinerant
    "3","Loisir", "Itinérant", "empty", "Plusieurs jours", 3 , NA, NA, NA, NA, NA, NA,   ## Itinerant
    "4","Loisir", "Itinérant", "empty", "Plusieurs jours", 5, 200, NA, NA, NA, NA, NA,   ## Itinerant
    "5","Utilitaire", "Itinérant", "empty", "journée", 1, 120, NA, NA, NA, NA, NA,       ## Itinerant
    "6","Utilitaire", "Itinérant", "empty", "journée", 3, NA, NA, NA, NA, NA, NA,        ## Utilitaire
    "7","Sportif", "Sportif", "Sportif", "journée", 4, NA, NA, NA, NA, NA, NA,           ## no change
    "8","Loisir", "Itinérant", "empty", "journée", 1, NA, NA, NA, NA, NA, NA,            ## Loisir
    "9","Loisir", "Itinérant", "empty", "Plusieurs jours", NA, 200, "answer", "answer", "answer", NA, NA,  ## Itinerant (dms NA)
    "10","Itinérant", "Sportif", "empty", "Plusieurs jours", NA, NA, NA, NA, NA, NA, NA,  ## Sportif (dms NA)
    "11", "Itinérant", "Sportif", "empty", "journée", 4, 120, "answer", "answer", "answer", NA, NA ## Itinérant ()
  )

  ## Expected categorie_corrige
  expected_out <- c("Sportif",
                    "Itinérant",
                    "Itinérant",
                    "Itinérant",
                    "Itinérant",
                    "Utilitaire",
                    "Sportif",
                    "Loisir",
                    "Itinérant",
                    "Sportif",
                    "Itinérant")

  ## Test that applying function creates no error
  expect_error(corrected_df <- correct_itinerant(df),
               regexp = NA)
  ## Test that the output is a data.frame
  expect_s3_class(corrected_df, "data.frame")

  ## Test that nothing as changed except for "categorie_corrige"
  expect_equal(select(df, -categorie_corrige),
               select(corrected_df, -categorie_corrige))

  ## Test that output is as expected
  expect_equal(corrected_df$categorie_corrige,
               expected_out)

})

test_that("isi_iti_coherent helper function works", {

  out <- is_iti_coherent(dms = c(5,5,2,NA,3),
                         iti_km_voyage = c(250, 250,70, 300, NA) ,
                         iti_experience =c("a",NA, NA, "a", "a" ) ,
                         iti_dep_iti_valide = c("a",NA, "a", NA, "a") ,
                         iti_arr_iti_valide = c("a",NA, "a", NA,"a") ,
                         iti_depart_initial = c("a",NA, NA, NA, "a") ,
                         iti_arrivee_final = c("a",NA, NA, NA, "a") )

  expected_out <- c(
    TRUE, # [iti_km_voyage] / [dms] > 40 km
    TRUE, # [iti_km_voyage] / [dms] > 40 km
    FALSE, # [iti_km_voyage] / [dms] < 40 km and missing iti_*_initial and iti_experience
    FALSE, # ([iti_km_voyage]) / [dms] unavailable and missing [iti_dep_iti_valide], [iti_arr_iti_valide]
    TRUE # ([iti_km_voyage]) / [dms] unavailable but all the rest is available
  )
  expect_equal(out, expected_out)
})

# Test choices between Sportif and Loisir------------------------------------------------------
test_that("correct_spor_lois works", {
  ## Input data.frame
  df <- tibble::tribble(
    ~id_quest, ~categorie, ~categorie_visuelle_cycliste, ~categorie_corrige, ~activites, ~km_sortie, ~nb_vae, ~nb_total_velo,
    "1","Loisir", "Sportif", "empty", "Aucune", 70, 0, 1,   ## Sportif
    "2","Sportif", "Loisir", "empty", NA, 60, 0, 2,         ## Sportif
    "3","Loisir", "Sportif", "empty", "Aucune", 70, 1, 1,   ## Loisir (VAE)
    "4","Sportif", "Loisir", "empty", NA, 40, 0, 2,         ## Loisir (nb_km)
    "5","Loisir", "Sportif", "empty", "Baignade", 70, 0, 1, ## Loisir (activites)
    "6","Sportif", "Loisir", "empty", "Visite", 40, 1, 2,   ## Loisir
    "7","Sportif", "Loisir", "empty", NA, 60, NA, NA,        ## Sportif (no answer to nb_vae assumes that there is none)
    "10","Itinérant", "Itinérant", "Itinérant",  "Aucune", 70, 0, 1 ## no changes
  )

  ## Expected categorie_corrige
  expected_out <- c("Sportif",
                    "Sportif",
                    "Loisir",
                    "Loisir",
                    "Loisir",
                    "Loisir",
                    "Sportif",
                    "Itinérant")

  ## Test that applying function creates no error
  expect_error(corrected_df <- correct_spor_lois(df),
               regexp = NA)
  ## Test that the output is a data.frame
  expect_s3_class(corrected_df, "data.frame")

  ## Test that nothing as changed except for "categorie_corrige"
  expect_equal(select(df, -categorie_corrige),
               select(corrected_df, -categorie_corrige))

  ## Test that output is as expected
  expect_equal(expected_out,
               corrected_df$categorie_corrige)

})

# Test choices between Utilitaire and Loisir------------------------------------------------------

test_that("correct_util_lois works", {
  ## Input data.frame
  df <- tibble::tribble(
    ~id_quest, ~categorie, ~categorie_visuelle_cycliste, ~categorie_corrige, ~activite_motiv,
    "1","Loisir", "Utilitaire","empty", "Je fais cette activité à l'occasion de ma randonnée", # Loisir
    "2","Utilitaire", "Loisir","empty", "Je fais cette activité à l'occasion de ma randonnée", # Loisir
    "3","Loisir", "Utilitaire","empty", "Cette activité est le but de ma randonnée",           # Utilitaire
    "4","Utilitaire", "Loisir","empty", "Cette activité est le but de ma randonnée",           # Utilitaire
    "5","Loisir", "Utilitaire","empty", NA,                                                    # Utilitaire
    "6","Utilitaire", "Loisir","empty", NA,                                                    # Loisir
    "10","Sportif", "Sportif", "Sportif",  "Cette activité est le but de ma randonnée" # no changes
  )

  ## Expected categorie_corrige
  expected_out <- c("Loisir",
                    "Loisir",
                    "Utilitaire",
                    "Utilitaire",
                    "Utilitaire",
                    "Loisir",
                    "Sportif")

  ## Test that applying function creates no error
  expect_error(corrected_df <- correct_util_lois(df),
               regexp = NA)
  ## Test that the output is a data.frame
  expect_s3_class(corrected_df, "data.frame")

  ## Test that nothing as changed except for "categorie_corrige"
  expect_equal(select(df, -categorie_corrige),
               select(corrected_df, -categorie_corrige))

  ## Test that output is as expected
  expect_equal(expected_out,
               corrected_df$categorie_corrige)

})

# Test choices between Utilitaire and Sportif------------------------------------------------------

test_that("correct_util_sport works", {
  ## Input data.frame
  df <- tibble::tribble(
    ~id_quest, ~categorie, ~categorie_visuelle_cycliste, ~categorie_corrige, ~km_sortie, ~type_trajet, ~nb_vae, ~nb_total_velo,
    "1","Utilitaire", "Sportif","empty", 80, "Aller-retour", 0, 1, # Sportif
    "2","Sportif", "Utilitaire","empty", 80, "Aller-retour", 0, 1, # Sportif
    "3","Utilitaire", "Sportif","empty", 20, "Aller-retour", 1, 1, # Utilitaire
    "4","Sportif", "Utilitaire","empty", 20, "Aller-retour", 1, 1, # Utilitaire
    "5","Utilitaire", "Sportif","empty", 40, "Aller simple", 0, 1, # Sportif
    "6","Sportif", "Utilitaire","empty", 40, "Aller simple", 0, 1, # Utilitaire
    "7","Utilitaire", "Sportif","empty", 20, "Aller simple", 1, 1, # Sportif
    "8","Sportif", "Utilitaire","empty", 100, "Aller simple", 1, 1, # Utilitaire
    "10","Utilitaire", "Utilitaire", "Utilitaire", 30, "Aller-retour", 0 ,1 # no changes
  )

  ## Expected categorie_corrige
  expected_out <- c("Sportif",
                    "Sportif",
                    "Utilitaire",
                    "Utilitaire",
                    "Sportif",
                    "Utilitaire",
                    "Sportif",
                    "Utilitaire",
                    "Utilitaire")

  ## Test that applying function creates no error
  expect_error(corrected_df <- correct_util_sport(df),
               regexp = NA)
  ## Test that the output is a data.frame
  expect_s3_class(corrected_df, "data.frame")

  ## Test that nothing as changed except for "categorie_corrige"
  expect_equal(select(df, -categorie_corrige),
               select(corrected_df, -categorie_corrige))

  ## Test that output is as expected
  expect_equal(expected_out,
               corrected_df$categorie_corrige)

})


# Empty actions -------------------------------------------------------------------------------

test_that("df with no choices to be made don't throw an error ", {
  ## Create a df with no value to update
  df <- tibble::tibble(
    id_quest = 1:10,
    categorie = "Utilitaire", categorie_visuelle_cycliste = "Utilitaire",
    categorie_corrige = "Uilitaire",
    type_sortie = NA, dms = 5, iti_km_voyage = 100, km_sortie = 20, type_trajet = NA, nb_vae = 0,
    nb_total_velo = 1, activites = NA, activite_motiv = NA, iti_experience = NA,
    iti_dep_iti_valide = NA, iti_arr_iti_valide = NA, iti_depart_initial = NA,
    iti_arrivee_final = NA
    )

  expect_equal(df,
               correct_itinerant(df))

  expect_equal(df,
               correct_spor_lois(df))

  expect_equal(df,
               correct_util_lois(df))

  expect_equal(df,
               correct_util_sport(df))


})
