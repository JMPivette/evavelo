
# Test Global function ------------------------------------------------------------------------

test_that("correct_categ works", {
  ## Import and clean data (temporary waiting for functions to do it automaticaly)
  xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
  #xlsx_path <- here::here("inst", "example-data", "02_simplified.xlsx")
  ## Sheet: Comptage Post Traitements-------------
  comptage <- openxlsx::read.xlsx(xlsx_path,
                                  sheet = "comptages_man_post_traitements")

  comptage <- comptage %>%
    dplyr::select(starts_with("[")) %>%  ## Don't take in account "old" col names that could create duplicated entries
    janitor::clean_names() %>%
    dplyr::mutate(categorie_breve = as.character(categorie_breve)) %>%
    dplyr::mutate(categorie_visuelle_cycliste = stringr::str_remove(categorie_visuelle_cycliste,
                                                             "s$"))

  ## Sheet: Enquetes Post Traitements-----------
  enquete <- openxlsx::read.xlsx(xlsx_path,
                                 sheet = "enquetes_post_traitement")
  enquete <- enquete %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      type_sortie = dplyr::case_when(
        type_sortie == "Demi journée" ~ "Demi-journée",
        type_sortie == "La journée" ~ "Journée",
        TRUE ~ type_sortie)
    ) %>%
    dplyr::mutate(
      type_trajet = dplyr::case_when(
        stringr::str_detect(type_trajet, "simple") ~ "Trajet simple",
        stringr::str_detect(type_trajet, "retour") ~ "Aller-retour",
        TRUE ~ type_trajet)
    )

  expect_type(correct_categ(comptage, enquete),
              "list")

})

test_that("wrong column names gives an error", {
  ## Lots of missing columns
  expect_error(correct_categ(comptage = data.frame(id_quest = 1:3),
                             enquete = data.frame(id_quest = 1:2)))

})



# Test Itinerant category ---------------------------------------------------------------------
test_that("correct_intinerant works", {
  ## Input data.frame
  df <- tibble::tribble(
    ~id_quest, ~categorie, ~categorie_visuelle_cycliste, ~categorie_corrige, ~type_sortie, ~dms, ~iti_km,
    "1","Itinérant", "Sportif", "empty", "Plusieurs jours", 1, NA,   ## Sportif
    "2","Itinérant", "Loisir", "empty", "Plusieurs jours", 1, 120,   ## Itinerant
    "3","Loisir", "Itinérant", "empty", "Plusieurs jours", 3 , NA,   ## Itinerant
    "4","Loisir", "Itinérant", "empty", "Plusieurs jours", 5, 200,   ## Itinerant
    "5","Utilitaire", "Itinérant", "empty", "journée", 1, 120,       ## Itinerant
    "6","Utilitaire", "Itinérant", "empty", "journée", 3, NA,        ## Utilitaire
    "7","Sportif", "Sportif", "Sportif", "journée", 4, NA,           ## no change
    "8","Loisir", "Itinérant", "empty", "journée", 1, NA,            ## Loisir
    "9","Loisir", "Itinérant", "empty", "Plusieurs jours", NA, 200,  ## Itinerant (dms NA)
    "10","Itinérant", "Sportif", "empty", "Plusieurs jours", NA, NA  ## Sportif (dms NA)
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
                    "Sportif")

  ## Test that applying function creates no error
  expect_error(corrected_df <- correct_itinerant(df),
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
    type_sortie = NA, dms = 5, iti_km = 100, km_sortie = 20, type_trajet = NA, nb_vae = 0,
    nb_total_velo = 1, activites = NA, activite_motiv = NA
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
