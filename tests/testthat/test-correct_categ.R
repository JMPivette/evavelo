




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
