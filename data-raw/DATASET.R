## code to prepare `DATASET` dataset goes here

xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
evavelo_example <- list(comptage = read_comptage(xlsx_path),
                        enquete = read_enquete(xlsx_path),
                        calendrier = read_enquete(xlsx_path))

usethis::use_data(evavelo_example, overwrite = TRUE, internal = TRUE)
