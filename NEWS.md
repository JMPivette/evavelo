# evavelo (development version)

## Bug Fixes 

- Fix issue #37 that didn't compare correctly id_quest from `enquete` and `comptage`

- Fix issue #40 where categories were not update in `comptage`

- Fix issue #42 when id_quest was stored in numeric columns in Excel.

# evavelo 0.4.0

- Create geocoding functions `geocode_table_communes()`, `geocode_cities` and `geocode_cities_cp()`

- Create distance computation function `calc_distance()` and include in `process_evavelo()`

# evavelo 0.3.0

- Add button in the UI to download full file and logs

- Add update_wb() function

# evavelo 0.2.0

- #21 Add check on similarity between sheets and their _post_traitement equivalent.

- Fix issue #22 on `correct_categ.R` with identical responses in `categorie`

- Fix issues #23, #25

# evavelo 0.1.0
 
- Add "coherence" in category correction
 
# evavelo 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
