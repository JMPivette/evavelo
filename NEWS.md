# evavelo 1.2.1

- Increase input file maximum size to 500MB

# evavelo 1.2.0

- Clustering: don't allow Dendogram visualisation above 50 sites. (otherwise rendering is way too slow)

# evavelo 1.1.5

- Fix bug in {openxlsx} when convering date-time on long object

# evavelo 1.1.4

- Fix a bug in distance calculation in 1.1.3 that was released too quickly. (bug introduced with latest version of {sf})

# evavelo 1.1.3

- Change max upload size from 30MB to 50MB

# evavelo 1.1.2

- Update README

# evavelo 1.1.1

- Allow `categorie_visuelle_cycliste` with plural version (Utilitaires instead of Utilitaire for example) #79

- Fix #81 to improve information on logs displayed in case of duplicated ids

- Fix #84 for better mismatch detection based on 'id_quest' value.

# evavelo 1.1.0

- Store correction proposition during geocoding phase. These proposition were only displayed in the log before. They now appear in new columns called "proposition_*" (Fix #65 #76)

- Fix #77 Missing data in one of the input worksheets now creates an error. Empty worksheet `comptages_automatiques` is allowed but creates a warning.

- Fix #75 that displayed a progress bar in the logs

- Fix a bug on check_evavelo() when a column was missing from *_post_traitement version of one of the sheets (linked to #78)

# evavelo 1.0.3

- Remove dependency to dev version of openxlsx (following release of 4.2.4)

- Fix bug when trying to display cluster graph without clicking on the button

- Fix bug when importing a file with empty "enquetes" sheet.

# evavelo 1.0.2

- Update documentation

- add examples as exported objects

# evavelo 1.0.1

- Update documentation

# evavelo 1.0.0

- Read counting data from worksheet `comptages_automatiques` 

- Add site clustering and dendogram visualization #24

# evavelo 0.8.1

- Improve UI #66

# evavelo 0.8.0

- Segmentation of `distance_dom_enq_reelle` in 3 fields `distance_dom_enq_reelle_regions` `distance_dom_enq_reelle_france` `distance_dom_enq_reelle_etranger` #63

- Detects foreign cities in `ville_res` #57. We use Open Street Map engine (Nomatim) that has a rate limit of 1 request per second. Geocoding can be slow if there are a lot of different cities outside of France. 

- Speed improvement in geocoding by using local list of cities (France and Outside)

# evavelo 0.7.1

- Detect outliers on tour_dep_transp in `check_num_outliers()` #59

# evavelo 0.7.0

- Improve documentation 

- Add a pkgdown website for documentation

# evavelo 0.6.0

- Add find_wrong_geocoding() to export all the cities that couldn't be geocoded.

- Improve UI to separate each treatment

- Add check_num_oultiers() function (#34)

- Exploratory analysis on outliers #34

# evavelo 0.5.1

- Add volume and `taille_groupe` check on multiple id_quest #53

- Detects potential multiple answers #54

# evavelo 0.5.0

- User Interface improvement #33

- Logs are now all written in french #20

# evavelo 0.4.2

- Fix issues #50 #51 #49 on categories.

- #39 : change `coherence` criteria

# evavelo 0.4.1

## Bug Fixes 

- Fix issue #37 that didn't compare correctly id_quest from `enquete` and `comptage`

- Fix issue #40 where categories were not update in `comptage`

- Fix issue #42 when id_quest was stored in numeric columns in Excel.

- Fix issue #44 when dms == 0

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
