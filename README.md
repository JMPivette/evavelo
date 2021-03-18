<!-- README.md is generated from README.Rmd. Please edit that file -->

# evavelo

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Codecov test coverage](https://codecov.io/gh/JMPivette/evavelo/branch/master/graph/badge.svg)](https://codecov.io/gh/JMPivette/evavelo?branch=master)

<!-- badges: end -->

Le but du package {evavelo} est d'appliquer des traitements automatiques sur des fichiers d'enquêtes liés à la méthode EVA-VELO (Méthode nationale evaluation des retombées des vélo-routes) développée par **Vélo et Territoires**

## Utilisation

Ce projet est en cours de developpement et ne peut pas encore être utilisé.

Il sera composé de fonctions de traitements de fichier mais aussi d'une interface web.

### Traitement enquête Eva-Velo

La function principale `process_evavelo()` permet de traiter un fichier d'enquête xlsx avec la méthode Eva-Vélo.

``` r
xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")

##eva_data <- evavelo::read_evavelo(xlsx_path)
eva_data_processed <- evavelo::process_evavelo(xlsx_path)
#> Vérification des noms de communes
#> ---------------------------------
#> ...Verification de table_communes.............
#> Les communes suivantes n'existent plus et doivent etre remplacees par les communes nouvelles:
#>  Hiers-Brouage(17189) -> Marennes-Hiers-Brouage(17219)
#>  Marennes(17219) -> Marennes-Hiers-Brouage(17219)
#>  Locmaria-Berrien(29129) -> Poullaouen(29227)
#>  Les Forges(56059) -> Forges de Lanouée(56102)
#>  Lanouée(56102) -> Forges de Lanouée(56102)
#>  Château-d Olonne(85060) -> Les Sables-d'Olonne(85194)
#>  Olonne-sur-Mer(85166) -> Les Sables-d'Olonne(85194)
#> Impossible de reconnaitre les communes suivantes:
#>  
#>  Bretignolles-sur-Mer(85035)
#> 
#> ...Verification de ville_heb.............
#> Impossible de trouver les communes suivantes:
#>   Camping-municipal-henvic
#>   Chaix
#>   Île de Ré
#> Interpretation de communes mal nommees:
#>  Chatelaillon -> Châtelaillon-Plage (17094) 
#>  Saint-Pol -> Saint-Pol-de-Léon (29259)
#> 
#> ...Verification de iti_depart_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Uméa
#> Interpretation de communes mal nommees:
#>  St Brieuc -> Saint-Brieuc (22278) 
#>  Vielle st Girons -> Vielle-Saint-Girons (40326)
#> 
#> ...Verification de iti_arrivee_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Dortmund
#> 
#> ...Verification de nom_site_enq.............
#> Interpretation de communes mal nommees:
#>  St Pol de Léon -> Saint-Pol-de-Léon (29259)
#> 
#> ...Verification de ville_res.............
#> Les villes suivantes ont ete ignorees. Propositions de corrections:
#>  Angoulins-sur-Mer (17890) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17691) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17960) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17690) ->    Angoulins (17690)
#>  Arboras (34151) ->  Arboras (34150)
#>  Avallon (89201) ->  Avallon (89200)
#>  Avanton (86171) ->  Avanton (86170)
#>  Cachan (94231) ->   Cachan (94230)
#>  Chatelaillon (17340) ->     Châtelaillon-Plage (17340)
#>  Fouras (17451) ->   Fouras (17450)
#>  Le Mans (72700) ->  Le Mans (72100)
#>  Plouénan (39420) ->     Plouénan (29420)
#>  Rennes (35160) ->   Rennes (35000)
#>  Rivedoux (17940) ->     Rivedoux-Plage (17940)
#>  Rivedoux-Plage (17590) ->   Rivedoux-Plage (17940)
#>  Saint-Germain-du-Puy (18391) ->     Saint-Germain-du-Puy (18390)
#>  Saint-Joseph (97980) ->     Saint-Joseph (97480)
#>  Saint-Pol-de-Léon (22250) ->    Saint-Pol-de-Léon (29250)
#> Correction de catégories pour 17 questionnaires ....
#> Il n'a pas été possible de corriger les catégories de 3 questionnaire(s).
#> La catégorie du déclarant sera utilisée:
#>  141A35, 38A45, 38A46
lapply(eva_data_processed, head)
#> $comptages_man_post_traitements
#>   id_quest categorie_visuelle_cycliste_corrige
#> 1     <NA>                                <NA>
#> 2     <NA>                              Loisir
#> 3     <NA>                                <NA>
#> 4     <NA>                              Loisir
#> 5   106aA1                             Sportif
#> 6     <NA>                                <NA>
#> 
#> $enquetes_post_traitement
#>   id_quest categorie_corrige distance_domicile_enq distance_dom_enq_reelle
#> 1   106aA1           Sportif              0.000000                0.000000
#> 2   106aA2        Utilitaire                    NA                      NA
#> 3   106aA3           Sportif              4.805045                6.419199
#> 4   106aA4            Loisir              0.000000                0.000000
#> 5   106aA5           Sportif                    NA                      NA
#> 6   106aA6            Loisir            157.663372              173.447543
#>   distance_heb_enq distance_heb_enq_reelle iti_dep_iti_valide
#> 1               NA                      NA               <NA>
#> 2               NA                      NA               <NA>
#> 3               NA                      NA               <NA>
#> 4               NA                      NA               <NA>
#> 5               NA                      NA               <NA>
#> 6               NA                      NA               <NA>
#>   id_section_origine iti_arr_iti_valide id_section_dest ville_heb_cog_lau
#> 1                 NA               <NA>              NA              <NA>
#> 2                 NA               <NA>              NA              <NA>
#> 3                 NA               <NA>              NA              <NA>
#> 4                 NA               <NA>              NA              <NA>
#> 5                 NA               <NA>              NA              <NA>
#> 6                 NA               <NA>              NA              <NA>
#>   ville_res_cog_lau
#> 1             17300
#> 2              <NA>
#> 3             17028
#> 4             17300
#> 5              <NA>
#> 6             33318
```

### Traitement étapes par étapes

Il est possible de décomposer le traitement étapes par étapes:

``` r
eva_data <- evavelo::read_evavelo(xlsx_path)

eva_data_geocoded <- evavelo::geocode_evavelo(eva_data)
#> Vérification des noms de communes
#> ---------------------------------
#> ...Verification de table_communes.............
#> Les communes suivantes n'existent plus et doivent etre remplacees par les communes nouvelles:
#>  Hiers-Brouage(17189) -> Marennes-Hiers-Brouage(17219)
#>  Marennes(17219) -> Marennes-Hiers-Brouage(17219)
#>  Locmaria-Berrien(29129) -> Poullaouen(29227)
#>  Les Forges(56059) -> Forges de Lanouée(56102)
#>  Lanouée(56102) -> Forges de Lanouée(56102)
#>  Château-d Olonne(85060) -> Les Sables-d'Olonne(85194)
#>  Olonne-sur-Mer(85166) -> Les Sables-d'Olonne(85194)
#> Impossible de reconnaitre les communes suivantes:
#>  
#>  Bretignolles-sur-Mer(85035)
#> 
#> ...Verification de ville_heb.............
#> Impossible de trouver les communes suivantes:
#>   Camping-municipal-henvic
#>   Chaix
#>   Île de Ré
#> Interpretation de communes mal nommees:
#>  Chatelaillon -> Châtelaillon-Plage (17094) 
#>  Saint-Pol -> Saint-Pol-de-Léon (29259)
#> 
#> ...Verification de iti_depart_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Uméa
#> Interpretation de communes mal nommees:
#>  St Brieuc -> Saint-Brieuc (22278) 
#>  Vielle st Girons -> Vielle-Saint-Girons (40326)
#> 
#> ...Verification de iti_arrivee_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Dortmund
#> 
#> ...Verification de nom_site_enq.............
#> Interpretation de communes mal nommees:
#>  St Pol de Léon -> Saint-Pol-de-Léon (29259)
#> 
#> ...Verification de ville_res.............
#> Les villes suivantes ont ete ignorees. Propositions de corrections:
#>  Angoulins-sur-Mer (17890) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17691) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17960) ->    Angoulins (17690)
#>  Angoulins-sur-Mer (17690) ->    Angoulins (17690)
#>  Arboras (34151) ->  Arboras (34150)
#>  Avallon (89201) ->  Avallon (89200)
#>  Avanton (86171) ->  Avanton (86170)
#>  Cachan (94231) ->   Cachan (94230)
#>  Chatelaillon (17340) ->     Châtelaillon-Plage (17340)
#>  Fouras (17451) ->   Fouras (17450)
#>  Le Mans (72700) ->  Le Mans (72100)
#>  Plouénan (39420) ->     Plouénan (29420)
#>  Rennes (35160) ->   Rennes (35000)
#>  Rivedoux (17940) ->     Rivedoux-Plage (17940)
#>  Rivedoux-Plage (17590) ->   Rivedoux-Plage (17940)
#>  Saint-Germain-du-Puy (18391) ->     Saint-Germain-du-Puy (18390)
#>  Saint-Joseph (97980) ->     Saint-Joseph (97480)
#>  Saint-Pol-de-Léon (22250) ->    Saint-Pol-de-Léon (29250)

eva_data_processed <- evavelo::process_evavelo(eva_data_geocoded)
#> Correction de catégories pour 17 questionnaires ....
#> Il n'a pas été possible de corriger les catégories de 3 questionnaire(s).
#> La catégorie du déclarant sera utilisée:
#>  141A35, 38A45, 38A46

lapply(eva_data_processed, head)
#> $comptages_man_post_traitements
#>   id_quest categorie_visuelle_cycliste_corrige
#> 1     <NA>                                <NA>
#> 2     <NA>                              Loisir
#> 3     <NA>                                <NA>
#> 4     <NA>                              Loisir
#> 5   106aA1                             Sportif
#> 6     <NA>                                <NA>
#> 
#> $enquetes_post_traitement
#>   id_quest categorie_corrige distance_domicile_enq distance_dom_enq_reelle
#> 1   106aA1           Sportif              0.000000                0.000000
#> 2   106aA2        Utilitaire                    NA                      NA
#> 3   106aA3           Sportif              4.805045                6.419199
#> 4   106aA4            Loisir              0.000000                0.000000
#> 5   106aA5           Sportif                    NA                      NA
#> 6   106aA6            Loisir            157.663372              173.447543
#>   distance_heb_enq distance_heb_enq_reelle iti_dep_iti_valide
#> 1               NA                      NA               <NA>
#> 2               NA                      NA               <NA>
#> 3               NA                      NA               <NA>
#> 4               NA                      NA               <NA>
#> 5               NA                      NA               <NA>
#> 6               NA                      NA               <NA>
#>   id_section_origine iti_arr_iti_valide id_section_dest ville_heb_cog_lau
#> 1                 NA               <NA>              NA              <NA>
#> 2                 NA               <NA>              NA              <NA>
#> 3                 NA               <NA>              NA              <NA>
#> 4                 NA               <NA>              NA              <NA>
#> 5                 NA               <NA>              NA              <NA>
#> 6                 NA               <NA>              NA              <NA>
#>   ville_res_cog_lau
#> 1             17300
#> 2              <NA>
#> 3             17028
#> 4             17300
#> 5              <NA>
#> 6             33318
```
