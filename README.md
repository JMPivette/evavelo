
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Eva-Scan

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/JMPivette/evavelo/branch/master/graph/badge.svg)](https://codecov.io/gh/JMPivette/evavelo?branch=master)

<!-- badges: end -->

## Résumé

Le package `evavelo` applique des traitements automatiques sur les
fichiers d’enquêtes liés à la méthode
[EVA-VELO](https://www.velo-territoires.org/ressources/categorie/publication-reference/?resource-id=18202#resource-eva-velo)
(Méthode nationale evaluation des retombées des vélo-routes) développée
par **Vélo et Territoires**

Ce projet est en cours de developpement et ne peut pas encore être
utilisé. Il est composé de fonctions de traitements de fichier mais
aussi d’une interface web.

## Installation

``` r
# Development version from GitHub
# install.packages("devtools")
devtools::install_github("JMPivette/evavelo")
```

## Traitement d’enquête Eva-Velo

La function principale `process_evavelo()` permet de traiter un fichier
d’enquête xlsx avec la méthode Eva-Vélo. Cette function va notamment:

-   Vérifier l’intégrité du fichier
-   Remplir le champ `categorie_corrige`
-   Calculer les différentes distances par rapport au point d’enquête
-   Trouver les codes COG des communes de résidence et d’herbergement
-   Remplir le champ `id_section_origine`

``` r
xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")


eva_data_processed <- evavelo::process_evavelo(xlsx_path)

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
#> 1   106aA1           Sportif              0.468112               0.6521081
#> 2   106aA2        Utilitaire                    NA                      NA
#> 3   106aA3           Sportif              5.112044               6.8109554
#> 4   106aA4            Loisir              0.468112               0.6521081
#> 5   106aA5           Sportif                    NA                      NA
#> 6   106aA6            Loisir            157.044731             172.7675257
#>   distance_dom_enq_reelle_regions distance_dom_enq_reelle_france
#> 1                       0.6521081                              0
#> 2                              NA                             NA
#> 3                       6.8109554                              0
#> 4                       0.6521081                              0
#> 5                              NA                             NA
#> 6                     172.7675257                              0
#>   distance_dom_enq_reelle_etranger distance_heb_enq distance_heb_enq_reelle
#> 1                                0               NA                      NA
#> 2                               NA               NA                      NA
#> 3                                0               NA                      NA
#> 4                                0               NA                      NA
#> 5                               NA               NA                      NA
#> 6                                0               NA                      NA
#>   iti_dep_iti_valide id_section_origine iti_arr_iti_valide id_section_dest
#> 1               <NA>                 NA               <NA>              NA
#> 2               <NA>                 NA               <NA>              NA
#> 3               <NA>                 NA               <NA>              NA
#> 4               <NA>                 NA               <NA>              NA
#> 5               <NA>                 NA               <NA>              NA
#> 6               <NA>                 NA               <NA>              NA
#>   ville_heb_cog_lau ville_res_cog_lau
#> 1              <NA>             17300
#> 2              <NA>              <NA>
#> 3              <NA>             17028
#> 4              <NA>             17300
#> 5              <NA>              <NA>
#> 6              <NA>             33318
```

## Traitement étapes par étapes

Il est possible de décomposer tous les traitement effectués par
`process_evavelo()` étape par étape:

### Lecture du fichier

La fonction `read_evavelo()` permet de lire un fichier xlsx et de
récupérer un objet avec toutes les informations intéressantes pour le
traitement.

``` r
eva_data <- evavelo::read_evavelo(xlsx_path)
```

### Géocodage des villes

La fonction `geocode_evavelo()` permet de modifier l’objet en géocodant
les noms de communes

``` r
eva_data_geocoded <- evavelo::geocode_evavelo(eva_data)
#> Vérification des noms de communes
#> ---------------------------------
#> ...Vérification de table_communes.............
#> Les communes suivantes n'existent plus et doivent etre remplacées par les communes nouvelles:
#>  Hiers-Brouage(17189) -> Marennes-Hiers-Brouage(17219)
#>  Marennes(17219) -> Marennes-Hiers-Brouage(17219)
#>  Locmaria-Berrien(29129) -> Poullaouen(29227)
#>  Les Forges(56059) -> Forges de Lanouée(56102)
#>  Lanouée(56102) -> Forges de Lanouée(56102)
#>  Château-d'Olonne(85060) -> Les Sables-d'Olonne(85194)
#>  Olonne-sur-Mer(85166) -> Les Sables-d'Olonne(85194)
#> 
#> ...Vérification de ville_res.............
#> Les villes suivantes ont ete ignorées. Propositions de corrections:
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
#> Interprétation de noms de communes étrangères:
#>  Chine : Benjing -> Beijing
#> Villes inconnues:
#>  Noor mout (Pays-Bas)
#> 
#> ...Vérification de ville_heb.............
#> Impossible de trouver les communes suivantes:
#>   Camping-municipal-henvic
#>   Chaix
#>   Île de Ré
#> Interpretation de communes mal nommées:
#>  Chatelaillon -> Châtelaillon-Plage (17094) 
#>  Saint-Pol -> Saint-Pol-de-Léon (29259)
#> 
#> ...Vérification de iti_depart_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Uméa
#> 
#> ...Vérification de iti_arrivee_itineraire.............
#> Impossible de trouver les communes suivantes:
#>   Dortmund
#> 
#> ...Vérification de nom_site_enq.............
```

### Process

La fonction `process_evavelo()` que nous avons déjà utilisé sur un
fichier Excel peut aussi être utilisée directement sur un objet géocodé
avec `geocode_evavelo()`

``` r
eva_data_processed <- evavelo::process_evavelo(eva_data_geocoded)
#> Calcul des distances...
#> Les villes de départ d'itinéraire suivantes sont trop éloignées de l'itinéraire (>30km):
#>  St Brieuc (38A23)
#>  
#> Les villes d'arrivée d'itinéraire suivantes sont trop éloignées de l'itinéraire (>30km):
#>  Paris (38A34)
#>  
#> Liste des régions de l'itinéraire à partir de tables_communes:
#>  Bretagne, Nouvelle-Aquitaine, Pays de la Loire
#> 
#> Correction de catégories pour 17 questionnaires ....
#> Il n'a pas été possible de corriger les catégories de 3 questionnaire(s).
#>  La catégorie du déclarant sera utilisée:
#>  141A35, 38A45, 38A46
```

## Interface Web

La fonction `evavelo::app_run()` permet d’utiliser le package via une
interface web:

![UI Screenshot](man/figures/eva-scan-ui.png?raw=true "Title")

## Classification de compteurs similaires

Le package `eva-scan` permet aussi de créer une classification
hiérarchique des compteurs à partir de données de comptages
automatiques. Cette classification est disponible dans l’interface Web.
