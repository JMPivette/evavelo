# geocode_cities_cp works

    # A tibble: 2 x 8
      city  postcode     a     b country   city_lat city_lon city_cog
      <lgl> <lgl>    <dbl> <dbl> <chr>        <dbl>    <dbl> <chr>   
    1 NA    NA           1     1 France          NA       NA <NA>    
    2 NA    NA           2     2 Australie       NA       NA <NA>    

---

    # A tibble: 2 x 8
      city         postcode     a     b country city_lat city_lon city_cog
      <chr>        <chr>    <dbl> <dbl> <chr>      <dbl>    <dbl> <chr>   
    1 Nantes       44000        1     1 France      47.2    -1.55 44109   
    2 Saint-Brieuc 22000        2     2 France      48.5    -2.76 22278   

---

    # A tibble: 2 x 10
      city  postcode     a     b country city_lat city_lon city_cog proposition_city
      <chr> <chr>    <dbl> <dbl> <chr>      <dbl>    <dbl> <chr>    <chr>           
    1 Nant~ 44000        1     1 France      47.2    -1.55 44109    <NA>            
    2 Sain~ 22278        2     2 France      NA      NA    <NA>     Saint-Brieuc    
    # ... with 1 more variable: proposition_postcode <chr>

---

    # A tibble: 2 x 8
      city      postcode     a     b country city_lat city_lon city_cog
      <chr>     <chr>    <dbl> <dbl> <chr>      <dbl>    <dbl> <chr>   
    1 Nantes    44000        1     1 France      47.2    -1.55 44109   
    2 St Brieuc 22000        2     2 France      48.5    -2.76 22278   

---

    # A tibble: 2 x 8
      city     postcode     a     b country   city_lat city_lon city_cog
      <chr>    <chr>    <dbl> <dbl> <chr>        <dbl>    <dbl> <chr>   
    1 Nantes   44000        1     1 France        47.2    -1.55 44109   
    2 Canberra 22000        2     2 Australie    -35.3   149.   <NA>    

---

    # A tibble: 3 x 8
      city     postcode     a     b country   city_lat city_lon city_cog
      <chr>    <chr>    <dbl> <dbl> <chr>        <dbl>    <dbl> <chr>   
    1 Nantes   44000        1     1 France        47.2    -1.55 44109   
    2 Canberra 22000        2     2 Australie    -35.3   149.   <NA>    
    3 Yamba    <NA>         3     3 Australia    -29.4   153.   <NA>    

---

    # A tibble: 3 x 10
      city  postcode     a     b country city_lat city_lon city_cog proposition_pos~
      <chr> <chr>    <dbl> <dbl> <chr>      <dbl>    <dbl> <chr>    <chr>           
    1 Nante 44000        1     1 France      NA        NA  <NA>     44100           
    2 Canb~ <NA>         2     2 Austra~    -35.3     149. <NA>     <NA>            
    3 sidn~ 22000        2     2 Austra~    -33.9     151. <NA>     <NA>            
    # ... with 1 more variable: proposition_city <chr>

# geocode_df_foreign_cities works

    # A tibble: 3 x 8
      city     postcode     a     b country   city_lat city_lon city_cog
      <chr>    <chr>    <dbl> <dbl> <chr>        <dbl>    <dbl> <chr>   
    1 Nantes   44000        1     1 France        NA        NA  <NA>    
    2 Canberra 22000        2     2 Australie    -35.3     149. <NA>    
    3 Yamba    <NA>         3     3 Australia    -29.4     153. <NA>    

