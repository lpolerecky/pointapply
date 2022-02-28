# reading the matlab ion count cubes works

    Code
      head(IC, 35)
    Output
      # A tibble: 35 x 9
         dim.nm grid.nm dim_name.nm grid_size.nm    N.rw  t.nm species.nm file.nm     
          <dbl>   <dbl> <chr>              <dbl>   <dbl> <dbl> <chr>      <chr>       
       1      1       1 height               100  45123.  25.6 12C        2020-08-20-~
       2      1       2 height               100  33587.  25.6 12C        2020-08-20-~
       3      1       3 height               100  33040.  25.6 12C        2020-08-20-~
       4      1       4 height               100  27524.  25.6 12C        2020-08-20-~
       5      2       1 height               100 144954.  51.2 12C        2020-08-20-~
       6      2       2 height               100 101299.  51.2 12C        2020-08-20-~
       7      2       3 height               100  99197.  51.2 12C        2020-08-20-~
       8      2       4 height               100  78800.  51.2 12C        2020-08-20-~
       9      3       1 height               100 518832.  76.8 12C        2020-08-20-~
      10      3       2 height               100 375918.  76.8 12C        2020-08-20-~
      # ... with 25 more rows, and 1 more variable: sample.nm <chr>

---

    Code
      tail(IC, 35)
    Output
      # A tibble: 35 x 9
         dim.nm grid.nm dim_name.nm grid_size.nm  N.rw  t.nm species.nm file.nm       
          <dbl>   <dbl> <chr>              <dbl> <dbl> <dbl> <chr>      <chr>         
       1    248       2 height               100 3295. 6349. 13C        2020-08-20-GL~
       2    248       3 height               100 3363. 6349. 13C        2020-08-20-GL~
       3    248       4 height               100 3202. 6349. 13C        2020-08-20-GL~
       4    249       1 height               100 1970. 6374. 13C        2020-08-20-GL~
       5    249       2 height               100 3329. 6374. 13C        2020-08-20-GL~
       6    249       3 height               100 3358. 6374. 13C        2020-08-20-GL~
       7    249       4 height               100 3153. 6374. 13C        2020-08-20-GL~
       8    250       1 height               100 1963. 6400  13C        2020-08-20-GL~
       9    250       2 height               100 3295. 6400  13C        2020-08-20-GL~
      10    250       3 height               100 3355. 6400  13C        2020-08-20-GL~
      # ... with 25 more rows, and 1 more variable: sample.nm <chr>

# flattening ot the cube works

    Code
      head(xc, 35)
    Output
      # A tibble: 35 x 9
         dim.nm grid.nm dim_name.nm grid_size.nm    N.rw  t.nm species.nm file.nm     
          <dbl>   <dbl> <chr>              <dbl>   <dbl> <dbl> <chr>      <chr>       
       1      1       1 height               100  45123.  25.6 12C        2020-08-20-~
       2      1       2 height               100  33587.  25.6 12C        2020-08-20-~
       3      1       3 height               100  33040.  25.6 12C        2020-08-20-~
       4      1       4 height               100  27524.  25.6 12C        2020-08-20-~
       5      2       1 height               100 144954.  51.2 12C        2020-08-20-~
       6      2       2 height               100 101299.  51.2 12C        2020-08-20-~
       7      2       3 height               100  99197.  51.2 12C        2020-08-20-~
       8      2       4 height               100  78800.  51.2 12C        2020-08-20-~
       9      3       1 height               100 518832.  76.8 12C        2020-08-20-~
      10      3       2 height               100 375918.  76.8 12C        2020-08-20-~
      # ... with 25 more rows, and 1 more variable: sample.nm <chr>

---

    Code
      tail(xc, 35)
    Output
      # A tibble: 35 x 9
         dim.nm grid.nm dim_name.nm grid_size.nm    N.rw  t.nm species.nm file.nm     
          <dbl>   <dbl> <chr>              <dbl>   <dbl> <dbl> <chr>      <chr>       
       1    248       2 height               100 323426. 6349. 12C        2020-08-20-~
       2    248       3 height               100 327163. 6349. 12C        2020-08-20-~
       3    248       4 height               100 310019. 6349. 12C        2020-08-20-~
       4    249       1 height               100 192332. 6374. 12C        2020-08-20-~
       5    249       2 height               100 324445. 6374. 12C        2020-08-20-~
       6    249       3 height               100 329322. 6374. 12C        2020-08-20-~
       7    249       4 height               100 307665. 6374. 12C        2020-08-20-~
       8    250       1 height               100 193866. 6400  12C        2020-08-20-~
       9    250       2 height               100 327083. 6400  12C        2020-08-20-~
      10    250       3 height               100 330350. 6400  12C        2020-08-20-~
      # ... with 25 more rows, and 1 more variable: sample.nm <chr>

