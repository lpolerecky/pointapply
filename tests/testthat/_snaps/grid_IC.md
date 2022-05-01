# gg_effect is consistent

    Code
      xc
    Output
      # A tibble: 3 x 5
        dim_name.nm `$\\hat{\\bar{R}}$` `$\\hat{\\epsilon}_{\\bar{R~` `$\\Delta AIC_~`
        <chr>                     <dbl>                         <dbl>            <dbl>
      1 depth                    0.0103                          2.20             4.58
      2 height                   0.0103                          2.33            11.6 
      3 width                    0.0103                          1.94             6.29
      # ... with 1 more variable: `$p_{\\bar{R}}$` <dbl>

---

    Code
      xc
    Output
      # A tibble: 3 x 5
        dim_name.nm `$\\hat{\\bar{R}}$` `$\\hat{\\epsilon}_{\\bar{R~` `$\\Delta AIC_~`
        <chr>                     <dbl>                         <dbl>            <dbl>
      1 depth                    0.0103                          5.56            36.8 
      2 height                   0.0103                          8.13            40.2 
      3 width                    0.0103                          1.31            -3.32
      # ... with 1 more variable: `$p_{\\bar{R}}$` <dbl>

# 3D configuration can be converted to 2D configuration

    Code
      dim_folds(map_raster_image_MEX, "raster", 256, 64)
    Output
      # A tibble: 1,081,344 x 11
         sample.nm file.nm    grid_size.nm grid.nm dim.nm dim_name.nm species.nm  N.rw
         <chr>     <chr>             <dbl>   <int>  <dbl> <chr>       <chr>      <dbl>
       1 MEX       map_raste~       0.0244       1      1 height      12C         1582
       2 MEX       map_raste~       0.0244       2      1 height      12C         1019
       3 MEX       map_raste~       0.0244       3      1 height      12C         1308
       4 MEX       map_raste~       0.0244       4      1 height      12C         1498
       5 MEX       map_raste~       0.0244       5      1 height      12C         1684
       6 MEX       map_raste~       0.0244       6      1 height      12C         1906
       7 MEX       map_raste~       0.0244       7      1 height      12C         1961
       8 MEX       map_raste~       0.0244       8      1 height      12C         2012
       9 MEX       map_raste~       0.0244       9      1 height      12C         2040
      10 MEX       map_raste~       0.0244      10      1 height      12C         2077
      # ... with 1,081,334 more rows, and 3 more variables: t.nm <dbl>,
      #   width.mt <int>, height.mt <int>

---

    Code
      dim_folds(IC, "grid", 256, 64)
    Output
      # A tibble: 16,896 x 14
         sample.nm file.nm    grid_size.nm grid.nm dim.nm dim_name.nm species.nm  t.nm
         <chr>     <chr>             <dbl>   <int>  <dbl> <chr>       <chr>      <dbl>
       1 MEX       map_sum_g~          100       1      1 height      12C         25.6
       2 MEX       map_sum_g~          100       2      1 height      12C         25.6
       3 MEX       map_sum_g~          100       3      1 height      12C         25.6
       4 MEX       map_sum_g~          100       4      1 height      12C         25.6
       5 MEX       map_sum_g~          100       1      2 height      12C         51.2
       6 MEX       map_sum_g~          100       2      2 height      12C         51.2
       7 MEX       map_sum_g~          100       3      2 height      12C         51.2
       8 MEX       map_sum_g~          100       4      2 height      12C         51.2
       9 MEX       map_sum_g~          100       1      3 height      12C         76.8
      10 MEX       map_sum_g~          100       2      3 height      12C         76.8
      # ... with 16,886 more rows, and 6 more variables: Xt.pr <dbl>, N.pr <dbl>,
      #   width.mt <dbl>, height.mt <dbl>, tc.mt <dbl>, det_type.mt <chr>

# diagnostics preserve metadata

    Code
      point::unfold(xc, merge = FALSE)
    Output
      # A tibble: 16,896 x 13
         sample.nm file.nm    grid_size.nm grid.nm dim.nm dim_name.nm species.nm  t.nm
         <chr>     <chr>             <dbl>   <int>  <dbl> <chr>       <chr>      <dbl>
       1 MEX       map_sum_g~          100       1      1 height      12C         25.6
       2 MEX       map_sum_g~          100       2      1 height      12C         25.6
       3 MEX       map_sum_g~          100       3      1 height      12C         25.6
       4 MEX       map_sum_g~          100       4      1 height      12C         25.6
       5 MEX       map_sum_g~          100       1      2 height      12C         51.2
       6 MEX       map_sum_g~          100       2      2 height      12C         51.2
       7 MEX       map_sum_g~          100       3      2 height      12C         51.2
       8 MEX       map_sum_g~          100       4      2 height      12C         51.2
       9 MEX       map_sum_g~          100       1      3 height      12C         76.8
      10 MEX       map_sum_g~          100       2      3 height      12C         76.8
      # ... with 16,886 more rows, and 5 more variables: depth.mt <dbl>,
      #   width.mt <dbl>, height.mt <dbl>, tc.mt <dbl>, det_type.mt <chr>

