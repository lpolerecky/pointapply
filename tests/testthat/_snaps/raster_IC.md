# height and depth dimensions can be aggregated

    Code
      dim_aggregate_(IC, 64, "height")
    Output
      # A tibble: 406,563 x 15
         species.nm width.mt depth.mt grid.nm  t.nm height.mt sample.nm file.nm       
         <chr>         <int>    <int>   <int> <dbl>     <int> <chr>     <chr>         
       1 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       2 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       3 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       4 12C               1        2   14598 0.006        NA MEX       map_raster_im~
       5 12C               1        3   23814 0.006        NA MEX       map_raster_im~
       6 12C               1        4   33030 0.006        NA MEX       map_raster_im~
       7 12C               1        5   50183 0.007        NA MEX       map_raster_im~
       8 12C               1        6   52998 0.006        NA MEX       map_raster_im~
       9 12C               1        7   62214 0.006        NA MEX       map_raster_im~
      10 12C               1        8   71430 0.006        NA MEX       map_raster_im~
      # ... with 406,553 more rows, and 7 more variables: grid_size.nm <dbl>,
      #   dim.nm <dbl>, dim_name.nm <chr>, Xt.pr <dbl>, N.pr <dbl>, tc.mt <dbl>,
      #   det_type.mt <chr>

---

    Code
      dim_aggregate(IC, 64)
    Output
      # A tibble: 1,075,946 x 15
         species.nm width.mt depth.mt grid.nm  t.nm height.mt sample.nm file.nm       
         <chr>         <int>    <int>   <int> <dbl>     <int> <chr>     <chr>         
       1 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       2 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       3 12C               1        1    5383 0.007        NA MEX       map_raster_im~
       4 12C               1        2   14598 0.006        NA MEX       map_raster_im~
       5 12C               1        3   23814 0.006        NA MEX       map_raster_im~
       6 12C               1        4   33030 0.006        NA MEX       map_raster_im~
       7 12C               1        5   50183 0.007        NA MEX       map_raster_im~
       8 12C               1        6   52998 0.006        NA MEX       map_raster_im~
       9 12C               1        7   62214 0.006        NA MEX       map_raster_im~
      10 12C               1        8   71430 0.006        NA MEX       map_raster_im~
      # ... with 1,075,936 more rows, and 7 more variables: grid_size.nm <dbl>,
      #   dim.nm <dbl>, dim_name.nm <chr>, Xt.pr <dbl>, N.pr <dbl>, tc.mt <dbl>,
      #   det_type.mt <chr>

