# height and depth dimensions can be aggregated

    Code
      dim_aggregate_(map_raster_image_MEX, 64, "height")
    Output
      # A tibble: 65,536 x 12
         species.nm width.mt depth.mt grid.nm  t.nm  N.rw height.mt sample.nm file.nm 
         <chr>         <int>    <int>   <int> <dbl> <dbl>     <int> <chr>     <chr>   
       1 12C               1        1    5383 0.007 3095.        NA MEX       map_ras~
       2 12C               1        2   14598 0.006    0         NA MEX       map_ras~
       3 12C               1        3   23814 0.006    0         NA MEX       map_ras~
       4 12C               1        4   33030 0.006    0         NA MEX       map_ras~
       5 12C               1        5   50183 0.007    0         NA MEX       map_ras~
       6 12C               1        6   52998 0.006    0         NA MEX       map_ras~
       7 12C               1        7   62214 0.006    0         NA MEX       map_ras~
       8 12C               1        8   71430 0.006 1556.        NA MEX       map_ras~
       9 12C               1        9   94983 0.007 7417.        NA MEX       map_ras~
      10 12C               1       10   91398 0.006 4937.        NA MEX       map_ras~
      # ... with 65,526 more rows, and 3 more variables: grid_size.nm <dbl>,
      #   dim.nm <dbl>, dim_name.nm <chr>

---

    Code
      dim_aggregate(map_raster_image_MEX, 64)
    Output
      # A tibble: 393,216 x 12
         species.nm width.mt depth.mt grid.nm  t.nm  N.rw height.mt sample.nm file.nm 
         <chr>         <int>    <int>   <int> <dbl> <dbl>     <int> <chr>     <chr>   
       1 12C               1        1    5383 0.007 3095.        NA MEX       map_ras~
       2 12C               1        2   14598 0.006    0         NA MEX       map_ras~
       3 12C               1        3   23814 0.006    0         NA MEX       map_ras~
       4 12C               1        4   33030 0.006    0         NA MEX       map_ras~
       5 12C               1        5   50183 0.007    0         NA MEX       map_ras~
       6 12C               1        6   52998 0.006    0         NA MEX       map_ras~
       7 12C               1        7   62214 0.006    0         NA MEX       map_ras~
       8 12C               1        8   71430 0.006 1556.        NA MEX       map_ras~
       9 12C               1        9   94983 0.007 7417.        NA MEX       map_ras~
      10 12C               1       10   91398 0.006 4937.        NA MEX       map_ras~
      # ... with 393,206 more rows, and 3 more variables: grid_size.nm <dbl>,
      #   dim.nm <dbl>, dim_name.nm <chr>

