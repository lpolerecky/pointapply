# sensitivity tests works

    Code
      test_sensitivity(var_T, var_I, reps = 10, mc_cores = 4)
    Output
      # A tibble: 17,280,000 x 11
         type.nm  trend.nm base.nm force.nm  t.nm bl.nm  n.rw spot.nm species.nm  N.sm
         <chr>       <dbl>   <dbl>    <dbl> <int> <int> <dbl>   <int> <chr>      <dbl>
       1 asymmet~        0       0      -22     1     1  3000       1 13C          305
       2 asymmet~        0       0      -22     1     1  3000       1 12C        29729
       3 asymmet~        0       0      -22     1     1  3000       2 13C          320
       4 asymmet~        0       0      -22     1     1  3000       2 12C        29678
       5 asymmet~        0       0      -22     1     1  3000       3 13C          341
       6 asymmet~        0       0      -22     1     1  3000       3 12C        29459
       7 asymmet~        0       0      -22     1     1  3000       4 13C          349
       8 asymmet~        0       0      -22     1     1  3000       4 12C        29429
       9 asymmet~        0       0      -22     1     1  3000       5 13C          354
      10 asymmet~        0       0      -22     1     1  3000       5 12C        29770
      # ... with 17,279,990 more rows, and 1 more variable: Xt.sm <dbl>

