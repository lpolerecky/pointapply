# significance stars can be produced

    Code
      sig_coder(0.01)
    Output
      [[1]]
      [1] "**"
      
      [[2]]
      [1] "0.1 ˙ 0.05 * 0.01 ** 0.001 *** 0 "
      

---

    Code
      sig_coder(c(0, 0.001, 0.3, NA))
    Output
      [[1]]
      [1] "***" "***" ""    "✕"  
      
      [[2]]
      [1] "0.1 ˙ 0.05 * 0.01 ** 0.001 *** 0  missing: ✕"
      

---

    Code
      sig_coder(c(0, 0.001, 0.3, 0.02))
    Output
      [[1]]
      [1] "***" "***" ""    "*"  
      
      [[2]]
      [1] "0.1 ˙ 0.05 * 0.01 ** 0.001 *** 0 "
      

---

    Code
      sig_coder(0, make_lab = FALSE)
    Output
      [1] "***"

