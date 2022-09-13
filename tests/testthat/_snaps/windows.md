# Sliding windows

    Code
      windows
    Output
      [[1]]
      [1] 1 2 3
      
      [[2]]
      [1] 1 2 3 4
      
      [[3]]
      [1] 1 2 3 4 5
      
      [[4]]
      [1] 2 3 4 5 6
      
      [[5]]
      [1] 3 4 5 6 7
      
      [[6]]
      [1] 4 5 6 7 8
      
      [[7]]
      [1] 5 6 7 8 9
      
      [[8]]
      [1]  6  7  8  9 10
      
      [[9]]
      [1]  7  8  9 10
      
      [[10]]
      [1]  8  9 10
      

# Tumbling windows

    Code
      window_tumbling(n = 10, m = 3, drop = FALSE)
    Output
      $`1`
      [1] 1 2 3 4
      
      $`2`
      [1] 5 6 7
      
      $`3`
      [1]  8  9 10
      

---

    Code
      window_tumbling(n = 10, m = 3, drop = TRUE)
    Output
      $`1`
      [1] 1 2 3
      
      $`2`
      [1] 4 5 6
      
      $`3`
      [1] 7 8 9
      

