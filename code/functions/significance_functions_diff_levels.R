# Alternatives to 95% significance tests ####

f_insert_99sig_table <- function(df, sheet, title, c = 1) {
  writeData(wb, sheet,
            x = title,
            startRow = r,
            startCol = c
  )
  
  addStyle(wb, sheet,
           style = pt2,
           rows = r,
           cols = c
  )
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 startCol = c,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE
  )
  
  addStyle(wb, sheet,
           style = ns3d,
           rows = (r + 1):(r + nrow(df) - 1),
           cols = (c + 1):(c + ncol(df) - 1),
           gridExpand = TRUE
  )
  
  addStyle(wb, sheet,
           style = ns_comma,
           rows = r + nrow(df),
           cols = (c + 1):(c + ncol(df) - 1),
           gridExpand = TRUE
  )
  
  addStyle(wb, sheet,
           style = ns,
           rows = (r + 1):(r + nrow(df)),
           cols = c,
           gridExpand = TRUE
  )
  
  if ("Z Score" %in% names(df)) {
    for (i in 1:(nrow(df) - 1)) {
      if (abs(df$`Z Score`[i]) > qnorm(0.995)) {
        addStyle(wb, sheet,
                 style = sig,
                 rows = r + i,
                 cols = c + ncol(df) - 1
        )
      } else {
        addStyle(wb, sheet,
                 style = not_sig,
                 rows = r + i,
                 cols = c + ncol(df) - 1
        )
      }
    }
  }
  
  r <<- r + nrow(df) + 2
}

# Inserts table into Excel work sheet containing z score comaprisons in a grid format
f_insert_99z_table <- function(df, sheet, title) {
  writeData(wb, sheet,
            x = title,
            startRow = r
  )
  
  addStyle(wb, sheet,
           style = pt2,
           rows = r,
           cols = 1
  )
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE
  )
  
  addStyle(wb, sheet,
           style = ns3d,
           rows = (r + 1):(r + nrow(df)),
           cols = 1:ncol(df),
           gridExpand = TRUE
  )
  
  for (i in 1:nrow(df)) {
    for (j in 2:ncol(df)) {
      if (!is.na(df[i, j])) {
        if (abs(df[i, j]) > qnorm(0.995)) {
          addStyle(wb, sheet,
                   style = sig,
                   rows = r + i,
                   cols = j
          )
        } else {
          addStyle(wb, sheet,
                   style = not_sig,
                   rows = r + i,
                   cols = j
          )
        }
      }
    }
  }
  
  for (i in 1:nrow(df)) {
    addStyle(wb, sheet,
             style = grey,
             rows = r + i,
             cols = 1 + i
    )
  }
  
  r <<- r + nrow(df) + 2
}
