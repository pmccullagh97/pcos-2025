f_embed_ods_timeseries <- function(df, sheet_title, tab_name, startyear, note = NULL, app_b = FALSE) {
  if (!dir.exists(paste0(here(), "/outputs/table_data"))) {
    dir.create(paste0(here(), "/outputs/table_data"))
  }

  wb <- createWorkbook(
    creator = "NISRA Statistical Support Branch",
    title = sheet_title,
    subject = "Public Awareness and Trust of Statistics",
    category = "Government"
  )

  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

  addWorksheet(wb, tab_name)

#note that worksheet contains one table removed, see #'s
  
  writeData(
    wb, tab_name,
    c(sheet_title)
  #  c(sheet_title, "This worksheet contains one table.")
  )

  setRowHeights(wb, tab_name, rows = 1, heights = 30)
  addStyle(wb, tab_name,
    style = pt,
    rows = 1,
    cols = 1
  )

  if(!is.null(note)) {
    writeData(
#      wb, tab_name, note, startRow =3
       wb, tab_name, note, startRow =2
    )
#   nextRow = 4
    nextRow = 3
  } else {
#   nextRow = 3
    nextRow = 2
  }
  
  writeDataTable(wb, tab_name,
    df,
    startRow = nextRow,
    tableStyle = "none",
    withFilter = FALSE,
    tableName = tab_name,
    headerStyle = ch_lined,
    keepNA = TRUE,
    na.string = "No data"
  )
  
  r = nextRow
  
  addStyle(wb, tab_name,
           rows = r,
           cols = 1,
           style = ch_lined_left
  )
  
  addStyle(wb, tab_name,
           rows = (r + 1):(r + nrow(df) - 1),
           cols = 2:(ncol(df)),
           style = ns,
           gridExpand = TRUE
  )
  
  addStyle(wb, tab_name,
           rows = (r + nrow(df)),
           cols = 2:(ncol(df)),
           style = ns_italic,
           gridExpand = TRUE
  )
  
  addStyle(wb, tab_name,
           rows = (r + nrow(df)),
           cols = 1,
           style = num_resp
  )
  
  addStyle(wb, tab_name,
           rows = (r + 1):(r + nrow(df) - 1),
           cols = 1,
           style = pt2
  )
  
  na_rows <- which(is.na(df)) %% nrow(df)
  na_cols <- (which(is.na(df)) - na_rows) / nrow(df)
  
  addStyle(wb, tab_name,
           rows = r + na_rows,
           cols = 1 + na_cols,
           style = wt,
           gridExpand = TRUE
  )

  setColWidths(wb,
               tab_name,
               cols = 1:ncol(df),
               widths = c(27, rep(12, ncol(df) - 1))
               )

  xl_name <- paste0(here(), "/outputs/table_data/", tab_name, "_", startyear, "_to_", current_year, ".xlsx")
  ods_name <- gsub(".xlsx", ".ods", xl_name)

  saveWorkbook(wb, xl_name, overwrite = TRUE)

  f_convert_to_ods(xl_name)

  ods_size <- paste0(
    round_half_up(file.size(ods_name) / 1000),
    "kB"
  )

  unlink(xl_name)
  
  if (tab_name == "trust_assembly_elected_body") {
    sheet_title <- paste0("Table 3.3a: Trust in the Northern Ireland Assembly/ Elected Bodies, 2014 to ", current_year)
  }
  
  sheet_title <- if (grepl("\\[Note", sheet_title)) {
    sub(" \\[.*", "", sheet_title)
  } else {
    sheet_title
  }

  if (app_b) {
    paste0(sheet_title, " (", embed_file(ods_name, text = ".ODS format"), "; ", ods_size, ")")
  } else {
    div(
      style = "margin-top: -20px; margin-bottom: 20px;",
      div(class = "download-button", embed_file(ods_name, text = "Download data")),
      span(class = "download-text", paste0(" - ", sheet_title, " (.ODS format; ", ods_size, ")"))
    )
  }
}
