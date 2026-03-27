library(here)
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
source(paste0(here(), "/code/pfg_tables/PfG Historic Data Prep.R"))

if (!dir.exists(paste0(here(), "/outputs/PfG"))) {
  dir.create(paste0(here(), "/outputs/PfG"))
}

wb_LGD <- createWorkbook()
wb_AA <- createWorkbook()

# Define all available years based on current_year value from config ####
data_years <- c(seq(2012, 2018, 2), 2019:current_year)

# Questions to analyse ####
questions <- c("TrustMedia2", "TrustAssemblyElectedBody2")

# Lookup table for LGD & AA labels (taken from PfG documentation) ####
LGD_labels <- read.xlsx(
  xlsxFile = paste0(here(), "/code/pfg_tables/Classifications_Equality Groups - Template.xlsx"),
  sheet = "Classifications_LGD - Template"
)

AA_labels <- read.xlsx(
  xlsxFile = paste0(here(), "/code/pfg_tables/Classifications_Equality Groups - Template.xlsx"),
  sheet = "Classifications_AA - Template"
)

# Loop through questions ####

for (question in questions) {
  ## Create template of LGD data frame ####
  
  ### for % data
  question_lgd_data <- data.frame(STATISTIC = character()) %>%
    mutate(
      `TLIST(A1)` = numeric(),
      LGD2014 = character(),
      `Variable name` = character(),
      `Lower limit` = numeric(),
      VALUE = numeric(),
      `Upper limit` = numeric()
    )

  ### for n data
  question_lgd_n_data <- data.frame(STATISTIC = character()) %>%
    mutate(
      `TLIST(A1)` = numeric(),
      LGD2014 = character(),
      `Variable name` = character(),
      VALUE = numeric()
    )
  
  ## Create template of AA data frame ####
  
  ### for % data
  question_aa_data <- data.frame(STATISTIC = character()) %>%
    mutate(
      `TLIST(A1)` = numeric(),
      AA = character(),
      `Variable name` = character(),
      `Lower limit` = numeric(),
      VALUE = numeric(),
      `Upper limit` = numeric()
    )
  
  ### for n data
  question_aa_n_data <- data.frame(STATISTIC = character()) %>%
    mutate(
      `TLIST(A1)` = numeric(),
      AA = character(),
      `Variable name` = character(),
      VALUE = numeric()
    )
  for (year in data_years) {

  ## Select data (historic data prep reads in data from remote location) ####
  pfg_data_year <- eval((as.name(paste0("data_",year))))

    ## Run Analysis only if question is present in data ####

    if (question %in% names(pfg_data_year)) {
      ### Which weight variables to use depending on year ####

      # Set Refusals to NA
      pfg_data_year[[question]][pfg_data_year[[question]] == "Refusal"] <- NA
      
      #### Set weight used for geog variables ####
      weight <- if (year == 2020) {
        "W4"
      } else if (year %in% 2012:2016) {
        "weight"
      } else {
        "W3"
      }

      ### Calculate data for LGD ####

     if ("LGD2014name" %in% names(pfg_data_year)) {
          
          #### Values to loop through ####

          LGDs <- levels(pfg_data_year[["LGD2014name"]])

          for (LGD in LGDs) {
            ##### Weighted p ####
            p_weighted <- pfg_data_year %>%
              filter(!is.na(.[[question]]) & .[["LGD2014name"]] == LGD & .[[question]] %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
              pull(weight) %>%
              sum() / pfg_data_year %>%
                filter(!is.na(.[[question]]) & .[["LGD2014name"]] == LGD) %>%
                pull(weight) %>%
                sum() * 100


            ##### Unweighted n #####
            n_value <- pfg_data_year %>%
              filter(!is.na(.[[question]]) & .[["LGD2014name"]] == LGD) %>%
              nrow()

            ##### Confidence interval ####
            ci <- f_confidence_interval(
              p = p_weighted / 100,
              n = n_value
            )

            ##### LGD lookup ####

            LGD2014_code <- if (LGD %in% LGD_labels$VALUE) {
              LGD_labels$CODE[LGD_labels$VALUE == LGD]
            } else {
              ""
            }

            #### for % data
            new_row <- data.frame(STATISTIC = character(1)) %>%
              mutate(
                `TLIST(A1)` = year,
                LGD2014 = LGD2014_code,
                `Variable name` = LGD,
                `Lower limit` = ci[["lower_cl"]] * 100,
                VALUE = p_weighted,
                `Upper limit` = ci[["upper_cl"]] * 100
              )

            question_lgd_data <- question_lgd_data %>%
              bind_rows(new_row)
            
            #### for n data
            new_n_row <- data.frame(STATISTIC = character(1)) %>%
              mutate(
                `TLIST(A1)` = year,
                LGD2014 = LGD2014_code,
                `Variable name` = LGD,
                VALUE = n_value
              )
            
            question_lgd_n_data <- question_lgd_n_data %>%
              bind_rows(new_n_row)
          }
        }
      }
  
  ### Calculate data for AA ####
  
  if ("AsmblyArea" %in% names(pfg_data_year)) {
    
    #### Values to loop through ####
    
    AAs <- levels(pfg_data_year[["AsmblyArea"]])
    
    for (AAname in AAs) {
      ##### Weighted p ####
      p_weighted <- pfg_data_year %>%
        filter(!is.na(.[[question]]) & .[["AsmblyArea"]] == AAname & .[[question]] %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
        pull(weight) %>%
        sum() / pfg_data_year %>%
        filter(!is.na(.[[question]]) & .[["AsmblyArea"]] == AAname) %>%
        pull(weight) %>%
        sum() * 100
      
      
      ##### Unweighted n #####
      n_value <- pfg_data_year %>%
        filter(!is.na(.[[question]]) & .[["AsmblyArea"]] == AAname) %>%
        nrow()
      
      ##### Confidence interval ####
      ci <- f_confidence_interval(
        p = p_weighted / 100,
        n = n_value
      )
      
      ##### AA lookup ####
      
      AA_code <- if (AAname %in% AA_labels$VALUE) {
        AA_labels$CODE[AA_labels$VALUE == AAname]
      } else {
        ""
      }
      
      #### for % data
      new_row <- data.frame(STATISTIC = character(1)) %>%
        mutate(
          `TLIST(A1)` = year,
          AA = AA_code,
          `Variable name` = AAname,
          `Lower limit` = ci[["lower_cl"]] * 100,
          VALUE = p_weighted,
          `Upper limit` = ci[["upper_cl"]] * 100
        )
      
      question_aa_data <- question_aa_data %>%
        bind_rows(new_row)
      
      #### for n data
      new_n_row <- data.frame(STATISTIC = character(1)) %>%
        mutate(
          `TLIST(A1)` = year,
          AA = AA_code,
          `Variable name` = AAname,
          VALUE = n_value
        )
      
      question_aa_n_data <- question_aa_n_data %>%
        bind_rows(new_n_row)
    }
  }
  }
  

  ## Sort final data frame ####
  
  ### LGD ####

  question_lgd_data <- question_lgd_data %>%
    mutate(`Variable name` = factor(`Variable name`,
      levels = levels(pfg_data_year[['LGD2014name']])
    )) %>%
    arrange(LGD2014, `TLIST(A1)`)
  
  question_lgd_data_rounded <- question_lgd_data %>%
    mutate(`Lower limit` = round_half_up(`Lower limit`, 1),
           VALUE = round_half_up(VALUE, 1),
           `Upper limit` = round_half_up(`Upper limit`, 1))
  
  question_lgd_n_data <- question_lgd_n_data %>%
    mutate(`Variable name` = factor(`Variable name`,
                                    levels = levels(pfg_data_year[['LGD2014name']])
    )) %>%
    arrange(LGD2014, `TLIST(A1)`)

  ### AA ####
  
  question_aa_data <- question_aa_data %>%
    mutate(`Variable name` = factor(`Variable name`,
                                    levels = levels(pfg_data_year[['AsmblyArea']])
    )) %>%
    arrange(AA, `TLIST(A1)`)
  
  question_aa_data_rounded <- question_aa_data %>%
    mutate(`Lower limit` = round_half_up(`Lower limit`, 1),
           VALUE = round_half_up(VALUE, 1),
           `Upper limit` = round_half_up(`Upper limit`, 1))
  
  question_aa_n_data <- question_aa_n_data %>%
    mutate(`Variable name` = factor(`Variable name`,
                                    levels = levels(pfg_data_year[['AsmblyArea']])
    )) %>%
    arrange(AA, `TLIST(A1)`)

  ## Write to Excel ####

  ### LGD ####

  addWorksheet(wb_LGD, question, tabColour = "#00205B")

  writeDataTable(wb_LGD, question,
    x = question_lgd_data_rounded,
    tableStyle = "none",
    withFilter = FALSE
  )

  addStyle(wb_LGD, question,
    style = ns_pfg,
    rows = 2:(nrow(question_lgd_data_rounded) + 1),
    cols = 5:7,
    gridExpand = TRUE
  )

  setColWidths(wb_LGD, question,
    cols = 1:7,
    widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
  )
  
  unrounded_sheet <- paste(substr(question, 1, 19), "(UNROUNDED)")
  
  addWorksheet(wb_LGD, unrounded_sheet, tabColour = "#3878C5")
  
  writeDataTable(wb_LGD, unrounded_sheet,
                 x = question_lgd_data,
                 tableStyle = "none",
                 withFilter = FALSE
  )
  
  addStyle(wb_LGD, unrounded_sheet,
           style = ns_pfg,
           rows = 2:(nrow(question_lgd_data) + 1),
           cols = 5:7,
           gridExpand = TRUE
  )
  
  setColWidths(wb_LGD, unrounded_sheet,
               cols = 1:7,
               widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
  )
  
  n_sheet <- paste(substr(question, 1, 19), "(SAMPLENUM)")
  
  addWorksheet(wb_LGD, n_sheet, tabColour = "#d3d3d3")
  
  writeDataTable(wb_LGD, n_sheet,
                 x = question_lgd_n_data,
                 tableStyle = "none",
                 withFilter = FALSE
  )
  
  addStyle(wb_LGD, n_sheet,
           style = ns_n_pfg,
           rows = 2:(nrow(question_lgd_n_data) + 1),
           cols = 5:7,
           gridExpand = TRUE
  )
  
  setColWidths(wb_LGD, n_sheet,
               cols = 1:7,
               widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
  )

### AA ####

addWorksheet(wb_AA, question, tabColour = "#00205B")

writeDataTable(wb_AA, question,
               x = question_aa_data_rounded,
               tableStyle = "none",
               withFilter = FALSE
)

addStyle(wb_AA, question,
         style = ns_pfg,
         rows = 2:(nrow(question_aa_data_rounded) + 1),
         cols = 5:7,
         gridExpand = TRUE
)

setColWidths(wb_AA, question,
             cols = 1:7,
             widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
)

unrounded_sheet <- paste(substr(question, 1, 19), "(UNROUNDED)")

addWorksheet(wb_AA, unrounded_sheet, tabColour = "#3878C5")

writeDataTable(wb_AA, unrounded_sheet,
               x = question_aa_data,
               tableStyle = "none",
               withFilter = FALSE
)

addStyle(wb_AA, unrounded_sheet,
         style = ns_pfg,
         rows = 2:(nrow(question_aa_data) + 1),
         cols = 5:7,
         gridExpand = TRUE
)

setColWidths(wb_AA, unrounded_sheet,
             cols = 1:7,
             widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
)

n_sheet <- paste(substr(question, 1, 19), "(SAMPLENUM)")

addWorksheet(wb_AA, n_sheet, tabColour = "#d3d3d3")

writeDataTable(wb_AA, n_sheet,
               x = question_aa_n_data,
               tableStyle = "none",
               withFilter = FALSE
)

addStyle(wb_AA, n_sheet,
         style = ns_n_pfg,
         rows = 2:(nrow(question_aa_n_data) + 1),
         cols = 5:7,
         gridExpand = TRUE
)

setColWidths(wb_AA, n_sheet,
             cols = 1:7,
             widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
)

}

xl_lgd_filename <- paste0(here(), "/outputs/PfG/PfG - LGD Data (pfg covariates) with sample numbers ", current_year, ".xlsx")
xl_aa_filename <- paste0(here(), "/outputs/PfG/PfG - Assembly Area Data (pfg covariates) with sample numbers ", current_year, ".xlsx")

saveWorkbook(wb_LGD, xl_lgd_filename, overwrite = TRUE)
saveWorkbook(wb_AA, xl_aa_filename, overwrite = TRUE)

openXL(xl_lgd_filename)
openXL(xl_aa_filename)




