message("data_prep.R has started running")
# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

# Old and new names for all "trust" based answers
# trust_body_var is defined in config.R
trust_q_old <- c("PCOS2a", "PCOS2b", "PCOS2b", "PCOS2c", "PCOS2d", "PCOS3")
trust_q_new <- c("TrustCivilService2", trust_body_var, "TrustAssemblyElectedBody2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2")

# Old and new names for all "agree" based answers
agree_q_old <- c("PCOS4", "PCOS5", "PCOS6")
agree_q_new <- c("NISRAstatsImp2", "Political2", "Confidential2")

# Read data in from SPSS ####

data_raw <- f_read_spss(filepath = paste0(data_folder, "Raw/", data_filename),
                        pass = password)

## Raw variable check ran and output to Excel workbook in Outputs folder ####
source(paste0(here(), "/code/html_publication/check_raw_variables.R"))

# Read in ONS data from Excel ####
data_ons_raw <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "weighted_pct") %>%
  filter(Year == ons_year)

names(data_ons_raw) <- gsub(".", " ", names(data_ons_raw), fixed = TRUE)

unweighted_ons <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "unweighted_n") %>%
  filter(Year == ons_year)

names(unweighted_ons) <- gsub(".", " ", names(unweighted_ons), fixed = TRUE)

unweighted_ons_base <- unweighted_ons %>%
  mutate(`Unweighted base` = `Unweighted base` - `Prefer not to answer`) %>%
  select(`Related Variable`, `Unweighted base`)

data_ons <- data_ons_raw %>%
  mutate(
    `Weighted base` = 100 - `Prefer not to answer`,
    across(.cols = `Don't know`:`Strongly disagree`, ~ .x / `Weighted base` * 100)
  ) %>%
  select(-`Prefer not to answer`, -`Weighted base`) %>%
  left_join(unweighted_ons_base, by = "Related Variable")

# Recode variables #####

## Recode age variable and DERHI ####
data_final <- data_raw %>%
  mutate(
    AGE2 = as.factor(case_when(
      AGE <= 24 ~ "16-24",
      AGE <= 34 ~ "25-34",
      AGE <= 44 ~ "35-44",
      AGE <= 54 ~ "45-54",
      AGE <= 64 ~ "55-64",
      AGE <= 74 ~ "65-74",
      TRUE ~ "75 and over"
    )),
    AGE2a = as.factor(case_when(
      AGE <= 15 ~ "0-15",
      AGE <= 29 ~ "20-29",
      AGE <= 39 ~ "30-39",
      AGE <= 49 ~ "40-49",
      AGE <= 59 ~ "59",
      TRUE ~ "60+"
    )),
    # DERHIanalysis = case_when(
    #   DERHI == "Other qualifications" ~ NA,
    #   TRUE ~ DERHI
    # ),
    # DERHIanalysis = factor(DERHIanalysis,
    #   levels = c(
    #     "Degree, or Degree equivalent and above",
    #     "Other higher education below degree level",
    #     "A levels, vocational level 3 and equivalents",
    #     "GCSE/O level grade A*-C. vocational level 2 and equivalents",
    #     "Qualifications at level 1 and below",
    #     "No qualification"
    #   )
    # ),
    # EMPST2 = factor(EMPST2, levels = c("In paid employment", "Not in paid employment")),
    remove = FALSE,
    AwareNISRA2 = factor(PCOS1, levels = setdiff(levels(PCOS1), "Refusal"), labels = gsub("DontKnow", "Don't know", setdiff(levels(PCOS1), "Refusal")))
  ) %>% ## Added for later
  relocate("AGE2", .after = "AGE") %>%
  relocate("AGE2a", .after = "AGE2")

attributes(data_final$AwareNISRA2)$label <- attributes(data_final$PCOS1)$label

## Could use this to be more robust ##
# data_final <- data_raw %>%
#   mutate(
#     ## Only create AGE2 if AGE exists
#     AGE2 = if ("AGE" %in% names(.)) {
#       as.factor(case_when(
#         AGE <= 24 ~ "16-24",
#         AGE <= 34 ~ "25-34",
#         AGE <= 44 ~ "35-44",
#         AGE <= 54 ~ "45-54",
#         AGE <= 64 ~ "55-64",
#         AGE <= 74 ~ "65-74",
#         TRUE ~ "75 and over"
#       ))
#     } else { NULL },
#     
#     ## Only create AGE2a if AGE exists
#     AGE2a = if ("AGE" %in% names(.)) {
#       as.factor(case_when(
#         AGE <= 15 ~ "0-15",
#         AGE <= 29 ~ "20-29",
#         AGE <= 39 ~ "30-39",
#         AGE <= 49 ~ "40-49",
#         AGE <= 59 ~ "59",
#         TRUE ~ "60+"
#       ))
#     } else { NULL },
#     
#     ## Only create DERHIanalysis if DERHI exists
#     DERHIanalysis = if ("DERHI" %in% names(.)) {
#       case_when(
#         DERHI == "Other qualifications" ~ NA,
#         TRUE ~ DERHI
#       )
#     } else { NULL }
#   ) %>%
#   mutate(
#     ## Only apply factor levels if DERHIanalysis exists
#     DERHIanalysis = if ("DERHIanalysis" %in% names(.)) {
#       factor(DERHIanalysis,
#              levels = c(
#                "Degree, or Degree equivalent and above",
#                "Other higher education below degree level",
#                "A levels, vocational level 3 and equivalents",
#                "GCSE/O level grade A*-C. vocational level 2 and equivalents",
#                "Qualifications at level 1 and below",
#                "No qualification"
#              ))
#     } else { NULL },
#     
#     EMPST2 = factor(EMPST2, levels = c("In paid employment", "Not in paid employment")),
#     remove = FALSE,
#     AwareNISRA2 = factor(PCOS1, levels = setdiff(levels(PCOS1), "Refusal"),
#                          labels = gsub("DontKnow", "Don't know", setdiff(levels(PCOS1), "Refusal")))
#   ) %>%
#   relocate("AGE2", .after = "AGE") %>%
#   relocate("AGE2a", .after = "AGE2")
# 
# attributes(data_final$AwareNISRA2)$label <- attributes(data_final$PCOS1)$label

## Loop to recode all "Trust" based answers ####

for (i in 1:length(trust_q_old)) {
  data_final[[trust_q_new[i]]] <- case_when(
    data_final[[trust_q_old[i]]] %in% c("Trust a great deal", "Tend to trust", "Trust them greatly", "Tend to trust them") ~ "Trust a great deal/Tend to trust",
    data_final[[trust_q_old[i]]] %in% c("Tend to distrust", "Distrust greatly", "Tend not to trust them", "Distrust them greatly") ~ "Tend to distrust/Distrust greatly",
    data_final[[trust_q_old[i]]] == "DontKnow" ~ "Don't know",
    TRUE ~ data_final[[trust_q_old[i]]]
  ) %>%
    factor(levels = c("Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly", "Don't know"))
  
  attributes(data_final[[trust_q_new[i]]])$label <- attributes(data_final[[trust_q_old[i]]])$label
}

## Loop to recode all "agree" based answers ####

for (i in 1:length(agree_q_old)) {
  data_final[[agree_q_new[i]]] <- case_when(
    data_final[[agree_q_old[i]]] %in% c("Strongly agree", "Tend to agree") ~ "Strongly Agree/Tend to Agree",
    data_final[[agree_q_old[i]]] %in% c("Tend to disagree", "Strongly disagree") ~ "Tend to disagree/Strongly disagree",
    data_final[[agree_q_old[i]]] == "DontKnow" ~ "Don't know",
    TRUE ~ data_final[[agree_q_old[i]]]
  ) %>%
    factor(levels = c("Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree", "Don't know"))
  
  attributes(data_final[[agree_q_new[i]]])$label <- attributes(data_final[[agree_q_old[i]]])$label
}

## Merge pcos3b 1/2/3 and pcos3c 1/2/3 ####
# unnecessary?

## Recode Refusals to Missing ####
# Added pcos1d10 and pcos3b/3c to recode logic

vars_to_recode_to_missing <- names(data_final)[c(
  which(names(data_final) == "PCOS1"):which(names(data_final) == "PCOS1d10"),
  which(names(data_final) == "TrustCivilService2"):which(names(data_final) == "Confidential2"),
  which(names(data_final) == "PCOS3b_1"):which(names(data_final) == "PCOS3cOth")
)]

## Convert all "" to NA in vars_to_recode_to_missing
# data_final <- data_final %>%
#   mutate(across(all_of(vars_to_recode_to_missing),
#                 ~ na_if(trimws(as.character(.)), "")))

problematic_cols <- c("PCOS3bOth", "PCOS3cOth")

data_final <- data_final %>%
  mutate(across(all_of(problematic_cols),
                ~ na_if(trimws(as.character(.)), "")))

## Maybe for future - more elegant way to deal with ""
# empty_cols <- data_final %>%
#   summarise(across(all_of(vars_to_recode_to_missing),
#                    ~ any(trimws(as.character(.)) == "", na.rm = TRUE))) %>%
#   select(where(~ .x)) %>%
#   names()
# 
# data_final <- data_final %>%
#   mutate(across(all_of(empty_cols),
#                 ~ na_if(trimws(as.character(.)), "")))

for (i in 1:length(vars_to_recode_to_missing)) {
  data_final[[vars_to_recode_to_missing[i]]] <- case_when(
    data_final[[vars_to_recode_to_missing[i]]] == "Refusal" ~ NA,
    TRUE ~ data_final[[vars_to_recode_to_missing[i]]]
  )
}

## Check all above variables for Missing across all Q's ####
for (i in 1:nrow(data_final)) {
  for (j in 1:length(vars_to_recode_to_missing)) {
    if (is.na(data_final[[vars_to_recode_to_missing[j]]][i])) {
      data_final$remove[i] <- TRUE
    } else {
      if (data_final[[vars_to_recode_to_missing[j]]][i] %in% c("DontKnow", "Don't know")) {
        data_final$remove[i] <- TRUE
      } else {
        data_final$remove[i] <- FALSE
        break
      }
    }
  }
}

## Test that NA/DK removal logic working - if any non zero, then no working
# data_final %>%
#   select(all_of(vars_to_recode_to_missing)) %>% 
#   summarise(across(everything(), ~ sum(. == "", na.rm=TRUE)))


## Tidy up data ####
data_final <- data_final %>%
  filter(!remove) %>%
  select(-remove) #%>%
  # relocate("DERHIanalysis", .after = "Confidential2")

saveRDS(data_final, paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))
#saveRDS(data_final, paste0(data_folder, "Final/PCOS ", current_year, " TEST.RDS"))

# Check for existence of pre 2021 data
if (!file.exists(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))) {
  source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
}

# Check for existence of previous year data (post 2021)
if (!file.exists(paste0(data_folder, "Final/PCOS ", current_year - 1, " Final Dataset.RDS"))) {
  print(paste0("Final Dataset RDS file missing for ", current_year - 1),
        ". Re-visit the Project for ", current_year - 1, " and re-run that year's data_prep.R script")
}

## Check created variables against originals (see outputs folder) ####
source(paste0(here(), "/code/html_publication/check_created_variables.R"))

# Might not be needed?
# calls supplementary tables function
## Supplementary tables output to outputs folder ####
f_supplementary_tables(data = data_final,
                       year = current_year,
                       trust_q = trust_q_new,
                       agree_q = agree_q_new,
                       co_var = c("AGE2"
                                  # , "DERHIanalysis", "EMPST2"
                                  ),
                       age_weight = age_weight,
                       sex_weight = sex_weight,
                       weight = weight)

# Create data frames for charts ####

# Check Trend folder for 2021 exists
if (!dir.exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
  source(paste0(here(), "/code/html_publication/trend_data_for_charts.R"))
  source(paste0(here(), "/code/ods_tables/ods_trend_data.R"))
}

## Read in all last year's trend data ####
for (file in list.files((paste0(data_folder, "Trend/", current_year - 1)))) {
  assign(
    sub(".RDS", "", file),
    readRDS(paste0(data_folder, "Trend/", current_year - 1, "/", file))
  )
}

# Check Trend folder for new year exists
if (!dir.exists(paste0(data_folder, "Trend/", current_year))) {
  dir.create(paste0(data_folder, "Trend/", current_year))
}

## Chart 1: Awareness of NISRA by year ####

aware_nisra_data <- aware_nisra_data %>%
  rbind(data.frame(
    year = current_year,
    pct = f_return_p(data_final, "AwareNISRA2", "Yes") * 100
  ))

saveRDS(aware_nisra_data, paste0(data_folder, "Trend/", current_year, "/aware_nisra_data.RDS"))

## Chart 2: Awareness of NISRA and ONS by year ####

aware_nisra_ons_data <- aware_nisra_ons_data %>%
  rbind(data.frame(
    year = current_year,
    nisra = aware_nisra_data$pct[aware_nisra_data$year == current_year],
    ons = if (current_year == ons_year) data_ons$Yes[data_ons$`Related Variable` == "PCOS1"] else NA
  ))

saveRDS(aware_nisra_ons_data, paste0(data_folder, "Trend/", current_year, "/aware_nisra_ons_data.RDS"))

aware_nisra_ons_data <- aware_nisra_ons_data %>%
  mutate(year = as.character(year))



## Chart 3: Awareness of specific NISRA statistics for respondents who were not aware of NISRA ####
## This chart/dataframe and its created object "aware_stats_data" encapsulate the old pcos1d and pcos1c variables
## and becomes "Awareness of specific NISRA statistics" only

# Use variable label to extract output name

aware_stats_data <- data.frame(
  output = character(),
  yes = numeric(),
  no = numeric(),
  dont_know = numeric()
)

for (i in 1:length(PCOS1d_vars)) {
  aware_stats_data <- aware_stats_data %>%
    rbind(data.frame(
      output = f_wrap_labels(sub("\\..*", "", attributes(data_final[[PCOS1d_vars[i]]])$label) %>% trimws(), 47),
      yes = f_return_p(data_final, PCOS1d_vars[i], "Yes") * 100,
      no = f_return_p(data_final, PCOS1d_vars[i], "No") * 100,
      dont_know = f_return_p(data_final, PCOS1d_vars[i], "DontKnow") * 100
    ))
}

aware_stats_data <- aware_stats_data %>%
  arrange(yes)

sort_order <- aware_stats_data %>%
  mutate(order = as.numeric(rownames(.))) %>%
  select(output, order)

# Removing chart 4 for 2025

## Chart 4: Awareness of specific NISRA statistics for respondents who were aware of NISRA ####

# aware_stats_by_nisra_data <- data.frame(
#   output = character(),
#   yes = numeric(),
#   no = numeric(),
#   dont_know = numeric()
# )
# 
# for (i in 1:length(PCOS1c_vars)) {
#   aware_stats_by_nisra_data <- aware_stats_by_nisra_data %>%
#     rbind(data.frame(
#       output = f_wrap_labels(sub("\\..*", "", attributes(data_final[[PCOS1c_vars[i]]])$label) %>% trimws(), 47),
#       yes = f_return_p(data_final, PCOS1c_vars[i], "Yes") * 100,
#       no = f_return_p(data_final, PCOS1c_vars[i], "No") * 100,
#       dont_know = f_return_p(data_final, PCOS1c_vars[i], "DontKnow") * 100
#     ))
# }
# 
# aware_stats_by_nisra_data <- aware_stats_by_nisra_data %>%
#   left_join(sort_order) %>%
#   arrange(order) %>%
#   select(-order)

## Chart 5: Trust in NISRA by year ####
trust_nisra_data <- trust_nisra_data %>%
  rbind(data.frame(
    year = current_year,
    trust = f_return_p(data_final, "TrustNISRA2", "Trust a great deal/Tend to trust") * 100,
    distrust =  f_return_p(data_final, "TrustNISRA2", "Tend to distrust/Distrust greatly") * 100,
    dont_know = f_return_p(data_final, "TrustNISRA2", "Don't know") * 100
  ))

saveRDS(trust_nisra_data, paste0(data_folder, "Trend/", current_year, "/trust_nisra_data.RDS"))

trust_nisra_data <- trust_nisra_data %>%
  mutate(year = as.character(year))

## PCOS3
total_n <- sum(data_raw$PCOS3 == "Tend to trust them") + sum(data_raw$PCOS3 == "Trust them greatly")
total_n2 <- sum(data_raw$PCOS3 == "Tend not to trust them") + sum(data_raw$PCOS3 == "Distrust them greatly")

#my_list_test <- c(as.character(data_final$PCOS3b_1), as.character(data_final$PCOS3b_2), as.character(data_final$PCOS3b_3))


## Create pcos3b dataframe
my_list <- c(as.character(data_final$PCOS3b_1), as.character(data_final$PCOS3b_2), as.character(data_final$PCOS3b_3))
counts <- as.data.frame(table(unlist(my_list)))
counts$percent <- (counts$Freq / total_n) * 100

#saveRDS(counts, paste0(data_folder, "Trend/", current_year, "/counts.RDS"))

# # Create graph from pcos3b dataframe
# library(ggplot2)
# ggplot(counts, aes(x = Var1, y = percent)) +
#   geom_col(fill = "steelblue") +
#   labs(
#     title = "Percentage of Each Category",
#     x = "Category",
#     y = "Percentage (%)"
#   ) +
#   theme_minimal()

########################################################################################

# wrap_label <- function(x, width = 26) {
#   sapply(x, function(s) paste(str_wrap(s, width = width), collapse = "<br>"))
# }
# 
# counts$Var1_wrapped <- wrap_label(counts$Var1, width = 26)


# library(plotly)
# library(stringr)
# fig <- plot_ly(
#   data = counts,
#   x = ~Var1_wrapped,
#   y = ~percent,
#   type = "bar",
#   marker = list(color = nisra_navy),
#   hovertext = ~paste0(Var1, ": ", round_half_up(percent), "%"),
#   hoverinfo = "text"
# ) %>%
#   layout(font = list(family = "Arial", size = 12),
#          bargap = 0.5,
#          height = 700,
#          legend = list(orientation = "h",
#                        x = 0.5,
#                        y = -0.05,
#                        xanchor = "center"),
#          yaxis = list(title = "",
#                       range = c(0,100),
#                       showline = TRUE,
#                       fixedrange = TRUE),
#          xaxis = list(title = "",
#                       fixedrange = TRUE,
#                       tickangle = 0),
#          annotations = list(text = "Percentage",
#                             x = 0,
#                             xref = "paper",
#                             xanchor = "center",
#                             y = 1.05,
#                             yref = "paper",
#                             yanchor = "bottom",
#                             showarrow = FALSE),
#          margin = list(t = 50)) %>%
#   config(displayModeBar = FALSE)
# 
# htmlwidgets::saveWidget(fig, "test_plot.html")
# browseURL("test_plot.html")

###################################################################################

## Create pcos3c dataframe
my_list2 <- c(as.character(data_final$PCOS3c_1), as.character(data_final$PCOS3c_2), as.character(data_final$PCOS3c_3))
counts2 <- as.data.frame(table(unlist(my_list2)))
counts2$percent <- (counts2$Freq / total_n2) * 100

#saveRDS(counts2, paste0(data_folder, "Trend/", current_year, "/counts2.RDS"))

# # Create graph from pcos3c dataframe
# library(ggplot2)
# ggplot(counts2, aes(x = Var1, y = percent)) +
#   geom_col(fill = "steelblue") +
#   labs(
#     title = "Percentage of Each Category",
#     x = "Category",
#     y = "Percentage (%)"
#   ) +
#   theme_minimal()

## Chart 6: Trust in NISRA and ONS as institutions ####

trust_ons_data <- data_ons %>%
  filter(`Related Variable` == "TrustNISRA2")

trust_nisra_ons_data <- trust_nisra_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    trust = trust_ons_data$`Trust a great deal` + trust_ons_data$`Tend to trust`,
    distrust = trust_ons_data$`Tend to distrust` + trust_ons_data$`Distrust greatly`,
    dont_know = trust_ons_data$`Don't know`
  ))


## Chart 7: Trust in institutions ####

if (trust_body_var == "TrustElectedRep2") {
  AssemblyElectedBody_name = "Elected Bodies"
} else {
  AssemblyElectedBody_name = "The NI Assembly"
}  
  trust_institutions_data <-
  rbind(
    data.frame(
      org = c(AssemblyElectedBody_name, "The Civil Service", "The media"),
      trust = c(
        f_return_p(data_final, "TrustAssemblyElectedBody2", "Trust a great deal/Tend to trust") * 100,
        f_return_p(data_final, "TrustCivilService2", "Trust a great deal/Tend to trust") * 100,
        f_return_p(data_final, "TrustMedia2", "Trust a great deal/Tend to trust") * 100
      ),
      distrust = c(
        f_return_p(data_final, "TrustAssemblyElectedBody2", "Tend to distrust/Distrust greatly") * 100,
        f_return_p(data_final, "TrustCivilService2", "Tend to distrust/Distrust greatly") * 100,
        f_return_p(data_final, "TrustMedia2", "Tend to distrust/Distrust greatly") * 100
      ),
      dont_know = c(
        f_return_p(data_final, "TrustAssemblyElectedBody2", "Don't know") * 100,
        f_return_p(data_final, "TrustCivilService2", "Don't know") * 100,
        f_return_p(data_final, "TrustMedia2", "Don't know") * 100
      )
    ),
    trust_nisra_data %>%
      filter(year == current_year) %>%
      mutate(org = "NISRA") %>%
      select(org, trust:dont_know)
  ) %>%
  mutate(org = paste0(org, " "))

## Chart 8: Trust in NISRA statistics by year ####

trust_stats_data <- trust_stats_data %>%
  rbind(data.frame(
    year = current_year,
    trust = f_return_p(data_final, "TrustNISRAstats2", "Trust a great deal/Tend to trust") * 100,
    distrust = f_return_p(data_final, "TrustNISRAstats2", "Tend to distrust/Distrust greatly") * 100,
    dont_know = f_return_p(data_final, "TrustNISRAstats2", "Don't know") * 100
  ))

saveRDS(trust_stats_data, paste0(data_folder, "Trend/", current_year, "/trust_stats_data.RDS"))

trust_stats_data <- trust_stats_data %>%
  mutate(year = as.character(year))

## Add charts for "Reasons for trusting NISRA statistics" and "Reasons for distrusting NISRA statistics" ####
# Need to create trend_data_for_charts.R section for each new chart? ods tables?

## Chart 9: Trust in statistics produced by NISRA  and ONS ####

trust_ons_stats_data <- data_ons %>%
  filter(`Related Variable` == "TrustNISRAstats2")

trust_stats_nisra_ons_data <- trust_stats_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    trust = trust_ons_stats_data$`Trust a great deal` + trust_ons_stats_data$`Tend to trust`,
    distrust = trust_ons_stats_data$`Tend to distrust` + trust_ons_stats_data$`Distrust greatly`,
    dont_know = trust_ons_stats_data$`Don't know`
  ))


## Chart 10: NISRA statistics are important to understand Northern Ireland by year ####

stats_important_data <- stats_important_data %>%
  rbind(data.frame(
    year = current_year,
    agree = f_return_p(data_final, "NISRAstatsImp2", "Strongly Agree/Tend to Agree") * 100,
    disagree = f_return_p(data_final, "NISRAstatsImp2", "Tend to disagree/Strongly disagree") * 100,
    dont_know = f_return_p(data_final, "NISRAstatsImp2", "Don't know") * 100
  ))

saveRDS(stats_important_data, paste0(data_folder, "Trend/", current_year, "/stats_important_data.RDS"))

stats_important_data <- stats_important_data %>%
  mutate(year = as.character(year))

## Chart 11: Statistics produced are important to understand our country, NISRA and ONS ####

stats_important_ons_data <- data_ons %>%
  filter(`Related Variable` == "NISRAstatsImp2")

stats_important_nisra_ons_data <- stats_important_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = stats_important_ons_data$`Strongly agree` + stats_important_ons_data$`Tend to agree`,
    disagree = stats_important_ons_data$`Strongly disagree` + stats_important_ons_data$`Tend to disagree`,
    dont_know = stats_important_ons_data$`Don't know`
  ))

## Chart 12: NISRA statistics are free from political interference by year ####

political_data <- political_data %>%
  rbind(data.frame(
    year = current_year,
    agree = f_return_p(data_final, "Political2", "Strongly Agree/Tend to Agree") * 100,
    disagree = f_return_p(data_final, "Political2", "Tend to disagree/Strongly disagree") * 100,
    dont_know = f_return_p(data_final, "Political2", "Don't know") * 100
  ))

saveRDS(political_data, paste0(data_folder, "Trend/", current_year, "/political_data.RDS"))

political_data <- political_data %>%
  mutate(year = as.character(year))

## Chart 13: Statistics produced are free from political interference, NISRA and ONS ####

political_ons_data <- data_ons %>%
  filter(`Related Variable` == "Political2")

political_nisra_ons_data <- political_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = political_ons_data$`Strongly agree` + political_ons_data$`Tend to agree`,
    disagree = political_ons_data$`Strongly disagree` + political_ons_data$`Tend to disagree`,
    dont_know = political_ons_data$`Don't know`
  ))

## Chart 14: Personal information provided to NISRA will be kept confidential by year ####

confidential_data <- confidential_data %>%
  rbind(data.frame(
    year = current_year,
    agree = f_return_p(data_final, "Confidential2", "Strongly Agree/Tend to Agree") * 100,
    disagree = f_return_p(data_final, "Confidential2", "Tend to disagree/Strongly disagree") * 100,
    dont_know = f_return_p(data_final, "Confidential2", "Don't know") * 100
  ))

saveRDS(confidential_data, paste0(data_folder, "Trend/", current_year, "/confidential_data.RDS"))

confidential_data <- confidential_data %>%
  mutate(year = as.character(year))

## Chart 15: Belief that personal information provided will be kept confidential, NISRA and ONS ####

condifential_ons_data <- data_ons %>%
  filter(`Related Variable` == "Confidential2")

condifential_nisra_ons_data <- confidential_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = condifential_ons_data$`Strongly agree` + condifential_ons_data$`Tend to agree`,
    disagree = condifential_ons_data$`Strongly disagree` + condifential_ons_data$`Tend to disagree`,
    dont_know = condifential_ons_data$`Don't know`
  ))

# Figures for commentary ####

## Introduction ####

sample_size <- prettyNum(nrow(data_final), big.mark = ",")
heard_of_nisra <- round_half_up(aware_nisra_data$pct[aware_nisra_data$year == current_year])
trust_in_nisra <- round_half_up(trust_nisra_data$trust[trust_nisra_data$year == current_year])
trust_in_nisra_stats <- round_half_up(trust_stats_data$trust[trust_stats_data$year == current_year])
importance_of_stats <- round_half_up(stats_important_data$agree[stats_important_data$year == current_year])
free_from_interference <- round_half_up(political_data$agree[political_data$year == current_year])

## Awareness of NISRA ####

aware_of_ons <- round_half_up(aware_nisra_ons_data$ons[aware_nisra_ons_data$year == ons_year])

## Awareness of NISRA Statistics ####
## We need: aware of all, aware of none and aware of each individual answer

### Not heard of NISRA and aware of all / none of the statistics ####

# Initialise new count column
data_final <- data_final %>%
  mutate(
    pcos_yes_count = NA
  )

# Loop only through PCOS1d_vars
for (i in 1:nrow(data_final)) {
  data_final$pcos_yes_count[i] <- 0
  
  for (j in 1:length(PCOS1d_vars)) {
    value <- data_final[[PCOS1d_vars[j]]][i]
    
    if (!is.na(value) && value == "Yes") {
      data_final$pcos_yes_count[i] <- data_final$pcos_yes_count[i] + 1
    }
  }
}

# Add summary flags
data_final <- data_final %>%
  mutate(
    aware_none = pcos_yes_count == 0,
    aware_all  = pcos_yes_count == length(PCOS1d_vars)
  )

aware_none <- round_half_up(f_return_p_group(data_final, "aware_none", TRUE, "PCOS1", "No") * 100)
aware_all <- round_half_up(f_return_p_group(data_final, "aware_all", TRUE, "PCOS1", "No") * 100)

### Not heard of NISRA but aware of outputs ####
## Removing census for 2025 and adding household waste

#not_heard_aware_census <- round_half_up(aware_stats_data$yes[grepl("NI Census", aware_stats_data$output)])
aware_unemployment <- round_half_up(aware_stats_data$yes[grepl("unemployment", aware_stats_data$output)])
aware_hospital <- round_half_up(aware_stats_data$yes[grepl("hospital", aware_stats_data$output)])
aware_people <- round_half_up(aware_stats_data$yes[grepl("number of people", aware_stats_data$output)])
aware_cycling <- round_half_up(aware_stats_data$yes[grepl("cycling", aware_stats_data$output)])
aware_waste <- round_half_up(aware_stats_data$yes[grepl("household waste", aware_stats_data$output)])

### Heard of NISRA and aware of all / none of the statistics ####

# heard_aware_none <- round_half_up(f_return_p_group(data_final, "heard_aware_none", TRUE, "PCOS1", "Yes") * 100)
# heard_aware_all <- round_half_up(f_return_p_group(data_final, "heard_aware_all", TRUE, "PCOS1", "Yes") * 100)

### Not heard of NISRA and aware of outputs ####

# heard_aware_census <- round_half_up(aware_stats_by_nisra_data$yes[grepl("NI Census", aware_stats_by_nisra_data$output)])
# heard_aware_people <- round_half_up(aware_stats_by_nisra_data$yes[grepl("number of people", aware_stats_by_nisra_data$output)])
# heard_aware_deaths <- round_half_up(aware_stats_by_nisra_data$yes[grepl("deaths", aware_stats_by_nisra_data$output)])
# heard_aware_unemployment <- round_half_up(aware_stats_by_nisra_data$yes[grepl("unemployment", aware_stats_by_nisra_data$output)])
# heard_of_aware_qualifications <- round_half_up(aware_stats_by_nisra_data$yes[grepl("Qualifications", aware_stats_by_nisra_data$output)])
# heard_of_aware_poverty <- round_half_up(aware_stats_by_nisra_data$yes[grepl("poverty", aware_stats_by_nisra_data$output)])
# heard_of_aware_cycling <- round_half_up(aware_stats_by_nisra_data$yes[grepl("cycling", aware_stats_by_nisra_data$output)])

## Trust in NISRA ####

dont_know_trust_nisra <- round_half_up(trust_nisra_data$dont_know[trust_nisra_data$year == current_year])
distrust_nisra <- round_half_up(trust_nisra_data$distrust[trust_nisra_data$year == current_year])

heard_of_and_trust_nisra <- round_half_up(f_return_p_group(data_final, "TrustNISRA2", "Trust a great deal/Tend to trust", "PCOS1", "Yes") * 100)

trust_in_ons <- round_half_up(trust_nisra_ons_data$trust[grepl("ONS", trust_nisra_ons_data$org)])
trust_in_media <- round_half_up(trust_institutions_data$trust[grepl("The media", trust_institutions_data$org)])
trust_in_assem_elect_body <- round_half_up(trust_institutions_data$trust[grepl(AssemblyElectedBody_name, trust_institutions_data$org)])
trust_in_nics <- round_half_up(trust_institutions_data$trust[grepl("Civil Service", trust_institutions_data$org)])


## Trust in NISRA Statistics ####

distrust_nisra_stats <- round_half_up(trust_stats_data$distrust[trust_stats_data == current_year])
dont_know_trust_nisra_stats <- round_half_up(trust_stats_data$dont_know[trust_stats_data == current_year])

heard_of_and_trust_nisra_stats <- round_half_up(f_return_p_group(data_final, "TrustNISRAstats2", "Trust a great deal/Tend to trust", "PCOS1", "Yes") * 100)

trust_in_ons_stats <- round_half_up(trust_stats_nisra_ons_data$trust[grepl("ONS", trust_stats_nisra_ons_data$org)])
distrust_ons_stats <- round_half_up(trust_stats_nisra_ons_data$distrust[grepl("ONS", trust_stats_nisra_ons_data$org)])
dont_know_trust_ons_stats <- round_half_up(trust_stats_nisra_ons_data$dont_know[grepl("ONS", trust_stats_nisra_ons_data$org)])

## Reasons Trust in NISRA Statistics ####
## Need to add figures for PCOS3b and PCOS3c

Reasons_trust_experience <- round_half_up(counts$percent[grepl("experience", counts$Var1)])
Reasons_trust_accurate <- round_half_up(counts$percent[grepl("accurate", counts$Var1)])
Reasons_trust_good <- round_half_up(counts$percent[grepl("good", counts$Var1)])
Reasons_trust_easy <- round_half_up(counts$percent[grepl("easy", counts$Var1)])
Reasons_trust_nisra_vested <- round_half_up(counts$percent[grepl("NISRA does not have a vested interest", counts$Var1)])
Reasons_trust_gov_vested <- round_half_up(counts$percent[grepl("Government departments in Northern Ireland", counts$Var1)])
Reasons_trust_experts <- round_half_up(counts$percent[grepl("experts", counts$Var1)])
Reasons_trust_other <- round_half_up(counts$percent[grepl("Other reason", counts$Var1)])

## Reasons distrust in NISRA Statistics ####

Reasons_distrust_experience <- round_half_up(counts2$percent[grepl("experience", counts2$Var1)])
Reasons_distrust_accurate <- round_half_up(counts2$percent[grepl("accurate", counts2$Var1)])
Reasons_distrust_bad <- round_half_up(counts2$percent[grepl("bad", counts2$Var1)])
Reasons_distrust_difficult <- round_half_up(counts2$percent[grepl("difficult", counts2$Var1)])
Reasons_distrust_nisra_vested <- round_half_up(counts2$percent[grepl("NISRA has a vested interest", counts2$Var1)])
Reasons_distrust_gov_vested <- round_half_up(counts2$percent[grepl("Government departments in Northern Ireland", counts2$Var1)])
Reasons_distrust_politicians <- round_half_up(counts2$percent[grepl("politicians", counts2$Var1)])
Reasons_distrust_media <- round_half_up(counts2$percent[grepl("media", counts2$Var1)])
Reasons_distrust_story <- round_half_up(counts2$percent[grepl("story", counts2$Var1)])
Reasons_distrust_understanding <- round_half_up(counts2$percent[grepl("understanding", counts2$Var1)])
Reasons_distrust_other <- round_half_up(counts2$percent[grepl("Other", counts2$Var1)])

## Value ####

disagree_importance <- round_half_up(stats_important_data$disagree[stats_important_data$year == current_year])
dont_know_importance <- round_half_up(stats_important_data$dont_know[stats_important_data$year == current_year])

importance_of_ons <- round_half_up(stats_important_nisra_ons_data$agree[grepl("ONS", stats_important_nisra_ons_data$org)])
disagre_importance_ons <- round_half_up(stats_important_nisra_ons_data$disagree[grepl("ONS", stats_important_nisra_ons_data$org)])
dont_know_importance_ons <- round_half_up(stats_important_nisra_ons_data$dont_know[grepl("ONS", stats_important_nisra_ons_data$org)])

## Political Interference ####

disagree_interference <- round_half_up(political_data$disagree[political_data == current_year])
dont_know_interference <- round_half_up(political_data$dont_know[political_data == current_year])

free_from_interference_ons <- round_half_up(political_nisra_ons_data$agree[grepl("ONS", political_nisra_ons_data$org)])

## Confidentiality ####

agree_confidential <- round_half_up(confidential_data$agree[confidential_data$year == current_year])
dont_know_confidential <- round_half_up(confidential_data$dont_know[confidential_data$year == current_year])
disagree_confidential <- round_half_up(confidential_data$disagree[confidential_data$year == current_year])

agree_confidential_ons <- round_half_up(condifential_nisra_ons_data$agree[grepl("ONS", condifential_nisra_ons_data$org)])
