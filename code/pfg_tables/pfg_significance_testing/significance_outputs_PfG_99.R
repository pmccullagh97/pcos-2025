# Select All and run this script to produce final output
# Or press Ctrl + Alt + R

library(here)
source(paste0(here(), "/code/config.R"))

# Define years to analyse based on current_year value from config (and assign to global environment) ####

#SELECT FIRST LINE FOR ALL YEARS; SECOND LINE FOR CURRENT YEAR ONLY - OR EDIT FOR SELECTED SINGLE YEAR

data_years <<- c(seq(2014, 2016, 2), 2019:current_year)
#data_years <<- c(current_year)

source(paste0(here(), "/code/pfg_tables/pfg_significance_testing/significance_testing_PfG.R"))

if (!exists(paste0(here(), "/outputs/PfG/significance outputs"))) {
  dir.create(paste0(here(), "/outputs/PfG/significance outputs"))
}

for (year in data_years) {

# Create NI Assembly Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

## Trust NI Assembly geography sheet for year####
  
addWorksheet(wb, paste("Trust NI Assembly, geog",year))

setColWidths(wb, paste("Trust NI Assembly, geog",year),
  cols = 1:ncol(eval(as.name(paste0("assembly_AA_stats_",year)))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_AA_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust NI Assembly, geog",year),
  x = c("Trust in NI Assembly (WEIGHTED)",
        "[Note: Qu in 2019 & 2023 was Elected bodies]"),
  startRow = r
)

addStyle(wb, paste("Trust NI Assembly, geog",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2


### Trust NI Assembly by Local Government District ####

if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_LGD_stats_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste("Trust in NI Assembly - by Local Government District -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_LGD_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("Yes - Trust in NI Assembly by Local Government District - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_LGD_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("No - Trust in NI Assembly by Local Government District - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_LGD_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("Don't know - Trust in NI Assembly by Local Government District - ", year)
) }


### Trust NI Assembly by Assembly Area #### 

if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_AA_stats_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste("Trust in NI Assembly - by Assembly Area -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_AA_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("Yes - Trust in NI Assembly by Assembly Area - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_AA_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("No - Trust in NI Assembly by Assembly Area - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_AA_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, geog",year),
  title = paste0("Don't know - Trust in NI Assembly by Assembly Area - ", year)
) }


## Trust NI Assembly area sheet for year (deprivation & urban-rural) ####

addWorksheet(wb, paste("Trust NI Assembly, area",year))

setColWidths(wb, paste("Trust NI Assembly, area",year),
             cols = 1:ncol(eval(as.name(paste0("assembly_age_stats_",year)))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust NI Assembly, area",year),
          x = c("Trust in NI Assembly (WEIGHTED)",
                "[Note: Qu in 2019 & 2023 was Elected bodies]"),
          startRow = r
)

addStyle(wb, paste("Trust NI Assembly, area",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust NI Assembly by Urban/Rural ####

if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_urbh_stats_",year)))),
  sheet = paste("Trust NI Assembly, area",year),
  title = paste("Trust in NI Assembly - by Urban/Rural -", year)
) }


### Trust NI Assembly by Deprivation ####

if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depriv_stats_",year)))),
  sheet = paste("Trust NI Assembly, area",year),
  title = paste("Trust in NI Assembly - by Deprivation -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_depriv_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, area",year),
  title = paste0("Yes - Trust in NI Assembly by Deprivation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_depriv_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, area",year),
  title = paste0("No - Trust in NI Assembly by Deprivation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_depriv_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, area",year),
  title = paste0("Don't know - Trust in NI Assembly by Deprivation - ", year)
) }


## Trust NI Assembly equality sheet for each year####

addWorksheet(wb, paste("Trust NI Assembly, equal",year))

setColWidths(wb, paste("Trust NI Assembly, equal",year),
             cols = 1:ncol(eval(as.name(paste0("assembly_age_stats_",year)))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust NI Assembly, equal",year),
          x = c("Trust in NI Assembly (WEIGHTED)",
                "[Note: Qu in 2019 & 2023 was Elected bodies]"),
          startRow = r
)

addStyle(wb, paste("Trust NI Assembly, equal",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust NI Assembly by Sex ####

if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_sex_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Sex -", year)
) }


### Trust NI Assembly by Age ####

if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_age_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Age Group -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Age Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Age Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Age Group - ", year)
) }


### Trust NI Assembly by Marital Status ####

if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ms_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Martial Status -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ms_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Marital Status - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_ms_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Marital Status - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_ms_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Marital Status - ", year)
) }


### Trust NI Assembly by Marital Status Group ####

if ("MS_GRP" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_msgrp_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Martial Status Group -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_msgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Martial Status Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_msgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Martial Status Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_msgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Martial Status Group - ", year)
) }


### Trust NI Assembly by Disability ####

if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_LimLongStand_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Disability -", year)
) }


### Trust NI Assembly by Religion ####

if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_relig_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Religion -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_relig_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Religion - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_relig_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Religion - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_relig_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Religion - ", year)
) }


### Trust NI Assembly by Sexual Orientation ####

if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_sexualorient_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Sexual Orientation -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_sexualorient_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Sexual Orientation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_sexualorient_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Sexual Orientation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_sexualorient_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Sexual Orientation - ", year)
) }


### Trust NI Assembly by Dependants ####

if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Dependants -", year)
) }


### Trust NI Assembly by Child Dependants ####

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_child_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Child Dependants -", year)
) }


### Trust NI Assembly by Disabled Dependants ####

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_disab_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Disabled Dependants -", year)
) }


### Trust NI Assembly by Elderly Dependants ####

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_elderly_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Elderly Dependants -", year)
) }


### Trust NI Assembly by Ethnicity #### 

if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnic_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Ethnicity -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ethnic_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Ethnicity - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_ethnic_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Ethnicity - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_ethnic_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Ethnicity - ", year)
) }


### Trust NI Assembly by Ethnic Group #### 

if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnicgrp_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Ethnic Group -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ethnicgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Yes - Trust in NI Assembly by Ethnic Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_disagree_ethnicgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("No - Trust in NI Assembly by Ethnic Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_dont_know_ethnicgrp_z_scores_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste0("Don't know - Trust in NI Assembly by Ethnic Group - ", year)
) }


### Trust NI Assembly by Ethnic White Other #### 

if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnicwo_stats_",year)))),
  sheet = paste("Trust NI Assembly, equal",year),
  title = paste("Trust in NI Assembly - by Ethnic White Other -", year)
) }


## Trust NI Assembly (exc DK) geography sheet for year ####

addWorksheet(wb, paste("TruNIAssemExDK, geog",year))

setColWidths(wb, paste("TruNIAssemExDK, geog",year),
  cols = 1:ncol((eval(as.name(paste0("assembly_AA_ex_dk_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_AA_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruNIAssemExDK, geog",year),
  x = c("Trust in NI Assembly (excluding Don't knows) (WEIGHTED)",
        "[Note: Qu in 2019 & 2023 was Elected bodies]"),
  startRow = r
)

addStyle(wb, paste("TruNIAssemExDK, geog",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 3


### Trust NI Assembly (exc DK) by Local Government District  ####

if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_LGD_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, geog",year),
  title = paste0("Trust in NI Assembly - by Local Government District - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_LGD_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, geog",year),
  title = paste0("Trust in NI Assembly by Local Government District - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Assembly Area  #### 

if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_AA_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, geog",year),
  title = paste0("Trust in NI Assembly - by Assembly Area - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_AA_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, geog",year),
  title = paste0("Trust in NI Assembly by Assembly Area - ", year, " (exc DKs)")
) }


## Trust NI Assembly (exc DK) area sheet for year (deprivation & urban-rural) ####

addWorksheet(wb, paste("TruNIAssemExDK, area",year))

setColWidths(wb, paste("TruNIAssemExDK, area",year),
             cols = 1:ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruNIAssemExDK, area",year),
          x = c("Trust in NI Assembly (excluding Don't knows) (WEIGHTED)",
                "[Note: Qu in 2019 & 2023 was Elected bodies]"),
          startRow = r
)

addStyle(wb, paste("TruNIAssemExDK, area",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 3


### Trust NI Assembly (exc DK) by Urban/Rural ####

if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_urbh_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, area",year),
  title = paste0("Trust in NI Assembly - by Urban/Rural - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Deprivation  ####

if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depriv_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, area",year),
  title = paste0("Trust in NI Assembly - by Deprivation - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_depriv_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, area",year),
  title = paste0("Trust in NI Assembly by Deprivation - ", year, " (exc DKs)")
) }


## Trust NI Assembly (exc DK) equality sheet for year ####

addWorksheet(wb, paste("TruNIAssemExDK, equal",year))

setColWidths(wb, paste("TruNIAssemExDK, equal",year),
             cols = 1:ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruNIAssemExDK, equal",year),
          x = c("Trust in NI Assembly (excluding Don't knows) (WEIGHTED)",
                "[Note: Qu in 2019 & 2023 was Elected bodies]"),
          startRow = r
)

addStyle(wb, paste("TruNIAssemExDK, equal",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 3


### Trust NI Assembly (exc DK) by Sex ####

if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_sex_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Sex - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Age ####

if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_age_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Age Group - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_age_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Age Group - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Marital Status  ####

if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ms_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Marital Status - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ms_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Marital Status - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Marital Status Group ####

if ("MS_GRP" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_msgrp_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Marital Status Group - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_msgrp_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Marital Status Group - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Disability  ####

if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_LimLongStand_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Disability - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Religion  ####

if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_relig_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Religion - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_relig_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Religion - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Sexual Orientation  ####

if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_sexualorient_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Sexual Orientation - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_sexualorient_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Sexual Orientation - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Dependants  ####

if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Dependants - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Child Dependants  ####

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_child_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Child Dependants - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Disabled Dependants  ####

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_disab_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Disabled Dependants - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Elderly Dependants  ####

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_depend_elderly_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Elderly Dependants - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Ethnicity  #### 

if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnic_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Ethnicity - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ethnic_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Ethnicity - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Ethnic Group  #### 

if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnicgrp_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Ethnic Group - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("assembly_ethnicgrp_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly by Ethnic Group - ", year, " (exc DKs)")
) }


### Trust NI Assembly (exc DK) by Ethnic White Other #### 

if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("assembly_ethnicwo_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK, equal",year),
  title = paste0("Trust in NI Assembly - by Ethnic White Other - ", year, " (exc DKs)")
) }  


# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/PfG/significance outputs/significance output 99 NI assembly ",year, ".xlsx"),
             overwrite = TRUE
)
openXL(paste0(here(), "/outputs/PfG/significance outputs/significance output 99 NI assembly ",year, ".xlsx"))

}


##### MEDIA ###

for (year in data_years) {

# Create media Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")


## Trust in media geography sheet for year ####

addWorksheet(wb, paste("Trust Media, geog",year))

setColWidths(wb, paste("Trust Media, geog",year),
             cols = 1:ncol((eval(as.name(paste0("media_AA_stats_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("media_AA_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust Media, geog",year),
          x = "Trust in the Media (WEIGHTED)",
          startRow = r
)

addStyle(wb, paste("Trust Media, geog",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust In media by Local Government District ####

if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_LGD_stats_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste("Trust in the Media - by Local Government District -", year)
  ) 
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_LGD_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("Yes - Trust in the Media by Local Government District - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_disagree_LGD_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("No - Trust in the Media by Local Government District - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_dont_know_LGD_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("Don't know - Trust in the Media by Local Government District - ", year)
  ) }


### Trust In media by Assembly Area #### 

if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_AA_stats_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste("Trust in the Media - by Assembly Area -", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_AA_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("Yes - Trust in the Media by Assembly Area - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_disagree_AA_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("No - Trust in the Media by Assembly Area - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_dont_know_AA_z_scores_",year)))),
    sheet = paste("Trust Media, geog",year),
    title = paste0("Don't know - Trust in the Media by Assembly Area - ", year)
  ) }


## Trust in media area sheet for each year (deprivation & urban-rural) ####

addWorksheet(wb, paste("Trust Media, area",year))

setColWidths(wb, paste("Trust Media, area",year),
  cols = 1:ncol((eval(as.name(paste0("media_age_stats_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust Media, area",year),
  x = "Trust in the Media (WEIGHTED)",
  startRow = r
)

addStyle(wb, paste("Trust Media, area",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2


### Trust In media by Urban/Rural ####

if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_urbh_stats_",year)))),
    sheet = paste("Trust Media, area",year),
    title = paste("Trust Media - by Urban/Rural -", year)
  ) }


### Trust In media by Deprivation ####

if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depriv_stats_",year)))),
    sheet = paste("Trust Media, area",year),
    title = paste("Trust in the Media - by Deprivation -", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_depriv_z_scores_",year)))),
    sheet = paste("Trust Media, area",year),
    title = paste0("Yes - Trust in the Media by Deprivation - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_disagree_depriv_z_scores_",year)))),
    sheet = paste("Trust Media, area",year),
    title = paste0("No - Trust in the Media by Deprivation - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_dont_know_depriv_z_scores_",year)))),
    sheet = paste("Trust Media, area",year),
    title = paste0("Don't know - Trust in the Media by Deprivation - ", year)
  ) }


## Trust in media area equality sheet for each year####

addWorksheet(wb, paste("Trust Media, equal",year))

setColWidths(wb, paste("Trust Media, equal",year),
             cols = 1:ncol((eval(as.name(paste0("media_age_stats_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust Media, equal",year),
          x = "Trust in the Media (WEIGHTED)",
          startRow = r
)

addStyle(wb, paste("Trust Media, equal",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust in media by Sex ####

if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_sex_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Sex -", year)
) }


### Trust in media by Age ####

if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_age_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Age Group -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_age_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Yes - Trust in the Media by Age Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_disagree_age_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("No - Trust in the Media by Age Group - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_dont_know_age_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Don't know - Trust in the Media by Age Group - ", year)
) }


### Trust In media by Marital Status ####

if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_ms_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Marital Status -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_ms_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Yes - Trust in the Media by Marital Status - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_disagree_ms_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("No - Trust in the Media by Marital Status - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_dont_know_ms_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Don't know - Trust in the Media by Marital Status - ", year)
) }


### Trust In media by Marital Status Group ####

if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_msgrp_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Marital Status Group -", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_msgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("Yes - Trust in the Media by Marital Status Group - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_disagree_msgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("No - Trust in the Media by Marital Status Group - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_dont_know_msgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("Don't know - Trust in the Media by Marital Status Group - ", year)
  ) }


### Trust In media by Disability ####

if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_LimLongStand_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Disability -", year)
) }


### Trust In media by Religion ####

if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_relig_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Religion -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_relig_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Yes - Trust in the Media by Religion - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_disagree_relig_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("No - Trust in the Media by Religion - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_dont_know_relig_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Don't know - Trust in the Media by Religion - ", year)
) }


### Trust In media by Sexual Orientation ####

if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_sexualorient_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Sexual Orientation -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_sexualorient_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Yes - Trust in the Media by Sexual Orientation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_disagree_sexualorient_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("No - Trust in the Media by Sexual Orientation - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_dont_know_sexualorient_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Don't know - Trust in the Media by Sexual Orientation - ", year)
) }


### Trust In media by Dependants ####

if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_depend_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Dependants -", year)
)}


### Trust In media by Child Dependants ####

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_child_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Child Dependants -", year)
  ) }


### Trust In media by Disabled Dependants ####

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_disab_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Disabled Dependants -", year)
  ) }


### Trust In media by Elderly Dependants ####

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_elderly_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Elderly Dependants -", year)
  ) }


### Trust In media by Ethnicity #### 

if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_ethnic_stats_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste("Trust in the Media - by Ethnicity -", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_ethnic_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Yes - Trust in the Media by Ethnicity - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_disagree_ethnic_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("No - Trust in the Media by Ethnicity - ", year)
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_dont_know_ethnic_z_scores_",year)))),
  sheet = paste("Trust Media, equal",year),
  title = paste0("Don't know - Trust in the Media by Ethnicity - ", year)
) }


### Trust In media by Ethnic Group #### 

if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_ethnicgrp_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Ethnic Group -", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_ethnicgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("Yes - Trust in the Media by Ethnic Group - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_disagree_ethnicgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("No - Trust in the Media by Ethnic Group - ", year)
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_dont_know_ethnicgrp_z_scores_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste0("Don't know - Trust in the Media by Ethnic Group - ", year)
  ) }


### Trust In media by Ethnic White Other #### 

if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_ethnicwo_stats_",year)))),
    sheet = paste("Trust Media, equal",year),
    title = paste("Trust in the Media - by Ethnic White Other -", year)
  ) }


## Trust in Media (exc DK) geography sheet for year ####

addWorksheet(wb, paste("TruMediaExDK, geog",year))

setColWidths(wb, paste("TruMediaExDK, geog",year),
  cols = 1:ncol((eval(as.name(paste0("media_AA_ex_dk_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("media_AA_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruMediaExDK, geog",year),
  x = "Trust in the Media (excluding Don't knows) (WEIGHTED)",
  startRow = r
)

addStyle(wb, paste("TruMediaExDK, geog",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2


### Trust in Media (exc DK) by Local Government District  ####

if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_LGD_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, geog",year),
    title = paste0("Trust the Media - by Local Government District - ", year, " (exc DKs)")
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_LGD_z_scores_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, geog",year),
    title = paste0("Trust the Media by Local Government District - ", year, " (exc DKs)")
  ) }

### Trust in Media (exc DK) by Assembly Area  #### 

if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_AA_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, geog",year),
    title = paste0("Trust the Media - by Assembly Area - ", year, " (exc DKs)")
  ) }

f_insert_99z_table(
  df = (eval(as.name(paste0("media_AA_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, geog",year),
  title = paste0("Trust the Media by Assembly Area - ", year, " (exc DKs)")
)


## Trust in Media (exc DK) area sheet for year ####

addWorksheet(wb, paste("TruMediaExDK, area",year))

setColWidths(wb, paste("TruMediaExDK, area",year),
             cols = 1:ncol((eval(as.name(paste0("media_age_ex_dk_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruMediaExDK, area",year),
          x = "Trust in the Media (excluding Don't knows) (WEIGHTED)",
          startRow = r
)

addStyle(wb, paste("TruMediaExDK, area",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust in Media (exc DK) by Urban/Rural  ####

if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_urbh_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, area",year),
    title = paste0("Trust the Media - by Urban/Rural - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Deprivation  ####

if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depriv_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, area",year),
    title = paste0("Trust the Media - by Deprivation- ", year, " (exc DKs)")
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_depriv_z_scores_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, area",year),
    title = paste0("Trust the Media by Deprivation - ", year, " (exc DKs)")
  ) }


## Trust in Media (exc DK) equality sheet for year ####

addWorksheet(wb, paste("TruMediaExDK, equal",year))

setColWidths(wb, paste("TruMediaExDK, equal",year),
             cols = 1:ncol((eval(as.name(paste0("media_age_ex_dk_",year))))),
             widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruMediaExDK, equal",year),
          x = "Trust in the Media (excluding Don't knows) (WEIGHTED)",
          startRow = r
)

addStyle(wb, paste("TruMediaExDK, equal",year),
         style = pt,
         rows = r,
         cols = 1
)

r <- r + 2


### Trust in Media (exc DK) by Sex ####

if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_sex_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Sex - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Age ####

if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_age_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Age Group - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_age_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media by Age Group - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Marital Status  ####

if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_ms_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Marital Status - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_ms_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media by Marital Status - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Marital Status Group ####

if ("MS_GRP" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_msgrp_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Marital Status Group - ", year, " (exc DKs)")
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_msgrp_z_scores_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media by Marital Status Group - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Disability  ####

if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_LimLongStand_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Disability - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Religion  ####

if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_relig_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Religion - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_relig_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media by Religion - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Sexual Orientation  ####

if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_sexualorient_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Sexual Orientation - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_sexualorient_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media by Sexual Orientation - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Dependants  ####

if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
f_insert_99sig_table(
  df = (eval(as.name(paste0("media_depend_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Dependants - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Child Dependants  ####

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_child_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Child Dependants - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Disabled Dependants  ####

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_disab_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Disabled Dependants - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Elderly Dependants  ####

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_depend_elderly_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Elderly Dependants - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Ethnicity ####

if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
  df = (eval(as.name(paste0("media_ethnic_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media - by Ethnicity - ", year, " (exc DKs)")
)

f_insert_99z_table(
  df = (eval(as.name(paste0("media_ethnic_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK, equal",year),
  title = paste0("Trust the Media by Ethnicity - ", year, " (exc DKs)")
) }


### Trust in Media (exc DK) by Ethnic Group ####

if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_ethnicgrp_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Ethnic Group - ", year, " (exc DKs)")
  )
  
  f_insert_99z_table(
    df = (eval(as.name(paste0("media_ethnicgrp_z_scores_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media by Ethnic Group - ", year, " (exc DKs)")
  ) }


### Trust in Media (exc DK) by Ethnic White Other ####

if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {
  f_insert_99sig_table(
    df = (eval(as.name(paste0("media_ethnicwo_ex_dk_",year)))),
    sheet = paste("TruMediaExDK, equal",year),
    title = paste0("Trust the Media - by Ethnic White Other - ", year, " (exc DKs)")
  ) }


# Save Workbook ####

saveWorkbook(wb,
  paste0(here(), "/outputs/PfG/significance outputs/significance output 99 media ",year,".xlsx"),
  overwrite = TRUE
)

openXL(paste0(here(), "/outputs/PfG/significance outputs/significance output 99 media ",year,".xlsx"))

}
