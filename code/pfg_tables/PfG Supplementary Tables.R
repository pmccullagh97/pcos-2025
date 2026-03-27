library(here)
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
source(paste0(here(), "/code/pfg_tables/PfG Historic Data Prep.R"))

if (!dir.exists(paste0(here(), "/outputs/PfG"))) {
  dir.create(paste0(here(), "/outputs/PfG"))
}

# SET YEAR RANGE - ALL YEARS OR CURRENT YEAR #### 
# by uncommenting first or second 'for' below
  # use first option to run complete time series
  # use second option if no changes from previous year

for (sup_year in c(seq(2014, 2016, 2), 2019:current_year)) {
#for (sup_year in c(current_year)) {

## Set questions to analyse ####
sup_pfg_q <- c("TrustAssemblyElectedBody2", "TrustMedia2")

## Select data ####
sup_data <- eval((as.name(paste0("data_",sup_year))))

## Remove DK & refusals from covariates #####
# (values listed below for each covariate must include all valid values) 

sup_data <- sup_data %>%
mutate(
  SEX = factor(SEX,levels = c("M","F"))
)

if ("OwnRelig2" %in% names(sup_data)) {
  sup_data <- sup_data %>%
  mutate(
    OwnRelig2 = factor(OwnRelig2,levels = c("Catholic", "Protestant", "Other/No Religion")))
}

## Select co-variates ####

if (sup_year %in% c(2022:current_year)) {
  sup_covar <- c("LGD2014name", "AsmblyArea", "URBH", "Deprivation", "SEX", "AGE2", "MS", "MS_GRP", "OwnRelig2", "LimLongStand", "ETHNIC", "Ethnic_group", "Ethnic_white_other", "Dependants", "DEPEND1", "DEPEND2", "DEPEND3", "Sexual_orient")}

if (sup_year %in% c(2021)) {
  sup_covar <- c("LGD2014name", "AsmblyArea", "URBH", "Deprivation", "SEX", "AGE2", "MS", "MS_GRP", "OwnRelig2", "LimLongStand", "ETHNIC", "Ethnic_group", "Ethnic_white_other", "Dependants", "DEPEND1", "DEPEND2", "DEPEND3")}

if (sup_year %in% c(2020)) {
  sup_covar <- c("LGD2014name", "AsmblyArea", "URBH", "Deprivation", "SEX", "AGE2", "MS", "MS_GRP", "OwnRelig2", "ETHNIC", "Ethnic_group", "Ethnic_white_other", "Dependants", "DEPEND1", "DEPEND2", "DEPEND3")}

if (sup_year %in% c(2019)) {
  sup_covar <- c("LGD2014name","AsmblyArea", "URBH", "Deprivation", "SEX", "AGE2","MS", "MS_GRP", "OwnRelig2", "LimLongStand", "ETHNIC", "Ethnic_group", "Ethnic_white_other", "Dependants", "DEPEND1", "DEPEND2", "DEPEND3", "Sexual_orient")}

if (sup_year %in% c(2014,2016)) {
  sup_covar <- c("LGD2014name","AsmblyArea", "SEX", "AGE2", "MS", "MS_GRP", "LimLongStand", "ETHNIC", "Ethnic_group", "Ethnic_white_other", "Dependants", "Sexual_orient")}

## Set age_weight ####

age_weight <- if (sup_year %in% 2012:2016) {
  "weight"
} else if (sup_year == 2020) {
  "W1a"
} else {
  "W1"
}

## Set sex_weight ####

sex_weight <- if (sup_year %in% 2012:2016) {
  "weight"
} else {
  "W2"
}

## Set general_weight ####

general_weight <- if (sup_year %in% 2012:2016) {
  "weight"
} else if (sup_year == 2020) {
  "W4"
} else {
  "W3"
}

## Call function ####

f_supplementary_pfg_tables(data = sup_data,
                       year = sup_year,
                       pfg_q = sup_pfg_q,
                       co_var = sup_covar,
                       age_weight = age_weight,
                       sex_weight = sex_weight,
                       weight = general_weight)
}