# This script does not need run. It will be sourced when you run significance_outputs_PfG.R

# Only make updates to this script to prepare new data frames for outputting to the PfG Significance Outputs

library(here)
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
source(paste0(here(), "/code/pfg_tables/PfG Historic Data Prep.R"))

for (year in data_years) {

# Standardise 'don't know' format ####

dont_know <- if (year %in% 2014:2016) {
  "Don't Know"
} else {
  "Don't know"
}

# recode trust & distrust (NIAssembly & Media) ####
  
trust <- if (year %in% 2014:2016) {
    "Tend to trust/trust a great deal"
} else {
    "Trust a great deal/Tend to trust"
}

distrust <- if (year %in% 2014:2016) {
  "Tend to distrust/distrust greatly"
} else {
  "Tend to distrust/Distrust greatly"
}


# Set weight variables ####

age_weight <- if (year %in% 2012:2016) {
  "weight"
} else if (year == 2020) {
  "W1a"
} else {
  "W1"
}

sex_weight <- if (year %in% 2012:2016) {
  "weight"
} else {
  "W2"
}

weight <- if (year %in% 2012:2016) {
  "weight"
} else if (year == 2020) {
  "W4"
} else {
  "W3"
}


# Trust NI Assembly ####

#AGE - Assembly
if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Age ####
assign(paste0("assembly_age_stats_",year),f_age_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Age (exc DK) ####
assign(paste0("assembly_age_ex_dk_",year),f_age_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_age_z_scores_ex_dk_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#SEX - Assembly
if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Sex ####
assign(paste0("assembly_sex_stats_",year),f_sex_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Sex (exc DK) ####
assign(paste0("assembly_sex_ex_dk_",year),f_sex_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#URBAN_RURAL - Assembly
if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Urban/Rural ####
assign(paste0("assembly_urbh_stats_",year),f_urbh_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Urban/Rural (exc DK) ####
assign(paste0("assembly_urbh_ex_dk_",year),f_urbh_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#LGD - Assembly
if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Local Government District ####
assign(paste0("assembly_LGD_stats_",year),f_lgd_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Local Government District (exc DK) ####
assign(paste0("assembly_LGD_ex_dk_",year),f_lgd_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_LGD_z_scores_ex_dk_",year),f_lgd_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#AA - Assembly
if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Assembly Area #### 
assign(paste0("assembly_AA_stats_",year),f_aa_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Assembly Area (exc DK) #### 
assign(paste0("assembly_AA_ex_dk_",year),f_aa_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_AA_z_scores_ex_dk_",year),f_aa_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#Deprivation - Assembly
if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Deprivation ####
assign(paste0("assembly_depriv_stats_",year),f_depriv_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Deprivation (exc DK) ####
assign(paste0("assembly_depriv_ex_dk_",year),f_depriv_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_depriv_z_scores_ex_dk_",year),f_depriv_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#MARITAL STATUS - Assembly
if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Marital Status ####
assign(paste0("assembly_ms_stats_",year),f_ms_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Marital Status (exc DK) ####
assign(paste0("assembly_ms_ex_dk_",year),f_ms_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_ms_z_scores_ex_dk_",year),f_ms_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#MARITAL STATUS GROUP - Assembly
if ("MS_GRP" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Marital Status group####
assign(paste0("assembly_msgrp_stats_",year),f_msgrp_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Marital Status group (exc DK) ####
assign(paste0("assembly_msgrp_ex_dk_",year),f_msgrp_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_msgrp_z_scores_ex_dk_",year),f_msgrp_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#DISABILITY - Assembly
if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust NI Assembly by Disability ####
assign(paste0("assembly_LimLongStand_stats_",year),f_limlongstand_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Disability (exc DK) ####
assign(paste0("assembly_LimLongStand_ex_dk_",year),f_limlongstand_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#RELIGION - Assembly
if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Religion ####
assign(paste0("assembly_relig_stats_",year),f_relig_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Religion (exc DK) ####
assign(paste0("assembly_relig_ex_dk_",year),f_relig_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_relig_z_scores_ex_dk_",year),f_relig_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#SEXUAL ORIENTATION - Assembly
if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Sexual Orientation ####
assign(paste0("assembly_sexualorient_stats_",year),f_sexualorient_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Sexual Orientation (exc DK) ####
assign(paste0("assembly_sexualorient_ex_dk_",year),f_sexualorient_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_sexualorient_z_scores_ex_dk_",year),f_sexualorient_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#DEPEND - Assembly
if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Dependants ####
assign(paste0("assembly_depend_stats_",year),f_depend_stats_year(year,"Dependants","TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Dependants (exc DK) ####
assign(paste0("assembly_depend_ex_dk_",year),f_depend_stats_year(year,"Dependants","TrustAssemblyElectedBody2", trust, dk = FALSE))
}

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust NI Assembly by Child Dependants ####
assign(paste0("assembly_depend_child_stats_",year),f_depend_stats_year(year,"DEPEND1","TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Child Dependants (exc DK) ####
assign(paste0("assembly_depend_child_ex_dk_",year),f_depend_stats_year(year,"DEPEND1","TrustAssemblyElectedBody2", trust, dk = FALSE))
}

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust NI Assembly by Disabled Dependants ####
assign(paste0("assembly_depend_disab_stats_",year),f_depend_stats_year(year,"DEPEND2","TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Disabled Dependants (exc DK) ####
assign(paste0("assembly_depend_disab_ex_dk_",year),f_depend_stats_year(year,"DEPEND2","TrustAssemblyElectedBody2", trust, dk = FALSE))
}

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust NI Assembly by Elderly Dependants ####
assign(paste0("assembly_depend_elderly_stats_",year),f_depend_stats_year(year,"DEPEND3","TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Elderly Dependants (exc DK) ####
assign(paste0("assembly_depend_elderly_ex_dk_",year),f_depend_stats_year(year,"DEPEND3","TrustAssemblyElectedBody2", trust, dk = FALSE))
}

#ETHNIC - Assembly
if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust NI Assembly by Ethnic ####
assign(paste0("assembly_ethnic_stats_",year),f_ethnic_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Ethnic (exc DK) ####
assign(paste0("assembly_ethnic_ex_dk_",year),f_ethnic_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_ethnic_z_scores_ex_dk_",year),f_ethnic_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#ETHNIC GROUP - Assembly
if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust NI Assembly by Ethnic Group ####
assign(paste0("assembly_ethnicgrp_stats_",year),f_ethnicgrp_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))
assign(paste0("assembly_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustAssemblyElectedBody2", trust))
assign(paste0("assembly_disagree_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))
assign(paste0("assembly_dont_know_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Ethnic Group (exc DK) ####
assign(paste0("assembly_ethnicgrp_ex_dk_",year),f_ethnicgrp_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
assign(paste0("assembly_ethnicgrp_z_scores_ex_dk_",year),f_ethnicgrp_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


#ETHNIC WHITE OTHER - Assembly
if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust NI Assembly by Ethnic White Other ####
assign(paste0("assembly_ethnicwo_stats_",year),f_ethnicwo_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Ethnic White Other (exc DK) ####
assign(paste0("assembly_ethnicwo_ex_dk_",year),f_ethnicwo_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))
}


# Trust the Media ####

#AGE - Media
if ("AGE2" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Age ####
assign(paste0("media_age_stats_",year),f_age_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Age (exc DK) ####
assign(paste0("media_age_ex_dk_",year),f_age_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_age_z_scores_ex_dk_",year),f_age_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#SEX - Media
if ("SEX" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Sex ####
assign(paste0("media_sex_stats_",year),f_sex_stats_year(year,"TrustMedia2", trust, distrust))

## Trust the Media by Sex (exc DK) ####
assign(paste0("media_sex_ex_dk_",year),f_sex_stats_year(year,"TrustMedia2", trust, dk = FALSE))
}


#URBAN_RURAL - Media
if ("URBH" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Urban/Rural ####
assign(paste0("media_urbh_stats_",year),f_urbh_stats_year(year,"TrustMedia2", trust, distrust))

## Trust the Media by Urban/Rural (exc DK) ####
assign(paste0("media_urbh_ex_dk_",year),f_urbh_stats_year(year,"TrustMedia2", trust, dk = FALSE))
}


#LGD - Media
if ("LGD2014name" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Local Government District ####
assign(paste0("media_LGD_stats_",year),f_lgd_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_LGD_z_scores_",year),f_lgd_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Local Government District (exc DK) ####
assign(paste0("media_LGD_ex_dk_",year),f_lgd_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_LGD_z_scores_ex_dk_",year),f_lgd_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#AA - Media
if ("AsmblyArea" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Assembly Area #### 
assign(paste0("media_AA_stats_",year),f_aa_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_AA_z_scores_",year),f_aa_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Assembly Area (exc DK) #### 
assign(paste0("media_AA_ex_dk_",year),f_aa_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_AA_z_scores_ex_dk_",year),f_aa_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#Deprivation - Media
if ("Deprivation" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Deprivation ####
assign(paste0("media_depriv_stats_",year),f_depriv_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_depriv_z_scores_",year),f_depriv_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Deprivation (exc DK) ####
assign(paste0("media_depriv_ex_dk_",year),f_depriv_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_depriv_z_scores_ex_dk_",year),f_depriv_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#MARITAL STATUS - Media
if ("MS" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Marital Status ####
assign(paste0("media_ms_stats_",year),f_ms_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_ms_z_scores_",year),f_ms_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Marital Status (exc DK) ####
assign(paste0("media_ms_ex_dk_",year),f_ms_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_ms_z_scores_ex_dk_",year),f_ms_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#MARITAL STATUS GROUP- Media
if ("MS_GRP" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Marital Status Group####
assign(paste0("media_msgrp_stats_",year),f_msgrp_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_msgrp_z_scores_",year),f_msgrp_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Marital Status Group (exc DK) ####
assign(paste0("media_msgrp_ex_dk_",year),f_msgrp_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_msgrp_z_scores_ex_dk_",year),f_msgrp_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}


#DISABILITY - Media
if ("LimLongStand" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Disability ####
assign(paste0("media_LimLongStand_stats_",year),f_limlongstand_stats_year(year,"TrustMedia2", trust, distrust))

## Trust the Media by Disability (exc DK) ####
assign(paste0("media_LimLongStand_ex_dk_",year),f_limlongstand_stats_year(year,"TrustMedia2", trust, dk = FALSE))
}


#RELIGION - Media

if ("OwnRelig2" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Religion ####
assign(paste0("media_relig_stats_",year),f_relig_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_relig_z_scores_",year),f_relig_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Religion (exc DK) ####
assign(paste0("media_relig_ex_dk_",year),f_relig_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_relig_z_scores_ex_dk_",year),f_relig_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}

#SEXUAL ORIENTATION - Media

if ("Sexual_orient" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Sexual Orientation ####
assign(paste0("media_sexualorient_stats_",year),f_sexualorient_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_sexualorient_z_scores_",year),f_sexualorient_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Sexual Orientation (exc DK) ####
assign(paste0("media_sexualorient_ex_dk_",year),f_sexualorient_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_sexualorient_z_scores_ex_dk_",year),f_sexualorient_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}

#DEPEND - Media
if ("Dependants" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Dependants ####
assign(paste0("media_depend_stats_",year),f_depend_stats_year(year,"Dependants","TrustMedia2", trust, distrust))

## Trust the Media by Dependants (exc DK) ####
assign(paste0("media_depend_ex_dk_",year),f_depend_stats_year(year,"Dependants","TrustMedia2", trust, dk = FALSE))
}

if ("DEPEND1" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust the Media by Child Dependants ####
assign(paste0("media_depend_child_stats_",year),f_depend_stats_year(year,"DEPEND1","TrustMedia2", trust, distrust))

## Trust the Media by Child Dependants (exc DK) ####
assign(paste0("media_depend_child_ex_dk_",year),f_depend_stats_year(year,"DEPEND1","TrustMedia2", trust, dk = FALSE))
}

if ("DEPEND2" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust the Media by Disabled Dependants ####
assign(paste0("media_depend_disab_stats_",year),f_depend_stats_year(year,"DEPEND2","TrustMedia2", trust, distrust))

## Trust the Media by Disabled Dependants (exc DK) ####
assign(paste0("media_depend_disab_ex_dk_",year),f_depend_stats_year(year,"DEPEND2","TrustMedia2", trust, dk = FALSE))
}

if ("DEPEND3" %in% names(eval(as.name(paste0("data_",year))))) {
## Trust the Media by Elderly Dependants ####
assign(paste0("media_depend_elderly_stats_",year),f_depend_stats_year(year,"DEPEND3","TrustMedia2", trust, distrust))

## Trust the Media by Elderly Dependants (exc DK) ####
assign(paste0("media_depend_elderly_ex_dk_",year),f_depend_stats_year(year,"DEPEND3","TrustMedia2", trust, dk = FALSE))
}


#ETHNIC - Media

if ("ETHNIC" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Ethnic ####
assign(paste0("media_ethnic_stats_",year),f_ethnic_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_ethnic_z_scores_",year),f_ethnic_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Ethnic (exc DK) ####
assign(paste0("media_ethnic_ex_dk_",year),f_ethnic_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_ethnic_z_scores_ex_dk_",year),f_ethnic_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}

#ETHNIC GROUP - Media

if ("Ethnic_group" %in% names(eval(as.name(paste0("data_",year))))) {

## Trust the Media by Ethnic Group ####
assign(paste0("media_ethnicgrp_stats_",year),f_ethnicgrp_stats_year(year,"TrustMedia2", trust, distrust))
assign(paste0("media_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustMedia2", trust))
assign(paste0("media_disagree_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustMedia2", distrust))
assign(paste0("media_dont_know_ethnicgrp_z_scores_",year),f_ethnicgrp_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Ethnic Group (exc DK) ####
assign(paste0("media_ethnicgrp_ex_dk_",year),f_ethnicgrp_stats_year(year,"TrustMedia2", trust, dk = FALSE))
assign(paste0("media_ethnicgrp_z_scores_ex_dk_",year),f_ethnicgrp_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))
}

#ETHNIC wHITE OTHER - Media

if ("Ethnic_white_other" %in% names(eval(as.name(paste0("data_",year))))) {
  
## Trust the Media by Ethnic White Other ####
assign(paste0("media_ethnicwo_stats_",year),f_ethnicwo_stats_year(year,"TrustMedia2", trust, distrust))


## Trust the Media by Ethnic White Other (exc DK) ####
assign(paste0("media_ethnicwo_ex_dk_",year),f_ethnicwo_stats_year(year,"TrustMedia2", trust, dk = FALSE))
}

}

