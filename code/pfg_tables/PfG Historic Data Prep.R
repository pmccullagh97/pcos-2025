# This code creates or renames the relevant S75 derived variables within the historic R data files
# This code DOES NOT SAVE the new dataset to the drive - uncomment the code at the end if you wish to do so

# The variable names used are 
# LGD2014name (available all years); 
#    coded from LGD14(LGD name) for 2014 and LGD2014(LGD code) for 2016, 2019 to 2023 and LGD2014(LGD name) for 2024)
# AsmblyArea (available all years); 
#    coded from AA2008 (AA code)
# URBH (only present 2019 onwards; 'urban' variable in 2014 & 2016 different)
# AGE2 (available all years); 
#    names aligned in Historic data to R
# SEX (available all years) 
#    names aligned in Historic data to R
# MS & MS_GRP (available all years, but response options in source variable differ in 2014, 2016, 2019 & 2024)
#    coded from marital (2014 & 2016) and MARSTT (2019 onwards)
# OwnRelig2 (only present 2019 onwards; Religion variable in 2014 & 2016 combines 'None' and 'Refusal/Missing' rather than 'None' & 'Other')
# LimLongStand (not available in 2020)
# Dependants (available all years)
#    coded from DEPEND1-DEPEND3 (for 2019 onwards); depens in source data in 2014 & 2016 
# DEPEND1 (child), DEPEND2 (disab) & DEPEND3 (elderly) (available 2019 onwards)
# Sexual_orient (available all years except 2020 and 2021?)
#    coded from SIDtel and SIDFTFQN for 2023 onwards; SIDtel for 2022; SIDFTFQN for 2019; sexident for 2014 & 2016
# Deprivation (available 2019 onwards; using 2017 based)
# ETHNIC, Ethnic_group & Ethnic_white_other (available all years)
#    coded from NIEthGrp (2014 & 2016) and ETHNIC (2019 onwards)

# These are used in PfG Table Output, PfG Supplementary Tables and PfG Significance testing

# The categories are:
# LGD2014name = ("Antrim and Newtownabbey", 
## "Armagh, Banbridge and Craigavon",
## "Belfast",
## "Causeway Coast and Glens",
## "Derry and Strabane",
## "Fermanagh and Omagh",
## "Lisburn and Castlereagh",
## "Mid and East Antrim",
## "Mid Ulster",
## "Newry, Mourne and Down",
## "North Down and Ards")
# AsmblyArea = ("Belfast North", 
## "Belfast South",
## "Belfast West",
## "East Antrim",
## "East Londonderry",
## "Fermanagh and South Tyrone",
## "Foyle",
## "Lagan Valley",
## "Mid Ulster",
## "Newry and Armagh",
## "North Antrim",
## "North Down",
## "South Antrim",
## "South Down",
## "Strangford",
## "Upper Bann",
## "West Tyrone")
# URBH = (URBAN, RURAL)
# AGE2 = (16-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75 and over)
# SEX = (M, F)
# MS = (Single, Married/CP, Separated, Divorced/Dissolved CP, Widowed/Surviving CP)
# MS_GRP = (Single, Married/CP, Other)
# OwnRelig2 = (Catholic, Protestant, Other/No Religion, Refusal, Dont know)
# LimLongStand = ("No Limiting longstanding illness","Limiting longstanding illness" )
# Dependants = ("Does not have Dependants", "Has dependants")
# Sexual_orient = ("Heterosexual or Straight", "Gay, Lesbian, bisexual or other sexual orientation")
# Deprivation = "Quintile 1 - Most deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 - Least deprived")
# ETHNIC = ("White", "Irish Traveller",
## "White and Black Caribbean", "White and Black African","White and Asian","Any other Mixed/Multiple ethnic background",
## "Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background",
## "African", "Caribbean","Any other Black/African/Caribbean background", 
## "Arab", "Any other ethnic group")
# Ethnic_group = ("White", "Irish Traveller", "Mixed/Multiple ethnic groups", "Asian/Asian British", "Black/African/Caribbean/Black British", "Other ethnic group")
# Ethnic_white_other = ("White", "Other")

# Currently, the following variables are provided with the correct labels:
#SEX, OwnRelig2, LimLongStand, ETHNIC

# In the 'Data Prep' code, for 2022 onwards:
# TrustAssemblyElectedBody2 set to relevant variable
# AGE2 calculated from AGE

# In the 'Historic Data to R' code, the following prep (relevant to S75/PfG) is carried out for 2012 to 2021:
# TrustAssemblyElectedBody2 = TrustNIAssembly2 (2014,2016,2020,2021) or TrustElectedRep2 (2019)
# SEX = persex (2014,2016)
# AGE2 = pcosage (2014,2016)
# Dependants = depens (2014,2016)
# OwnRelig2 = OwnRelig (2021,2020) with labels tidied up in 2020
# OwnRelig2 = Religion2 (2019), with "Refusal" = "Unwilling to answer" and "Dont Know" = "Undefined"; other labels tidied up
# URBH labels changed to "URBAN" and "RURAL" (2020,2021); not present (2014, 2016, 2019)


#START OF CODE

library(here)
source(paste0(here(), "/code/config.R"))

# Check for existence of pre 2021 data ####

if (!file.exists(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))) {
  source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
}

# Check for existence of previous year data (post 2021) ####

for (year in c(2022:current_year)) {
  
  if (!file.exists(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))) {
    print(paste0("Final Dataset RDS file missing for ", current_year - 1),
          ". Re-visit the Project for ", year, " and re-run that year's data_prep.R script")}
}

# For each year... ####

for (year in c(seq(2014, 2016, 2), 2019:current_year)) {

  # Read in data file ####
  
  data_year <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))
  
  ## RECODE ####
  
  ### District Council (LGD2014) ####
  # recode where present in data (in 2019 use LGD14)
  
  if (("LGD2014" %in% names(data_year)) & year != 2024) {
    data_year <- data_year %>%
      mutate(
        LGD2014name = as.factor(case_when(
          LGD2014 == "N09000001" ~ "Antrim and Newtownabbey",
          LGD2014 == "N09000002" ~ "Armagh, Banbridge and Craigavon",
          LGD2014 == "N09000003" ~ "Belfast",
          LGD2014 == "N09000004" ~ "Causeway Coast and Glens",
          LGD2014 == "N09000005" ~ "Derry and Strabane",
          LGD2014 == "N09000006" ~ "Fermanagh and Omagh",
          LGD2014 == "N09000007" ~ "Lisburn and Castlereagh",
          LGD2014 == "N09000008" ~ "Mid and East Antrim",
          LGD2014 == "N09000009" ~ "Mid Ulster",
          LGD2014 == "N09000010" ~ "Newry, Mourne and Down",
          LGD2014 == "N09000011" ~ "North Down and Ards")),
        LGD2014name = factor(LGD2014name,
                             levels=c("Antrim and Newtownabbey", "Armagh, Banbridge and Craigavon",
                                      "Belfast", "Causeway Coast and Glens", "Derry and Strabane",
                                      "Fermanagh and Omagh", "Lisburn and Castlereagh",
                                      "Mid and East Antrim", "Mid Ulster",
                                      "Newry, Mourne and Down", "North Down and Ards"),
                             labels=c("Antrim and Newtownabbey", "Armagh, Banbridge and Craigavon",
                                      "Belfast", "Causeway Coast and Glens", "Derry and Strabane",
                                      "Fermanagh and Omagh", "Lisburn and Castlereagh",
                                      "Mid and East Antrim", "Mid Ulster",
                                      "Newry, Mourne and Down", "North Down and Ards"))
      )
  }
  
  if (year == 2019) {
    data_year <- data_year %>%
      mutate(
        LGD2014name = factor(LGD14,
                             levels=c("Antrim and Newtownabbey", "Armagh, Banbridge and Craigavon",
                                      "Belfast", "Causeway Coast and Glens", "Derry and Strabane",
                                      "Fermanagh and Omagh", "Lisburn and Castlereagh",
                                      "Mid and East Antrim", "Mid Ulster",
                                      "Newry, Mourne and Down", "North Down and Ards"),
                             labels=c("Antrim and Newtownabbey", "Armagh, Banbridge and Craigavon",
                                      "Belfast", "Causeway Coast and Glens", "Derry and Strabane",
                                      "Fermanagh and Omagh", "Lisburn and Castlereagh",
                                      "Mid and East Antrim", "Mid Ulster",
                                      "Newry, Mourne and Down", "North Down and Ards"))
      )
  }  
    
  if (year == 2024) {
    data_year <- data_year %>%
      mutate(
        LGD2014name = factor(LGD2014,
                             levels=c("Antrim & Newtownabbey", "Armagh, Banbridge & Craigavon",
                                      "Belfast", "Causeway Coast & Glens", "Derry & Strabane",
                                      "Fermanagh & Omagh", "Lisburn & Castlreagh",
                                      "Mid & East Antrim", "Mid Ulster",
                                      "Newry, Mourne & Down", "North Down & Ards"),
                             labels=c("Antrim and Newtownabbey", "Armagh, Banbridge and Craigavon",
                                      "Belfast", "Causeway Coast and Glens", "Derry and Strabane",
                                      "Fermanagh and Omagh", "Lisburn and Castlereagh",
                                      "Mid and East Antrim", "Mid Ulster",
                                      "Newry, Mourne and Down", "North Down and Ards"))
      )
  }
  
  ### Assembly Area (AA2008) ####
  # recode where present in data ()
  
  if ("AA2008" %in% names(data_year)) {
    data_year <- data_year %>%
      mutate(
        AsmblyArea = as.factor(case_when(
          AA2008 == "N06000001" ~ "Belfast East",
          AA2008 == "N06000002" ~ "Belfast North",
          AA2008 == "N06000003" ~ "Belfast South",
          AA2008 == "N06000004" ~ "Belfast West",
          AA2008 == "N06000005" ~ "East Antrim",
          AA2008 == "N06000006" ~ "East Londonderry",
          AA2008 == "N06000007" ~ "Fermanagh and South Tyrone",
          AA2008 == "N06000008" ~ "Foyle",
          AA2008 == "N06000009" ~ "Lagan Valley",
          AA2008 == "N06000010" ~ "Mid Ulster",
          AA2008 == "N06000011" ~ "Newry and Armagh",
          AA2008 == "N06000012" ~ "North Antrim",
          AA2008 == "N06000013" ~ "North Down",
          AA2008 == "N06000014" ~ "South Antrim",
          AA2008 == "N06000015" ~ "South Down",
          AA2008 == "N06000016" ~ "Strangford",
          AA2008 == "N06000017" ~ "Upper Bann",
          AA2008 == "N06000018" ~ "West Tyrone",))
      )
  }
   
  ### Urban Rural (URBH) ####
  # convert to factor
  
  if ("URBH" %in% names(data_year)) {
    data_year <- data_year %>%
      mutate(
        URBH = factor(URBH,
                      levels = c("URBAN", "RURAL"),
                      labels = c("URBAN", "RURAL"))
      )
  }
  
  ### Marital/Civil Partnership status (MARSTT) ####
  # recode
  
  if ("MARSTT" %in% names(data_year) & !year %in% c(2019,2021:2024)) {
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Separated, but still legally married", "Separated, but still legally in a civil partnership") ~ "Separated",
          MARSTT %in% c("Divorced", "Formerly in a civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
          MARSTT %in% c("Widowed", "Surviving partner from a civil partnership") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Separated, but still legally married", "Separated, but still legally in a civil partnership", 
                        "Divorced", "Formerly in a civil partnership which is now legally dissolved",
                        "Widowed", "Surviving partner from a civil partnership") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
  
  if ("MARSTT" %in% names(data_year) & year %in% c(2021,2022,2023)) {
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Seperated, but still legally married", "Seperated, but still legally in a civil partnership") ~ "Separated",
          MARSTT %in% c("Divorced", "Formerly in a civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
          MARSTT %in% c("Widowed", "Surviving partner from a civil partnership") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Seperated, but still legally married", "Seperated, but still legally in a civil partnership", 
                        "Divorced", "Formerly in a civil partnership which is now legally dissolved",
                        "Widowed", "Surviving partner from a civil partnership") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
  
  if (year == 2019) {
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          MARSTT %in% c("Single, never married") ~ "Single",
          MARSTT %in% c("Married and living with husband/wife", "In a registered same-sex civil partnership") ~ "Married/CP",
          MARSTT %in% c("Married and separated from husband/wife", "SPONTANEOUS ONLY Separated, but still legally in a same-sex civil partnership") ~ "Separated",
          MARSTT %in% c("Divorced", "SPONTANEOUS ONLY Formerly in a same-sex civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
          MARSTT %in% c("Widowed?", "SPONTANEOUS ONLY Surviving partner from a same-sex civil partnership") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          MARSTT %in% c("Single, never married") ~ "Single",
          MARSTT %in% c("Married and living with husband/wife", "In a registered same-sex civil partnership") ~ "Married/CP",
          MARSTT %in% c("Married and separated from husband/wife", "SPONTANEOUS ONLY Separated, but still legally in a same-sex civil partnership", 
                        "Divorced", "SPONTANEOUS ONLY Formerly in a same-sex civil partnership which is now legally dissolved",
                        "Widowed?", "SPONTANEOUS ONLY Surviving partner from a same-sex civil partnership") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
 
  if (year == 2024) {
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          MARSTT %in% c("1. Never married and never legally registered in a civil partnership") ~ "Single",
          MARSTT %in% c("2. Married", "3. In a legally registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("4. Separated, but still legally married", "5. Separated, but still legally in a civil partnership") ~ "Separated",
          MARSTT %in% c("6. Divorced", "7. Formerly in a civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
          MARSTT %in% c("8. Widowed", "9. Surviving partner of a lgally registered civil partnership") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          MARSTT %in% c("1. Never married and never legally registered in a civil partnership") ~ "Single",
          MARSTT %in% c("2. Married", "3. In a legally registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("4. Separated, but still legally married", "5. Separated, but still legally in a civil partnership", 
                        "6. Divorced", "7. Formerly in a civil partnership which is now legally dissolved",
                        "8. Widowed", "9. Surviving partner of a lgally registered civil partnership") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
 
  if (year %in% c(2014,2016)) {
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          marital %in% c("Single, that is never married") ~ "Single",
          marital %in% c("Married and \\nliving with \\nhusband/wife \\n or in a civil \\npartnership", "A civil partner in a legally-recognised Civil Partnership") ~ "Married/CP",
          marital %in% c("Married and separated from husband/wife", "In a legally-recognised Civil Partnership & separated from civil partner") ~ "Separated",
          marital %in% c("Divorced", "Formerly a civil partner, the Civil Partnership now legally dissolved") ~ "Divorced/Dissolved CP",
          marital %in% c("Widowed", "A surviving civil partner: his/her partner having since died") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          marital %in% c("Single, that is never married") ~ "Single",
          marital %in% c("Married and \\nliving with \\nhusband/wife \\n or in a civil \\npartnership", "A civil partner in a legally-recognised Civil Partnership") ~ "Married/CP",
          marital %in% c("Married and separated from husband/wife", "In a legally-recognised Civil Partnership & separated from civil partner", 
                         "Divorced", "Formerly a civil partner, the Civil Partnership now legally dissolved",
                         "Widowed", "A surviving civil partner: his/her partner having since died") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
  
  ### LimLongStand ####
  # needs to be created from component variables (HLONGILL & REDACT) in 2019 & 2021, called DISABIL in 2014 & 2016
  
  if (year %in% c(2019,2021)) {
    data_year <- data_year %>%
      mutate(  
        LimLongStand = as.factor(case_when(
          HLONGILL == "Yes" & REDACT %in% c("Yes, a lot", "Yes, a little") ~ "Limiting longstanding illness",
          HLONGILL == "Yes" & REDACT %in% c("Not at all") ~ "No Limiting longstanding illness",
          HLONGILL == "No" ~ "No Limiting longstanding illness")
        )
      )
  }
  
  if (year %in% c(2014,2016)) {
    data_year <- data_year %>%
      mutate(  
        LimLongStand = factor(DISABIL,
                              levels = levels(DISABIL),
                              labels = c("Limiting longstanding illness", "No Limiting longstanding illness"))
      )
  }
  
  ### ETHNIC ####
  # recode where present in data
  
 if ((year %in% c(2019:current_year)) & year != 2020 & year != 2024) {
    data_year <- data_year %>%
      mutate(
        ETHNIC = factor(ETHNIC,
                             levels=c("White", "Irish Traveller", 
                                      "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                      "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian background",
                                      "African", "Caribbean", "Other Black/African/Caribbean",
                                      "Arab", "Any other ethnic background",
                                      "Refusal", "DontKnow"),
                             labels=c("White", "Irish Traveller", 
                                      "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                      "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                                      "African", "Caribbean", "Any other Black/African/Caribbean",
                                      "Arab", "Any other ethnic group",
                                      "Refusal", "DontKnow"))
      )
  }
 if (year == 2024) {
    data_year <- data_year %>%
      mutate(
        ETHNIC = factor(ETHNIC,
                        levels=c("1. White", "2. Irish Traveller", 
                                 "3. White and Black Caribbean", "4. White and Black African", "5. White and Asian", "6. other Mixed/Multiple background",
                                 "7. Indian", "8. Pakistani", "9. Bangladeshi", "10. Chinese", "11. Other Asian background",
                                 "12. African", "13. Caribbean", "14. Other Black/African/Caribbean",
                                 "15. Arab", "16. Any other ethnic background",
                                 "Refusal", "DontKnow"),
                        labels=c("White", "Irish Traveller", 
                                 "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                 "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                                 "African", "Caribbean", "Any other Black/African/Caribbean",
                                 "Arab", "Any other ethnic group",
                                 "Refusal", "DontKnow"))
      )
  }
  if (year == 2020) {
    data_year <- data_year %>%
      mutate(
        ETHNIC = factor(ETHNIC,
                        levels=c("White", "Irish Traveller", 
                                 "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                 "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian background",
                                 "African", "Caribbean", "Other Black/African/Caribbean",
                                 "Arab", "Any other ethnic background",
                                 "Refusal", "Dont Know"),
                        labels=c("White", "Irish Traveller", 
                                 "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                 "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                                 "African", "Caribbean", "Any other Black/African/Caribbean",
                                 "Arab", "Any other ethnic group",
                                 "Refusal", "DontKnow"))
      )
  }
  if (year %in% c(2014:2016)) {
    data_year <- data_year %>%
      mutate(
        ETHNIC = factor(NIEthGrp,
                        levels=c("White", "Irish Traveller",
                                 "Mixed - White and Black Caribbean", "Mixed - White and Black African", "Mixed - White and Asian", "Any other mixed multiple ethnic background (please describe)",
                                 "Asian - Indian", "Asian - Pakistani", "Asian - Bangladeshi", "Chinese", "Any other Asian background (please describe)",
                                 "Black - African", "Black - Caribbean", "Any other Black/African/Caribbean (please describ",
                                 "Arab", "Any other ethnic background (please describe)",
                                 "Refusal", "Don't Know"),
                        labels=c("White", "Irish Traveller", 
                                 "White and Black Caribbean", "White and Black African", "White and Asian", "other Mixed/Multiple background",
                                 "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                                 "African", "Caribbean", "Any other Black/African/Caribbean",
                                 "Arab", "Any other ethnic group",
                                 "Refusal", "DontKnow"))
      )
  }
  
  # create collapsed variables: Ethnic_group & Ethnic_White_Other, where ETHNIC present
  
  if (year %in% c(seq(2014, 2016, 2), 2019:current_year)) {
    data_year <- data_year %>%
      mutate(  
        Ethnic_group = as.factor(case_when(
          ETHNIC %in% c("White") ~ "White",
          ETHNIC %in% c("Irish Traveller") ~ "Irish Traveller",
          ETHNIC %in% c("White and Black Caribbean", "White and Black African","White and Asian","other Mixed/Multiple background") ~ "Mixed/Multiple ethnic groups",
          ETHNIC %in% c("Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background") ~ "Asian/Asian British",
          ETHNIC %in% c("African", "Caribbean","Any other Black/African/Carribean background") ~ "Black/African/Caribbean/Black British",
          ETHNIC %in% c("Arab", "Any other ethnic group") ~ "Other ethnic group")
        ),
        Ethnic_group = factor(Ethnic_group,
                             levels=c("White", "Irish Traveller",
                                      "Mixed/Multiple ethnic groups",
                                      "Asian/Asian British",
                                      "Black/African/Caribbean/Black British",
                                      "Other ethnic group"),
                             labels=c("White", "Irish Traveller",
                                      "Mixed/Multiple ethnic groups",
                                      "Asian/Asian British",
                                      "Black/African/Caribbean/Black British",
                                      "Other ethnic group")),
        Ethnic_white_other = as.factor(case_when(
          ETHNIC %in% c("White") ~ "White",
          ETHNIC %in% c("Irish Traveller", 
                        "White and Black Caribbean", "White and Black African","White and Asian","other Mixed/Multiple background",
                        "Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background",
                        "African", "Caribbean","Any other Black/African/Carribean background",
                        "Arab", "Any other ethnic group") ~ "Other")
        )
      )
  }

  
  ### Dependants ####
  # needs to be created from component variables (DEPEND1, DEPEND2 & DEPEND3) from 2019 onwards
  
  # For 2019, recode "Yes" to "Has dependants" & "No" to "Does not have dependants" for DEPEND1, DEPEND2 & DEPEND3
  
  if (year %in% c(2019:current_year)) {
    data_year <- data_year %>%
      mutate(  
        DEPEND1 = as.factor(case_when(
          DEPEND1 == "Yes" ~ "Has dependants",
          DEPEND1 == "No" ~ "Does not have dependants")
        ),  
        DEPEND2 = as.factor(case_when(
          DEPEND2 == "Yes" ~ "Has dependants",
          DEPEND2 == "No" ~ "Does not have dependants")
        ),
        DEPEND3 = as.factor(case_when(
          DEPEND3 == "Yes" ~ "Has dependants",
          DEPEND3 == "No" ~ "Does not have dependants") 
        )
      )
  }
  
  #Create Dependants from depens (2014 & 2016) and DEPEND1, DEPEND2 & DEPEND3 from 2019 onwards
  
  if (year %in% c(2014:2016)) {
    data_year <- data_year %>%
      mutate(  
        Dependants = as.factor(case_when(
          depens == "Has\\n dependants" ~ "Has dependants",
          depens == "No \\ndependants" ~ "Does not have dependants",)
        )
      )
  }
  
  if (year %in% c(2019:current_year)) {
    data_year <- data_year %>%
      mutate(  
        Dependants = as.factor(case_when(
          DEPEND1 == "Has dependants" | DEPEND2 == "Has dependants" | DEPEND3 == "Has dependants" ~ "Has dependants",
          DEPEND1 == "Does not have dependants" & DEPEND2 == "Does not have dependants" & DEPEND3 == "Does not have dependants" ~ "Does not have dependants",)
        )
      )
  }
  
  ### AGE2 ####
  # tidy up AGE2 labels for 2014 & 2016
  
  if (year %in% c(2014, 2016)) {
    levels(data_year$AGE2)[levels(data_year$AGE2) == "16 - 24"] <- "16-24" 
    levels(data_year$AGE2)[levels(data_year$AGE2) == "25 - 34"] <- "25-34"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "35 - 44"] <- "35-44"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "45 - 54"] <- "45-54"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "55 - 64"] <- "55-64"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "65 - 74"] <- "65-74"
  }
  
  if (year == 2021) {
     data_year$AGE2 <- as.factor(data_year$AGE2)
     data_year <- data_year %>%
        mutate(
          AGE2 = factor(AGE2,
                levels = c("1", "2", "3", "4", "5", "6", "7"),
                labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 and over"))
     )
  } 

  ### SEX ####
  # recode SEX labels for 2014, 2016, 2020 & 2021
  
  if (year %in% c(2012:2016,2020:2021)) {
    levels(data_year$SEX)[levels(data_year$SEX) == "Male"] <- "M" 
    levels(data_year$SEX)[levels(data_year$SEX) == "Female"] <- "F" 
  }  
  
  ### Sexual Orientation (SIDtel & SIDFTFQN) ####
  # recode SIDtel and SIDFTFQN for 2019 onwards; sexident in 2014 & 2016 data; and for 2020/2021 query to be resolved
  
  if (year %in% c(2023:current_year)) {
    data_year <- data_year %>%
      mutate(  
        Sexual_orient = as.factor(case_when(
          SIDtel %in% c("Heterosexual or Straight") | SIDFTFQN %in% c("Heterosexual / Straight")  ~ "Heterosexual or Straight",
          SIDtel %in% c("Gay or Lesbian", "Bisexual", "Other") |  SIDFTFQN %in% c("Gay / Lesbian", "Bisexual", "Other") ~ "Gay, Lesbian, bisexual or other sexual orientation",
          SIDtel %in% c("Refusal") | SIDFTFQN %in% c("Refusal") ~ "Refusal",
          SIDtel %in% c("DontKnow") | SIDFTFQN %in% c("Dont Know") ~ "Dont Know"))
      )
  } else if (year %in% c(2022)) {
    data_year <- data_year %>%
      mutate(  
        Sexual_orient = as.factor(case_when(
          SIDtel %in% c("Heterosexual or Straight") ~ "Heterosexual or Straight",
          SIDtel %in% c("Gay or Lesbian", "Bisexual", "Other") ~ "Gay, Lesbian, bisexual or other sexual orientation",
          SIDtel %in% c("Refusal") ~ "Refusal",
          SIDtel %in% c("DontKnow") ~ "Dont Know"))
      )
  } else if (year %in% c(2019)) {
    data_year <- data_year %>%
      mutate(  
        Sexual_orient = as.factor(case_when(
          SIDFTFQN %in% c("Heterosexual / Straight")  ~ "Heterosexual or Straight",
          SIDFTFQN %in% c("Gay / Lesbian", "Bisexual", "Other") ~ "Gay, Lesbian, bisexual or other sexual orientation",
          SIDFTFQN %in% c("Refusal") ~ "Refusal",
          SIDFTFQN %in% c("Dont Know") ~ "Dont Know"))
      )
  } else if (year %in% c(2014, 2016)) {
    data_year <- data_year %>%
      mutate(  
        Sexual_orient = as.factor(case_when(
          sexident %in% c("Heterosexual/straight")  ~ "Heterosexual or Straight",
          sexident %in% c("Gay/Lesbian", "Bisexual", "Other") ~ "Gay, Lesbian, bisexual or other sexual orientation",
          sexident %in% c("Refusal") ~ "Refusal",
          sexident %in% c("Don't Know") ~ "Dont Know"))
      )
  }
  
  if ("Sexual_orient" %in% names(data_year)) {
    data_year <- data_year %>%
      mutate(
        Sexual_orient = factor(Sexual_orient,
                        levels=c("Heterosexual or Straight", "Gay, Lesbian, bisexual or other sexual orientation",
                                 "Refusal", "Dont Know"),
                        labels=c("Heterosexual or Straight", "Gay, Lesbian, bisexual or other sexual orientation",
                                 "Refusal", "Dont Know"))
      )
  }  
 
  
  ### Deprivation ####
  
  # recode quintiles 1-5 for 2019 onwards (waiting on 2014 & 2016 dataset)
  
  if (year %in% c(2019:current_year)) {
    data_year <- data_year %>%
      mutate(
        Deprivation = as.factor(case_when(
          MDMQUIN17 == "1" ~ "Quintile 1 - Most deprived",
          MDMQUIN17 == "2" ~ "Quintile 2",
          MDMQUIN17 == "3" ~ "Quintile 3",
          MDMQUIN17 == "4" ~ "Quintile 4",
          MDMQUIN17 == "5" ~ "Quintile 5 - Least deprived"))
      )
  }
  
  ## Save to relevant 'data_year' ####
  
  assign(paste0("data_",year),data_year)
  
  #If you want to save the dataset with the extra variables to the drive, then uncomment the 'saveRDS' line below.
  #WARNING: this will overwrite the previous version
  
  #saveRDS(data_year, paste0(data_folder, "Final/PCOS ",year," Final Dataset.RDS"))
}

