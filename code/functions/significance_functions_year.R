# Year significance functions ####

#GUIDANCE

# create additional significance functions (for other covariates) as required
# where recoding is required, include the recoding code after 
  # "data_year <- eval((as.name(paste0("data_",year))))"
# use the recoding code in PfG Historic Data Prep

#FUNCTIONS

## AGE ####

f_age_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all age groups in AGE2
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  age_groups <- levels(data_year$AGE2)
  
  if (dk) {
    age_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (age in age_groups) {
      age_stats_year[[age]] <- c(
        f_return_p_group(data_year, var, value1, "AGE2", age, weight = age_weight) * 100,
        f_return_p_group(data_year, var, value2, "AGE2", age, weight = age_weight) * 100,
        f_return_p_group(data_year, var, dont_know, "AGE2", age, weight = age_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$AGE2, age))
    }
  } else {
    age_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (age in age_groups) {
      age_stats_year[[age]] <- c(
        f_return_p_group(data_year, var, value1, "AGE2", age, weight = age_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$AGE2 == age) %>%
          nrow())
    }
  }
  
  names(age_stats_year)[names(age_stats_year) == "stat"] <- " "
  age_stats_year
}


f_age_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all age groups in AGE2
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  age_groups <- levels(data_year$AGE2)
  
  age_z_scores_year <- data.frame(age = age_groups)
  
  for (i in 1:length(age_groups)) {
    col <- c()
    for (j in 1:length(age_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AGE2", age_groups[j], weight = age_weight),
            n1 = f_return_n_group(data_year[[var]], data_year$AGE2, age_groups[j]),
            p2 = f_return_p_group(data_year, var, value, "AGE2", age_groups[i], weight = age_weight),
            n2 = f_return_n_group(data_year[[var]], data_year$AGE2, age_groups[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AGE2", age_groups[j], weight = age_weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & AGE2 == age_groups[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "AGE2", age_groups[i], weight = age_weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & AGE2 == age_groups[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    age_z_scores_year[[age_groups[i]]] <- col
  }
  
  names(age_z_scores_year)[names(age_z_scores_year) == "age"] <- " "
  age_z_scores_year
}


## SEX ####

f_sex_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across sex
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  
  if (dk) {
    sex_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK","Base"),
      male = c(
        f_return_p_group(data_year, var, value1, "SEX", "M",weight = sex_weight) * 100,
        f_return_p_group(data_year, var, value2, "SEX", "M", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, dont_know, "SEX", "M", weight = sex_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$SEX, "M")
      ),
      female = c(
        f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, value2, "SEX", "F", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, dont_know, "SEX", "F", weight = sex_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$SEX, "F")
      )
      ,
      z = c(
        f_return_z(
          p1 = f_return_p_group(data_year, var, value1, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, value2, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, value2, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, dont_know, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, dont_know, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        NA
      )
    )
  } else {
    sex_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
      male = c(
        f_return_p_group(data_year, var, value1, "SEX", "M", weight = sex_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & SEX == "M") %>%
          nrow()
      ),
      female = c(
        f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & SEX == "F") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = male / 100, 
                          n1 = male[trust == "Base"],
                          p2 = female / 100,
                          n2 = female[trust == "Base"])
      ))
  }
  
  names(sex_stats_year) <- c(" ", "Male", "Female", "Z Score")
  sex_stats_year
}


## URBH ####

f_urbh_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across urban-rural categories
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
 
  if (dk) {
    urbh_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK","Base"),
      urban = c(
        f_return_p_group(data_year, var, value1, "URBH", "URBAN",weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "URBH", "URBAN", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "URBH", "URBAN", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$URBH, "URBAN")
      ),
      rural = c(
        f_return_p_group(data_year, var, value1, "URBH", "RURAL", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "URBH", "RURAL", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "URBH", "RURAL", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$URBH, "RURAL")
      )
      ,
      z = c(
        f_return_z(
          p1 = f_return_p_group(data_year, var, value1, "URBH", "URBAN", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$URBH, "URBAN"),
          p2 = f_return_p_group(data_year, var, value1, "URBH", "RURAL", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$URBH, "RURAL")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, value2, "URBH", "URBAN", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$URBH, "URBAN"),
          p2 = f_return_p_group(data_year, var, value2, "URBH", "RURAL", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$URBH, "RURAL")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, dont_know, "URBH", "URBAN", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$URBH, "URBAN"),
          p2 = f_return_p_group(data_year, var, dont_know, "URBH", "RURAL", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$URBH, "RURAL")
        ),
        NA
      )
    )
  } else {
    urbh_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
      urban = c(
        f_return_p_group(data_year, var, value1, "URBH", "URBAN", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & URBH == "URBAN") %>%
          nrow()
      ),
      rural = c(
        f_return_p_group(data_year, var, value1, "URBH", "RURAL", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & URBH == "RURAL") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = urban / 100, 
                          n1 = urban[trust == "Base"],
                          p2 = rural / 100,
                          n2 = rural [trust == "Base"])
      ))
  }
  
  names(urbh_stats_year) <- c(" ", "Urban", "Rural", "Z Score")
  urbh_stats_year
}

## LGD 2014 ####

f_lgd_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all LGDs
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  LGDs <- levels(data_year$LGD2014name)
  
  if (dk) {
    LGD_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (LGD in LGDs) {
      LGD_stats_year[[LGD]] <- c(
        f_return_p_group(data_year, var, value1, "LGD2014name", LGD, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "LGD2014name", LGD, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "LGD2014name", LGD, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$LGD2014name, LGD))
    }
  } else {
    LGD_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (LGD in LGDs) {
      LGD_stats_year[[LGD]] <- c(
        f_return_p_group(data_year, var, value1, "LGD2014name", LGD, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$LGD2014name == LGD) %>%
          nrow() )
    }
  }
  
  names(LGD_stats_year)[names(LGD_stats_year) == "stat"] <- " "
  LGD_stats_year
}


f_lgd_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all LGDs
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  LGDs <- levels(data_year$LGD2014name)
  
  LGD_z_scores_year <- data.frame(LGD = LGDs)
  
  for (i in 1:length(LGDs)) {
    col <- c()
    for (j in 1:length(LGDs)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "LGD2014name", LGDs[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$LGD2014name, LGDs[j]),
            p2 = f_return_p_group(data_year, var, value, "LGD2014name", LGDs[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$LGD2014name, LGDs[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "LGD2014name", LGDs[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & LGD2014name == LGDs[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "LGD2014name", LGDs[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & LGD2014name == LGDs[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    LGD_z_scores_year[[LGDs[i]]] <- col
  }
  
  names(LGD_z_scores_year)[names(LGD_z_scores_year) == "Local government district"] <- " "
  LGD_z_scores_year
}


## AA 2008 ####

f_aa_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all Assembly Areas
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  AAs <- levels(data_year$AsmblyArea)

  if (dk) {
    AA_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (AA in AAs) {
      AA_stats_year[[AA]] <- c(
        f_return_p_group(data_year, var, value1, "AsmblyArea", AA, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "AsmblyArea", AA, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "AsmblyArea", AA, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$AsmblyArea, AA))
    }
  } else {
    AA_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (AA in AAs) {
      AA_stats_year[[AA]] <- c(
        f_return_p_group(data_year, var, value1, "AsmblyArea", AA, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$AsmblyArea == AA) %>%
          nrow() )
    }
  }

  names(AA_stats_year)[names(AA_stats_year) == "stat"] <- " "
  AA_stats_year
  }


f_aa_z_scores_year  <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all Assembly Areas
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  AAs <- levels(data_year$AsmblyArea)
  
  AA_z_scores_year <- data.frame(AA = AAs)
  
  for (i in 1:length(AAs)) {
    col <- c()
    for (j in 1:length(AAs)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AsmblyArea", AAs[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$AsmblyArea, AAs[j]),
            p2 = f_return_p_group(data_year, var, value, "AsmblyArea", AAs[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$AsmblyArea, AAs[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AsmblyArea", AAs[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & AsmblyArea == AAs[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "AsmblyArea", AAs[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & AsmblyArea == AAs[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    AA_z_scores_year[[AAs[i]]] <- col
  }
  
  names(AA_z_scores_year)[names(AA_z_scores_year) == "Assembly Area"] <- " "
  AA_z_scores_year
}


## Deprivation ####

f_depriv_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
  # Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all deprivation quintiles
  # Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  depriv_groups <- levels(data_year$Deprivation)
  
  if (dk) {
    depriv_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (depriv in depriv_groups) {
      depriv_stats_year[[depriv]] <- c(
        f_return_p_group(data_year, var, value1, "Deprivation", depriv, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "Deprivation", depriv, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "Deprivation", depriv, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$Deprivation, depriv))
    }
  } else {
    depriv_stats_year <- data.frame(stat = c("% Yes", "Base"))
    
    for (depriv in depriv_groups) {
      depriv_stats_year[[depriv]] <- c(
        f_return_p_group(data_year, var, value1, "Deprivation", depriv, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$Deprivation == depriv) %>%
          nrow() )
    }
  }
  
  names(depriv_stats_year)[names(depriv_stats_year) == "stat"] <- " "
  depriv_stats_year
}


f_depriv_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across deprivation quintiles
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  depriv_groups <- levels(data_year$Deprivation)
  
  depriv_z_scores_year <- data.frame(depriv = depriv_groups)
  
  for (i in 1:length(depriv_groups)) {
    col <- c()
    for (j in 1:length(depriv_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Deprivation", depriv_groups[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$Deprivation, depriv_groups[j]),
            p2 = f_return_p_group(data_year, var, value, "Deprivation", depriv_groups[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$Deprivation, depriv_groups[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Deprivation", depriv_groups[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Deprivation == depriv_groups[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "Deprivation", depriv_groups[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Deprivation == depriv_groups[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    depriv_z_scores_year[[depriv_groups[i]]] <- col
  }
  
  names(depriv_z_scores_year)[names(depriv_z_scores_year) == "deprivation"] <- " "
  depriv_z_scores_year
}


## Marital Status####

f_ms_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all marital statuses in MS
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ms_categories <- levels(data_year$MS)
  
  if (dk) {
    ms_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (ms in ms_categories) {
      ms_stats_year[[ms]] <- c(
        f_return_p_group(data_year, var, value1, "MS", ms, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "MS", ms, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "MS", ms, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$MS, ms))
    }
  } else {
    ms_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (ms in ms_categories) {
      ms_stats_year[[ms]] <- c(
        f_return_p_group(data_year, var, value1, "MS", ms, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$MS == ms) %>%
          nrow() )
    }
  }
  
  names(ms_stats_year)[names(ms_stats_year) == "stat"] <- " "
  ms_stats_year
}


f_ms_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all marital statuses in MS
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ms_categories <- levels(data_year$MS)
  
  ms_z_scores_year <- data.frame(ms = ms_categories)
  
  for (i in 1:length(ms_categories)) {
    col <- c()
    for (j in 1:length(ms_categories)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "MS", ms_categories[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$MS, ms_categories[j]),
            p2 = f_return_p_group(data_year, var, value, "MS", ms_categories[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$MS, ms_categories[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "MS", ms_categories[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & MS == ms_categories[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "MS", ms_categories[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & MS == ms_categories[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    ms_z_scores_year[[ms_categories[i]]] <- col
  }
  
  names(ms_z_scores_year)[names(ms_z_scores_year) == "Marital Status Group"] <- " "
  ms_z_scores_year
}
  

## Marital Status group ####

f_msgrp_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
  # Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all marital status groups in MS_GRP
  # Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  msgrps <- levels(data_year$MS_GRP)
  
  if (dk) {
    msgrp_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (msgrp in msgrps) {
      msgrp_stats_year[[msgrp]] <- c(
        f_return_p_group(data_year, var, value1, "MS_GRP", msgrp, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "MS_GRP", msgrp, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "MS_GRP", msgrp, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$MS_GRP, msgrp))
    }
  } else {
    msgrp_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (msgrp in msgrps) {
      msgrp_stats_year[[msgrp]] <- c(
        f_return_p_group(data_year, var, value1, "MS_GRP", msgrp, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$MS_GRP == msgrp) %>%
          nrow() )
    }
  }
  
  names(msgrp_stats_year)[names(msgrp_stats_year) == "stat"] <- " "
  msgrp_stats_year
}


f_msgrp_z_scores_year <- function(year, var, value, dk = TRUE) {
  # Returns data frame comparing significant differences of "value" in "var" across all marital status groups in MS_GRP
  # Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  msgrps <- levels(data_year$MS_GRP)
  
  msgrp_z_scores_year <- data.frame(msgrp = msgrps)
  
  for (i in 1:length(msgrps)) {
    col <- c()
    for (j in 1:length(msgrps)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "MS_GRP", msgrps[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$MS_GRP, msgrps[j]),
            p2 = f_return_p_group(data_year, var, value, "MS_GRP", msgrps[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$MS_GRP, msgrps[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "MS_GRP", msgrps[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & MS_GRP == msgrps[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "MS_GRP", msgrps[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & MS_GRP == msgrps[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    msgrp_z_scores_year[[msgrps[i]]] <- col
  }
  
  names(msgrp_z_scores_year)[names(msgrp_z_scores_year) == "Marital Status Group"] <- " "
  msgrp_z_scores_year
}

## Disability (LimLongStand) ####

f_limlongstand_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across limiting longstanding illness (Yes/No)
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  
  if (dk) {
    disab_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK", "Base"),
      yes = c(
        f_return_p_group(data_year, var, value1, "LimLongStand", "Limiting longstanding illness", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "LimLongStand", "Limiting longstanding illness", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "LimLongStand", "Limiting longstanding illness", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$LimLongStand, "Limiting longstanding illness")
      ),
      no = c(
        f_return_p_group(data_year, var, value1, "LimLongStand", "No Limiting longstanding illness", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "LimLongStand", "No Limiting longstanding illness", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "LimLongStand", "No Limiting longstanding illness", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$LimLongStand, "No Limiting longstanding illness")
      )
      ,
      z = c(f_return_z(
        p1 = f_return_p_group(data_year, var, value1, "LimLongStand", "Limiting longstanding illness", weight = weight),
        n1 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "Limiting longstanding illness"),
        p2 = f_return_p_group(data_year, var, value1, "LimLongStand", "No Limiting longstanding illness", weight = weight),
        n2 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "No Limiting longstanding illness")
      ),
      f_return_z(
        p1 = f_return_p_group(data_year, var, value2, "LimLongStand", "Limiting longstanding illness", weight = weight),
        n1 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "Limiting longstanding illness"),
        p2 = f_return_p_group(data_year, var, value2, "LimLongStand", "No Limiting longstanding illness", weight = weight),
        n2 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "No Limiting longstanding illness")
      ),
      f_return_z(
        p1 = f_return_p_group(data_year, var, dont_know, "LimLongStand", "Limiting longstanding illness", weight = weight),
        n1 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "Limiting longstanding illness"),
        p2 = f_return_p_group(data_year, var, dont_know, "LimLongStand", "No Limiting longstanding illness", weight = weight),
        n2 = f_return_n_group(data_year[[var]], data_year$LimLongStand, "No Limiting longstanding illness")
      ),
      NA
     )
    )
  } else {
    disab_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
      yes = c(
        f_return_p_group(data_year, var, value1, "LimLongStand", "Limiting longstanding illness", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & LimLongStand == "Limiting longstanding illness") %>%
          nrow()
      ),
      no = c(
        f_return_p_group(data_year, var, value1, "LimLongStand", "No Limiting longstanding illness", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & LimLongStand == "No Limiting longstanding illness") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = yes / 100, 
                          n1 = yes[trust == "Base"],
                          p2 = no / 100,
                          n2 = no[trust == "Base"])
      ))
  }
    
  names(disab_stats_year) <- c(" ","Limiting longstanding illness","No Limiting longstanding illness","Z Score")
  disab_stats_year
}


## Religion ####

f_relig_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across Religion categories
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  relig_groups <- levels(data_year$OwnRelig2)

  if (dk) {
    relig_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (relig in relig_groups) {
      relig_stats_year[[relig]] <- c(
        f_return_p_group(data_year, var, value1, "OwnRelig2", relig, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "OwnRelig2", relig, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "OwnRelig2", relig, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$OwnRelig2, relig))
    }
  } else {
    relig_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (relig in relig_groups) {
      relig_stats_year[[relig]] <- c(
        f_return_p_group(data_year, var, value1, "OwnRelig2", relig, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$OwnRelig2 == relig) %>%
          nrow() )
    }
  }

  names(relig_stats_year)[names(relig_stats_year) == "stat"] <- " "
  relig_stats_year
}


f_relig_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across Religion categories
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  relig_groups <- levels(data_year$OwnRelig2)
  
  relig_z_scores_year <- data.frame(relig = relig_groups)
  
  for (i in 1:length(relig_groups)) {
    col <- c()
    for (j in 1:length(relig_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "OwnRelig2", relig_groups[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$OwnRelig2, relig_groups[j]),
            p2 = f_return_p_group(data_year, var, value, "OwnRelig2", relig_groups[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$OwnRelig2, relig_groups[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "OwnRelig2", relig_groups[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & OwnRelig2 == relig_groups[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "OwnRelig2", relig_groups[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & OwnRelig2 == relig_groups[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    relig_z_scores_year[[relig_groups[i]]] <- col
  }
  
  names(relig_z_scores_year)[names(relig_z_scores_year) == "Religion"] <- " "
  relig_z_scores_year
}


## Sexual Orientation ####

f_sexualorient_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all sexual orientation groups in Sexual_orient
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  sexualorient_groups <- levels(data_year$Sexual_orient)
  
  if (dk) {
    sexualorient_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (sexualorient in sexualorient_groups) {
      sexualorient_stats_year[[sexualorient]] <- c(
        f_return_p_group(data_year, var, value1, "Sexual_orient", sexualorient, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "Sexual_orient", sexualorient, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "Sexual_orient", sexualorient, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$Sexual_orient, sexualorient))
    }
  } else {
    sexualorient_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (sexualorient in sexualorient_groups) {
      sexualorient_stats_year[[sexualorient]] <- c(
        f_return_p_group(data_year, var, value1, "Sexual_orient", sexualorient, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$Sexual_orient == sexualorient) %>%
          nrow() )
    }
  }
  
  names(sexualorient_stats_year)[names(sexualorient_stats_year) == "stat"] <- " "
  sexualorient_stats_year
}


f_sexualorient_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all sexual orientation groups in Sexual_orient
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  sexualorient_groups <- levels(data_year$Sexual_orient)
  
  sexualorient_z_scores_year <- data.frame(sexualorient = sexualorient_groups)
  
  for (i in 1:length(sexualorient_groups)) {
    col <- c()
    for (j in 1:length(sexualorient_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Sexual_orient", sexualorient_groups[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$Sexual_orient, sexualorient_groups[j]),
            p2 = f_return_p_group(data_year, var, value, "Sexual_orient", sexualorient_groups[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$Sexual_orient, sexualorient_groups[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Sexual_orient", sexualorient_groups[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Sexual_orient == sexualorient_groups[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "Sexual_orient", sexualorient_groups[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Sexual_orient == sexualorient_groups[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    sexualorient_z_scores_year[[sexualorient_groups[i]]] <- col
  }
  
  names(sexualorient_z_scores_year)[names(sexualorient_z_scores_year) == "Sexual Orientation"] <- " "
  sexualorient_z_scores_year
}


## Dependants ####

f_depend_stats_year <- function(year, depend_var, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across dependants status for depend_var
# DEPEND1 = child; DEPEND2 = disability; DEPEND3 = elderly; Dependants = any
# Default behaviour "dk" includes don't knows.

  data_year <- eval((as.name(paste0("data_",year))))
  
  if (dk) {
    depend_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK","Base"),
      Yes = c(
        f_return_p_group(data_year, var, value1, depend_var, "Has dependants",weight = weight) * 100,
        f_return_p_group(data_year, var, value2, depend_var, "Has dependants", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, depend_var, "Has dependants", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year[[depend_var]], "Has dependants")
      ),
      No = c(
        f_return_p_group(data_year, var, value1, depend_var, "Does not have dependants", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, depend_var, "Does not have dependants", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, depend_var, "Does not have dependants", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year[[depend_var]], "Does not have dependants")
      )
      ,
      z = c(
        f_return_z(
          p1 = f_return_p_group(data_year, var, value1, depend_var, "Has dependants", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Has dependants"),
          p2 = f_return_p_group(data_year, var, value1, depend_var, "Does not have dependants", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Does not have dependants")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, value2, depend_var, "Has dependants", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Has dependants"),
          p2 = f_return_p_group(data_year, var, value2, depend_var, "Does not have dependants", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Does not have dependants")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, dont_know, depend_var, "Has dependants", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Has dependants"),
          p2 = f_return_p_group(data_year, var, dont_know, depend_var, "Does not have dependants", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year[[depend_var]], "Does not have dependants")
        ),
        NA
      )
    )
  } else {
    depend_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
     Yes  = c(
        f_return_p_group(data_year, var, value1, depend_var, "Has dependants", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & .[[depend_var]] == "Has dependants") %>%
          nrow()
      ),
      No = c(
        f_return_p_group(data_year, var, value1, depend_var, "Does not have dependants", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & .[[depend_var]] == "Does not have dependants") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = Yes / 100, 
                          n1 = Yes[trust == "Base"],
                          p2 = No / 100,
                          n2 = No[trust == "Base"])
      ))
  }
  
  names(depend_stats_year) <- c(" ", "Has dependants", "Does not have dependants", "Z Score")
  depend_stats_year
}

## Ethnic ####

f_ethnic_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
  # Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all ethnicities in ETHNIC
  # Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ethnic_categories <- levels(data_year$ETHNIC)
  
  if (dk) {
    ethnic_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (ethnic in ethnic_categories) {
      ethnic_stats_year[[ethnic]] <- c(
        f_return_p_group(data_year, var, value1, "ETHNIC", ethnic, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "ETHNIC", ethnic, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "ETHNIC", ethnic, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$ETHNIC, ethnic))
    }
  } else {
    ethnic_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (ethnic in ethnic_categories) {
      ethnic_stats_year[[ethnic]] <- c(
        f_return_p_group(data_year, var, value1, "ETHNIC", ethnic, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$ETHNIC == ethnic) %>%
          nrow() )
    }
  }
  
  names(ethnic_stats_year)[names(ethnic_stats_year) == "stat"] <- " "
  ethnic_stats_year
}


f_ethnic_z_scores_year <- function(year, var, value, dk = TRUE) {
  # Returns data frame comparing significant differences of "value" in "var" across all marital status groups in MS_GRP
  # Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ethnic_categories <- levels(data_year$ETHNIC)
  
  ethnic_z_scores_year <- data.frame(ethnic = ethnic_categories)
  
  for (i in 1:length(ethnic_categories)) {
    col <- c()
    for (j in 1:length(ethnic_categories)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "ETHNIC", ethnic_categories[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$ETHNIC, ethnic_categories[j]),
            p2 = f_return_p_group(data_year, var, value, "ETHNIC", ethnic_categories[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$ETHNIC, ethnic_categories[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "ETHNIC", ethnic_categories[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & ETHNIC == ethnic_categories[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "ETHNIC", ethnic_categories[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & ETHNIC == ethnic_categories[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    ethnic_z_scores_year[[ethnic_categories[i]]] <- col
  }
  
  names(ethnic_z_scores_year)[names(ethnic_z_scores_year) == "Ethnic"] <- " "
  ethnic_z_scores_year
}

## Ethnic Group ####

f_ethnicgrp_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all ethnic groups in Ethnic_group
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ethnicgrps <- levels(data_year$Ethnic_group)
  
  if (dk) {
    ethnicgrp_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    for (ethnicgrp in ethnicgrps) {
      ethnicgrp_stats_year[[ethnicgrp]] <- c(
        f_return_p_group(data_year, var, value1, "Ethnic_group", ethnicgrp, weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "Ethnic_group", ethnicgrp, weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "Ethnic_group", ethnicgrp, weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$Ethnic_group, ethnicgrp))
    }
  } else {
    ethnicgrp_stats_year <- data.frame(stat = c("% Yes", "Base"))
    for (ethnicgrp in ethnicgrps) {
      ethnicgrp_stats_year[[ethnicgrp]] <- c(
        f_return_p_group(data_year, var, value1, "Ethnic_group", ethnicgrp, weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & data_year$Ethnic_group == ethnicgrp) %>%
          nrow() )
    }
  }
  
  names(ethnicgrp_stats_year)[names(ethnicgrp_stats_year) == "stat"] <- " "
  ethnicgrp_stats_year
}


f_ethnicgrp_z_scores_year <- function(year, var, value, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across all marital status groups in MS_GRP
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
  ethnicgrps <- levels(data_year$Ethnic_group)
  
  ethnicgrp_z_scores_year <- data.frame(ethnicgrp = ethnicgrps)
  
  for (i in 1:length(ethnicgrps)) {
    col <- c()
    for (j in 1:length(ethnicgrps)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Ethnic_group", ethnicgrps[j], weight = weight),
            n1 = f_return_n_group(data_year[[var]], data_year$Ethnic_group, ethnicgrps[j]),
            p2 = f_return_p_group(data_year, var, value, "Ethnic_group", ethnicgrps[i], weight = weight),
            n2 = f_return_n_group(data_year[[var]], data_year$Ethnic_group, ethnicgrps[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "Ethnic_group", ethnicgrps[j], weight = weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Ethnic_group == ethnicgrps[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "Ethnic_group", ethnicgrps[i], weight = weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != dont_know & Ethnic_group == ethnicgrps[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    ethnicgrp_z_scores_year[[ethnicgrps[i]]] <- col
  }
  
  names(ethnicgrp_z_scores_year)[names(ethnicgrp_z_scores_year) == "Ethnic Group"] <- " "
  ethnicgrp_z_scores_year
}


## Ethnic White Other ####

f_ethnicwo_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
# Returns data frame comparing significant differences of "value" in "var" across white ethnic and other based on Ethnic_white_other
# Default behaviour "dk" includes don't knows.
  
  data_year <- eval((as.name(paste0("data_",year))))
   
  if (dk) {
    ethnicwo_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK", "Base"),
      white = c(
        f_return_p_group(data_year, var, value1, "Ethnic_white_other", "White", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "Ethnic_white_other", "White", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "Ethnic_white_other", "White", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "White")
      ),
      other = c(
        f_return_p_group(data_year, var, value1, "Ethnic_white_other", "Other", weight = weight) * 100,
        f_return_p_group(data_year, var, value2, "Ethnic_white_other", "Other", weight = weight) * 100,
        f_return_p_group(data_year, var, dont_know, "Ethnic_white_other", "Other", weight = weight) * 100,
        f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "Other")
      )
      ,
      z = c(
        f_return_z(
          p1 = f_return_p_group(data_year, var, value1, "Ethnic_white_other", "White", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "White"),
          p2 = f_return_p_group(data_year, var, value1, "Ethnic_white_other", "Other", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "Other")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, value2, "Ethnic_white_other", "White", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "White"),
          p2 = f_return_p_group(data_year, var, value2, "Ethnic_white_other", "Other", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "Other")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, dont_know, "Ethnic_white_other", "White", weight = weight),
          n1 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "White"),
          p2 = f_return_p_group(data_year, var, dont_know, "Ethnic_white_other", "Other", weight = weight),
          n2 = f_return_n_group(data_year[[var]], data_year$Ethnic_white_other, "Other")
        ),
        NA
      )
    )
  } else {
    ethnicwo_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
      white = c(
        f_return_p_group(data_year, var, value1, "Ethnic_white_other", "White", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & Ethnic_white_other == "White") %>%
          nrow()
      ),
      other = c(
        f_return_p_group(data_year, var, value1, "Ethnic_white_other", "Other", weight = weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & Ethnic_white_other == "White") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = white / 100, 
                          n1 = white[trust == "Base"],
                          p2 = other / 100,
                          n2 = other[trust == "Base"])
      ))
  }
    
  names(ethnicwo_stats_year) <- c(" ","White","Other","Z Score")
  ethnicwo_stats_year
}