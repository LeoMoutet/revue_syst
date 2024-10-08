# Conversion factor
library(here)


####################################################################
# Extrapolate the rate for any target year
extrapolate_rate <- function(data, base_year1, base_year2, measure_id_1, measure_id_4, target_year) {
  
  # Calculate the ratio for the first base year (base_year1)
  rate_year1 <- tapply(data$val, data$year == base_year1 & data$measure_id == measure_id_4, sum) / 
    tapply(data$val, data$year == base_year1 & data$measure_id == measure_id_1, sum)
  
  # Calculate the ratio for the second base year (base_year2)
  rate_year2 <- tapply(data$val, data$year == base_year2 & data$measure_id == measure_id_4, sum) / 
    tapply(data$val, data$year == base_year2 & data$measure_id == measure_id_1, sum)
  
  # Calculate the yearly rate of change between base_year1 and base_year2
  yearly_change <- (rate_year2[["TRUE"]] - rate_year1[["TRUE"]]) / (base_year2 - base_year1)
  
  # Extrapolate the rate for the target year based on the yearly change
  projected_rate <- rate_year2[["TRUE"]] + (yearly_change * (target_year - base_year2))
  
  return(projected_rate)
}

####################################################################

# Milner, 2023

milner_diet <- read.csv(here("data","cv_milner_2023_diet.csv"))
extrapolate_rate(milner_diet, 2016, 2020, 1, 4, 2035)

milner_air <- read.csv(here("data","cv_milner_2023_air.csv"))
extrapolate_rate(milner_air, 2016, 2020, 1, 4, 2035)

milner_pa <- read.csv(here("data","cv_milner_2023_physical.csv"))
extrapolate_rate(milner_pa, 2016, 2020, 1, 4, 2035)


# Williams, 2018

williams <- read.csv(here("data","cv_williams_2018.csv"))
extrapolate_rate(williams, 2011, 2020, 1, 4, 2080)


# zysk, 2020

zysk <- read.csv(here("data","cv_zysk_2020.csv"))
extrapolate_rate(zysk, 2011, 2020, 1, 4, 2050)


# phillips, 2021

phillips <- read.csv(here("data","cv_phillips_2021.csv"))
extrapolate_rate(phillips, 2011, 2020, 1, 4, 2050)


#### China 2050 ####

china <- read.csv(here("data","cv_china_2050.csv"))
extrapolate_rate(china, 2011, 2020, 1, 4, 2030)
extrapolate_rate(china, 2011, 2020, 1, 4, 2035)
extrapolate_rate(china, 2011, 2020, 1, 4, 2050)
extrapolate_rate(china, 2011, 2020, 1, 4, 2054)
extrapolate_rate(china, 2011, 2020, 1, 4, 2060)

  # +25 years old
china_25 <- read.csv(here("data","cv_china_25+.csv"))
extrapolate_rate(china_25, 2011, 2020, 1, 4, 2050)
extrapolate_rate(china_25, 2011, 2020, 1, 4, 2060)


# Hamilton, 2021

hamilton <- read.csv(here("data","cv_hamilton.csv"))
extrapolate_rate(hamilton, 2011, 2020, 1, 4, 2040)


# California

california <- read.csv(here("data","cv_california.csv"))
extrapolate_rate(california, 2011, 2020, 1, 4, 2040)


# Dimitrova, 2021

india <- read.csv(here("data","cv_dimitrova_2021.csv"))
extrapolate_rate(india, 2011, 2020, 1, 4, 2040)


# World
world <- read.csv(here("data","cv_world.csv"))
extrapolate_rate(world, 2011, 2020, 1, 4, 2030)
extrapolate_rate(world, 2011, 2020, 1, 4, 2040)
extrapolate_rate(world, 2011, 2020, 1, 4, 2047)
extrapolate_rate(world, 2011, 2020, 1, 4, 2050)
extrapolate_rate(world, 2011, 2020, 1, 4, 2060)


# Rafaj, 2013

rafaj2013 <- read.csv(here("data","cv_europe.csv"))
extrapolate_rate(rafaj2013, 2011, 2020, 1, 4, 2050)


# Rafaj, 2021

rafaj2021 <- read.csv(here("data","cv_rafaj2021.csv"))
extrapolate_rate(rafaj2021, 2011, 2020, 1, 4, 2050)


# Nawaz

nawaz <- read.csv(here("data","cv_nawaz.csv"))
extrapolate_rate(nawaz, 2011, 2020, 1, 4, 2040)


# Barban

barban <- read.csv(here("data","cv_barban.csv"))
extrapolate_rate(barban, 2011, 2020, 1, 4, 2045)


# Shindell

shindell <- read.csv(here("data","cv_usa.csv"))
extrapolate_rate(shindell, 2011, 2020, 1, 4, 2050)





