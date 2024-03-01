# Packages
pacman::p_load(dplyr,
               tidyr,
               readxl,
               ggplot2,
               readr,
               ggpubr,
               here,
               treemap,
               sf,
               rnaturalearth,
               networkD3,
               mapproj,
               ggrepel,
               ggdist,
               wesanderson,
               RColorBrewer,
               jsonlite,
               lattice,
               ggVennDiagram,
               venneuler)



# Data
mortality_proj_wpp_2022 <- read_excel(here("data","mortality_medium_variant_wpp_2022.xlsx"))
population_proj_wpp_2022 <- read_excel(here("data","population_medium_variant_wpp_2022.xlsx"))



# Table format

## Mortality
names(mortality_proj_wpp_2022) <- mortality_proj_wpp_2022[12,]

mortality_proj <- mortality_proj_wpp_2022[-c(1:12),]

i <- c(11:112) 
mortality_proj[ , i] <- apply(mortality_proj[ , i], 2,  
                              function(x) as.numeric(x))

### Getting sum of deaths for each region and year
mortality_proj$n_death <- rowSums( mortality_proj[,12:112] ) *1000      #Deaths are expressed in thousand in raw data

mortality_proj_clean <- mortality_proj %>%
  rename (region = `Region, subregion, country or area *`) %>%
  select(c(region,Type,Year,n_death))


## Population
names(population_proj_wpp_2022) <- population_proj_wpp_2022[12,]

population_proj <- population_proj_wpp_2022[-c(1:12),]

i <- c(11:112) 
population_proj[ , i] <- apply(population_proj[ , i], 2,  
                              function(x) as.numeric(x))

### Getting sum of pop for each region and year
population_proj$n_pop <- rowSums( population_proj[,12:112] ) *1000      #Deaths are expressed in thousand in raw data

population_proj_clean <- population_proj %>%
  rename (region = `Region, subregion, country or area *`) %>%
  select(c(region,Type,Year,n_pop))






# Table for extraction grid

mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "WORLD",  Year == "2030" |Year == "2050")

sum(mortality_proj_syst_review$n_death)


population_proj_syst_review <- population_proj_clean %>%
  filter(region == "WORLD" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)

# Markandya
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "WORLD",  Year == "2030" )

sum(mortality_proj_syst_review$n_death)

# Weyant

mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "WORLD",  Year == "2032" )

sum(mortality_proj_syst_review$n_death)


# Barban, 2022  
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "France" 
         , Year == "2045")

sum(mortality_proj_syst_review$n_death)



population_proj_syst_review <- population_proj_clean %>%
  filter(region == "France" 
         , Year == "2045")

sum(population_proj_syst_review$n_pop)


# China, 2035/2050/2060 (Cai, Chen, Tang, Xing, Xie)
population_proj_syst_review <- population_proj_clean %>%
  filter(region == "China" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)


population_proj_syst_review <- population_proj_clean %>%
  filter(region == "China" 
         , Year == "2060")

sum(population_proj_syst_review$n_pop)
sum(population_proj_syst_review$n_pop)*0.043  # Anhui

population_proj_syst_review <- population_proj_clean %>%
  filter(region == "China" 
         , Year == "2035")

sum(population_proj_syst_review$n_pop)

# Conibear, 2022
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "China" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)



# Dimitrova, 2021
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "India" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)

mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "India" 
         , Year == "2030")

sum(mortality_proj_syst_review$n_death)


# Hamilton, 2021
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Brazil" |region == "China" |region == "Germany" |region == "India" 
         |region == "Indonesia" |region == "Nigeria"|region == "South Africa" |region == "United Kingdom" 
         |region == "United States of America" 
         , Year == "2040")

sum(mortality_proj_syst_review$n_death)


# Milner, 2023 / Williams, 2018
population_proj_syst_review <- population_proj_clean %>%
  filter(region == "United Kingdom" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)


population_proj_syst_review <- population_proj_clean %>%
  filter(region == "United Kingdom" 
         , Year == "2021" | Year == "2022" | Year == "2023" | Year == "2024" | Year == "2025" | Year == "2026" | Year == "2027" | Year == "2028" | Year == "2029" | Year == "2050" |
           Year == "2031" | Year == "2032" | Year == "2033" | Year == "2034" | Year == "2035" | Year == "2036" | Year == "2037" | Year == "2038" | Year == "2039" | Year == "2050" |
           Year == "2041" | Year == "2042" | Year == "2043" | Year == "2044" | Year == "2045" | Year == "2046" | Year == "2047" | Year == "2048" | Year == "2049" | Year == "2050" )

sum(population_proj_syst_review$n_pop)


mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "United Kingdom" , Year == "2050")

sum(mortality_proj_syst_review$n_death)

mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "United Kingdom" , Year == "2035")

sum(mortality_proj_syst_review$n_death)


# Nawaz, 2022
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Argentina" |region == "Australia" |region == "Austria" |region == "Luxembourg" 
         |region == "Belgium" |region == "Bulgaria"|region == "Brazil" |region == "Canada" |region == "China" 
         |region == "Cyprus" |region == "Czechia"|region == "Germany" |region == "Denmark" |region == "Spain" 
         |region == "Estonia" |region == "Finland"|region == "France" |region == "United Kingdom" |region == "Greece" 
         |region == "Croatia" |region == "Hungary"|region == "Indonesia" |region == "India" |region == "Ireland"
         |region == "Italy" |region == "Japan"|region == "Lithuania" |region == "Dem. People's Republic of Korea" 
         |region == "Latvia" |region == "Mexico"|region == "Malta" |region == "Netherlands" |region == "Poland" 
         |region == "Portugal" |region == "Romania"|region == "Russian Federation" |region == "Türkiye"
         |region == "Saudi Arabia" |region == "Slovakia"|region == "Slovenia"|region == "Sweden"
         , Year == "2040")

sum(mortality_proj_syst_review$n_death)


# Phillips, 2021
population_proj_syst_review <- population_proj_clean %>%
  filter(region == "Dem. People's Republic of Korea" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)


mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Dem. People's Republic of Korea" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)


# Qu, 2020
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "China" 
         , Year == "2030")

sum(mortality_proj_syst_review$n_death)


# Rafaj, 2018 outdoor
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "EUROPE" |region == "China" |region == "Indonesia" 
         |region == "India" |region == "South Africa", Year == "2040")

sum(mortality_proj_syst_review$n_death)

# Rafaj, 2018 indoor
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Brazil" |region == "China" |region == "Indonesia" 
         |region == "India" |region == "South Africa", Year == "2040")

sum(mortality_proj_syst_review$n_death)



# Rafaj, 2021
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "China" |region == "India" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)


# Rafaj, 2012
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Austria" |region == "Belgium" |region == "Bulgaria"|region == "China" 
         |region == "Cyprus" |region == "Czechia"|region == "Germany" |region == "Denmark" |region == "Spain" 
         |region == "Estonia" |region == "Finland"|region == "France" |region == "United Kingdom" |region == "Greece" 
         |region == "Croatia" |region == "Hungary"|region == "India" |region == "Ireland" 
         |region == "Italy" |region == "Lithuania" |region == "Luxembourg" 
         |region == "Latvia" |region == "Malta" |region == "Netherlands" |region == "Poland" 
         |region == "Portugal" |region == "Romania" |region == "Slovakia"|region == "Slovenia"|region == "Sweden"
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)




# Shen, 2022
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "China" 
         , Year == "2060")

sum(mortality_proj_syst_review$n_death)



# Shindell, 2020
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "United States of America" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)


population_proj_syst_review <- population_proj_clean %>%
  filter(region == "United States of America" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)


# Xing, 2020
mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "China" 
         , Year == "2035")

sum(mortality_proj_syst_review$n_death)



# Zyzk, 2020
population_proj_syst_review <- population_proj_clean %>%
  filter(region == "Poland" 
         , Year == "2050")

sum(population_proj_syst_review$n_pop)


mortality_proj_syst_review <- mortality_proj_clean %>%
  filter(region == "Poland" 
         , Year == "2050")

sum(mortality_proj_syst_review$n_death)











