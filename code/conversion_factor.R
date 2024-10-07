# Conversion factor
library(here)

# Milner, 2023

milner_diet <- read.csv(here("data","cv_milner_2023_diet.csv"))


four = tapply(milner_diet$val, milner_diet$year == "2016" & milner_diet$measure_id == "4", sum)/tapply(milner_diet$val, milner_diet$year == "2016" & milner_diet$measure_id == "1", sum)
nine = tapply(milner_diet$val, milner_diet$year == "2020" & milner_diet$measure_id == "4", sum)/tapply(milner_diet$val, milner_diet$year == "2020" & milner_diet$measure_id == "1", sum)

  # For 2035, using decrease rate of 2016 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))




milner_air <- read.csv(here("data","cv_milner_2023_air.csv"))


four = tapply(milner_air$val, milner_air$year == "2016" & milner_air$measure_id == "4", sum)/tapply(milner_air$val, milner_air$year == "2016" & milner_air$measure_id == "1", sum)
nine = tapply(milner_air$val, milner_air$year == "2020" & milner_air$measure_id == "4", sum)/tapply(milner_air$val, milner_air$year == "2020" & milner_air$measure_id == "1", sum)

# For 2035, using decrease rate of 2016 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))





milner_pa <- read.csv(here("data","cv_milner_2023_physical.csv"))


four = tapply(milner_pa$val, milner_pa$year == "2016" & milner_pa$measure_id == "4", sum)/tapply(milner_pa$val, milner_pa$year == "2016" & milner_pa$measure_id == "1", sum)
nine = tapply(milner_pa$val, milner_pa$year == "2020" & milner_pa$measure_id == "4", sum)/tapply(milner_pa$val, milner_pa$year == "2020" & milner_pa$measure_id == "1", sum)

# For 2035, using decrease rate of 2016 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))



# Williams, 2018

williams <- read.csv(here("data","cv_williams_2018.csv"))


one = tapply(williams$val, williams$year == "2011" & williams$measure_id == "4", sum)/tapply(williams$val, williams$year == "2011" & williams$measure_id == "1", sum)
nine = tapply(williams$val, williams$year == "2020" & williams$measure_id == "4", sum)/tapply(williams$val, williams$year == "2020" & williams$measure_id == "1", sum)


  # For 2080 (2011 + ((2154-2011)/2)), using decrease rate of 2011 - 2020 (6 times)
nine [["TRUE"]]+(6*(nine [["TRUE"]] - one [["TRUE"]]))




# zysk, 2020

zysk <- read.csv(here("data","cv_zysk_2020.csv"))


four = tapply(zysk$val, zysk$year == "2011" & zysk$measure_id == "4", sum)/tapply(zysk$val, zysk$year == "2011" & zysk$measure_id == "1", sum)
nine = tapply(zysk$val, zysk$year == "2020" & zysk$measure_id == "4", sum)/tapply(zysk$val, zysk$year == "2020" & zysk$measure_id == "1", sum)

# For 2050, using decrease rate of 2014 - 2019 (6 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))



# phillips, 2021

phillips <- read.csv(here("data","cv_phillips_2021.csv"))


four = tapply(phillips$val, phillips$year == "2011" & phillips$measure_id == "4", sum)/tapply(phillips$val, phillips$year == "2011" & phillips$measure_id == "1", sum)
nine = tapply(phillips$val, phillips$year == "2020" & phillips$measure_id == "4", sum)/tapply(phillips$val, phillips$year == "2020" & phillips$measure_id == "1", sum)

# For 2050, using decrease rate of 2011 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))


#### China 2050 ####

china <- read.csv(here("data","cv_china_2050.csv"))


four = tapply(china$val, china$year == "2011" & china$measure_id == "4", sum)/tapply(china$val, china$year == "2011" & china$measure_id == "1", sum)
nine = tapply(china$val, china$year == "2020" & china$measure_id == "4", sum)/tapply(china$val, china$year == "2020" & china$measure_id == "1", sum)

# For 2030, using decrease rate of 2011 - 2020 (once)
nine [["TRUE"]]+(1*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2050, using decrease rate of 2011 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2060, using decrease rate of 2011 - 2020 (4 times)
nine [["TRUE"]]+(4*(nine [["TRUE"]] - four [["TRUE"]]))

  # +25 years old
china_25 <- read.csv(here("data","cv_china_25+.csv"))

four = tapply(china_25$val, china_25$year == "2011" & china_25$measure_id == "4", sum)/tapply(china_25$val, china_25$year == "2011" & china_25$measure_id == "1", sum)
nine = tapply(china_25$val, china_25$year == "2020" & china_25$measure_id == "4", sum)/tapply(china_25$val, china_25$year == "2020" & china_25$measure_id == "1", sum)

# For 2050, using decrease rate of 2011 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2060, using decrease rate of 2011 - 2020 (4 times)
nine [["TRUE"]]+(4*(nine [["TRUE"]] - four [["TRUE"]]))



# Hamilton, 2021

hamilton <- read.csv(here("data","cv_hamilton.csv"))


four = tapply(hamilton$val, hamilton$year == "2011" & hamilton$measure_id == "4", sum)/tapply(hamilton$val, hamilton$year == "2011" & hamilton$measure_id == "1", sum)
nine = tapply(hamilton$val, hamilton$year == "2020" & hamilton$measure_id == "4", sum)/tapply(hamilton$val, hamilton$year == "2020" & hamilton$measure_id == "1", sum)

# For 2040, using decrease rate of 2011 - 2020 (twice)
nine [["TRUE"]]+(2*(nine [["TRUE"]] - four [["TRUE"]]))




# California

california <- read.csv(here("data","cv_california.csv"))


four = tapply(california$val, california$year == "2011" & california$measure_id == "4", sum)/tapply(california$val, california$year == "2011" & california$measure_id == "1", sum)
nine = tapply(california$val, california$year == "2020" & california$measure_id == "4", sum)/tapply(california$val, california$year == "2020" & california$measure_id == "1", sum)

# For 2050, using decrease rate of 2011 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))





# Dimitrova, 2021

india <- read.csv(here("data","cv_dimitrova_2021.csv"))


four = tapply(india$val, india$year == "2011" & india$measure_id == "4", sum)/tapply(india$val, india$year == "2011" & india$measure_id == "1", sum)
nine = tapply(india$val, india$year == "2020" & india$measure_id == "4", sum)/tapply(india$val, india$year == "2020" & india$measure_id == "1", sum)

# For 2050, using decrease rate of 2011 - 2020 (Once)
nine [["TRUE"]]+(1*(nine [["TRUE"]] - four [["TRUE"]]))




# World
world <- read.csv(here("data","cv_world.csv"))


four = tapply(world$val, world$year == "2011" & world$measure_id == "4", sum)/tapply(world$val, world$year == "2011" & world$measure_id == "1", sum)
nine = tapply(world$val, world$year == "2020" & world$measure_id == "4", sum)/tapply(world$val, world$year == "2020" & world$measure_id == "1", sum)

# For 2030, using decrease rate of 2011 - 2020 (once)
nine [["TRUE"]]+(1*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2040, using decrease rate of 2011 - 2020 (2 times)
nine [["TRUE"]]+(2*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2050, using decrease rate of 2011 - 2020 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))

# For 2060, using decrease rate of 2011 - 2020 (4 times)
nine [["TRUE"]]+(4*(nine [["TRUE"]] - four [["TRUE"]]))




