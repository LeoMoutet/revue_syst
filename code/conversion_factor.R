# Conversion factor
library(here)

# Milner, 2023

milner_diet <- read.csv(here("data","milner_diet.csv"))


four = tapply(milner_diet$val, milner_diet$year == "2014" & milner_diet$measure_id == "4", sum)/tapply(milner_diet$val, milner_diet$year == "2014" & milner_diet$measure_id == "1", sum)
nine = tapply(milner_diet$val, milner_diet$year == "2019" & milner_diet$measure_id == "4", sum)/tapply(milner_diet$val, milner_diet$year == "2019" & milner_diet$measure_id == "1", sum)

  # For 2035, using decrease rate of 2014 - 2019 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))




milner_air <- read.csv(here("data","milner_air.csv"))


four = tapply(milner_air$val, milner_air$year == "2014" & milner_air$measure_id == "4", sum)/tapply(milner_air$val, milner_air$year == "2014" & milner_air$measure_id == "1", sum)
nine = tapply(milner_air$val, milner_air$year == "2019" & milner_air$measure_id == "4", sum)/tapply(milner_air$val, milner_air$year == "2019" & milner_air$measure_id == "1", sum)

# For 2035, using decrease rate of 2014 - 2019 (3 times)
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))





milner_pa <- read.csv(here("data","milner_pa.csv"))


four = tapply(milner_pa$val, milner_pa$year == "2014" & milner_pa$measure_id == "4", sum)/tapply(milner_pa$val, milner_pa$year == "2014" & milner_pa$measure_id == "1", sum)
nine = tapply(milner_pa$val, milner_pa$year == "2019" & milner_pa$measure_id == "4", sum)/tapply(milner_pa$val, milner_pa$year == "2019" & milner_pa$measure_id == "1", sum)

# For 2035, using decrease rate of 2014 - 2019 
nine [["TRUE"]]+(3*(nine [["TRUE"]] - four [["TRUE"]]))



# Williams, 2018

williams <- read.csv(here("data","williams.csv"))


one = tapply(williams$val, williams$year == "2011" & williams$measure_id == "4", sum)/tapply(williams$val, williams$year == "2011" & williams$measure_id == "1", sum)
nine = tapply(williams$val, williams$year == "2019" & williams$measure_id == "4", sum)/tapply(williams$val, williams$year == "2019" & williams$measure_id == "1", sum)


  # For 2082 (2011 + ((2154-2011)/2)), using decrease rate of 2011 - 2019 (10 times)
nine [["TRUE"]]+(10*(nine [["TRUE"]] - one [["TRUE"]]))




# zysk, 2020

zysk <- read.csv(here("data","zysk.csv"))


four = tapply(zysk$val, zysk$year == "2014" & zysk$measure_id == "4", sum)/tapply(zysk$val, zysk$year == "2014" & zysk$measure_id == "1", sum)
nine = tapply(zysk$val, zysk$year == "2019" & zysk$measure_id == "4", sum)/tapply(zysk$val, zysk$year == "2019" & zysk$measure_id == "1", sum)

# For 2050, using decrease rate of 2014 - 2019 (6 times)
nine [["TRUE"]]+(6*(nine [["TRUE"]] - four [["TRUE"]]))



# phillips, 2021

phillips <- read.csv(here("data","phillips.csv"))


four = tapply(phillips$val, phillips$year == "2014" & phillips$measure_id == "4", sum)/tapply(phillips$val, phillips$year == "2014" & phillips$measure_id == "1", sum)
nine = tapply(phillips$val, phillips$year == "2019" & phillips$measure_id == "4", sum)/tapply(phillips$val, phillips$year == "2019" & phillips$measure_id == "1", sum)

# For 2050, using decrease rate of 2014 - 2019 (6 times)
nine [["TRUE"]]+(6*(nine [["TRUE"]] - four [["TRUE"]]))





