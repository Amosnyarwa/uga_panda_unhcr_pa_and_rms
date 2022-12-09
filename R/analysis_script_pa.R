library(tidyverse)
library(srvyr)

# clean data with weights
data_path <- "inputs/clean_data_unhcr_pa.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_ref_with_weights <- readxl::read_excel(path = data_path, sheet = "UGA2207_PA", col_types = c_types)

# dap
dap <- read_csv("inputs/r_dap_uga_pa.csv")

# set up design objects
ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )