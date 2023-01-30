library(tidyverse)
library(srvyr)

# clean data with weights
data_path <- "inputs/clean_data_unhcr_rms.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "RMS Uganda 2022 UNHCR_cleaned"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "RMS Uganda 2022 UNHCR_cleaned", col_types = c_types, na = "NA")
df_roster_clean_data <- readxl::read_excel(path = data_path, sheet = "hh_roster", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/RMS_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_uga_rms.csv")

# set up design objects
ref_svy <- as_survey(.data = df_main_clean_data)

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_dap = dap)


# merge analysis ----------------------------------------------------------

combined_analysis <- df_main_analysis
 
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>%
  left_join(df_question_type_data, by = c("int.variable" = "kobo_question")) |> 
  select(`Question`= label, variable, `choices/options` = variable_val, `Results(mean/percentage)` = `mean/pct`, n_unweighted, population, subset_1_name, subset_1_val)

write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_rms.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_rms.csv"), na="")
