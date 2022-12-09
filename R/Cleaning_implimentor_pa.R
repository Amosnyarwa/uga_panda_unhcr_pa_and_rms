# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")
source("R/composite_indicators_and_weights.R")

# Read data and checking log

df_cleaning_log <- read_csv("inputs/combined_checks_PA.csv", col_types = cols(sheet = "c", index = "i")) |> 
  filter(!adjust_log %in% c("delete_log")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)


# handle data -------------------------------------------------------------

data_path <- "inputs/partipatory_assessment_data.xlsx"

# main data
cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = data_path, sheet = "UGA2207_PA", col_types = c_types) |> 
  mutate(number = "NA") |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# loops
# hh_roster
hh_roster <- readxl::read_excel(path = data_path, sheet = "hh_roster") |> 
  mutate(name_hh = openssl::md5(name_hh)) 

df_raw_data_hh_roster <- df_raw_data |> 
  select(-`_index`) |> 
  inner_join(hh_roster, by = c("_uuid" = "_submission__uuid") )

# tool
df_survey <- readxl::read_excel("inputs/participatory_assessment_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/participatory_assessment_tool.xlsx", sheet = "choices")

# main dataset ------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log |> 
  filter(is.na(sheet))

df_cleaned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data,
                                              input_df_survey = df_survey,
                                              input_df_choices = df_choices,
                                              input_df_cleaning_log = df_cleaning_log_main) |> 
  mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))

# clean repeats -----------------------------------------------------------

other_repeat_col <- c("start", "end", "today", "settlement", "hoh_equivalent", "respondent_gender", 
                      "respondent_age", "status")

df_cleaning_log_roster <- df_cleaning_log |> 
  filter(uuid %in% df_raw_data_hh_roster$`_uuid`, name %in% colnames(df_raw_data_hh_roster))

df_cleaned_data_hh_roster <- implement_cleaning_support(input_df_raw_data = df_raw_data_hh_roster,
                                                        input_df_survey = df_survey,
                                                        input_df_choices = df_choices,
                                                        input_df_cleaning_log = df_cleaning_log_roster) |> 
  select(any_of(other_repeat_col), any_of(colnames(hh_roster)), `_index` = index, `_submission__uuid` = uuid) |> 
  mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))




# add composite indicators ------------------------------------------------

df_main_with_composites <- create_composite_indicators_pa(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.settlement, "_refugee"),
                            status == "host" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  )) |> 
  filter(status == "refugee")

# create weights

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_main_with_composites, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_main_with_composites %>% 
  left_join(ref_weight_table, by = "strata")

# write final modified data -----------------------------------------------

list_of_clean_datasets <- list("UGA2207_PA" = df_cleaned_data,
                               "hh_roster" = df_cleaned_data_hh_roster
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_unhcr_pa.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")