# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")
source("R/composite_indicators.R")

# Read data and checking log
df_full_cl_log <- read_csv("inputs/combined_checks_PA.csv", col_types = cols(sheet = "c", index = "i")) 

df_cleaning_log <- df_full_cl_log |> 
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

# handling Personally Identifiable Information(PII)
vars_to_remove_from_data <- c("deviceid", "audit",
                              "audit_URL", "instance_name",
                              "complainant_name", "complainant_id",
                              "respondent_telephone", "name_pers_recording",
                              "geopoint",
                              "_geopoint_latitude", "_geopoint_longitude",
                              "_geopoint_altitude", "_geopoint_precision",
                              "number", "number_confirm", "mm_name")

df_raw_data <- readxl::read_excel(path = data_path, sheet = "UGA2207_PA", col_types = c_types) |> 
  mutate(across(.cols = any_of(vars_to_remove_from_data), .fns = ~na_if(., .))) |>  
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) |> 
  select(-c(`water_access_challenges_faced/lack_of_support_from_unhcr_and_partners`,
            `water_access_challenges_faced/lack_of_material_to_build_or_repair_the_shelter_structure`,
            `water_access_challenges_faced/materials_are_too_expensive`,
            `water_access_challenges_faced/members_of_the_household_have_a_disability`,
            `water_access_challenges_faced/bad_weather_often_damages_our_shelter_structures`,
            `water_access_challenges_faced/land_available_is_too_small`))

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
                                              input_df_cleaning_log = df_cleaning_log_main) #|> 
  # mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
  #               .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))

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
  # mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
  #               .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))
  filter(`_submission__uuid` %in% df_cleaned_data$uuid)

# deletion log ------------------------------------------------------------

df_deletion_log <- df_cleaning_log |> 
  filter(type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup()


# add composite indicators ------------------------------------------------

df_main_with_composites <- create_composite_indicators_pa(input_df = df_cleaned_data) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.settlement, "_refugee"),
                            status == "host" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  )) |> 
  filter(status == "refugee")


# write final modified data -----------------------------------------------

list_of_clean_datasets <- list("Raw_main" = df_raw_data,
                               "Raw_roster" = hh_roster,
                               "cleaning_log" = df_full_cl_log,
                               "deletion_log" = df_deletion_log,
                               "UGA2207_PA" = df_main_with_composites,
                               "hh_roster" = df_cleaned_data_hh_roster
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_unhcr_pa.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")
