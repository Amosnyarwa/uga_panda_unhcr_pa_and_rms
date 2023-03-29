# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(checksupporteR)

source("R/support_functions.R")

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
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) |> 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = case_when(settlement %in% c("rhino_camp") ~ "madi_okollo",
                                           settlement %in% c("bidibidi") ~ "yumbe",
                                           settlement %in% c("imvepi") ~ "terego",
                                           settlement %in% c("palabek") ~ "lamwo",
                                           settlement %in% c("kyangwali") ~ "kikube",
                                           settlement %in% c("lobule") ~ "koboko",
                                           settlement %in% c("nakivale") ~ "isingiro",
                                           settlement %in% c("oruchinga") ~ "isingiro",
                                           settlement %in% c("palorinya") ~ "obongi",
                                           settlement %in% c("rwamwanja") ~ "kamwenge",
                                           settlement %in% c("kyaka_ii") ~ "kyegegwa",
                                           settlement %in% c("any_adjumani_settlements") ~ "adjumani",
                                           TRUE ~ settlement),
         district_name = i.check.district_name,
         i.check.settlement = settlement,
         # i.check.point_number = household_id,
         start = as_datetime(start),
         end = as_datetime(end)
         )

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

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids

df_c_duplicate_uuid <- check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_uuid")

# testing_data
df_testing_data <- df_raw_data |> 
  filter(i.check.start_date < as_date("2022-11-22") | str_detect(string = household_id, pattern = fixed('test', ignore_case = TRUE))) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_testing_data")

# host data
df_host_data <- df_raw_data |> 
  filter(str_detect(string = status, pattern = fixed('host', ignore_case = TRUE))) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_host_data",
         i.check.issue = "Host community not samples for surveys",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_host_data")

# count hh roster is not equal to mentioned number in the main dataset 
df_count_hh_number_less_20 <- df_raw_data_hh_roster |>
  group_by(`_uuid`) |>
  mutate(int.roster_count = n()) |> 
  filter(row_number() == 1) |> 
  filter(int.roster_count != count_hh_number) |> 
  ungroup() |> 
  mutate(i.check.type = "change_response",
         i.check.name = "count_hh_number ",
         int.roster_count_difference = int.roster_count - count_hh_number,
         i.check.current_value = as.character(count_hh_number),
         i.check.value = as.character(count_hh_number + int.roster_count_difference),
         i.check.issue_id = "logic_c_count_hh_number_less_20",
         i.check.issue = glue("int.roster_count : {int.roster_count}, hh_count not equal to roster composition"),
         i.check.other_text = "",
         i.check.checked_by = "AN",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "",
         i.check.sheet = "",
         i.check.index = "") |>
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_count_hh_number_less_20")

# checks on hhids ----------------------------------------------------------

sample_hhid_nos <- df_sample_data |> 
  pull(unique_hhid_number) |> 
  unique()

# duplicate point numbers
df_c_duplicate_hhid_nos <- check_duplicate_hhid_numbers(input_tool_data = df_tool_data,
                                                        input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_hhid_nos")

# pt id does not exist in sample
df_c_hhid_not_in_sample <- check_hhid_number_not_in_samples(input_tool_data = df_tool_data, 
                                                            input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_hhid_not_in_sample")


# HoH details in the household composition --------------------------------

# Respondent is not HoH & did not include HoH details in the household composition i.e. hoh_yn == "no" & relationship_to_hoh != hh_head in the hh roster

df_hoh_details_and_hh_roster_6 <- df_raw_data_hh_roster |>
  filter(hoh_yn == "no")  |>
  group_by(`_uuid`) |>
  mutate(int.hoh_bio = ifelse(relation_to_hoh_hhmembers %in% c("hh_head"), "given", "not")) |> 
  filter(!str_detect(string = paste(int.hoh_bio, collapse = ":"), pattern = "given")) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(i.check.type = "change_response",
         i.check.name = "relation_to_hoh_hhmembers ",
         i.check.current_value = relation_to_hoh_hhmembers,
         i.check.value = "",
         i.check.issue_id = "logic_c_hoh_details_and_hh_roster_6",
         i.check.issue = glue("relation_to_hoh_hhmembers : {relation_to_hoh_hhmembers}, hoh not in roster"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "",
         i.check.index = `_index`,
         i.check.sheet = "hh_roster") |>
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hoh_details_and_hh_roster_6")


# education group extra cleaning ------------------------------------------

df_logic_c_noschool_aged_but_attending_school_7a <- df_raw_data |>
  filter(num_children_school_aged == 0, !school_aged_children_attending_school %in% c("there_are_no_school_aged_children"))  |>
  mutate(i.check.type = "change_response",
         i.check.name = "school_aged_children_attending_school ",
         i.check.current_value = school_aged_children_attending_school,
         i.check.value = "there_are_no_school_aged_children",
         i.check.issue_id = "logic_c_noschool_aged_but_attending_school_7a",
         i.check.issue = glue("num_children_school_aged : {num_children_school_aged}, school_aged_children_attending_school : {school_aged_children_attending_school}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |>
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_noschool_aged_but_attending_school_7a")

df_logic_c_noschool_aged_but_attending_school_7b <- df_raw_data |>
  filter(num_children_school_aged == 0, !school_aged_children_attending_school %in% c("there_are_no_school_aged_children"))  |>
  mutate(i.check.type = "change_response",
         i.check.name = "reason_child_not_attending_school ",
         i.check.current_value = reason_child_not_attending_school,
         i.check.value = "NA",
         i.check.issue_id = "logic_c_noschool_aged_but_attending_school_7b",
         i.check.issue = glue("num_children_school_aged : {num_children_school_aged}, reason_child_not_attending_school : {reason_child_not_attending_school}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |>
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_noschool_aged_but_attending_school_7b")


