# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# Read data and checking log

df_cleaning_log <- read_csv("inputs/combined_checks_RMS.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  filter(!adjust_log %in% c("delete_log")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         relevant = NA) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)


# handle data -------------------------------------------------------------

data_path <- "inputs/RMS_Uganda_2022_Data.xlsx"

# main data
cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = data_path, col_types = c_types) %>% 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# loops
# S1 loop
hh_roster <- readxl::read_excel(path = data_path, sheet = "S1") %>% 
  mutate(HH02 = openssl::md5(HH02)) 

df_raw_data_hh_roster <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(hh_roster, by = c("_uuid" = "_submission__uuid") )