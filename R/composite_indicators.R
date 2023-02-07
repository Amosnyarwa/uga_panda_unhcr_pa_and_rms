
# creating composite indicators -------------------------------------------

create_composite_indicators_pa <- function(input_df) {
  input_df %>% 
    mutate(
      i.settlement = ifelse(settlement_confirm == "any_adjumani_settlements", "adjumani", settlement_confirm),
      i.region = case_when(i.settlement %in% c("bidibidi","imvepi","kiryandongo","lobule","palabek",
                                               "palorinya","rhino_camp","adjumani") & status =="refugee" ~ "west_nile",
                           i.settlement %in% c("kyaka_ii","kyangwali","nakivale","oruchinga","rwamwanja") & status =="refugee" ~ "south_west",
                           TRUE ~ i.settlement),
      i.respondent_age = case_when(respondent_age < 18 ~ "age_12_17",
                                   respondent_age <= 59 ~ "age_18_59",
                                   respondent_age > 59 ~ "age_greater_59",
                                   TRUE ~ "NA"),
      int.disability = paste(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate,vulnerability_self_care, vulnerability_communicate),
      i.disability = ifelse(str_detect(string = int.disability, pattern = "a_lot_of_difficulty|cannot_do_at_all"), "yes_disability", "no_disability"),
      i.respondent_education = case_when(respondent_education %in% c("attending_primary", "completed_primary", "attending_secondary") ~ "low",
                                         respondent_education %in% c("completed_secondary", "attending_tertiary") ~ "middle",
                                         respondent_education %in% c("ccompleted_tertiary") ~ "higher",
                                         respondent_education %in% c("dk") ~ "dk",
                                    TRUE ~ respondent_education
                                    ),
      int.year_arrival_interval = interval(as_date(paste0(year_arrival, "0101")), today),
      int.length_since_year_arrival = time_length(int.year_arrival_interval, "year"),
      i.year_arrival = case_when(int.length_since_year_arrival <= 1 ~ "within_1_yr_ago",
                                 int.length_since_year_arrival <= 5 ~ "1_and_5_yrs_ago",
                                 int.length_since_year_arrival <= 10 ~ "5_and_10_yrs_ago",
                                 int.length_since_year_arrival > 10 ~ "greater_10_yrs_ago",
                                 TRUE ~ "NA"
                                 )
    ) |> 
    select(-c(starts_with("int.")))
}

create_composite_indicators_rms <- function(input_df) {
  input_df %>% 
    mutate(
      i.settlement_confirm = ifelse(settlement_confirm == "any_adjumani_settlements", "adjumani", settlement_confirm),
      i.respondent_age = case_when(respondent_age < 18 ~ "age_12_17",
                                   respondent_age <= 59 ~ "age_18_59",
                                   respondent_age > 59 ~ "age_greater_59",
                                   TRUE ~ "NA"),
      i.UNI01 = case_when(UNI01 > 0 ~ "Yes",
                          TRUE ~ "No"),
      int.REF12a = case_when(str_detect(string = REF12a, pattern = "-") & !str_detect(string = REF12a, pattern = "9998|9999") ~ str_extract(string = my(REF12a), pattern = "^[0-9]{4}"), 
                             str_detect(string = REF12a, pattern = "/") & !str_detect(string = REF12a, pattern = "9998|9999") ~ str_extract(string = REF12a, pattern = "[0-9]{4}$")
      ),
      int.REF12b = case_when(str_detect(string = REF12b, pattern = "-") & !str_detect(string = REF12b, pattern = "9998|9999") ~ str_extract(string = my(REF12b), pattern = "^[0-9]{4}"), 
                             str_detect(string = REF12b, pattern = "/") & !str_detect(string = REF12b, pattern = "9998|9999") ~ str_extract(string = REF12b, pattern = "[0-9]{4}$")
      ),
      int.REF12 = ifelse(is.na(int.REF12a) & !is.na(int.REF12b), int.REF12b, int.REF12a),
      int.year_arrival_interval = interval(as_date(paste0(int.REF12, "0101")), as_date(today)),
      int.length_since_year_arrival = time_length(int.year_arrival_interval, "year"),
      i.REF12 = case_when(int.length_since_year_arrival <= 1 ~ "within_1_yr_ago",
                          int.length_since_year_arrival <= 5 ~ "1_and_5_yrs_ago",
                          int.length_since_year_arrival <= 10 ~ "5_and_10_yrs_ago",
                          int.length_since_year_arrival > 10 ~ "greater_10_yrs_ago"),
      i.DWA03 = case_when(DWA03a == 1 ~ DWA03b,
                          DWA03a == 2 ~ DWA03b*60)
    ) |> 
    select(-c(starts_with("int.")))
}