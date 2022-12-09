
# creating composite indicators -------------------------------------------

create_composite_indicators_pa <- function(input_df) {
  input_df %>% 
    mutate(
      i.settlement = ifelse(settlement == "any_adjumani_settlements", "adjumani", settlement),
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
      int.date_arrival_interval = interval(as_date(date_arrival), today),
      int.length_since_date_arrival = time_length(int.date_arrival_interval, "year"),
      i.date_arrival = case_when(int.length_since_date_arrival <= 02.5 ~ "last_3_months",
                                 int.length_since_date_arrival <= 0.5 ~ "3_and_6_month_ago",
                                 int.length_since_date_arrival <= 1 ~ "6_month_1_yr_ago",
                                 int.length_since_date_arrival <= 5 ~ "1_and_5_yrs_ago",
                                 int.length_since_date_arrival <= 10 ~ "5_and_10_yrs_ago",
                                 int.length_since_date_arrival > 10 ~ "greater_10_yrs_ago",
                                 TRUE ~ "NA"
                                 )
    ) |> 
    select(-c(starts_with("int.")))
}


# refugee weights ---------------------------------------------------------

make_refugee_weight_table<- function(input_df_ref, input_refugee_pop){
  refugee_pop <- input_refugee_pop %>% 
    mutate(strata= paste0(strata, "_refugee"),
           pop_status= "refugee")
  
  refugee_weight_table <- input_df_ref %>% 
    group_by(strata) %>% 
    summarise(
      sample_strata=  n()
    ) %>% 
    mutate(
      sample_global= sum(sample_strata)) %>% 
    left_join(refugee_pop) %>% 
    rename(pop_strata= "total_pop") %>% 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}