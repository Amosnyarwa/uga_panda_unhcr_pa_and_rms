library(tidyverse)
library(srvyr)
library(labelled)

source("R/composite_indicators.R")

# clean data with weights
data_path <- "inputs/clean_data_unhcr_rms.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "RMS Uganda 2022 UNHCR_cleaned"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "RMS Uganda 2022 UNHCR_cleaned", col_types = c_types, na = "NA")
df_roster_clean_data <- readxl::read_excel(path = data_path, sheet = "hh_roster", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/RMS_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English (en)`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_uga_rms.csv")


# add composite indicators ------------------------------------------------

#Create function that turn character values into numeric 
labelled_chr2dbl <- function(x) { 
  varlab <- var_label(x) 
  vallab <- val_labels(x) 
  vallab <- setNames(as.numeric(vallab), names(vallab)) 
  x <- as.numeric(as.character(x)) 
  var_label(x) <- varlab 
  val_labels(x) <- vallab 
  x 
}

# main

df_rms_clean_data_composites <- create_composite_indicators_rms(df_main_clean_data) |> 
  mutate( # primary citizenship from REF01 and REF02     
    citizenship = REF02 
  ) |> 
  mutate(citizenship = labelled(citizenship, labels = val_labels(df_main_clean_data$REF02), label = var_label(df_main_clean_data$REF02))) |> 
  mutate( 
    # disability identifier variables according to Washington Group standards 
    disaux1_234 = DIS01 %in% c("2","3","4"), # indicator variables for all 6 domains with value TRUE if SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disaux2_234 = DIS02 %in% c("2","3","4"), 
    disaux3_234 = DIS03 %in% c("2","3","4"), 
    disaux4_234 = DIS04 %in% c("2","3","4"), 
    disaux5_234 = DIS05 %in% c("2","3","4"), 
    disaux6_234 = DIS06 %in% c("2","3","4"), 
    disaux1_34 = DIS01 %in% c("3","4"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disaux2_34 = DIS02 %in% c("3","4"), 
    disaux3_34 = DIS03 %in% c("3","4"), 
    disaux4_34 = DIS04 %in% c("3","4"), 
    disaux5_34 = DIS05 %in% c("3","4"), 
    disaux6_34 = DIS06 %in% c("3","4") ) |> 
  mutate( disSum234 = sum(c_across(disaux1_234:disaux6_234)), # count number of TRUE indicator variables over 6 domains 
          disSum34 = sum(c_across(disaux1_34:disaux6_34)) # count number of TRUE indicator variables over 6 domains 
  ) |>
  mutate( DISABILITY1 = case_when( # : the level of inclusion is at least one domain/question is coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL. 
    disSum234 >= 1 ~ 1, 
    disSum234 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY2 = case_when( # : the level of inclusion is at least two domains/questions are coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL or any 1 domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL 
    disSum234 >= 2 | disSum34 >=1 ~ 1, 
    disSum234 < 2 & disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 
    disSum34 >= 1 ~ 1, disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY4 = case_when( # : the level of inclusion is at least one domain/question is coded CANNOT DO AT ALL. 
    DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4" ~ 1, 
    !(DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4") & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0, 
    DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98 ) ) |> 
  mutate( DISABILITY1 = labelled(DISABILITY1, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 1"), 
          DISABILITY2 = labelled(DISABILITY2, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 2"), 
          DISABILITY3 = labelled(DISABILITY3, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 3"), 
          DISABILITY4 = labelled(DISABILITY4, labels = c( "Without disability" = 0, "With disability" = 1, "Unknown" = 98 ), label = "Washington Group disability identifier 4")) |> 
  ###Calculate having at least one disability identifier among 4 categories 
  mutate(disab = case_when(DISABILITY1==1 | DISABILITY2==1 | DISABILITY3==1 | DISABILITY4==1 ~ 1, DISABILITY1==0 | DISABILITY2==0 | DISABILITY3==0 | DISABILITY4==0 ~ 0, TRUE ~ NA_real_) ) |> 
  mutate(disab = labelled(disab, labels = c( "Without disability" = 0, "With disability" = 1) )) |> 
  mutate(citizenship_com   = citizenship) # adding this that is mentioned in aggregation

# add extra indicators accordining to the guide
df_rms_main_composites_extra <- df_rms_clean_data_composites |> 
  mutate(HEA01_num = labelled_chr2dbl(HEA01),
         health_acc = case_when(HEA01_num!=98 & HEA03 <= 60 ~ 1,
                                TRUE ~ 0),
         health_acc = labelled(health_acc,
                               labels = c("Health facility is available" = 1,
                                          "Health facilitiy is not available" = 0),
                               label = "Access to health facilities"),
         LIGHT01_num = labelled_chr2dbl(LIGHT01),
         LIGHT02_num = labelled_chr2dbl(LIGHT02),
         LIGHT03_num = labelled_chr2dbl(LIGHT03),
         electricity = case_when(LIGHT01_num ==1 & (LIGHT02_num==1 |LIGHT02_num==3 | 
                                                      LIGHT02_num==5 | LIGHT02_num==6 
                                                    | LIGHT02_num==7 | LIGHT02_num==8) & (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                                 TRUE ~ 0),
         electricity = labelled(electricity, labels = c("Yes" = 1, "No" = 0),
                                label = "Access to electricity"),
         DWA03a_num = labelled_chr2dbl(DWA03a),
         DWA02_num = labelled_chr2dbl(DWA02),
         DWA01_num = labelled_chr2dbl(DWA01),
         DWA04_num = labelled_chr2dbl(DWA04),
         time_DWA = case_when( DWA03a_num==1 ~ 1, DWA03a_num==2 ~ 60),
         time_tot = time_DWA*DWA03b,
         dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1), ##Accessible under 30 minutes
         dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 | DWA01_num != 96 |DWA01_num !=98 ~ 1,
                             TRUE ~ 0), ## protected source
         dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1), ## It was available in the last 30 days
         drinkingwater=case_when((dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0),
         drinkingwater = labelled(drinkingwater,  labels = c("Yes" = 1, "No" = 0),
                                  label = "Access to drinking water"),
         DWE01_num = labelled_chr2dbl(DWE01),
         DWE02_num = labelled_chr2dbl(DWE02),
         DWE03_num = labelled_chr2dbl(DWE03),
         DWE04_num = labelled_chr2dbl(DWE04),
         dwe01_cat=case_when( #Only apartment and house
           (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 ),
         dwe02_cat=case_when( #unimproved floor when earth,sand,clay,mud, dung or other
           (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 ),
         dwe03_cat=case_when( #unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
           (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
              DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0),
         dwe04_cat=case_when( #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
           (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13| DWE04_num==14| DWE04_num==15) ~ 1,
           TRUE ~ 0),
         crowding = DWE05/HH01,
         dwe05_cat = case_when( ##if crowding < 3 
           crowding < 3 ~ 1, TRUE ~ 0),
         DWE08_num = labelled_chr2dbl(DWE08),
         DWE09_num = labelled_chr2dbl(DWE09),
         dwe09_cat = case_when( #affordable if HH pays rent and often and always without financial distress
           (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1, 
           (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,  DWE08_num==0 ~ NA_real_),
         shelter=case_when(
           dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0, 
           dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1),
         shelter = labelled(shelter, labels = c("Yes" = 1, "No" = 0),
                            label = "Habitable and affordable shelter"),
         impact2_2=case_when(
           shelter==0 | electricity==0 | drinkingwater==0 | health_acc==0 ~ 0,
           shelter==1 & electricity==1 & drinkingwater==1 & health_acc==1 ~ 1),
         impact2_2=labelled(impact2_2, labels =c("Yes"=1, "No"=0),
                            label="PoCs residing in physically safe and secure settlements with access to basic facilities"),
         HACC01_num = labelled_chr2dbl(HACC01),
         HACC03_num = labelled_chr2dbl(HACC03),
         health_NOacc=case_when(
           HACC03_num==1 & (`HACC04/7`==1 | `HACC04/8`==1 | `HACC04/96`==1 ) ~ 0,
           HACC03_num==1 & (`HACC04/1`==1 | `HACC04/2`==1 | `HACC04/3`==1 |`HACC04/4`==1 |`HACC04/5`==1 |
                              `HACC04/6`==1 | `HACC04/9`==1 | `HACC04/10`==1) ~ 1, TRUE ~ 0),
         HACC_need=HACC01_num + health_NOacc,
         impact2_3=HACC01_num/HACC_need,
         impact2_3=labelled(impact2_3,
                            labels =c("Yes"=1, "No"=0),
                            label="PoC has access to health services in the last 30 days when needed"),
         impact2_3 = ifelse(is.nan(impact2_3), NA, impact2_3),
         SAF01_num = labelled_chr2dbl(SAF01),
         impact3_3=case_when(
           SAF01_num==1 | SAF01_num==2 ~ 1,
           SAF01_num==3 | SAF01_num==4 | SAF01_num==98 ~ 0, SAF01_num==99 ~ NA_real_),
         impact3_3=labelled(impact3_3, labels =c( "Yes"=1, "No"=0),
                            label="PoC feeling safe walking alone"),
         GBV01a_num = labelled_chr2dbl(GBV01a), # health services
         GBV01b_num = labelled_chr2dbl(GBV01b), # psycho-social services
         GBV01c_num = labelled_chr2dbl(GBV01c), # safety and security services
         GBV01d_num = labelled_chr2dbl(GBV01d), # legal assistance
         outcome4_1 = case_when(GBV01a_num==1 |  GBV01b_num==1 |  GBV01c_num==1 |  GBV01d_num==1 ~ 1,
                                TRUE ~ 0),
         outcome4_1=labelled(outcome4_1,
                             labels=c("Yes"=1, "No"=0),
                             label="Poc who know where to access available GBV services"
         ),
         DWE01_num = labelled_chr2dbl(DWE01),
         DWE02_num = labelled_chr2dbl(DWE02),
         DWE03_num = labelled_chr2dbl(DWE03),
         DWE04_num = labelled_chr2dbl(DWE04),
         dwe01_cat=case_when( #Only apartment and house
           (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 ),
         dwe02_cat=case_when( #unimproved floor when earth,sand,clay,mud, dung or other
           (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 ),
         dwe03_cat=case_when( #unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
           (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
              DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0),
         dwe04_cat=case_when( #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
           (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13| DWE04_num==14| DWE04_num==15) ~ 1,
           TRUE ~ 0),
         crowding=DWE05/HH01, ############ repeated
         dwe05_cat =case_when( ##if crowding < 3 
           crowding < 3 ~ 1, TRUE ~ 0), ############ repeated
         DWE08_num = labelled_chr2dbl(DWE08),
         DWE09_num = labelled_chr2dbl(DWE09),
         dwe09_cat=case_when( #affordable if HH pays rent and often and always without financial distress
           (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1, 
           (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,  DWE08_num==0 ~ NA_real_),
         outcome9_1=case_when(
           dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0, 
           dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1),
         outcome9_1 = labelled(outcome9_1, labels = c("Yes" = 1, "No" = 0),
                               label = "PoC living in habitable and affordable housing"),
         
         LIGHT01_num = labelled_chr2dbl(LIGHT01), ############ repeated
         LIGHT02_num = labelled_chr2dbl(LIGHT02), ############ repeated
         LIGHT03_num = labelled_chr2dbl(LIGHT03), ############ repeated
         outcome9_2=
           case_when(LIGHT01_num==1 & (LIGHT02_num==1 |LIGHT02_num==3 | LIGHT02_num==5 | LIGHT02_num==6 
                                       | LIGHT02_num==7 | LIGHT02_num==8) & 
                       (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                     TRUE ~ 0),
         outcome9_2 = labelled(outcome9_2, labels = c("Yes" = 1, "No" = 0),
                               label = "PoC that have energy to ensure lighting"),
         DWA03a_num = labelled_chr2dbl(DWA03a), ############ repeated
         DWA02_num = labelled_chr2dbl(DWA02), ############ repeated
         DWA01_num = labelled_chr2dbl(DWA01), ############ repeated
         DWA04_num = labelled_chr2dbl(DWA04), ############ repeated
         time_DWA=case_when(
           DWA03a_num==1~1, DWA03a_num==2~60), ############ repeated
         time_tot=time_DWA*DWA03b, ############ repeated
         dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1), ## accessible under 30 minutes
         dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 | DWA01_num != 96 |DWA01_num !=98 ~ 1,
                             TRUE ~ 0), ## protected source
         dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1), ## drinking water was available in the last 30 days
         outcome12_1=case_when(
           (dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0),
         outcome12_1 = labelled(outcome12_1, labels = c("Yes" = 1, "No" = 0),
                                label = "PoC using at least basic drinking water services"),
         INC01_num = labelled_chr2dbl(INC01),
         outcome13_2=case_when(INC01_num==1 ~ 1,
                               INC01_num==2 |INC01_num==3 |INC01_num==98 ~ 0 ),
         outcome13_2 = labelled(outcome13_2, labels = c("Yes" = 1, "No" = 0),
                                label = "PoC who self-report positive changes in their income compared
                                to previous year"),
         UNEM01_num = labelled_chr2dbl(UNEM01),
         UNEM02_num = labelled_chr2dbl(UNEM02),
         UNEM03_num = labelled_chr2dbl(UNEM03),
         UNEM04_num = labelled_chr2dbl(UNEM04),
         UNEM05_num = labelled_chr2dbl(UNEM05),
         UNEM06_num = labelled_chr2dbl(UNEM06),
         UNEM07_num = labelled_chr2dbl(UNEM07),
         UNEM08_num = labelled_chr2dbl(UNEM08),
         UNEM09_num = labelled_chr2dbl(UNEM09),
         UNEM10_num = labelled_chr2dbl(UNEM10),
         employed = case_when(UNEM01_num==1 ~ 1,
                              UNEM02_num==1 & UNEM07_num==3 ~ 1,
                              UNEM04_num==1 ~ 1,
                              UNEM02_num==1 & UNEM07_num==1 & (UNEM08_num==1 | UNEM08_num==2) ~ 1,
                              UNEM05_num==1 & UNEM06_num==3 ~ 1,
                              UNEM05_num==1 & (UNEM06_num==1 | UNEM06_num==2) & (UNEM08_num==1 | UNEM08_num==2) ~ 1),
         unemployed = case_when(employed==0 & UNEM09_num==1 & UNEM10_num==1 ~ 1,
                                TRUE ~ 0),
         labour_force = case_when(employed==1 | unemployed==1 ~ 1),
         outcome13_3 = unemployed/labour_force
  )

# roster

df_rms_roster_up_age <- df_rms_roster |> 
  mutate(HH07 = ifelse((is.na(HH07)|HH07 %in% c("NA")) & !(is.na(HH07_months)|HH07_months %in% c("NA")), ceiling(as.numeric(HH07_months)/12), HH07)) |> # update HH07 based on HH07_months
  mutate(HH07_cat = cut(as.numeric(HH07), breaks = c(-1, 4, 17, 59, Inf), labels = c("0-4", "5-17", "18-59", "60+"))) |> 
  mutate(HH07_cat2 = cut(as.numeric(HH07), breaks = c(-1, 17, Inf), labels = c("0-17", "18-60+")))



# Analysis ----------------------------------------------------------------

# set up design objects
ref_svy <- as_survey(.data = df_rms_main_composites_extra)

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
