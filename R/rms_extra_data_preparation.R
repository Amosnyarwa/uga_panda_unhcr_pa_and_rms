# ## devtools::install_github("dickoa/robotoolbox") 
# library(haven) 
# library(tidyverse) 
# library(srvyr) 
# library(robotoolbox) 
library(labelled)
# library(remotes) 
# library(dm) # install.packages("dm")
# library(janitor)

df_rms_clean_data <- readxl::read_excel("outputs/clean_data_unhcr_rms.xlsx", sheet = "RMS Uganda 2022 UNHCR_cleaned")
df_rms_roster <- readxl::read_excel("outputs/clean_data_unhcr_rms.xlsx", sheet = "hh_roster_cleaned")

s1.oldestHHM <- df_rms_roster |> 
  group_by(`_parent_index`) |> 
  slice_max(HH07, with_ties = FALSE) |> 
  ungroup() |> 
  mutate(oldestHHM = 1) |> 
  select(`_index`, oldestHHM)

s1.oldestHead <- df_rms_roster |> # find oldest head of household per HH 
  filter(HH03 == "1") |> 
  group_by(`_parent_index`) |> 
  slice_max(HH07, with_ties = FALSE) |> 
  ungroup() |> 
  mutate(oldestHead = 1) |> 
  select(`_index`, oldestHead)

df_rms_roster_updated <- df_rms_roster |> 
  left_join(s1.oldestHHM) |> 
  left_join(s1.oldestHead) |> 
  group_by(`_parent_index`) |> 
  mutate( householdHead = case_when( sum(HH03 == "1") == 1 & HH03 == "1" ~ 1, 
                                     sum(HH03 == "1") == 1 & HH03 != "1" ~ 0, 
                                     sum(HH03 == "1") == 0 & oldestHHM == 1 ~ 1, 
                                     sum(HH03 == "1") == 0 & is.na(oldestHHM) ~ 0,
                                     sum(HH03 == "1") > 1 & HH03 == "1" & oldestHead == 1 ~ 1, 
                                     sum(HH03 == "1") > 1 & HH03 == "1" & is.na(oldestHead) ~ 0, 
                                     sum(HH03 == "1") > 1 & HH03 != "1" ~ 0 )) |> 
  mutate( householdHead = labelled(householdHead, 
                                   labels = c( "Head of household" = 1, "Not head of household" = 0 ), 
                                   label = "Head of household")) |> 
  ungroup()


# 3. The household size equals to the number of individuals in the --------

###Check HH size
df_rms_clean_data |> 
  count(HH01) 

df_rms_clean_data |> 
  summarise(avg_hh_size = mean(HH01, na.rm = TRUE), 
            med_hh_size = median(HH01, na.rm = TRUE)) 

##Check HH size in individual dataset 
df_rms_roster |> add_count(`_parent_index`, name = "hhsize") |> 
  select(`_index`, `_parent_index`, hhsize) 

### adding it back, by zooming in the S1 table 
raw <- raw |> 
  dm_zoom_to(S1) |> 
  count(`_parent_index`, name = "hhsize") |> 
  dm_insert_zoomed("S1_hhsize") 

## Merge count to the main table 
df <- df |> 
  dm_zoom_to(main) |> 
  left_join(S1_hhsize) |> 
  dm_update_zoomed()


# Adding extra variables --------------------------------------------------

###Create function that turn character values into numeric 
labelled_chr2dbl <- function(x) { 
  varlab <- var_label(x) 
  vallab <- val_labels(x) 
  vallab <- setNames(as.numeric(vallab), names(vallab)) 
  x <- as.numeric(as.character(x)) 
  var_label(x) <- varlab 
  val_labels(x) <- vallab 
  x 
  }


# main --------------------------------------------------------------------

df_rms_clean_data_composites <- df_rms_clean_data |> 
  mutate( # primary citizenship from REF01 and REF02 
    citizenship = REF02 ) |> 
  mutate(citizenship = labelled(citizenship, labels = val_labels(df_rms_clean_data$REF02), label = var_label(df_rms_clean_data$REF02))) |> 
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
  mutate(disab= case_when(DISABILITY1==1 | DISABILITY2==1 | DISABILITY3==1 | DISABILITY4==1 ~ 1, DISABILITY1==0 | DISABILITY2==0 | DISABILITY3==0 | DISABILITY4==0 ~ 0, TRUE ~ NA_real_) ) |> 
  mutate(disab = labelled(disab, labels = c( "Without disability" = 0, "With disability" = 1) ))


# roster ------------------------------------------------------------------

df_rms_roster_up_age <- df_rms_roster |> 
  mutate(HH07_cat = cut(as.numeric(HH07), breaks = c(-1, 4, 17, 59, Inf), labels = c("0-4", "5-17", "18-59", "60+"))) |> 
  mutate(HH07_cat2 = cut(as.numeric(HH07), breaks = c(-1, 17, Inf), labels = c("0-17", "18-60+")))

