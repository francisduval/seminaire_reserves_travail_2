#################################################################################################################################
### But: Mettre la base de données en format longitudinal et séparer en ensemble d'entrainement et de validation              ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: claim_data.RDS                                                                                                     ###
### Output: train.RDS, valid_RBNS.RDS, valid_IBNR.RDS                                                                         ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer données --------------------------------------------------------------------------------------------------------------
claim_data <- readRDS(here("0_data", "claim_data.RDS"))


# Date d'évaluation choisie (en nombre d'années depuis le 1er janvier 1994) -----------------------------------------------------
tau <- 12


# Arranger les données en format longitudinal -----------------------------------------------------------------------------------
payment_long_data <- claim_data %>% 
  select(ClNr, starts_with("Pay")) %>% 
  gather("dev_year", "payment", -ClNr) %>% 
  arrange(ClNr) %>% 
  mutate(dev_year = as.numeric(str_sub(dev_year, -2, -1)))

long_data <- claim_data %>%
  select(-starts_with("Pay")) %>% 
  gather("dev_year", "open", starts_with("Open")) %>% 
  arrange(ClNr) %>% 
  mutate(dev_year = as.numeric(str_sub(dev_year, -2, -1))) %>% 
  left_join(payment_long_data, by = c("ClNr", "dev_year"))


# Créer des variables utiles ----------------------------------------------------------------------------------------------------
long_data %<>% 
  mutate(
    accident_time = AY + 0.25 * (AQ - 1) - 1994,
    reporting_time = accident_time + RepDel,
    calendar_time = accident_time + dev_year,
    dev_time_at_valuation = tau - accident_time,
    log_payment = case_when(
      payment == 0 ~ NA_real_, 
      TRUE ~ log(payment)
    )
  ) %>% 
  group_by(ClNr) %>% 
  mutate(
    settlement_index = min(which(open == 0 | row_number() == n())),
    settlement_time = nth(calendar_time, settlement_index[1])
  )


# Enlever les lignes après la fermeture de chaque réclamation -------------------------------------------------------------------
long_data %<>%
  group_by(ClNr) %>%
  filter(row_number() <= settlement_index) %>% 
  ungroup()


# Créer la variable qui indique le type d'événement, (NA si aucun événement) ----------------------------------------------------
long_data %<>%
  mutate(
    event_type = case_when(
      open == 0 & payment == 0 ~ 1,
      open == 0 & payment != 0 ~ 2,
      open == 1 & payment != 0 ~ 3,
      TRUE ~ NA_real_
    )
  )


# Séparation en base d'entrainement et de validation (date d'évaluation = 1er janvier 2006) -------------------------------------
train <- long_data %>% 
  filter(reporting_time < tau) %>% 
  filter(calendar_time < tau)

valid_RBNS <- long_data %>% 
  filter(reporting_time < tau) %>% 
  filter(calendar_time >= tau)

valid_IBNR <- long_data %>% 
  filter(accident_time < tau) %>% 
  filter(reporting_time >= tau) %>% 
  filter(calendar_time >= tau)


# Exporter les bases d'entrainement et de validation ----------------------------------------------------------------------------
saveRDS(train, file = here("2_pipeline", "01_data_wrangling", "train.RDS"))
saveRDS(valid_RBNS, file = here("2_pipeline", "01_data_wrangling", "valid_RBNS.RDS"))
saveRDS(valid_IBNR, file = here("2_pipeline", "01_data_wrangling", "valid_IBNR.RDS"))
