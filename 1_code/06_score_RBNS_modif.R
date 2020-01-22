#################################################################################################################################
### But: Obtenir la distribution prédictive des RBNS avec le modèle d'Antonio et Plat modifié                                 ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: train.RDS                                                                                                          ###
### Output: RBNS_sim_modif.RDS                                                                                                ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer le jeu d'entrainement et la liste de paramètres estimés --------------------------------------------------------------
train <- readRDS(here("2_pipeline", "01_data_wrangling", "train.RDS"))
param <- readRDS(here("2_pipeline", "03_entrainement", "param.RDS"))


# Créer une base de données contenant seulement les réclamations RBNS (enlever les fermées à tau) -------------------------------
train_RBNS <- train %>% 
  filter(settlement_time >= tau) %>%
  group_by(ClNr) %>% 
  slice(n()) %>% 
  ungroup()


# Fonction qui simule le montant de RBNS ----------------------------------------------------------------------------------------
simulate_RBNS_modif <- function(nsim) {
  f <- function(seed) {
    set.seed(seed)

    events <- train_RBNS %>% 
      mutate(events = map(dev_time_at_valuation, simulate_claim_path_modif)) %>% 
      mutate(event_times = map(events, "times")) %>% 
      mutate(event_types = map(events, "types")) %>% 
      select(-events) %>% 
      unnest() %>% 
      filter(event_types != 0) %>% 
      filter(event_times <= 11)
    
    RBNS <- events %>% 
      filter(event_types %in% 2:3) %>% 
      mutate(dev_year = event_times) %>% 
      select(accident_time, age, inj_part, RepDel, dev_year) %>% 
      mutate(mu = predict(param[["log_payment_fit"]], newdata = .)) %>% 
      mutate(log_payment = rnorm(nrow(.), mean = mu, sd = sigma(param[["log_payment_fit"]]))) %>% 
      mutate(payment = exp(log_payment)) %>% 
      pull(payment) %>% 
      sum()
    
    res <- tibble(
      nb_events_tot = nrow(events),
      nb_events_T1 = sum(events$event_types == 1),
      nb_events_T2 = sum(events$event_types == 2),
      nb_events_T3 = sum(events$event_types == 3),
      RBNS = RBNS
    )
    
    return(res)
  }
  
  return(map_df(1:nsim, f))  
}


# Simuler la distribution prédictive de la réserve RBNS et exporter -------------------------------------------------------------
RBNS_sim_modif <- simulate_RBNS_modif(5000)
saveRDS(RBNS_sim_modif, file = here("2_pipeline", "06_score_RBNS_modif", "RBNS_sim_modif.RDS"))
