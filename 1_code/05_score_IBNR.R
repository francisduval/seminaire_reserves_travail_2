#################################################################################################################################
### But: Obtenir la distribution prédictive des IBNR avec le modèle d'Antonio et Plat                                         ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: train.RDS                                                                                                          ###
### Output: IBNR_sim.RDS                                                                                                      ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer le jeu d'entrainement et la liste de paramètres estimés --------------------------------------------------------------
train <- readRDS(here("2_pipeline", "01_data_wrangling", "train.RDS"))
param <- readRDS(here("2_pipeline", "03_entrainement", "param.RDS"))


# Fonction qui simule le montant de IBNR. Retourne un data frame dont chaque ligne est une simulation ---------------------------
simulate_IBNR <- function(nsim) {
  f <- function(seed) {
    set.seed(seed)

    claims <- tibble(
      accident_time = seq(0, tau - 0.25, by = 0.25), 
      nb_claims = rpois(length(param[["intensite"]]), lambda = param[["intensite"]])
    )
    
    events <- claims %>% 
      uncount(nb_claims) %>% 
      mutate(RepDel = sim_delay(accident_time)) %>% 
      mutate(reporting_time = accident_time + RepDel) %>% 
      mutate(age = sim_age(nrow(.))) %>% 
      mutate(inj_part = sim_inj_part(nrow(.))) %>% 
      mutate(events = map(RepDel, simulate_claim_path)) %>% 
      mutate(event_times = map(events, "times")) %>% 
      mutate(event_types = map(events, "types")) %>% 
      select(-events) %>% 
      unnest() %>%        
      filter(event_types != 0) %>% 
      filter(event_times <= 11)
    
    IBNR <- events %>% 
      filter(event_types %in% 2:3) %>% 
      mutate(dev_year = event_times) %>% 
      select(accident_time, age, inj_part, RepDel, dev_year) %>% 
      mutate(mu = predict(param[["log_payment_fit"]], newdata = .)) %>% 
      mutate(log_payment = rnorm(nrow(.), mean = mu, sd = sigma(param[["log_payment_fit"]]))) %>% 
      mutate(payment = exp(log_payment)) %>% 
      pull(payment) %>% 
      sum()
    
    res <- tibble(
      nb_claims = sum(claims$nb_claims),
      nb_events_tot = nrow(events),
      nb_events_T1 = sum(events$event_types == 1),
      nb_events_T2 = sum(events$event_types == 2),
      nb_events_T3 = sum(events$event_types == 3),
      IBNR = IBNR
    )
    
    return(res)
  }
  
  return(map_df(1:nsim, f)) 
}


# Simuler la distribution prédictive de la réserve IBNR et exporter -------------------------------------------------------------
IBNR_sim <- simulate_IBNR(5000)
saveRDS(IBNR_sim, file = here("2_pipeline", "05_score_IBNR", "IBNR_sim.RDS"))
