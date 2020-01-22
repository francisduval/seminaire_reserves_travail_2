#################################################################################################################################
### But: Visualiser les distributions prédictives du modèle individuel modiifé, pour les RBNS et IBNR séparément              ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: valid_RBNS.RDS, valid_IBNR.RDS et résultats des simulations                                                        ###
### Output: Plusieurs figures dans le répertoire output -> figures                                                            ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer les bases de données validation --------------------------------------------------------------------------------------
valid_RBNS <- readRDS(here("2_pipeline", "01_data_wrangling", "valid_RBNS.RDS"))
valid_IBNR <- readRDS(here("2_pipeline", "01_data_wrangling", "valid_IBNR.RDS"))


# Importer les résultats de simulation ------------------------------------------------------------------------------------------
RBNS_sim <- readRDS(here("2_pipeline", "06_score_RBNS_modif", "RBNS_sim_modif.RDS"))
IBNR_sim <- readRDS(here("2_pipeline", "07_score_IBNR_modif", "IBNR_sim_modif.RDS"))


# Calcul des quantités observées ------------------------------------------------------------------------------------------------
RBNS_obs <- sum(valid_RBNS$payment)
IBNR_obs <- sum(valid_IBNR$payment)
nb_IBNR_claims_obs <- length(unique(valid_IBNR$ClNr))

nb_events_T1_RBNS_obs <- sum(valid_RBNS$event_type == 1, na.rm = T)
nb_events_T2_RBNS_obs <- sum(valid_RBNS$event_type == 2, na.rm = T)
nb_events_T3_RBNS_obs <- sum(valid_RBNS$event_type == 3, na.rm = T)
nb_events_tot_RBNS_obs <- sum(valid_RBNS$event_type %in% 1:3, na.rm = T)

nb_events_T1_IBNR_obs <- sum(valid_IBNR$event_type == 1, na.rm = T)
nb_events_T2_IBNR_obs <- sum(valid_IBNR$event_type == 2, na.rm = T)
nb_events_T3_IBNR_obs <- sum(valid_IBNR$event_type == 3, na.rm = T)
nb_events_T2_T3_IBNR_obs <- sum(valid_IBNR$event_type %in% 2:3, na.rm = T)
nb_events_tot_IBNR_obs <- sum(valid_IBNR$event_type %in% 1:3, na.rm = T)

mean_pmt_RBNS_obs <- valid_RBNS %>% filter(event_type %in% 2:3) %>% pull(payment) %>% mean()
mean_pmt_IBNR_obs <- valid_IBNR %>% filter(event_type %in% 2:3) %>% pull(payment) %>% mean()


# Calcul des distributions prédictives ------------------------------------------------------------------------------------------
RBNS_pd <- RBNS_sim$RBNS
IBNR_pd <- IBNR_sim$IBNR
nb_IBNR_claims_pd <- IBNR_sim$nb_claims

nb_events_T1_RBNS_pd <- RBNS_sim$nb_events_T1
nb_events_T2_RBNS_pd <- RBNS_sim$nb_events_T2
nb_events_T3_RBNS_pd <- RBNS_sim$nb_events_T3
nb_events_tot_RBNS_pd <- RBNS_sim$nb_events_tot

nb_events_T1_IBNR_pd <- IBNR_sim$nb_events_T1
nb_events_T2_IBNR_pd <- IBNR_sim$nb_events_T2
nb_events_T3_IBNR_pd <- IBNR_sim$nb_events_T3
nb_events_T2_T3_IBNR_pd <- IBNR_sim$nb_events_T2 + IBNR_sim$nb_events_T3
nb_events_tot_IBNR_pd <- IBNR_sim$nb_events_tot

mean_pmt_RBNS_pd <- RBNS_sim$RBNS / (RBNS_sim$nb_events_T2 + RBNS_sim$nb_events_T3)
mean_pmt_IBNR_pd <- IBNR_sim$IBNR / (IBNR_sim$nb_events_T2 + IBNR_sim$nb_events_T3)


# Fonction pour tracer les distributions prédictives ----------------------------------------------------------------------------
plot_pred_dist <- function(sim_vec, obs, title = NULL, sub = NULL, xlab, bw = 100000, 
                           lims = c(min(obs - 10, sim_vec), max(obs + 10, sim_vec))) {
  df <- enframe(sim_vec)
  
  ggplot(df, aes(x = value)) + 
    geom_histogram(binwidth = bw, col = "black", fill = "white") +
    geom_vline(xintercept = obs, color = "#00743F", size = 0.8) +
    xlab(xlab) +
    labs(title = title, subtitle = sub) +
    ylab("Fréquence") +
    scale_x_continuous(labels = comma, limits = lims) +
    theme_bw()
}


# Résultats RBNS ----------------------------------------------------------------------------------------------------------------
p1 <- plot_pred_dist(RBNS_pd, obs = RBNS_obs, sub = "Réserve RBNS", xlab = "Réserve", bw = 200000)
p2 <- plot_pred_dist(mean_pmt_RBNS_pd, obs = mean_pmt_RBNS_obs, sub = "Paiement moyen RBNS", xlab = "Montant moyen", bw = 50)
p3 <- plot_pred_dist(nb_events_tot_RBNS_pd, obs = nb_events_tot_RBNS_obs, sub = "Événements RBNS (type 1, 2, et 3)", xlab = "Nombre", bw = 10)
p4 <- plot_pred_dist(nb_events_T1_RBNS_pd, obs = nb_events_T1_RBNS_obs, sub = "Événements RBNS (type 1)", xlab = "Nombre", bw = 10)
p5 <- plot_pred_dist(nb_events_T2_RBNS_pd, obs = nb_events_T2_RBNS_obs, sub = "Événements RBNS (type 2)", xlab = "Nombre", bw = 7)
p6 <- plot_pred_dist(nb_events_T3_RBNS_pd, obs = nb_events_T3_RBNS_obs, sub = "Événements RBNS (type 3)", xlab = "Nombre", bw = 10)

pdf(file = here("3_output", "results_RBNS_modif.pdf"), width = 12, height = 5)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)
dev.off()


# Résultats IBNR ----------------------------------------------------------------------------------------------------------------
p7 <- plot_pred_dist(IBNR_pd, obs = IBNR_obs, sub = "Réserve IBNR", xlab = "Réserve", bw = 50000)
p8 <- plot_pred_dist(nb_IBNR_claims_pd, obs = nb_IBNR_claims_obs, sub = "Réclamations IBNR", xlab = "Nombre", bw = 6)
p9 <- plot_pred_dist(mean_pmt_IBNR_pd, obs = mean_pmt_IBNR_obs, sub = "Paiement moyen IBNR", xlab = "Montant moyen", bw = 120)
p10 <- plot_pred_dist(nb_events_tot_IBNR_pd, obs = nb_events_tot_IBNR_obs, sub = "Événements IBNR (type 1, 2, et 3)", xlab = "Nombre", bw = 8)
p11 <- plot_pred_dist(nb_events_T1_IBNR_pd, obs = nb_events_T1_IBNR_obs, sub = "Événements IBNR (type 1)", xlab = "Nombre", bw = 7)
p12 <- plot_pred_dist(nb_events_T2_T3_IBNR_pd, obs = nb_events_T2_T3_IBNR_obs, sub = "Événements IBNR (type 2 et 3)", xlab = "Nombre", bw = 4)

pdf(file = here("3_output", "results_IBNR_modif.pdf"), width = 12, height = 5)
grid.arrange(p7, p8, p9, p10, p11, p12, ncol = 3, nrow = 2)
dev.off()
