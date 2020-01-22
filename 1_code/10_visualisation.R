#################################################################################################################################
### But: Visualiser les résultats de tous les modèles                                                                         ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: valid_RBNS.RDS, valid_IBNR.RDS et les résultats des simulations pour tous les modèles                              ###
### Output: Plusieurs figures dans le répertoire output -> figures                                                            ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer les bases de données validation --------------------------------------------------------------------------------------
valid_RBNS <- readRDS(here("2_pipeline", "01_data_wrangling", "valid_RBNS.RDS"))
valid_IBNR <- readRDS(here("2_pipeline", "01_data_wrangling", "valid_IBNR.RDS"))


# Importer les distributions prédictives ----------------------------------------------------------------------------------------
sim_boot_CL_odp <- readRDS(here("2_pipeline", "02_modeles_collectifs", "sim_boot_CL_odp.RDS"))
sim_GLM_odp <- readRDS(here("2_pipeline", "02_modeles_collectifs", "sim_GLM_odp.RDS"))
RBNS_sim <- readRDS(here("2_pipeline", "04_score_RBNS", "RBNS_sim.RDS"))
IBNR_sim <- readRDS(here("2_pipeline", "05_score_IBNR", "IBNR_sim.RDS"))
RBNS_sim_modif <- readRDS(here("2_pipeline", "06_score_RBNS_modif", "RBNS_sim_modif.RDS"))
IBNR_sim_modif <- readRDS(here("2_pipeline", "07_score_IBNR_modif", "IBNR_sim_modif.RDS"))


# Importer les paramètres estimés -----------------------------------------------------------------------------------------------
param <- readRDS(here("2_pipeline", "03_entrainement", "param.RDS"))


# Calcul la réserve totale observée ---------------------------------------------------------------------------------------------
total_obs <- sum(valid_RBNS$payment, valid_IBNR$payment)


# Sommer les distributions IBNR et RBNS pour les 2 modèles individuels pour obtenir la distribution totale ----------------------
total_sim <- RBNS_sim$RBNS + IBNR_sim$IBNR
total_sim_modif <- RBNS_sim_modif$RBNS + IBNR_sim_modif$IBNR


# Créer un data frame "tidy" pour stocker les distributions prédictives ---------------------------------------------------------
pred_dist_df1 <- tibble(
  CL = sim_boot_CL_odp,
  GLM = sim_GLM_odp,
  ind = total_sim,
  ind_mod = total_sim_modif
)

pred_dist_df1 %<>% gather()


# Data frame pour les quantiles des distributions prédictives -------------------------------------------------------------------
quantiles_df1 <- pred_dist_df1 %>%
  group_by(key) %>%
  summarize(
    q95 = quantile(value, probs = 0.95),
    q99 = quantile(value, probs = 0.99)
  )


# Graphique pour comparer les 4 modèles (2 collectifs, individuel et individuel modifié) ----------------------------------------
pdf(file = here("3_output", "comparaison_ind_coll.pdf"), width = 9) 
ggplot(pred_dist_df1, aes(x = value)) + 
  geom_histogram(binwidth = 100000, col = "black", fill = "white") +
  geom_vline(xintercept = total_obs, color = "#00743F", size = 0.8) +
  geom_vline(aes(xintercept = q95), data = quantiles_df1, col = "#F2A104", lty = "dashed", size = 0.7) +
  geom_vline(aes(xintercept = q99), data = quantiles_df1, col = "red", lty = "dashed", size = 0.7) +
  facet_grid(
    key ~., 
    labeller = labeller(
      key = c(
        CL = "Chain-ladder", 
        GLM = "GLM", 
        ind = "Individuel", 
        ind_mod = "Individuel modifié"
      )
    )
  ) +
  xlab("Réserve (RBNS + IBNR)") +
  ylab("Fréquence") +
  scale_x_continuous(labels = comma, limits = c(13946388, 23427562)) +
  theme_bw()
dev.off()


# Tableau des résultats ---------------------------------------------------------------------------------------------------------
list_dist <- list(sim_boot_CL_odp, sim_GLM_odp, total_sim, total_sim_modif)

table <- tibble(
  `Modèle` = c("Bootstrap chain-ladder", "GLM Poisson surdispersée", "Modèle individuel", "Modèle individuel modifié"),
  `Moyenne` = map_dbl(list_dist, mean),
  `Médiane` =  map_dbl(list_dist, median),
  `VaR_95` =  map_dbl(list_dist, quantile, probs = 0.95),
  `Var_99` =  map_dbl(list_dist, quantile, probs = 0.99)
)

kable(table, "latex", booktabs = T, digits = 0, format.args = list(decimal.mark = ",", big.mark = " ")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  row_spec(nrow(table), hline_after = TRUE)


# Graphiques des fonctions h (avec et sans splines) -----------------------------------------------------------------------------
data_h <- tibble(
  x = seq(0, 12, by = 0.01),
  h1 = param[["intensity_h1"]](x),
  h2 = param[["intensity_h2"]](x),
  h3 = param[["intensity_h3"]](x),
  h1_mod = param[["intensity_h1_modif"]](x),
  h2_mod = param[["intensity_h2_modif"]](x),
  h3_mod = param[["intensity_h3_modif"]](x)
) 

# ----------

pdf(file = here("3_output", "fonctions_intensite_evenements.pdf"), width = 8, height = 5) 
ggplot(data_h) +
  geom_line(aes(x = x, y = h1, col = "Type 1", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h2, col = "Type 2", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h3, col = "Type 3", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h1_mod, col = "Type 1", lty = "Spline cubique"), size = 0.5) +
  geom_line(aes(x = x, y = h2_mod, col = "Type 2", lty = "Spline cubique"), size = 0.5) +
  geom_line(aes(x = x, y = h3_mod, col = "Type 3", lty = "Spline cubique"), size = 0.5) +
  scale_color_manual(
    name = "Événements",
    breaks = c("Type 1", "Type 2", "Type 3"),
    values = c("Type 1" = "#1D65A6", "Type 2" = "#00743F", "Type 3" = "#F2A104")
  ) +
  scale_linetype_manual(
    name = "Spécification",
    breaks = c("Constante par parties", "Spline cubique"),
    values = c("Constante par parties" = "dashed", "Spline cubique" = "solid")
  ) +
  xlab("Année de développement") +
  ylab("Intensité") +
  ylim(c(0, 0.6)) +
  scale_x_continuous(breaks = 0:12) +
  theme_bw()
dev.off()

# ----------

data_h_tidy <- data_h %>% 
  select(x, h1, h2, h3) %>% 
  gather("key", "value", -x)

pdf(file = here("3_output", "fonctions_intensite_evenements_janvier_2020.pdf"), width = 8, height = 5) 
ggplot(data_h_tidy, mapping = aes(x = x, y = value, color = key)) +
  geom_line(size = 0.7) +
  ggtitle("Exemple d'estimation de l'intensité de survenance des événements") +
  scale_x_continuous(breaks = 0:12) +
  scale_color_manual(
    name = "Type d'événement",
    labels = c("Fermeture", "Fermeture + paiement", "Paiement"),
    values = c("#1D65A6", "#00743F", "#F2A104")
  ) +
  xlab("Année de développement") +
  ylab("Intensité du processus de Poisson") +
  theme_bw() +
  theme(
    legend.position = c(0.55, 0.7),
    panel.grid = element_blank()
  )
dev.off()

# ----------

pdf(file = here("3_output", "fonctions_intensite_evenements_sans_spline.pdf"), width = 8, height = 5) 
ggplot(data_h) +
  geom_line(aes(x = x, y = h1, col = "Type 1", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h2, col = "Type 2", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h3, col = "Type 3", lty = "Constante par parties"), size = 0.4) +
  geom_line(aes(x = x, y = h1_mod, col = "Type 1", lty = "Spline cubique"), size = 0.5, alpha = 0) +
  geom_line(aes(x = x, y = h2_mod, col = "Type 2", lty = "Spline cubique"), size = 0.5, alpha = 0) +
  geom_line(aes(x = x, y = h3_mod, col = "Type 3", lty = "Spline cubique"), size = 0.5, alpha = 0) +
  scale_color_manual(
    name = "Événements",
    breaks = c("Type 1", "Type 2", "Type 3"),
    values = c("Type 1" = "#1D65A6", "Type 2" = "#00743F", "Type 3" = "#F2A104")
  ) +
  scale_linetype_manual(
    name = "Spécification",
    breaks = c("Constante par parties", "Spline cubique"),
    values = c("Constante par parties" = "dashed", "Spline cubique" = "solid")
  ) +
  xlab("Année de développement") +
  ylab("Intensité") +
  ylim(c(0, 0.6)) +
  scale_x_continuous(breaks = 0:12) +
  theme_bw()
dev.off()
