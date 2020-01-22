#################################################################################################################################
### But: Ajustement des modèles collectifs                                                                                    ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: train.RDS                                                                                                          ###
### Output: Vecteur de la distribution prédictive des 2 modèles collectifs                                                    ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer le jeu d'entrainement et le jeu de validation ------------------------------------------------------------------------
train <- readRDS(here("2_pipeline", "01_data_wrangling", "train.RDS"))


# Nombre de simulations ---------------------------------------------------------------------------------------------------------
nsim <- 5000


# Création des 2 triangles de développement (cumulatif et incrémental) ----------------------------------------------------------
triangle_inc <- as.triangle(train, dev = "dev_year", origin = "AY", value = "payment")
triangle_cum <- incr2cum(triangle_inc)


# Bootstrap Chain-Ladder Poisson surdispersée -----------------------------------------------------------------------------------
boot_CL_odp <- BootChainLadder(triangle_cum, R = nsim, process.distr = "od.pois")


# GLM Poisson surdispersée ------------------------------------------------------------------------------------------------------
data_GLM <- as.LongTriangle(triangle_inc, na.rm = FALSE)
data_GLM_train <- na.omit(data_GLM)
data_GLM_score <- data_GLM %>% filter(is.na(value)) %>% select(-value)

GLM_odp <- glm(value ~ AY + dev_year, family = quasipoisson(link = "log"), data = data_GLM_train)
lambda_GLM_odp <- predict(GLM_odp, newdata = data_GLM_score, type = "response")
dispersion_GLM_odp <- summary(GLM_odp)$dispersion


# Fonction pour simuler une variable aléatoire Poisson surdispersée -------------------------------------------------------------
rqpois <-  function(n, lambda, phi) {
  b <- phi
  a <- lambda / phi
  r <- rgamma(n, shape = a, scale = b)
  return(r)
}


# Obtenir la distribution prédictive pour chaque modèle -------------------------------------------------------------------------
simulate_GLM_odp <- function(nsim) {
  sim <- rqpois(n = nsim * length(lambda_GLM_odp), lambda = lambda_GLM_odp, phi = dispersion_GLM_odp)
  mat <- matrix(sim, ncol = nsim)
  res <- colSums(mat)
  return(res)
}

sim_GLM_odp <- simulate_GLM_odp(nsim)
sim_boot_CL_odp <- boot_CL_odp$IBNR.Totals


# Sauvegarder les distributions prédictives -------------------------------------------------------------------------------------
saveRDS(sim_GLM_odp, file = here("2_pipeline", "02_modeles_collectifs", "sim_GLM_odp.RDS"))
saveRDS(sim_boot_CL_odp, file = here("2_pipeline", "02_modeles_collectifs", "sim_boot_CL_odp.RDS"))
