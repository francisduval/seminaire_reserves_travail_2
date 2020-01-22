#################################################################################################################################
### But: Entrainer le modèle sur l'ensemble d'entrainement. Obtenir tous les paramètres nécessaires                           ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Input: train.RDS,                                                                                                         ###
### Output: Les estimés de tous les paramètres du modèle                                                                      ###
#################################################################################################################################

source("1_code/00_source.R")


# Importer le jeu d'entrainement ------------------------------------------------------------------------------------------------
train <- readRDS(here("2_pipeline", "01_data_wrangling", "train.RDS"))


# Initialiser la liste de paramètres --------------------------------------------------------------------------------------------
param <- list()


# Estimer les taux de panne pour chaque type d'événement (h chapeaux, constants sur chaque année de développement) --------------
expo <- table(train$dev_year)
N_event <- map(split(train, train$event_type), ~ table(.x$dev_year))
param[["h_hat"]] <- invoke(rbind, map(N_event, ~ .x / expo))
param[["h_tot"]] <- colSums(param[["h_hat"]])


# Générer les fonctions d'intensité des événements ------------------------------------------------------------------------------
param[["intensity_h_tot"]] <- intensity(param[["h_tot"]])
param[["intensity_h1"]] <- intensity(param[["h_hat"]][1, ])
param[["intensity_h2"]] <- intensity(param[["h_hat"]][2, ])
param[["intensity_h3"]] <- intensity(param[["h_hat"]][3, ])


# Générer les fonctions d'intensité pour le modèle modifié ----------------------------------------------------------------------
x <- 0:(tau - 1)
param[["intensity_h_tot_modif"]] <- splinefun(x, param[["h_tot"]], method = "fmm")
param[["intensity_h1_modif"]] <- splinefun(x, param[["h_hat"]][1, ], method = "fmm")
param[["intensity_h2_modif"]] <- splinefun(x, param[["h_hat"]][2, ], method = "fmm")
param[["intensity_h3_modif"]] <- splinefun(x, param[["h_hat"]][3, ], method = "fmm")


# Ajuster le modèle pour la sévérité --------------------------------------------------------------------------------------------
payments <- train %>%
  filter(event_type %in% 2:3) %>%
  select(
    log_payment,
    accident_time,
    age,
    inj_part,
    RepDel,
    dev_year
  )

param[["log_payment_fit"]] <- lm(log_payment ~ ., data = payments)


# Ajuster une binomiale négative sur le délai de déclaration --------------------------------------------------------------------
train_delay <- train %>% 
  group_by(ClNr) %>% 
  slice(1)

rep_delays <- train_delay %>% pull(RepDel)
occ_times <- train_delay %>% pull(accident_time)

neg_log_like <- function(params) {
  x1 <- log(dnbinom(rep_delays, size = params[1], mu = params[2]))
  x2 <- log(pnbinom(tau - occ_times - 0.25, size = params[1], mu = params[2]))
  -sum(x1 - x2)
}

opt_params <- optim(par = c(1, 1), fn = neg_log_like, method = "L-BFGS-B", lower = 0.00001)

param[["size"]] <- opt_params$par[1]
param[["mu"]] <- opt_params$par[2]


# Estimer les intensités d'occurence (lambda chapeaux, constants sur chaque trimestre) ------------------------------------------
CDF_repdel <- function(x) pnbinom(x, size = param[["size"]], mu = param[["mu"]])
f1 <- function(t) CDF_repdel(tau - t)

times <- sort(unique(train$accident_time))
deltas_lower <- lag(c(times, tau))[-1]
deltas_upper <- c(times, tau)[-1]

Noc <- train %>% 
  group_by(ClNr) %>% 
  slice(1) %>% 
  pull(accident_time) %>% 
  table()

param[["lambdas"]] <- Noc / map2_dbl(deltas_lower, deltas_upper, ~ integrate(f1, .x, .y)$value)


# Calculer l'intensité du processus de Poisson des IBNR pour chaque trimestre de 1994 à 2006 ------------------------------------
f2 <- function(t) 1 - pnbinom(tau - t, size = param[["size"]], mu = param[["mu"]])

integrals <- map_dbl(seq_along(param[["lambdas"]]), ~ integrate(f2, lower = (. - 1) * 0.25, upper = (. - 1) * 0.25 + 0.25)$value)
param[["intensite"]] <- param[["lambdas"]] * integrals


# Sauvegarder la liste de paramètres --------------------------------------------------------------------------------------------
saveRDS(param, file = here("2_pipeline", "03_entrainement", "param.RDS"))
