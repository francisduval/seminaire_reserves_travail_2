# -------------------------------------------------------------------------------------------------------------------------------
# Packages ----------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(fitdistrplus)
library(here)
library(parallel)
library(foreach)
library(doParallel)
library(MASS)
library(rprojroot)
library(poisson)
library(logNormReg)
library(ChainLadder)
library(scales)
library(splines)
library(kableExtra)
library(gridExtra)
library(viridis)
library(hrbrthemes)
library(rmutil)
options(scipen = 999)
select <- dplyr::select


# -------------------------------------------------------------------------------------------------------------------------------
# Constantes---------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------

# Date d'évaluation (en nombre d'années depuis le 1er janvier 1994) -------------------------------------------------------------
tau <- 12


# -------------------------------------------------------------------------------------------------------------------------------
# Fonctions ---------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------

# Fonction qui renvoie une fonction d'intensité constante par parties -----------------------------------------------------------
intensity <- function(constantes) {
  function(t) {
    f <- function(t){
      if(t <= 0) {
        constantes[1]
      } else if(t > tau) {
        tail(constantes, 1)
      } else {
        constantes[floor(t) + 1]
      }
    }
    return(map_dbl(t, f))
  }
}


# Fonction pour intégrer plus rapidement les fonctions constantes par parties que la fonction "integrate" -----------------------
integrate_intensity_func <- function(h, lower, upper) {
  h <- c(h, rep(tail(h, 1), times = 100))
  
  ind_lower <- floor(lower) + 1
  ind_upper <- floor(upper) + 1
  
  frac_lower <- ind_lower - lower
  frac_upper <- 1 - (ind_upper - upper)
  
  if(identical(ind_lower + 1, ind_upper)) {
    int <- h[ind_lower] * frac_lower + h[ind_upper] * frac_upper
  } else if(identical(ind_lower, ind_upper)) {
    int <- (upper - lower) * h[ind_lower]
  } else {
    ind_full <- (ind_lower + 1):(ind_upper - 1)
    int <- sum(h[ind_full]) + h[ind_lower] * frac_lower + h[ind_upper] * frac_upper
  }
  
  return(as.numeric(int))
}


# Simule le temps avant le prochain événement étant donné à quel moment t on est dans la vie de la réclamation ------------------
simulate_next_event <- function(t) {
  p <- runif(1)
  f <- function(x) {1 - exp(-integrate_intensity_func(param[["h_tot"]], lower = t, upper = t + x)) - p}
  time <- uniroot(f, c(0, 50))$root + t
  prob <-  c(param[["intensity_h1"]](time), param[["intensity_h2"]](time), param[["intensity_h3"]](time))
  type <- sample(1:3, 1, prob = prob)
  return(list(dev_time = time, type = type))
}


# Simule les temps d'événements ainsi que leur type (1, 2, ou 3) pour une réclamation censurée à t ------------------------------
simulate_claim_path <- function(t) {
  event_times <- vector("double", length = 10)
  event_types <- vector("integer", length = 10)
  
  i <- 1
  event <- simulate_next_event(t)
  event_times[i] <- event$dev_time
  event_types[i] <- event$type
  
  while(event$type == 3) {
    i <- i + 1
    t <- event$dev_time
    
    event <- simulate_next_event(t)
    
    event_times[i] <- event$dev_time
    event_types[i] <- event$type
  }
  
  return(list(times = event_times, types = event_types))
}


# Fonction de répartition du délai de déclaration -------------------------------------------------------------------------------
CDF_repdel <- function(x) pnbinom(x, size = param[["size"]], mu = param[["mu"]])


# Fonction de répartition du délai de déclaration sachant qu'il est plus grand que tau - t --------------------------------------
CDF_repdel_cond <- function(u, t) {
  f <- function(u) {
    if(u < tau - t) {
      0
    } else {
      (CDF_repdel(u) - CDF_repdel(tau - t - 0.1)) / (1 - CDF_repdel(tau - t - 0.1))
    }
  }
  return(map_dbl(u, f))
}


# Fonction qui simule un délai de déclaration étant donné l'occurence t de la réclamation ---------------------------------------
sim_delay <- function(t) {
  f1 <- function(t) {
    p <- runif(1)
    f2 <- function(u) CDF_repdel_cond(u, t) - p
    round(uniroot(f2, c(0, 50))$root)
  }
  return(map_dbl(t, f1))
}


# Fonctions pour simuler les variables explicatives (l'age et l'inj_part) -------------------------------------------------------
sim_age <- function(nsim) {
  ages <- train %>% 
    group_by(ClNr) %>% 
    slice(1) %>% 
    pull(age)
  
  prob <- table(ages)
  res <- sample(sort(unique(ages)), nsim, prob = prob, replace = T)
  return(res)
}

sim_inj_part <- function(nsim) {
  inj_part <- train %>% 
    group_by(ClNr) %>% 
    slice(1) %>% 
    pull(inj_part)
  
  prob <- table(inj_part)
  res <- sample(sort(unique(inj_part)), nsim, prob = prob, replace = T)
  return(res)
}


# Version modifiée de la fonction simulate_next_event ---------------------------------------------------------------------------
simulate_next_event_modif <- function(t) {
  p <- runif(1)
  f <- function(x) {1 - exp(-integrate(param[["intensity_h_tot_modif"]], lower = t, upper = t + x)$value) - p}
  time <- uniroot(f, c(0, 50))$root + t
  p3 <- ifelse(time > 11, 0, param[["intensity_h3_modif"]](time))
  prob <-  c(param[["intensity_h1_modif"]](time), param[["intensity_h2_modif"]](time), p3)
  type <- sample(1:3, 1, prob = prob)
  return(list(dev_time = time, type = type))
}


# Version modifiée de la fonction simulate_claim_path ---------------------------------------------------------------------------
simulate_claim_path_modif <- function(t) {
  event_times <- vector("double", length = 10)
  event_types <- vector("integer", length = 10)
  
  i <- 1
  event <- simulate_next_event_modif(t)
  event_times[i] <- event$dev_time
  event_types[i] <- event$type
  
  while(event$type == 3) {
    i <- i + 1
    t <- event$dev_time
    
    event <- simulate_next_event_modif(t)
    
    event_times[i] <- event$dev_time
    event_types[i] <- event$type
  }
  
  return(list(times = event_times, types = event_types))
}
