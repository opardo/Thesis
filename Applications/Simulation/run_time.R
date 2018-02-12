library(readr)
library(tibble)
library(xtable)

local_path <- "/Users/opardo/Documents/Projects/Personal/"
sim_path <- paste0(local_path,"Thesis/Applications/Simulation/")
fit_path <- "/models/time_fit.rds"
fit_trad_path <- "/models/time_trad_fit.rds"
pred_path <- "/predictions/time_pred.rds"
pred_trad_path <- "/predictions/time_trad_pred.rds"

get_fit_time <- function(folder, model) {
  if (model == "GPDP") {
    return(unname(read_rds(paste0(sim_path, folder, fit_path))[3]))
  } else if (model == "Trad") {
    return(unname(read_rds(paste0(sim_path, folder, fit_trad_path))[3]))
  }
}

get_pred_time <- function(folder, model) {
  if (model == "GPDP") {
    return(unname(read_rds(paste0(sim_path, folder, pred_path))[3]))
  } else if (model == "Trad") {
    return(unname(read_rds(paste0(sim_path, folder, pred_trad_path))[3]))
  }
}

cases <- c(
  "classic",
  "heavy_tails",
  "heteroscedasticity",
  "asymmetric",
  "discontinuous"
)

labels = c(
  "Supuestos tradicionales",
  "Colas pesadas",
  "Heterocedasticidad",
  "Error asimétrico",
  "Discontinuidades"
)

fit_trad_times <- unlist(lapply(cases, get_fit_time, model = "Trad"))
fit_times <- unlist(lapply(cases, get_fit_time, model = "GPDP"))
pred_trad_times <- unlist(lapply(cases, get_pred_time, model = "Trad"))
pred_times <- unlist(lapply(cases, get_pred_time, model = "GPDP"))

fit <- data_frame(
  Datos = labels,
  `Tradicional (seg)` = fit_trad_times,
  `GPDP (seg)` = fit_times
)

pred <- data_frame(
  Datos = labels,
  `Tradicional (seg)` = pred_trad_times,
  `GPDP (seg)` = pred_times
)

print(
  xtable(
    fit,
    caption = "Tiempo de ajuste por conjunto de datos, para cada modelo.",
    align = c("c","c","c", "c"),
    digits = c(0,0,3,2),
    label = "fit_time"
  ),
  include.rownames = FALSE
)

print(
  xtable(
    pred,
    caption = "Tiempo de predicción por conjunto de datos, para cada modelo.",
    align = c("c","c","c", "c"),
    digits = c(0,0,3,2),
    label = "pred_time"
  ),
  include.rownames = FALSE
)


