
####== 1) Preparación ---------------------------------------
rm(list = ls())
set.seed(123)

require(dplyr)
require(ggplot2)
require(rio)

####== 2) Datos sintéticos (empresas_dummy) -----------------
n <- 800

sectores <- c("Comercio", "Servicios", "Manufactura", "Tecnología", "Salud")
comunas  <- as.character(1:22)

empresas_dummy <- tibble(
  id              = 1:n,
  nombre_empresa  = paste0("Empresa_", sprintf("%04d", 1:n)),
  sector          = sample(sectores, n, replace = TRUE, prob = c(0.30,0.28,0.20,0.12,0.10)),
  comuna          = sample(comunas, n, replace = TRUE, prob = dpois(1:22, lambda = 10) + 0.01),
  empleados_base  = rpois(n, lambda = 25),
  # premium de sector sobre ingresos
  k_sector        = case_when(
    sector == "Tecnología"  ~ 1.35,
    sector == "Salud"       ~ 1.20,
    sector == "Manufactura" ~ 1.10,
    sector == "Servicios"   ~ 1.00,
    TRUE                    ~ 0.90  # Comercio
  ),
  # ingresos dependen de empleados con ruido log-normal y premio sectorial
  ingresos        = round(exp(rnorm(n, mean = log(pmax(empleados_base,1)) + log(12000000*k_sector), sd = 0.65))),
  empleados       = pmax(1L, round(empleados_base * rlnorm(n, 0, 0.25))),
  anio_fund       = sample(1975:2024, n, replace = TRUE),
  # flag de “grandes”
  grande          = empleados >= 100
) |>
  select(-empleados_base, -k_sector)

# Introducir algunos NA para ejercicios de limpieza/etiquetado
idx_na <- sample(1:n, size = 25)
empresas_dummy$ingresos[idx_na] <- NA_integer_



## exportar data
export(empresas_dummy , "week-04/preparar_task/output/data-task-ggplot.csv")

