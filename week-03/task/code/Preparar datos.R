


rm(list=ls())
## librerias
library(tidyverse)
require(rio)
require(data.table)

## Crear datos limpios
df <- import("week-04/input/RUES_clean_dirv2.rds" , setclass = "data.table")
df <- df[cod_mpio==76001 & year_renueva>=2023]
df <- df[,.(dpto_name,mpio_name,unique_id,razon_social,year_matricula,year_cancela,year_renueva,ciiu1,ciiu2,ciiu3,ciiu4)]

## Determinar el 10% de filas a contaminar
set.seed(123)
filas_error <- sample(1:nrow(df) , size = round(nrow(df) * 0.05))

## crear variables
df$`Activos M.` <- round(rnorm(nrow(df),10000,2500))
df$`Ingresos M.` <- round(rnorm(nrow(df),5000,2000))
df$`Pasivos M.` <- round(rnorm(nrow(df),-2000,2000))
df$`Patrimonio M.` <- df$`Activos M.` + df$`Pasivos M.`

## agregar error
df$`Activos M.`[filas_error] <- 999999

## export datos
export(df , "week-04/output/empresas_cali.rds")




##
df <- df[,.(unique_id,razon_social)]

## crear variables
set.seed(123)
df$`empleados` <- round(rnorm(nrow(df),100,25))
df$`sedes` <- round(rnorm(nrow(df),10,2.8))

## export datos
export(df , "week-04/output/sedes_cali.rds")

##
df <- df[,.(unique_id,razon_social)]

## crear variables
set.seed(123)
df_24 <- df[,.(unique_id,razon_social)]
df_24$clientes <- round(rnorm(nrow(df),1000,200))
df_24$year <- 2024

df_25$clientes <- df_24$clientes + round(rnorm(nrow(df),100,10))
df_25$year <- 2025

db <- bind_rows(df_24 , df_25)

## export datos
export(db , "week-04/output/clientes_cali.rds")









db <- left_join(clientes,sedes,c("unique_id","razon_social")) %>%
      group_by(unique_id) %>%
      mutate("Ventas en millones"=rnorm(2,2000,1000))

export(db , "../02-tasks/week-03/task/data.csv")


