# Script para generar las bases: "listas_estudiantes_full.xlsx" y "placebos.xlsx"

library(tidyverse)
library(readxl)
library(writexl)

# Limpieza del entorno de trabajo
rm(list = ls())

# Nota importante: LLene a mano la información de econometríaII para las bases "placebo" y "lista_estudiantes_full". 
#                  Mirar como incluirlas de manera automática y no manual

# Directorio de trabajo
list_estud_pre_exp = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023-2/bases/lista_estudiantes_pre_experimentos"
placebos = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023-2/bases/placebos"
output = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_full/tests_asignaturas/bases_de_datos/output"

# Funciones 

# Limpieza base
limpieza_base = function(llamado1, llamado2){
  
  # Limpieza bases de datos
  base_limpia = bind_rows(llamado1, llamado2) %>% 
    distinct(Correos, .keep_all = TRUE) %>% 
    rename(CORREO = Correos)
  
  return(base_limpia)
}

# Importación base de datos ----

# Lista de estudiantes (target) ----

# Nota: Las bases de datos a tratar no incluyen los placebos

setwd(list_estud_pre_exp)

# Microeconomia1 
micro1_llamado1 = read_xlsx("lista_estudiantes_pre_experimentos(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "Micro1")
micro1_llamado2 = read_xlsx("segundo_llamado(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "micro1")

# Modelación estática 
mod_est_llamado1 = read_xlsx("lista_estudiantes_pre_experimentos(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "Modelacion estatica")
mod_est_llamado2 = read_xlsx("segundo_llamado(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "modelacion_estatica")

# Modelación dinámica 
mod_din_llamado1 = read_xlsx("lista_estudiantes_pre_experimentos(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "Modelacion dinamica")
mod_din_llamado2 = read_xlsx("segundo_llamado(micro1, modelacion estatica y modelacion dinamica).xlsx", sheet = "modelacion_dinamica")

# Probabilidad y estadística fundamental 
prob_estad_fund = read_xlsx("lista_estudiantes_pre_experimentos(probabilidad_estadistica_fundamental).xlsx", sheet = "probabilidad_estad_fundam")

# Procesamiento base de datos ----

# Microeconomia1
micro1_full = limpieza_base(micro1_llamado1, micro1_llamado2)

# Modelación estática
mod_est_full = limpieza_base(mod_est_llamado1, mod_est_llamado2)

# Modelación dinámica
mod_din_full = limpieza_base(mod_din_llamado1, mod_din_llamado2)

# Probabilidad
prob_estad_fund_full = prob_estad_fund %>% 
  rename(CORREO = Correos)

# Placebos ----
setwd(placebos)

placebos_micro1_demas = read_xlsx("placebos.xlsx", sheet = "Micro1, estatica y dinamica")
placebos_prob_estad = read_xlsx("placebos.xlsx", sheet = "prob y estad funda")

placebos_micro1_demas = placebos_micro1_demas %>%
  rename(CORREO = Correos)

placebos_prob_estad = placebos_prob_estad %>% 
  rename(CORREO = Correos)

# Exportar bases de datos -------------------------------------------------
setwd(output)

# Exportación base estudiantes target ----

# Bases de datos 
list_bases = list("micro1" = micro1_full, 
                  "mod_est" = mod_est_full, 
                  "mod_din" = mod_din_full,
                  "prob_estad_fund" = prob_estad_fund_full) 

# Exportación base de datos
write_xlsx(list_bases, "listas_estudiantes_full.xlsx")

# Exportación base placebos ----

list_placebos = list("Micro1, estatica y dinamica" = placebos_micro1_demas, 
                     "prob y estad funda" = placebos_prob_estad) 

# Exportación base de datos
write_xlsx(list_placebos, "placebos.xlsx")
