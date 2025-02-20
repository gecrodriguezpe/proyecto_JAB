# Procesamiento final bases de datos pruebas 

library(tidyverse)
library(readxl)
library(writexl)

# Limpieza del entorno de trabajo
rm(list = ls())

# Directorios de trabajo
output = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"
adicionales = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/adicionales"

# Importación base de datos -----------------------------------------------

# Directorio de resultados
setwd(output)

# Placebos
placebos_micro1_demas = read_xlsx("placebos.xlsx", sheet = "Micro1, estatica y dinamica")
placebos_prob_estad = read_xlsx("placebos.xlsx", sheet = "prob y estad funda")
placebos_econometria2 = read_xlsx("placebos.xlsx", sheet = "econometria2")

# Falta la encuesta de percepcion !!!

# Resultados de las pruebas 

# 1. Microeconomia 1 ----

micro1_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "micro1")

# Lista de estudiantes completos que vieron y aprobaron la asignatura 
micro1_full_list = read_xlsx("listas_estudiantes_full.xlsx", sheet = "micro1")

# "Joining" las dos base de datos 

# Falta la encuesta de percepción 
micro1 = micro1_full_list %>% 
  right_join(micro1_pruebas, by = c("CORREO")) %>% 
  mutate(Periodo = ifelse(is.na(Periodo), "placebos", Periodo)) %>% 
  mutate(prueba = "micro1")

# "Resultados" de las pruebas de acuerdo a grupos/cohortes estudiantiles 

# Micro1 

# Por grupo de estudiantes 
micro1_resultados_grupos = micro1 %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); micro1_resultados_grupos

# 2. Modelación estática ----

mod_est_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "mod_est")

# Lista de estudiantes completos que vieron y aprobaron la asignatura 
mod_est_full_list = read_xlsx("listas_estudiantes_full.xlsx", sheet = "mod_est")

# "Joining" las dos base de datos 

# Falta la encuesta de percepción 
mod_est = mod_est_full_list %>% 
  right_join(mod_est_pruebas, by = c("CORREO")) %>% 
  mutate(Periodo = ifelse(is.na(Periodo), "placebos", Periodo)) %>% 
  mutate(prueba = "mod_est")

# "Resultados" de las pruebas de acuerdo a grupos/cohortes estudiantiles 

# Modelación estática

# Por grupo de estudiantes 
mod_est_resultados_grupos = mod_est %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); mod_est_resultados_grupos

# 3. Modelación dinámica ----

mod_din_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "mod_din")

# Lista de estudiantes completos que vieron y aprobaron la asignatura 
mod_din_full_list = read_xlsx("listas_estudiantes_full.xlsx", sheet = "mod_din")

# "Joining" las dos base de datos 

# Falta la encuesta de percepción 
mod_din = mod_din_full_list %>% 
  right_join(mod_din_pruebas, by = c("CORREO")) %>% 
  mutate(Periodo = ifelse(is.na(Periodo), "placebos", Periodo)) %>% 
  mutate(prueba = "mod_din")

# "Resultados" de las pruebas de acuerdo a grupos/cohortes estudiantiles 

# Modelación dinámica

# Por grupo de estudiantes 
mod_din_resultados_grupos = mod_din %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); mod_din_resultados_grupos

# 4. Probabilidad y estadística fundamental ----

prob_estad_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "prob_estad_fund")

# Lista de estudiantes completos que vieron y aprobaron la asignatura 
prob_estad_full_list = read_xlsx("listas_estudiantes_full.xlsx", sheet = "prob_estad_fund")

# "Joining" las dos base de datos 

# Falta la encuesta de percepción 
prob_estad = prob_estad_full_list %>% 
  right_join(prob_estad_pruebas, by = c("CORREO")) %>% 
  mutate(Periodo = ifelse(is.na(Periodo), "placebos", Periodo)) %>% 
  mutate(prueba = "prob_estad")

# "Resultados" de las pruebas de acuerdo a grupos/cohortes estudiantiles 

# Probabilidad y estadística fundamental

# Por grupo de estudiantes 
prob_estad_resultados_grupos = prob_estad %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); prob_estad_resultados_grupos

# 5. Econometría II ----

econometria2_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "econometria2")

# Lista de estudiantes completos que vieron y aprobaron la asignatura 
econometria2_full_list = read_xlsx("listas_estudiantes_full.xlsx", sheet = "econometria2")

# "Joining" las dos base de datos 

# Falta la encuesta de percepción 
econometria2 = econometria2_full_list %>% 
  right_join(econometria2_pruebas, by = c("CORREO")) %>% 
  mutate(Periodo = ifelse(is.na(Periodo), "placebos", Periodo)) %>% 
  mutate(prueba = "econometria2")

# "Resultados" de las pruebas de acuerdo a grupos/cohortes estudiantiles 

# Econometria2

# Por grupo de estudiantes 
econometria2_resultados_grupos = econometria2 %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); econometria2_resultados_grupos


# 6. Todas las asignaturas ----

todas = bind_rows(micro1, mod_est, mod_din, prob_estad, econometria2)

todas_resultados_grupos = todas %>% 
  group_by(Periodo) %>% 
  summarise(num_estudiantes = n(), 
            media_prueba1 = mean(`Calificación/10.00_prueba1`), 
            media_prueba2 = mean(`Calificación/10.00_prueba2`),
            mediana_prueba1 = median(`Calificación/10.00_prueba1`), 
            mediana_prueba2 = median(`Calificación/10.00_prueba2`)); todas_resultados_grupos

# Escribir los resultados de todos en un Excel 
setwd(adicionales)

write_xlsx(todas_resultados_grupos, "resultados_medias_por_grupo.xlsx")

# Exportación resultados de las pruebas agregadas ------------------------------------
setwd(output)

# Exportación del resultado de todas las pruebas
write_xlsx(todas, "resultados_pruebas_agregado.xlsx")
