# Datos panel: Ejercicio de Milena

rm(list = ls())

library(readxl)
library(stringr)
library(plm)

# Preliminares ------------------------------------------------------------

# Rutas de trabajo
output_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"

# Generación de la base de datos principal
# base_principal = read_xlsx("base_principal_procesada.xlsx")

base_principal = readRDS("base_principal_procesada.rds")

# Procesamiento bases de datos --------------------------------------------

# Identificación de estudiantes duplicados: Hicierón dos pruebas diferentes
duplicados = base_principal %>%
  group_by(CORREO) %>%
  filter(n() > 1) %>%
  ungroup() 

# Procesamiento bases de datos principal 
base_limpia = base_principal %>% 
  select(-starts_with("P. ")) %>%
  group_by(CORREO) %>%
  mutate(CORREO = ifelse(row_number() > 1, paste0(CORREO, "2"), CORREO)) %>%
  ungroup()

# Transformación de la base de datos a formato de datos panel 
base_limpia_panel = base_limpia %>% 
  pivot_longer(cols = starts_with("Calificación/10.00_prueba"),
               names_to = "periodos_pruebas",
               values_to = "calificacion_pruebas") %>% 
  select(CORREO, periodos_pruebas, calificacion_pruebas, everything()) %>% 
  mutate(periodos_pruebas = str_replace(periodos_pruebas, "Calificación/10.00_prueba", ""))

# Creción de ls Dummy
base_limpia_panel = base_limpia_panel %>% 
  mutate(AV = as.factor(ifelse(Periodo_agregado == "virtual", 1, 0)),
         AP = as.factor(ifelse(Periodo_agregado == "presencial", 1, 0)),
         PL = as.factor(ifelse(Periodo_agregado == "placebo", 1, 0)),
         EX2 = as.factor(ifelse(periodos_pruebas == 2, 1, 0)))

# Base de datos tipo panel --------------------------------------------------

# Panel completo (incluyendo placebos) ----

# Construcción del panel
panel = pdata.frame(base_limpia_panel, index = c("CORREO", "periodos_pruebas"))

# Para conocer las dimensiones del panel
pdim(panel)

# Regresiones -------------------------------------------------------------

# Nota: Las regresiones tendrán placebos

# Pooled OLS ----

# Modelo sin regresoras
pooled_ols_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2, 
                               data = base_limpia_panel, 
                               model = "pooling"); summary(pooled_ols_sin_reg)

# Modelo con regresoras
pooled_ols_con_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO, 
                               data = base_limpia_panel, 
                               model = "pooling"); summary(pooled_ols_con_reg)

# Efectos fijos ----

# Modelo sin regresoras 
fe_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO,
                       data = base_limpia_panel,
                       model = "within"); summary(fe_sin_reg)

# Modelo con regresoras
fe_con_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM * EX2 + PAPA_PERIODO * EX2,
                  data = base_limpia_panel,
                  model = "within"); summary(fe_con_reg)

# Primeras diferencias ----

# Modelo sin regresoras 
fd_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO,
                  data = base_limpia_panel,
                  model = "fd"); summary(fd_sin_reg)

# Modelo con regresoras
fd_con_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM * EX2 + PAPA_PERIODO * EX2,
                  data = base_limpia_panel,
                  model = "fd"); summary(fd_con_reg)
