# Datos panel: Primeros Ejercicios 

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

# Base sin placebos
base_limpia_sin_placebos = base_limpia_panel %>% 
  filter(Periodo_agregado != "placebos") %>% 
  mutate(virtual_dummy = as.factor(ifelse(Periodo_agregado == "virtual", 1, 0)),
         prueba2_dummy = as.factor(ifelse(periodos_pruebas == 2, 1, 0)))


# Base de datos tipo panel --------------------------------------------------

# Panel completo (incluyendo placebos) ----

# Construcción del panel
panel = pdata.frame(base_limpia_panel, index = c("CORREO", "periodos_pruebas"))

# Para conocer las dimensiones del panel
pdim(panel)

# Panel sin placebos ----

# Construcción del panel
panel_sin_placebos = pdata.frame(base_limpia_sin_placebos, index = c("CORREO", "periodos_pruebas"))

# Para conocer las dimensiones del panel
pdim(panel_sin_placebos)


# Regresiones -------------------------------------------------------------


# Regresiones sin placebos ----

# Pooled OLS

# Modelo sin regresoras
pooled_ols1_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy, 
                              data = panel_sin_placebos, 
                              model = "pooling"); summary(pooled_ols1_sin_placebos)

# Modelo con regresoras
pooled_ols2_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy + 
                                 PBM + PAPA_PERIODO, 
                              data = panel_sin_placebos, 
                              model = "pooling"); summary(pooled_ols2_sin_placebos)

# Efectos fijos 

# Modelo sin regresoras 
fe1_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy, 
                               data = panel_sin_placebos, 
                               model = "within"); summary(fe1_sin_placebos)

# Modelo con regresoras
fe2_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy + PBM * prueba2_dummy + 
                                 PAPA_PERIODO * prueba2_dummy, 
                               data = panel_sin_placebos, 
                               model = "within"); summary(fe2_sin_placebos)

# Primeras diferencias 

# Modelo sin regresoras 
fd1_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy, 
                       data = panel_sin_placebos, 
                       model = "fd"); summary(fd1_sin_placebos)

# Modelo con regresoras
fd2_sin_placebos = plm(calificacion_pruebas ~ virtual_dummy + prueba2_dummy + virtual_dummy * prueba2_dummy + PBM * prueba2_dummy + 
                         PAPA_PERIODO * prueba2_dummy, 
                       data = panel_sin_placebos, 
                       model = "fd"); summary(fd2_sin_placebos)

# Regresiones con placebos ----








