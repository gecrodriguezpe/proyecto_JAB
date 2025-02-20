# Script para procesar las bases de datos de los test de conocimiento de moodle 
# "Prueba 1" y "Prueba 2"

# Librerias de trabajo 
library(tidyverse)
library(readxl)
library(writexl)

# Limpieza del entorno de trabajo
rm(list = ls())

# Preliminares ------------------------------------------------------------

# Directorios de trabajo pruebas 
resultado_pruebas = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/resultados_pruebas_arreglados"
output = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"


# Funciones ---------------------------------------------------------------

# 1. Arreglar base de datos de moodle 
limpiar_base_moodle = function(moodle_base, vars_not_select, tipo_prueba){
  
  #tipo_prueba: Distinguir el tipo de prueba
  
  # Procesar la base de datos para que solo tenga la información de importancia 
  nueva_base = moodle_base %>% 
    filter(`Apellido(s)` != "Promedio general") %>% 
    filter(`Apellido(s)` != "Promedio del grupo") %>% 
    distinct(`Número de ID`, .keep_all = TRUE, first = TRUE) %>% 
    rename(CORREO = `Dirección de correo`) %>% 
    select(!any_of(vars_not_select))
  
  # Para distinguir el tipo de prueba: Prueba1 vs Prueba2
  new_names <- paste0(names(nueva_base), tipo_prueba)
  names(nueva_base) <- new_names
  
  return(nueva_base)
}

# 2. Combinar observaciones que hayan completado prueba1 y prueba2 
ambas_pruebas_completas = function(prueba1, prueba2){
  
  # Solo seleccionar observaciones que hayan completado tanto la prueba1 como la prueba2
  bases_juntas = prueba1 %>% 
    inner_join(prueba2, by = c("CORREO_prueba1" = "CORREO_prueba2")) %>% 
    rename("CORREO" = "CORREO_prueba1")
  
  # 
  return(bases_juntas)
}

# Procesamiento bases de datos --------------------------------------------

# Definir directorio de trabajo
setwd(resultado_pruebas)

# Variables que serán descartadas en 
vars_not_select = c("Apellido(s)", "Nombre", "Número de ID")


# 1. Micro1 (Asignatura 1) -------------------------------------------------------------------------

# Prueba1 

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign1_prueba1_g1 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba1.xlsx", sheet = "7_8_30")
viernes29_sept_asign1_prueba1_g2 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba1.xlsx", sheet = "11_12_30")
viernes29_sept_asign1_prueba1_g3 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba1.xlsx", sheet = "12_30_2")
viernes29_sept_asign1_prueba1_g4 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba1.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign1_prueba1_g1 = read_xlsx("lunes30_oct/asignatura1/lunes30_oct_asign1_prueba1.xlsx", sheet = "9_10_30")
lunes30_oct_asign1_prueba1_g2 = read_xlsx("lunes30_oct/asignatura1/lunes30_oct_asign1_prueba1.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign1_prueba1_g1 = read_xlsx("miercoles1_nov/asignatura1/miercoles1_nov_asign1_prueba1.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign1_prueba1_g1 = read_xlsx("viernes3_nov/asignatura1/viernes3_nov_asign1_prueba1.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 1/prueba 1 en una sola base
asignatura1_prueba1 = bind_rows(viernes29_sept_asign1_prueba1_g1, viernes29_sept_asign1_prueba1_g2, viernes29_sept_asign1_prueba1_g3, viernes29_sept_asign1_prueba1_g4,
                                lunes30_oct_asign1_prueba1_g1, lunes30_oct_asign1_prueba1_g2, miercoles1_nov_asign1_prueba1_g1, viernes3_nov_asign1_prueba1_g1)

# Limpiar la base de datos de la asignatura 1/prueba 1
asignatura1_prueba1_limpia = limpiar_base_moodle(asignatura1_prueba1, vars_not_select, "_prueba1")

# Prueba2

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign1_prueba2_g1 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba2.xlsx", sheet = "7_8_30")
viernes29_sept_asign1_prueba2_g2 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba2.xlsx", sheet = "11_12_30")
viernes29_sept_asign1_prueba2_g3 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba2.xlsx", sheet = "12_30_2")
viernes29_sept_asign1_prueba2_g4 = read_xlsx("viernes29_sept/asignatura1/viernes29_sept_asign1_prueba2.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign1_prueba2_g1 = read_xlsx("lunes30_oct/asignatura1/lunes30_oct_asign1_prueba2.xlsx", sheet = "9_10_30")
lunes30_oct_asign1_prueba2_g2 = read_xlsx("lunes30_oct/asignatura1/lunes30_oct_asign1_prueba2.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign1_prueba2_g1 = read_xlsx("miercoles1_nov/asignatura1/miercoles1_nov_asign1_prueba2.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign1_prueba2_g1 = read_xlsx("viernes3_nov/asignatura1/viernes3_nov_asign1_prueba2.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 1/prueba 2 en una sola base
asignatura1_prueba2 = bind_rows(viernes29_sept_asign1_prueba2_g1, viernes29_sept_asign1_prueba2_g2, viernes29_sept_asign1_prueba2_g3, viernes29_sept_asign1_prueba2_g4,
                                lunes30_oct_asign1_prueba2_g1, lunes30_oct_asign1_prueba2_g2, miercoles1_nov_asign1_prueba2_g1, viernes3_nov_asign1_prueba2_g1)

# Limpiar la base de datos de la asignatura 1/prueba 2
asignatura1_prueba2_limpia = limpiar_base_moodle(asignatura1_prueba2, vars_not_select, "_prueba2")

# Juntar los resultados de ambas pruebas 

# Combina las bases de datos de la prueba1 y la prueba2 para cada estudiante
asignatura1_complete = ambas_pruebas_completas(asignatura1_prueba1_limpia, asignatura1_prueba2_limpia)

asignatura1_complete = asignatura1_complete %>% 
  mutate(asignatura = "micro1")

# 2. Modelación estática (Asignatura 2) -------------------------------------------------------------------------

# Prueba1 

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign2_prueba1_g1 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba1.xlsx", sheet = "7_8_30")
viernes29_sept_asign2_prueba1_g2 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba1.xlsx", sheet = "11_12_30")
viernes29_sept_asign2_prueba1_g3 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba1.xlsx", sheet = "12_30_2")
viernes29_sept_asign2_prueba1_g4 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba1.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign2_prueba1_g1 = read_xlsx("lunes30_oct/asignatura2/lunes30_oct_asign2_prueba1.xlsx", sheet = "9_10_30")
lunes30_oct_asign2_prueba1_g2 = read_xlsx("lunes30_oct/asignatura2/lunes30_oct_asign2_prueba1.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign2_prueba1_g1 = read_xlsx("miercoles1_nov/asignatura2/miercoles1_nov_asign2_prueba1.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign2_prueba1_g1 = read_xlsx("viernes3_nov/asignatura2/viernes3_nov_asign2_prueba1.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 2/prueba 1 en una sola base
asignatura2_prueba1 = bind_rows(viernes29_sept_asign2_prueba1_g1, viernes29_sept_asign2_prueba1_g2, viernes29_sept_asign2_prueba1_g3, viernes29_sept_asign2_prueba1_g4,
                                lunes30_oct_asign2_prueba1_g1, lunes30_oct_asign2_prueba1_g2, miercoles1_nov_asign2_prueba1_g1, viernes3_nov_asign2_prueba1_g1)

# Limpiar la base de datos de la asignatura 2/prueba 1
asignatura2_prueba1_limpia = limpiar_base_moodle(asignatura2_prueba1, vars_not_select, "_prueba1")

# Prueba2

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign2_prueba2_g1 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba2.xlsx", sheet = "7_8_30")
viernes29_sept_asign2_prueba2_g2 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba2.xlsx", sheet = "11_12_30")
viernes29_sept_asign2_prueba2_g3 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba2.xlsx", sheet = "12_30_2")
viernes29_sept_asign2_prueba2_g4 = read_xlsx("viernes29_sept/asignatura2/viernes29_sept_asign2_prueba2.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign2_prueba2_g1 = read_xlsx("lunes30_oct/asignatura2/lunes30_oct_asign2_prueba2.xlsx", sheet = "9_10_30")
lunes30_oct_asign2_prueba2_g2 = read_xlsx("lunes30_oct/asignatura2/lunes30_oct_asign2_prueba2.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign2_prueba2_g1 = read_xlsx("miercoles1_nov/asignatura2/miercoles1_nov_asign2_prueba2.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign2_prueba2_g1 = read_xlsx("viernes3_nov/asignatura2/viernes3_nov_asign2_prueba2.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 2/prueba 2 en una sola base
asignatura2_prueba2 = bind_rows(viernes29_sept_asign2_prueba2_g1, viernes29_sept_asign2_prueba2_g2, viernes29_sept_asign2_prueba2_g3, viernes29_sept_asign2_prueba2_g4,
                                lunes30_oct_asign2_prueba2_g1, lunes30_oct_asign2_prueba2_g2, miercoles1_nov_asign2_prueba2_g1, viernes3_nov_asign2_prueba2_g1)

# Limpiar la base de datos de la asignatura 2/prueba 2
asignatura2_prueba2_limpia = limpiar_base_moodle(asignatura2_prueba2, vars_not_select, "_prueba2")

# Juntar los resultados de ambas pruebas 

# Combina las bases de datos de la prueba1 y la prueba2 para cada estudiante
asignatura2_complete = ambas_pruebas_completas(asignatura2_prueba1_limpia, asignatura2_prueba2_limpia)

asignatura2_complete = asignatura2_complete %>% 
  mutate(asignatura = "mod_est")

# 3. Modelación dinámica (Asignatura 3) -------------------------------------------------------------------------

# Prueba1 

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign3_prueba1_g1 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba1.xlsx", sheet = "7_8_30")
viernes29_sept_asign3_prueba1_g2 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba1.xlsx", sheet = "11_12_30")
viernes29_sept_asign3_prueba1_g3 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba1.xlsx", sheet = "12_30_2")
viernes29_sept_asign3_prueba1_g4 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba1.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign3_prueba1_g1 = read_xlsx("lunes30_oct/asignatura3/lunes30_oct_asign3_prueba1.xlsx", sheet = "9_10_30")
lunes30_oct_asign3_prueba1_g2 = read_xlsx("lunes30_oct/asignatura3/lunes30_oct_asign3_prueba1.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign3_prueba1_g1 = read_xlsx("miercoles1_nov/asignatura3/miercoles1_nov_asign3_prueba1.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign3_prueba1_g1 = read_xlsx("viernes3_nov/asignatura3/viernes3_nov_asign3_prueba1.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 3/prueba 1 en una sola base
asignatura3_prueba1 = bind_rows(viernes29_sept_asign3_prueba1_g1, viernes29_sept_asign3_prueba1_g2, viernes29_sept_asign3_prueba1_g3, viernes29_sept_asign3_prueba1_g4,
                                lunes30_oct_asign3_prueba1_g1, lunes30_oct_asign3_prueba1_g2, miercoles1_nov_asign3_prueba1_g1, viernes3_nov_asign3_prueba1_g1)

# Limpiar la base de datos de la asignatura 3/prueba 1
asignatura3_prueba1_limpia = limpiar_base_moodle(asignatura3_prueba1, vars_not_select, "_prueba1")

# Prueba2

# Viernes 29 septiembre (asignatura1)
viernes29_sept_asign3_prueba2_g1 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba2.xlsx", sheet = "7_8_30")
viernes29_sept_asign3_prueba2_g2 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba2.xlsx", sheet = "11_12_30")
viernes29_sept_asign3_prueba2_g3 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba2.xlsx", sheet = "12_30_2")
viernes29_sept_asign3_prueba2_g4 = read_xlsx("viernes29_sept/asignatura3/viernes29_sept_asign3_prueba2.xlsx", sheet = "4_5_30")

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign3_prueba2_g1 = read_xlsx("lunes30_oct/asignatura3/lunes30_oct_asign3_prueba2.xlsx", sheet = "9_10_30")
lunes30_oct_asign3_prueba2_g2 = read_xlsx("lunes30_oct/asignatura3/lunes30_oct_asign3_prueba2.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign3_prueba2_g1 = read_xlsx("miercoles1_nov/asignatura3/miercoles1_nov_asign3_prueba2.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign3_prueba2_g1 = read_xlsx("viernes3_nov/asignatura3/viernes3_nov_asign3_prueba2.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 3/prueba 2 en una sola base
asignatura3_prueba2 = bind_rows(viernes29_sept_asign3_prueba2_g1, viernes29_sept_asign3_prueba2_g2, viernes29_sept_asign3_prueba2_g3, viernes29_sept_asign3_prueba2_g4,
                                lunes30_oct_asign3_prueba2_g1, lunes30_oct_asign3_prueba2_g2, miercoles1_nov_asign3_prueba2_g1, viernes3_nov_asign3_prueba2_g1)

# Limpiar la base de datos de la asignatura 3/prueba 2
asignatura3_prueba2_limpia = limpiar_base_moodle(asignatura3_prueba2, vars_not_select, "_prueba2")

# Juntar los resultados de ambas pruebas 

# Combina las bases de datos de la prueba1 y la prueba2 para cada estudiante
asignatura3_complete = ambas_pruebas_completas(asignatura3_prueba1_limpia, asignatura3_prueba2_limpia)

asignatura3_complete = asignatura3_complete %>% 
  mutate(asignatura = "mod_dinam")

# 4. Probabilidad y estadística fundamental (Asignatura 4) -------------------------------------------------------------------------

# Prueba1 

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign4_prueba1_g1 = read_xlsx("lunes30_oct/asignatura4/lunes30_oct_asign4_prueba1.xlsx", sheet = "9_10_30")
lunes30_oct_asign4_prueba1_g2 = read_xlsx("lunes30_oct/asignatura4/lunes30_oct_asign4_prueba1.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign4_prueba1_g1 = read_xlsx("miercoles1_nov/asignatura4/miercoles1_nov_asign4_prueba1.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign4_prueba1_g1 = read_xlsx("viernes3_nov/asignatura4/viernes3_nov_asign4_prueba1.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 4/prueba 1 en una sola base
asignatura4_prueba1 = bind_rows(lunes30_oct_asign4_prueba1_g1, lunes30_oct_asign4_prueba1_g2, miercoles1_nov_asign4_prueba1_g1, viernes3_nov_asign4_prueba1_g1)

# Limpiar la base de datos de la asignatura 4/prueba 1
asignatura4_prueba1_limpia = limpiar_base_moodle(asignatura4_prueba1, vars_not_select, "_prueba1")

# Prueba2

# Lunes 30 octubre (asignatura1)
lunes30_oct_asign4_prueba2_g1 = read_xlsx("lunes30_oct/asignatura4/lunes30_oct_asign4_prueba2.xlsx", sheet = "9_10_30")
lunes30_oct_asign4_prueba2_g2 = read_xlsx("lunes30_oct/asignatura4/lunes30_oct_asign4_prueba2.xlsx", sheet = "2_3_30")

# Miercoles 1 noviembre (asignatura1)
miercoles1_nov_asign4_prueba2_g1 = read_xlsx("miercoles1_nov/asignatura4/miercoles1_nov_asign4_prueba2.xlsx", sheet = "2_3_30")

# Viernes 3 noviembre (asignatura1)
viernes3_nov_asign4_prueba2_g1 = read_xlsx("viernes3_nov/asignatura4/viernes3_nov_asign4_prueba2.xlsx", sheet = "11_12_30")


# Combinar los estudiantes de la asignatura 4/prueba 2 en una sola base
asignatura4_prueba2 = bind_rows(lunes30_oct_asign4_prueba2_g1, lunes30_oct_asign4_prueba2_g2, miercoles1_nov_asign4_prueba2_g1, viernes3_nov_asign4_prueba2_g1)

# Limpiar la base de datos de la asignatura 4/prueba 2
asignatura4_prueba2_limpia = limpiar_base_moodle(asignatura4_prueba2, vars_not_select, "_prueba2")

# Juntar los resultados de ambas pruebas 

# Combina las bases de datos de la prueba1 y la prueba2 para cada estudiante
asignatura4_complete = ambas_pruebas_completas(asignatura4_prueba1_limpia, asignatura4_prueba2_limpia)

asignatura4_complete = asignatura4_complete %>% 
  mutate(asignatura = "prob_estad")

# 5. Econometría II (Asignatura 5) -------------------------------------------------------------------------

# Prueba1 

# Miércoles 24 mayo (asignatura1)
miercoles24_mayo_asign5_prueba1_g1 = read_xlsx("miercoles24_mayo/Econometria2/miercoles24_mayo_econometria2_prueba1.xlsx", sheet = "12_1_30")
miercoles24_mayo_asign5_prueba1_g2 = read_xlsx("miercoles24_mayo/Econometria2/miercoles24_mayo_econometria2_prueba1.xlsx", sheet = "1_2_30")

# Combinar los estudiantes de la asignatura 1/prueba 1 en una sola base
asignatura5_prueba1 = bind_rows(miercoles24_mayo_asign5_prueba1_g1, miercoles24_mayo_asign5_prueba1_g2)

# Limpiar la base de datos de la asignatura 1/prueba 1
asignatura5_prueba1_limpia = limpiar_base_moodle(asignatura5_prueba1, vars_not_select, "_prueba1")

# Prueba2

# Miércoles 24 mayo (asignatura1)
miercoles24_mayo_asign5_prueba2_g1 = read_xlsx("miercoles24_mayo/Econometria2/miercoles24_mayo_econometria2_prueba2.xlsx", sheet = "12_1_30")
miercoles24_mayo_asign5_prueba2_g2 = read_xlsx("miercoles24_mayo/Econometria2/miercoles24_mayo_econometria2_prueba2.xlsx", sheet = "1_2_30")

# Combinar los estudiantes de la asignatura 1/prueba 2 en una sola base
asignatura5_prueba2 = bind_rows(miercoles24_mayo_asign5_prueba2_g1, miercoles24_mayo_asign5_prueba2_g2)

# Limpiar la base de datos de la asignatura 1/prueba 2
asignatura5_prueba2_limpia = limpiar_base_moodle(asignatura5_prueba2, vars_not_select, "_prueba2")

# Juntar los resultados de ambas pruebas 

# Combina las bases de datos de la prueba1 y la prueba2 para cada estudiante
asignatura5_complete = ambas_pruebas_completas(asignatura5_prueba1_limpia, asignatura5_prueba2_limpia)

asignatura5_complete = asignatura5_complete %>% 
  mutate(asignatura = "econometria2")

# Exportación resultados de las pruebas ------------------------------------

# Exportación resultados de las pruebas a nivel desagregado ------------------------------------

# Directorio de resultados
setwd(output)

# Exportación base de datos de perecepción

# Lista de bases de datos de asignaturas
asignaturas_lst = list("micro1" = asignatura1_complete,
                       "mod_est" = asignatura2_complete, 
                       "mod_din" = asignatura3_complete, 
                       "prob_estad_fund" = asignatura4_complete,
                       "econometria2" = asignatura5_complete)

write_xlsx(asignaturas_lst, "resultados_pruebas_asignaturas.xlsx")
