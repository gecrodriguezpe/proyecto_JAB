library(tidyverse)
library(readxl)
library(pwr)

# Importación de datos

adicionales = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/adicionales"
setwd(adicionales)

# Importación base de datos
todas_resultados_grupos = read_xlsx("resultados_medias_por_grupo.xlsx")

# Ejercicios de diferencia

# Grupo1: 2021-2S
# Grupo2: 2022-1S
# Grupo3: 2022-2S
# Grupo4: 2023-1S
# Grupo5: placebos

# Nota: Para el poder estadístico de los test se uso "https://www.statmethods.net/stats/power.html"

# Todas las asignaturas ---------------------------------------------------

# Diferencia de pruebas: Test2 - Test1
diferencia_pruebas = todas_resultados_grupos$media_prueba2 - todas_resultados_grupos$media_prueba1

# 1. Ejercicio1 - T: 2021-2S y C: 2022-1S ----

# Todas las asignaturas 

# Doble diferencia

# tratmiento - control (el signo va a depender del orden de la resta)
diferencia_T_C = diferencia_pruebas[1] - diferencia_pruebas[2]; diferencia_T_C

# Triple diferencia
triple_diferencia = (diferencia_pruebas[1] - diferencia_pruebas[5]) - (diferencia_pruebas[2] - diferencia_pruebas[5]); triple_diferencia

# Pruebas estadísticas

# Poder de la prueba para dos muestras 
pwr.t2n.test(n1 = 35, n2= 29, d = triple_diferencia, sig.level = 0.05)

# Estadístico-t para diferencia de medias 

# 2021-2S
todas_2021_2s = todas %>% 
  filter(Periodo == "2021-2S")

todas_2021_2s_notas1 = todas_2021_2s$`Calificación/10.00_prueba1`
todas_2021_2s_notas2 = todas_2021_2s$`Calificación/10.00_prueba2`

# 2022-1S
todas_2022_1s = todas %>% 
  filter(Periodo == "2022-1S")

todas_2022_1s_notas1 = todas_2022_1s$`Calificación/10.00_prueba1`
todas_2022_1s_notas2 = todas_2022_1s$`Calificación/10.00_prueba2`

# t-test para diferencia de medias
t.test(todas_2021_2s_notas1, todas_2022_1s_notas1)
t.test(todas_2021_2s_notas2, todas_2022_1s_notas2)

# 2. Ejercicio2 - T: 2021-2S y C: 2022-1S, 2022-2S y 2023-1S ----

avg_control_name = "average_control"

# Calculo de la media y la mediana de todo el grup presencial
tot_est_avg_control = sum(c(todas_resultados_grupos$num_estudiantes[2], todas_resultados_grupos$num_estudiantes[3], todas_resultados_grupos$num_estudiantes[4]))
media_prueba1_avg_control = mean(c(todas_resultados_grupos$media_prueba1[2], todas_resultados_grupos$media_prueba1[3], todas_resultados_grupos$media_prueba1[4]))
media_prueba2_avg_control = mean(c(todas_resultados_grupos$media_prueba2[2], todas_resultados_grupos$media_prueba2[3], todas_resultados_grupos$media_prueba2[4]))
mediana_prueba1_avg_control = mean(c(todas_resultados_grupos$mediana_prueba1[2], todas_resultados_grupos$mediana_prueba1[3], todas_resultados_grupos$mediana_prueba1[4]))
mediana_prueba2_avg_control = mean(c(todas_resultados_grupos$mediana_prueba2[2], todas_resultados_grupos$mediana_prueba2[3], todas_resultados_grupos$mediana_prueba2[4]))


average_control_lst = list(avg_control_name, tot_est_avg_control, 
                    media_prueba1_avg_control, media_prueba2_avg_control,
                    mediana_prueba1_avg_control, mediana_prueba2_avg_control)

todas_resultados_grupos[6,] = average_control_lst

# Todas las asignaturas 

# Diferencia de pruebas: Test2 - Test1
diferencia_pruebas = todas_resultados_grupos$media_prueba2 - todas_resultados_grupos$media_prueba1

# Doble diferencia

# tratmiento - control (el signo va a depender del orden de la resta)
diferencia_T_C = diferencia_pruebas[1] - diferencia_pruebas[6]; diferencia_T_C

# Triple diferencia
triple_diferencia = (diferencia_pruebas[1] - diferencia_pruebas[5]) - (diferencia_pruebas[6] - diferencia_pruebas[5]); triple_diferencia

# Pruebas estadísticas

# Poder de la prueba para dos muestras 
pwr.t2n.test(n1 = 43, n2= 92, d = triple_diferencia, sig.level = 0.05)

# Estadístico-t para diferencia de medias 

# 2021-2S
todas_2021_2s = todas %>% 
  filter(Periodo == "2021-2S")

todas_2021_2s_notas1 = todas_2021_2s$`Calificación/10.00_prueba1`
todas_2021_2s_notas2 = todas_2021_2s$`Calificación/10.00_prueba2`

# 2022-1S, 2022-2S and 2023-1S
todas_C = todas %>% 
  filter(Periodo %in% c("2022-1S", "2022-2S", "2023-1S"))

todas_C_notas1 = todas_2022_1s$`Calificación/10.00_prueba1`
todas_C_notas2 = todas_2022_1s$`Calificación/10.00_prueba2`

# t-test para diferencia de medias
t.test(todas_2021_2s_notas1, todas_C_notas1)
t.test(todas_2021_2s_notas2, todas_C_notas2)

# 3. Revisión con placebos ----

# Placebos
todas_placebo = todas %>% 
  filter(Periodo == "placebos")

todas_placebo_notas1 = todas_placebo$`Calificación/10.00_prueba1`
todas_placebo_notas2 = todas_placebo$`Calificación/10.00_prueba2`

# t-test para diferencia de medias virtual vs Placebo
t.test(todas_2021_2s_notas1, todas_placebo_notas1)
t.test(todas_2021_2s_notas2, todas_placebo_notas2)

# t-test para diferencia de medias presencial 2022-1S vs Placebo
t.test(todas_2022_1s_notas1, todas_placebo_notas1)
t.test(todas_2022_1s_notas2, todas_placebo_notas2)

# t-test para diferencia de medias presencial (control) vs Placebo
t.test(todas_C_notas1, todas_placebo_notas1)
t.test(todas_C_notas2, todas_placebo_notas2)
