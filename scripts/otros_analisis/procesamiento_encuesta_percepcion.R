# Procesamiento Encuesta de Perecepción

rm(list = ls())

library(tidyverse)
library(readxl)
library(rlang)
library(writexl)
library(glue)

# Preliminares ------------------------------------------------------------

# Rutas de trabajo

encuesta_percepcion_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/encuesta_percepcion"
output_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"
graficas_dispersion_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/figuras/graficas_dispersion"

# Importación Bases de datos 

## Importación Encuesta de Percepción

setwd(encuesta_percepcion_path)

# Encuesa de percepción
encuesta_percepcion = read_xlsx("Encuesta de percepción - FCE (Econometria II) (respuestas).xlsx")

## Importación Base Milena

setwd(output_path)

# Base de datos generada por Milena 
base_virtuales = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Virtuales")

base_presenciales = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Presenciales")

base_placebos = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Placebos")

base_principal = bind_rows(base_virtuales, base_presenciales, base_placebos)

# 1. Procesamiento encuesta de percepción  --------------------------------

# Se renombran las varibles de la base para que sea más facil trabajar con ellas
encuesta_percepcion_procesada = encuesta_percepcion %>% 
  rename(CORREO = `Dirección de correo electrónico`,
         edad_ep = `¿Cuántos años tiene?`,
         grupo_etnico = `¿Pertenece a un grupo étnico minoritario? ¿Cuál?`,
         municipio_bachillerato = `Ciudad o municipio donde terminó su bachillerato`, 
         estrato_socioeconomico_ep = `Estrato socioeconómico de su vivienda, según recibo de energía eléctrica`,
         nivel_educ_padre = `Nivel educativo más alto alcanzado por el padre`,
         nivel_educ_madre = `Nivel educativo más alto alcanzado por la madre`,
         oficio_padre = `Ocupación u oficio del padre`,
         oficio_madre = `Ocupación u oficio de la madre`, 
         personas_hogar = `¿Cuántas personas conforman el hogar donde vive actualmente, incluido usted?`,
         lectura_entretenimiento = `¿Cuánto tiempo al día dedica a leer por entretenimiento?`,
         lectura_academica = `¿Cuánto tiempo al día dedica a navegar en internet con fines académicos?`,
         estres_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de estrés?  [Antes de pandemia]`,
         estres_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de estrés?  [Durante pandemia]`,
         estres_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de estrés?  [Después de pandemia]`,
         motivacion_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de motivación?  [Antes de pandemia]`,
         motivacion_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de motivación?  [Durante pandemia]`,
         motivacion_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de motivación?  [Después de pandemia]`,
         atencion_clase_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención en clase?  [Antes de pandemia]`,
         atencion_clase_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención en clase?  [Durante pandemia]`,
         atencion_clase_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención en clase?  [Después de pandemia]`,
         atencion_trabajo_autonomo_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención realizando trabajo autónomo?  [Antes de pandemia]`,
         atencion_trabajo_autonomo_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención realizando trabajo autónomo?  [Durante pandemia]`,
         atencion_trabajo_autonomo_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál fue su nivel de atención realizando trabajo autónomo?  [Después de pandemia]`,
         nivel_aprendizaje_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál considera que fue su nivel de aprendizaje?  [Antes de pandemia]`,
         nivel_aprendizaje_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál considera que fue su nivel de aprendizaje?  [Durante pandemia]`,
         nivel_aprendizaje_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Cuál considera que fue su nivel de aprendizaje?  [Después de pandemia]`,
         labores_hogar_AP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tanto tiempo le dedicó a las labores del hogar?  [Antes de pandemia]`,
         labores_hogar_P = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tanto tiempo le dedicó a las labores del hogar?  [Durante pandemia]`,
         labores_hogar_DP = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tanto tiempo le dedicó a las labores del hogar?  [Después de pandemia]`,
         calidad_internet_AP = `En una escala de 1 a 10, donde 1 es mala y 10 es muy buena ¿Cómo fue la calidad de su internet?  [Antes de pandemia]`,
         calidad_internet_P = `En una escala de 1 a 10, donde 1 es mala y 10 es muy buena ¿Cómo fue la calidad de su internet?  [Durante pandemia]`,
         calidad_internet_DP = `En una escala de 1 a 10, donde 1 es mala y 10 es muy buena ¿Cómo fue la calidad de su internet?  [Después de pandemia]`,
         computador_dedicacion_exlusiva = `¿Contaba con computador personal de dedicación exclusiva durante pandemia?`,
         situacion_laboral_jefe_hogar_AP = `¿Cuál fue la situación laboral del jefe de hogar? [Antes de pandemia]`,
         situacion_laboral_jefe_hogar_P = `¿Cuál fue la situación laboral del jefe de hogar? [Durante pandemia]`,
         situacion_laboral_jefe_hogar_DP = `¿Cuál fue la situación laboral del jefe de hogar? [Después de pandemia]`,
         horas_dedicadas_trabajo_remunerado_AP = `¿Cuántas horas semanales dedicó a trabajo remunerado?  [Antes de pandemia]`,
         horas_dedicadas_trabajo_remunerado_P = `¿Cuántas horas semanales dedicó a trabajo remunerado?  [Durante pandemia]`,
         horas_dedicadas_trabajo_remunerado_DP = `¿Cuántas horas semanales dedicó a trabajo remunerado?  [Después de pandemia]`,
         num_semestres_curados_ep = `Número de semestres cursados`,
         mod_aprendizaje_preferencia = `¿Si pudiera escoger una modalidad de aprendizaje, usted preferiría`,
         diplomados_asig_evaluada = `¿Ha visto más asignaturas/diplomados/cursos del área de la asignatura evaluada? Cuáles?`,
         aplicacion_evaluda = `\n¿ En qué ha aplicado lo aprendido en la asignatura evaluada?`,
         gusto_asignatura = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tanto le gustó la asignatura evaluada?`,
         evento_afecto_desempeño_asignatura = `¿Hubo alguna actividad o evento que haya afectado su desempeño en la asignatura? ¿Cuál?`,
         dificultad_prueba1 = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tan difícil le pareció la prueba 1 de conocimientos?`,
         dificultad_prueba2 = `En una escala de 1 a 10, donde 1 es poco y 10 es mucho ¿Qué tan difícil le pareció la prueba 2 de conocimientos?`)

# 2. Procesamiento encuesta de percepción  --------------------------------


# Función para procesar la cadena de texto ingresada por el usuario
limpiar_cadena_de_texto <- function(input_string) {
  
  # Remover acentos
  input_string <- stringi::stri_trans_general(input_string, "Latin-ASCII")
  
  # Convertir todo a minúscula
  input_string <- tolower(input_string)
  
  # Eliminar espacios iniciales antes de la cadena de texto y finales después de la cadena de texto
  input_string <- trimws(input_string)
  
  # Retornar la cadena de carácteres limpia
  return(input_string)
}

# Procesamiento adicional variables de 
encuesta_percepcion_procesada = encuesta_percepcion_procesada %>% 
  mutate(municipio_bachillerato = limpiar_cadena_de_texto(municipio_bachillerato),
         municipio_bachillerato_categorica =  as_factor(ifelse(str_detect(municipio_bachillerato, "bogota"), "bogota", "fuera_bogota")),
         lectura_entretenimiento = as_factor(lectura_entretenimiento), 
         lectura_academica = as_factor(lectura_academica),
         estrato_socioeconomico_ep = fct_collapse(as_factor(estrato_socioeconomico_ep), "4 o más" = c("4", "5")),
         personas_hogar = fct_collapse(as_factor(personas_hogar), "5 o más" = c("5", "6", "7", "8", "9", "10")))

# Inner Join con la base de datos de Milena 

encuesta_percepcion_procesada = base_principal %>%
  inner_join(encuesta_percepcion_procesada, by = c("CORREO"))

# Exportación base de datos 
write_xlsx(encuesta_percepcion_procesada, "base_milena_con_encuesta_percepcion.xlsx")


# 3. Gráficas de frecuencia acumulada --------------------------------

# Función que genera la gráfica de la distribución acumulada
distribucion_acumulada_grafica = function(base, nombre_variable_AP, nombre_variable_P, nombre_variable_DP, titulo_grafica, nombre_generico_variable){
  
  # Vector con el nombre de las variables
  vect_names = c(nombre_variable_AP, nombre_variable_P, nombre_variable_DP)
  
  # Función que genera la base de datos para cada variable 
  base_ggplot_var = function(base, nombre_variable){
    
    if(str_ends(nombre_variable, "_AP$")){
      
      # Se ordena la variable de menor a mayor
      var_ordenada <- sort(base[[nombre_variable]])
      
      # Se cálcula la frecuencia relativa
      frecuencia_acumulada = cumsum(table(var_ordenada))
      
      # Create a data frame for plotting
      frecuencia_acumulada_df <- data.frame(
        escala_numerica = as.numeric(names(frecuencia_acumulada)),
        frecuencia_acumulada = as.numeric(frecuencia_acumulada),
        grupo = "Antes de Pandemía")
      
    }else if(str_ends(nombre_variable, "_P$")){
      
      # Se ordena la variable de menor a mayor
      var_ordenada <- sort(base[[nombre_variable]])
      
      # Se cálcula la frecuencia relativa
      frecuencia_acumulada = cumsum(table(var_ordenada))
      
      # Create a data frame for plotting
      frecuencia_acumulada_df <- data.frame(
        escala_numerica = as.numeric(names(frecuencia_acumulada)),
        frecuencia_acumulada = as.numeric(frecuencia_acumulada),
        grupo = "Durante Pandemía")
      
    }else if(str_ends(nombre_variable, "_DP$")){
      
      # Se ordena la variable de menor a mayor
      var_ordenada <- sort(base[[nombre_variable]])
      
      # Se cálcula la frecuencia relativa
      frecuencia_acumulada = cumsum(table(var_ordenada))
      
      # Create a data frame for plotting
      frecuencia_acumulada_df <- data.frame(
        escala_numerica = as.numeric(names(frecuencia_acumulada)),
        frecuencia_acumulada = as.numeric(frecuencia_acumulada),
        grupo = "Después de Pandemía")
      
    }
    
    return(frecuencia_acumulada_df)
    
  }
  
  
  # Base de datos para la graficación de la grafica de distribución acumulada
  df_ggplot_final = data.frame(escala_numerica = as.double(), frecuencia_acumulada  = as.double(), grupo  = as.character())
  
  # Generación de la base de datos para la graficación de la grafica de distribución acumulada
  for (var_name in vect_names){
    df_ggplot_final = bind_rows(df_ggplot_final, base_ggplot_var(encuesta_percepcion_procesada, var_name))
  } 
  
  # Calcular la frecuencia total de cada grupo
  df_ggplot_final <- df_ggplot_final %>%
    group_by(grupo) %>%
    mutate(total_frecuencia = max(frecuencia_acumulada)) %>%
    ungroup()
  
  # Se calcular la frecuencia como porcentaje 
  df_ggplot_final <- df_ggplot_final %>%
    mutate(frecuencia_acumulada_pct = (frecuencia_acumulada / total_frecuencia) * 100)
  
  # Se realiza la gráfica de distribución acumulada 
  distribucion_acumulada_plot = ggplot(df_ggplot_final, aes(x = escala_numerica, y = frecuencia_acumulada_pct, color = grupo)) +
    geom_line() +
    geom_point() +
    scale_color_manual(
      values = c("Antes de Pandemía" = "#F8766D", "Durante Pandemía" = "#619CFF", "Después de Pandemía" = "#00BA38"),
      breaks = c("Antes de Pandemía", "Durante Pandemía", "Después de Pandemía")
    ) +
    # labs(title = titulo_grafica,
    #      x = glue("{nombre_generico_variable}"),
    #      y = "Frecuencia acmulada") +
    theme_light() +
    theme(legend.position = "bottom", # Posición de la leyenda
          legend.title = element_blank(), # Leyenda sin título
          axis.title.x = element_blank(), # Eliminar etiqueta del eje x
          axis.title.y = element_blank(), # Eliminar etiqueta del eje y
          panel.grid = element_blank(),
          axis.text = element_text(size = 12)) 
  
  return(distribucion_acumulada_plot)
  
}

# 3.1 Graficas de distribuciones acumuladas ----

# Distribucion acumulada "variables de estrés"
distro_acumulada_estres = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                             "estres_AP", 
                                                             "estres_P", 
                                                             "estres_DP",
                                                             #titulo_grafica  = "Distribucion acumulada nivel de estrés percibido por los estudiantes",
                                                             nombre_generico_variable = "Nivel de estrés"); distro_acumulada_estres

# Distribucion acumulada "variables de motivación"
distro_acumulada_motivacion = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                             "motivacion_AP", 
                                                             "motivacion_P", 
                                                             "motivacion_DP",
                                                             #titulo_grafica  = "Distribucion acumulada nivel de motivación percibido por los estudiantes",
                                                             nombre_generico_variable = "Nivel de motivación"); distro_acumulada_motivacion

# Distribucion acumulada "variables atención en clase"
distro_acumulada_atencion_clase = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                             "atencion_clase_AP", 
                                                             "atencion_clase_P", 
                                                             "atencion_clase_DP",
                                                             #titulo_grafica  = "Distribucion acumulada nivel de atención en clase percibido por los estudiantes",
                                                             nombre_generico_variable = "Nivel de atención en clase"); distro_acumulada_atencion_clase

# Distribucion acumulada "variables atención trabajo autónomo"
distro_acumulada_atencion_trabajo_autonomo = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                                 "atencion_trabajo_autonomo_AP", 
                                                                 "atencion_trabajo_autonomo_P", 
                                                                 "atencion_trabajo_autonomo_DP",
                                                                 #titulo_grafica  = "Distribucion acumulada nivel de atención trabajo autonómo percibido por los estudiantes",
                                                                 nombre_generico_variable = "Nivel de atención trabajo autonómo"); distro_acumulada_atencion_trabajo_autonomo

# Distribucion acumulada "variables nivel de aprendizaje"
distro_acumulada_nivel_aprendizaje = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                                            "nivel_aprendizaje_AP", 
                                                                            "nivel_aprendizaje_P", 
                                                                            "nivel_aprendizaje_DP",
                                                                            #titulo_grafica  = "Distribucion acumulada nivel de aprendizaje percibido por los estudiantes",
                                                                            nombre_generico_variable = "Nivel de aprendizaje"); distro_acumulada_nivel_aprendizaje

# Distribucion acumulada "variables labores del hogar"
distro_acumulada_labores_hogar = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                                    "labores_hogar_AP", 
                                                                    "labores_hogar_P", 
                                                                    "labores_hogar_DP",
                                                                    #titulo_grafica  = "Distribucion acumulada nivel de realización de labores del hogar percibido por los estudiantes",
                                                                    nombre_generico_variable = "Nivel trabajo en el hogar"); distro_acumulada_labores_hogar
# Distribucion acumulada "variables calidad del internet"
distro_acumulada_calidad_internet = distribucion_acumulada_grafica(encuesta_percepcion_procesada, 
                                                                "calidad_internet_AP", 
                                                                "calidad_internet_P", 
                                                                "calidad_internet_DP",
                                                                #titulo_grafica  = "Distribucion acumulada calidad del internet percibido por los estudiantes",
                                                                nombre_generico_variable = "Calidad del internet"); distro_acumulada_calidad_internet

# 4. Exportación de las gráficas ----

setwd(graficas_dispersion_path)

# Exportación "variables de estrés"
ggsave("distro_acumulada_estres.pdf", plot = distro_acumulada_estres, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

# Exportación "variables de motivación"
ggsave("distro_acumulada_motivacion.pdf", plot = distro_acumulada_motivacion, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

# Exportación "variables atención en clase"
ggsave("distro_acumulada_atencion_clase.pdf", plot = distro_acumulada_atencion_clase, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

# Exportación "variables atención trabajo autónomo"
ggsave("distro_acumulada_atencion_trabajo_autonomo.pdf", plot = distro_acumulada_atencion_trabajo_autonomo, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

# Exportación "variables nivel de aprendizaje"
ggsave("distro_acumulada_nivel_aprendizaje.pdf", plot = distro_acumulada_nivel_aprendizaje, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

# Exportación "variables calidad del internet"
ggsave("distro_acumulada_calidad_internet.pdf", plot = distro_acumulada_calidad_internet, width = 507 / 96, height = 414 / 96, units = "in", dpi = 96, device = "pdf")

