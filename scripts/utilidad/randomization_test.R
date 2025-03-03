# Randomness Test: Script de test de aleatorización 
#
# Función principal: 
## test_de_randomizacion: Realiza el test de randomización
# 
# Etapas para realizar el test de randomización  
### 1. Generación de variable sintética
### 2. Construcción base de datos panel 
### 3. Histograma del coeficiente
### 4. "Randomization test"


library(tidyverse)
library(broom)


# Modulos externos 
source(glue("{here()}/scripts/utilidad/util.R"))


# 1. Función: "Generación de variable sintética" -----------------------------

# Nota: En nuestro caso particular la variable sintética será "Periodo_agregado_sintetico" que asigna de manera aleatoria
#       los grupos "placebos", "virtual" y "presencial" a cada una de las observaciones de la base de datos

#' Title
#'
#' @param df 
#'
#' @return df
#' @export
#'
#' @examples
generacion_variable_sintetica = function(df){

  # Número de filas de la base de datos sobre las que se itera 
  nrows = nrow(df)
  
  # Vector que almacena el número de observaciones por grupo tratado 
  grupos_experimento = summary(df$Periodo_agregado)
  
  # Número total de estudiantes para cada uno de los grupos del experimento: Placebo, virtuales y presenciales
  t_placebos = grupos_experimento["placebos"]
  t_virtuales = grupos_experimento["virtual"] 
  t_presenciales =  grupos_experimento["presencial"]
  
  
  # Variable sintética: Se va a llenar con el for loop, agregando grupos 
  var_sintetica = c()
  
  # Incialización del loop
  n_placebos = 0
  n_virtuales = 0
  n_presenciales = 0
  
  # Loop que itera a través de la base de datos
  for (fila in 1:nrows){
    
    # Inicialización del while 
    
    flag_while = TRUE
    
    while (flag_while){
      
      # Se usa para asignar aleatoriamente 
      numero_aleatorio = runif(1)  
      
      # Asignación sintética del grupo a cada observación
      if ( n_placebos < t_placebos &&  0 < numero_aleatorio && numero_aleatorio < 1/3){
        var_sintetica = append(var_sintetica, "placebos")
        n_placebos = n_placebos + 1 
        flag_while = FALSE
      }else if(n_virtuales < t_virtuales &&  1/3 < numero_aleatorio && numero_aleatorio < 2/3){
        var_sintetica = append(var_sintetica, "virtual")
        n_virtuales = n_virtuales + 1 
        flag_while = FALSE
      }else if(n_presenciales < t_presenciales &&  2/3 < numero_aleatorio && numero_aleatorio < 1){
        var_sintetica = append(var_sintetica, "presencial")
        n_presenciales = n_presenciales + 1 
        flag_while = FALSE
      }
    }
  }
  
  # Se agrega la variable sintética al df
  df$Periodo_agregado_sintetico =  as.factor(var_sintetica)
  
  return(df)

}

# 2. Función: "Construcción base datos panel" -------------------------------------

#' Title
#'
#' @param df_pre_panel 
#'
#' @return
#' @export
#'
#' @examples
# generacion_base_datos_panel = function(df_pre_panel){
#  
#   # Transformación de la base de datos a formato de datos panel 
#   df_panel = df_pre_panel %>% 
#     pivot_longer(cols = starts_with("Calificación/10.00_prueba"),
#                  names_to = "periodos_pruebas",
#                  values_to = "calificacion_pruebas") %>% 
#     select(CORREO, periodos_pruebas, calificacion_pruebas, everything()) %>% 
#     mutate(periodos_pruebas = str_replace(periodos_pruebas, "Calificación/10.00_prueba", ""))
#   
#   # Creción de las Dummies a partir de la "variable sintética" creada
#   df_panel = df_panel %>%
#     mutate(AV = ifelse(Periodo_agregado_sintetico == "virtual", 1, 0),
#            AP = ifelse(Periodo_agregado_sintetico == "presencial", 1, 0),
#            PL = ifelse(Periodo_agregado_sintetico == "placebos", 1, 0),
#            INST = ifelse(Periodo_agregado_sintetico == "placebos", 0, 1),
#            EX2 = ifelse(periodos_pruebas == 2, 1, 0))
#   
#   # Agregar variables adicionales relacionadas con los tests
#   df_panel = df_panel %>% 
#     mutate(Tiempo_requerido_prueba = ifelse(periodos_pruebas == 1, `Tiempo requerido_prueba1`, `Tiempo requerido_prueba2`),
#            minutos_prueba = as.numeric(str_extract(`Tiempo_requerido_prueba`, "\\d+")),
#            segundos_prueba = as.numeric(str_extract(str_remove(`Tiempo_requerido_prueba`, "\\d+ minutos "), "\\d+")),
#            tiempo_total_prueba = minutos_prueba * 60 + segundos_prueba)
#    
#   return(df_panel)
# }

# 3. Función: "Histograma del coeficiente" --------------------------------

#' Title
#'
#' @param params 
#' @param coeficiente_original 
#' @param p_value_original 
#'
#' @return
#' @export
#'
#' @examples
graficacion_histograma = function(params, coeficiente_original, p_value_original){
  
  # Generación de la gráfica con la densidad y el scatterplot
  histograma = ggplot(params, aes(x = coef)) +
    # Scatterplot: coef vs. p_value
    geom_point(aes(y = p_value), color = "blue") +
    geom_hline(yintercept = p_value_original, linetype = "dashed", color = "red") +  
    geom_vline(xintercept = coeficiente_original, linetype = "dashed", color = "red")+    
    # Density plot: scale density to match the range of p_value
    geom_density(aes(y = ..density.. * max(params$p_value))) +
    # Use a secondary axis for the density scale
    scale_y_continuous(
      name = "p_value",
      sec.axis = sec_axis(~ . / max(params$p_value), name = "Density")
    ) +
    theme_light()  
  
  return(histograma)
  
}


# 4. "Función: Randomization test" ------------------------------------

#' Title
#'
#' @param df_original 
#' @param formula_estimacion 
#' @param coef_interes 
#' @param estimacion_original 
#' @param sample_size 
#'
#' @return
#' @export
#'
#' @examples
test_de_randomizacion = function(df_original, formula_estimacion, coef_interes, estimacion_original, sample_size){
  
  # Coeficiente de la variable de interés de la regresión original 
  original_coef_interest = tidy(estimacion_original) %>% filter(term == coef_interes)
  
  # Coeficiente original y p-value original 
  coeficiente_original = as.numeric(original_coef_interest$estimate)
  p_value_original = as.numeric(original_coef_interest$p.value)
  
  # Dataframe que almacenará los valores del coeficiente de la variable de interés en cada estimación sintética 
  params = data.frame(coef = numeric(), p_value = numeric())
  
  # for loop para generar las muestras sintéticas y estimar las regresiones en base a las muestras sintéticas 
  for (i in 1:sample_size){
    
    # 1. Generación de la variable sintética 
    df = generacion_variable_sintetica(df_original)
    
    # 2. Construcción de la base de datos para la estimación 
    df_panel = generacion_base_datos_panel(df, Periodo_agregado_sintetico)
    
    # 3. Estimación del modelo 
    estimacion_sintetico = feols(formula_estimacion, 
                                 data = df_panel, 
                                 panel.id = c("CORREO", "periodos_pruebas"),
                                 cluster = ~prueba,
                                 verbose = 0); 
    
    # Resultados de la estimación en formato de data frame 
    res_estimacion = tidy(estimacion_sintetico) %>% filter(term == coef_interes)
    
    # Llenado del data frame que contiene los coeficientes de la variable de interés de cada estimación sintética
    params[i, ] = c(res_estimacion$estimate, res_estimacion$p.value)
    
  }
  
  # Graficación test de randomización
  grafica_test_randomizacion = graficacion_histograma(params, coeficiente_original, p_value_original)  
  print(grafica_test_randomizacion)
  
  
  # return(params)  
  
}
