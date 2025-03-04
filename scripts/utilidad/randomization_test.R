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

# 3. Función: "Histograma del coeficiente" --------------------------------



graficacion_histograma = function(params_lst, 
                                  p_value = NULL, 
                                  x_label, 
                                  y_label_p_value,
                                  y_label_density,
                                  bw = "nrd0", 
                                  adjust = 1, 
                                  kernel = "gaussian") {
  
  # Coeficientes estimados en el test de randomización
  params = params_lst$parametros
  
  # Coeficiente original y 
  coeficiente_original = as.numeric(params_lst$coefientes_originales$estimate)
  
  # Para determinar el p_value que va a estar en la gráfica 
  if(is.null(p_value)){
    p_value_original = as.numeric(params_lst$coefientes_originales$p.value)  
  }else{
    p_value_original = p_value
  }
  
  # Generación de la gráfica con la densidad y el scatterplot
  histograma = ggplot(params, aes(x = coef)) +
    # Scatterplot: coef vs. p_value
    geom_point(aes(y = p_value), color = "blue") +
    geom_hline(yintercept = p_value_original, linetype = "dashed", color = "red") +  
    geom_vline(xintercept = coeficiente_original, linetype = "dashed", color = "red") +   
    # Kernel Density Estimation with parameterized bandwidth, adjust, and kernel
    geom_density(aes(y = after_stat(density) * max(params$p_value)), 
                 bw = bw, adjust = adjust, kernel = kernel) +
    # Use a secondary axis for the density scale
    scale_y_continuous(
      name = y_label_p_value,
      sec.axis = sec_axis(~ . / max(params$p_value), name = y_label_density)
    ) +
    labs(x = x_label) +
    theme_light()  
  
  return(histograma)
}




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
# graficacion_histograma = function(params_lst, coeficiente_original, p_value_original, x_label, y_label){
# 
#   # Coeficientes estimados en el test de randomización
#   params = params_lst$parametros
# 
#   # Coeficiente original y p-value original
#   coeficiente_original = as.numeric(params_lst$coefientes_originales$estimate)
#   p_value_original = as.numeric(params_lst$coefientes_originales$p.value)
# 
#   # Generación de la gráfica con la densidad y el scatterplot
#   histograma = ggplot(params, aes(x = coef)) +
#     # Scatterplot: coef vs. p_value
#     geom_point(aes(y = p_value), color = "blue") +
#     geom_hline(yintercept = p_value_original, linetype = "dashed", color = "red") +
#     geom_vline(xintercept = coeficiente_original, linetype = "dashed", color = "red")+
#     # Kernel Density Estimation
#     # Density plot: scale density to match the range of p_value
#     geom_density(aes(y = ..density.. * max(params$p_value))) +
#     # Use a secondary axis for the density scale
#     scale_y_continuous(
#       name = "p_value",
#       sec.axis = sec_axis(~ . / max(params$p_value), name = "Density")
#     ) +
#     labs(x = x_label,
#          y = y_label) +
#     theme_light()
# 
#   return(histograma)
# 
# }


# Coeficientes significativos test de permuación ------------------------------------------

coef_signif_test_randomizacion = function(df, p_value_umbral, coef_negativo){
  
  # Calcular el número de coeficientes significativos 
  
  if (coef_negativo){
    
    coef_sig = df %>%
      filter(coef < 0) %>% 
      filter(p_value < p_value_umbral) %>% 
      nrow()  
    
    mensaje = glue("El número de coeficientes negativos significativos al {p_value_umbral} es: {coef_sig}")
    
  }else{
    
    coef_sig = df %>%
      filter(coef > 0) %>% 
      filter(p_value < p_value_umbral) %>% 
      nrow()  
    
    mensaje = glue("El número de coeficientes positivos significativos al {p_value_umbral} es: {coef_sig}")
    
  }
  
  print(mensaje)
}




# 4. "Función: Randomization test" ------------------------------------

# TODO: Explorar la posibilidad de hacer bootstrapping en lugar de asumir una distribución específica
# TODO: Revisar la sección de "bootstrapping" de "Sampling with R" en DataCamp para ello

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
test_de_randomizacion = function(df_original, 
                                 formula_estimacion, 
                                 coef_interes, 
                                 estimacion_original, 
                                 sample_size, 
                                 seed = NULL,
                                 full_randomness = TRUE,
                                 distro_synthetic_score = rnorm){
  
  # Especificar la semilla en caso tal de que el usuario haya asignado alguna semilla
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  # Número de filas de la base de datos sobre las que se itera 
  nrows = nrow(df_original)
  
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
    
    # df que se genera si el usuario selecciona la opcion "full_randomness = TRUE"
    if (full_randomness){
      
      # Media y desviación estándar de la primera etapa 
      mean_prueba1 = mean(df$`Calificación/10.00_prueba1`) 
      sd_prueba1 = sd(df$`Calificación/10.00_prueba1`) 
      
      # Media y desviación estándar de la segunda etapa 
      mean_prueba2 = mean(df$`Calificación/10.00_prueba2`) 
      sd_prueba2 = sd(df$`Calificación/10.00_prueba2`)       
      
      # Variable sintéticas
      df = df %>% 
        mutate(`Calificación/10.00_prueba1` = distro_synthetic_score(nrows, mean_prueba1, sd_prueba1),
               `Calificación/10.00_prueba2` = distro_synthetic_score(nrows, mean_prueba2, sd_prueba2))
      
    }
    
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
  # grafica_test_randomizacion = graficacion_histograma(params, 
  #                                                     coeficiente_original, 
  #                                                     p_value_original,
  #                                                     x_label = "Coeficiente estimado", 
  #                                                     y_label = "P-value")  
  # print(grafica_test_randomizacion)
  
  # Resultados del test
  lst_resultados = list("parametros" = params, 
                         "coefientes_originales" = original_coef_interest)
  
  return(lst_resultados)
  
}
