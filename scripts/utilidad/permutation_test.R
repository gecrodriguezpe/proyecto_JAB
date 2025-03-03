# Permutation Test: Script de test de permutación 
#
# Función principal: 
## test_de_permutacion: Realiza el test de permutación
# 
# Etapas para realizar el test de permutación
## 1. Iterar a través de 
## 2. 
## 3. 
## 4. "Permutation test"


# Paquetes 
library(tidyverse)
library(broom)


# Modulos externos 
source(glue("{here()}/scripts/utilidad/util.R"))


# Función de distribución acumulada (CDF) ---------------------------------

graficacion_CDF = function(params, coeficiente_original){
  
    # Generación de la CDF empírica
    cdf = ggplot(params, aes(x = coef)) +     
      stat_ecdf(geom = "step", color = "blue") +
      geom_vline(xintercept = coeficiente_original, linetype = "dashed", color = "red") + 
      # labs(x = "Coeficiente estimado",
      #      y = "Función de distribución acumulada empírica") +
      theme_minimal()
  
  return(cdf)
  
}

# 1. Función: "Generación de variable sintética" -----------------------------

test_de_permutacion = function(df_original, formula_estimacion, coef_interes, estimacion_original){
  
  # Coeficiente de la variable de interés de la regresión original 
  original_coef_interest = tidy(estimacion_original) %>% filter(term == coef_interes)
  
  # Coeficiente original y p-value original 
  coeficiente_original = as.numeric(original_coef_interest$estimate)
  p_value_original = as.numeric(original_coef_interest$p.value)
  
  # Dataframe que almacenará los valores del coeficiente de la variable de interés en cada estimación sintética 
  params = data.frame(coef = numeric(), p_value = numeric())
  
  # Número de filas de la base de datos sobre las que se itera 
  nrows = nrow(df_original)
  
  # Contador para llevar trazabilidad de cuántas permutaciones se han realizado. En total se deberían realizar "n_virtuales * n_presenciales" permutaciones
  cont_perm = 1
  
  # for loop para identicar las observaciones "virtuales"
  for (i in 1:nrows){
    
    # Solo me concentro en las ovservaciones virtuales para hacer el "swap"
    if (df_original$Periodo_agregado[i] == "virtual"){
      
      # for loop para identificar las observaciones "presenciales"
      for (j in 1:nrows){
        
        # Solo me concentro en las observaciones presenciales para hacer el "swap"
        if (df_original$Periodo_agregado[j] == "presencial"){
          
          # Generación de una nueva base de datos que va a contener la columna con el "swap" de la permutación y que se usa en la estimación 
          df_new = df_original 
          
          # Swap de las dos observaciones entre virtuales y presenciales (permutación)
          df_new$Periodo_agregado[i] = "presencial"
          df_new$Periodo_agregado[j] = "virtual"
          
          # Construcción de la base de datos tipo panel para la estimación
          df_panel = generacion_base_datos_panel(df_new, Periodo_agregado)
          
          # Estimación del modelo con la nueva base de datos 
          estimacion_sintetico = feols(formula_estimacion, 
                                       data = df_panel, 
                                       panel.id = c("CORREO", "periodos_pruebas"),
                                       cluster = ~prueba,
                                       verbose = 0); 
          
          # Resultados de la estimación en formato de data frame 
          res_estimacion = tidy(estimacion_sintetico) %>% filter(term == coef_interes)
          
          # Llenado del data frame que contiene los coeficientes de la variable de interés de cada estimación sintética
          params[cont_perm, ] = c(res_estimacion$estimate, res_estimacion$p.value)
          
          # Actualización del contador que lleva trazabilidad del número de permutaciones realizadas
          cont_perm = cont_perm + 1
          
        }
        
      }
      
    }
    
  }  
  
  # Graficación test de permutación
  grafica_test_permutacion = graficacion_CDF(params, coeficiente_original)
  print(grafica_test_permutacion)
  
  # return(params)
  
}
  


# Prueba de la función 

# res_test = test_de_permutacion(df_original = base_limpia, 
#                     formula_estimacion = fe1_formula, 
#                     coef_interes = "I(EX2 * AV)", 
#                     estimacion_original = fe1 )
