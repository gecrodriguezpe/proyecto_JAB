# Script utilidad: Contiene funciones de utilidad para el resto de los scripts del proyecto 
#
# Funciones:
### generacion_base_datos_panel: Construcción base de datos panel


# Generación base de datos panel ------------------------------------------

#' Title
#'
#' @param df_pre_panel 
#'
#' @return
#' @export
#'
#' @examples
generacion_base_datos_panel = function(df_pre_panel, variable_de_interes){
  
  # Transformación de la base de datos a formato de datos panel 
  df_panel = df_pre_panel %>% 
    pivot_longer(cols = starts_with("Calificación/10.00_prueba"),
                 names_to = "periodos_pruebas",
                 values_to = "calificacion_pruebas") %>% 
    select(CORREO, periodos_pruebas, calificacion_pruebas, everything()) %>% 
    mutate(periodos_pruebas = str_replace(periodos_pruebas, "Calificación/10.00_prueba", ""))
  
  # Creción de las Dummies a partir de la "variable sintética" creada
  df_panel = df_panel %>%
    mutate(AV = ifelse({{variable_de_interes}} == "virtual", 1, 0),
           AP = ifelse({{variable_de_interes}} == "presencial", 1, 0),
           PL = ifelse({{variable_de_interes}} == "placebos", 1, 0),
           INST = ifelse({{variable_de_interes}} == "placebos", 0, 1),
           EX2 = ifelse(periodos_pruebas == 2, 1, 0))
  
  # Agregar variables adicionales relacionadas con los tests
  df_panel = df_panel %>% 
    mutate(Tiempo_requerido_prueba = ifelse(periodos_pruebas == 1, `Tiempo requerido_prueba1`, `Tiempo requerido_prueba2`),
           minutos_prueba = as.numeric(str_extract(`Tiempo_requerido_prueba`, "\\d+")),
           segundos_prueba = as.numeric(str_extract(str_remove(`Tiempo_requerido_prueba`, "\\d+ minutos "), "\\d+")),
           tiempo_total_prueba = minutos_prueba * 60 + segundos_prueba)
  
  return(df_panel)
}

