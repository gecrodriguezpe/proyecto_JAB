# Análisis de regresión 

# Preliminares ----

# Librerias 
library(tidyverse)
library(readxl)

# Rutas de trabajo 
matriculados_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023-2/tests_asignaturas/bases_de_datos/bases_regresiones/matriculados"
resultados_pruebas_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023-2/tests_asignaturas/bases_de_datos/bases_regresiones/resultados_pruebas"

# Importación bases de datos ----

# Importacion bases de matriculas 

setwd(matriculados_path)

# Matriculas
matricula_2020_1 = read_xlsx("Matriculados_fce_2020-1S.xlsx")
matricula_2020_2 = read_xlsx("Matriculados_fce_2020-2S.xlsx")
matricula_2021_1 = read_xlsx("Matriculados_fce_2021-1S.xlsx")
matricula_2021_2 = read_xlsx("Matriculados_fce_2021-2S.xlsx")
matricula_2022_1 = read_xlsx("Matriculados_fce_2022-1S.xlsx")
matricula_2022_2 = read_xlsx("Matriculados_fce_2022-2S.xlsx")

# Resultado de las pruebas 

setwd(resultados_pruebas_path)

resultados_pruebas = read_xlsx("resultados_pruebas.xlsx")

# Funciones ---- 

transformacion_bases = function(base){
  
  nueva_base = base %>% 
    mutate(PERIODO = as.factor(PERIODO), 
           SEXO = as.factor(SEXO),
           EDAD = as.numeric(EDAD),
           MUNICIPIO_PROCEDENCIA = as.factor(MUNICIPIO_PROCEDENCIA),
           ESTRATO = as.factor(ESTRATO),
           MATRICULAS = as.factor(MATRICULAS),
           TIPCOLEGIO = as.factor(TIPCOLEGIO), 
           PBM = as.numeric(PBM),
           MUNICIPIO_COLEGIO = as.factor(MUNICIPIO_COLEGIO),
           PROM_ACADEMICO_ACTUAL = as.numeric(PROM_ACADEMICO_ACTUAL), 
           PAPA_PERIODO = as.numeric(PAPA_PERIODO), 
           DOCUMENTO = as.factor(DOCUMENTO)) %>% 
    select(NOMBRES, APELLIDO1, APELLIDO2, DOCUMENTO, CORREO, PERIODO, SEXO, EDAD, MUNICIPIO_PROCEDENCIA, 
           ESTRATO, MATRICULAS, TIPCOLEGIO, PBM, MUNICIPIO_COLEGIO, PROM_ACADEMICO_ACTUAL, PAPA_PERIODO)
  
  return(nueva_base)
  
}

# Procesamiento de las bases de datos ----

## Matriculas 

# Transformacion bases de datos 
matricula_2020_1 = transformacion_bases(matricula_2020_1)
matricula_2020_2 = transformacion_bases(matricula_2020_2)
matricula_2021_1 = transformacion_bases(matricula_2021_1)
matricula_2021_2 = transformacion_bases(matricula_2021_2)
matricula_2022_1 = transformacion_bases(matricula_2022_1)
matricula_2022_2 = transformacion_bases(matricula_2022_2)


# Todos los matriculados 
matriculados = bind_rows(matricula_2020_1, matricula_2020_2, matricula_2021_1, matricula_2021_2, 
                         matricula_2022_1, matricula_2022_2) %>% 
  distinct(CORREO, .keep_all = TRUE)

## Resultados prueba 
  
  
# Regresión 1  

base_reg1 = inner_join(resultados_pruebas, matriculados, by = c("CORREO"))

# Resultados Test 1 ----

reg1.1 = lm(`Calificación/10.00_prueba1` ~ EDAD + ESTRATO + MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.1)
reg1.2 = lm(`Calificación/10.00_prueba1` ~ MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.2)
reg1.3 = lm(`Calificación/10.00_prueba1` ~ PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.3)
reg1.4 = lm(`Calificación/10.00_prueba1` ~ Periodo + PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.4)


# Resultados Test 2 ----

reg1.1 = lm(`Calificación/10.00_prueba2` ~ EDAD + ESTRATO + MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.1)
reg1.2 = lm(`Calificación/10.00_prueba2` ~ MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.2)
reg1.3 = lm(`Calificación/10.00_prueba2` ~ PBM + PROM_ACADEMICO_ACTUAL, data = base_reg1); summary(reg1.3)

# Regresión 2

join_matricula_2021_2 = inner_join(resultados_pruebas, matricula_2021_2, by = c("CORREO" = "CORREO", "Periodo" = "PERIODO"))
join_matricula_2022_1 = inner_join(resultados_pruebas, matricula_2022_1, by = c("CORREO" = "CORREO", "Periodo" = "PERIODO"))
join_matricula_2022_2 = inner_join(resultados_pruebas, matricula_2022_2, by = c("CORREO" = "CORREO", "Periodo" = "PERIODO"))

joins = bind_rows(join_matricula_2021_2, join_matricula_2022_1, join_matricula_2022_2) %>% 
  distinct(CORREO, .keep_all = TRUE)

# Resultados Test 1 ----

reg1.1 = lm(`Calificación/10.00_prueba1` ~ EDAD + ESTRATO + MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.1)
reg1.2 = lm(`Calificación/10.00_prueba1` ~ MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.2)
reg1.3 = lm(`Calificación/10.00_prueba1` ~ PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.3)
reg1.4 = lm(`Calificación/10.00_prueba1` ~ Periodo + PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.4)

# Resultados Test 2 ----

reg1.1 = lm(`Calificación/10.00_prueba2` ~ EDAD + ESTRATO + MATRICULAS + TIPCOLEGIO + PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.1)
reg1.3 = lm(`Calificación/10.00_prueba2` ~ PBM + PROM_ACADEMICO_ACTUAL, data = joins); summary(reg1.3)