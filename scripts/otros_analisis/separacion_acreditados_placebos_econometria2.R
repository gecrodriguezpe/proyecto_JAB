library(tidyverse)
library(readxl)
library(writexl)

output = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"
adicionales = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/adicionales"
matriculados_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/bases_regresiones/matriculados"

# Importación bases de datos ----

# Estudiantes econometria 2 full list 
setwd(output)

estudiantes_econometria2_full = read_xlsx("listas_estudiantes_full.xlsx", sheet = "econometria2")

# Estudiantes econometria 2 presentacion prueba
# setwd(econometria2_tests)

estudiantes_econometria2_pruebas = read_xlsx("resultados_pruebas_asignaturas.xlsx", sheet = "econometria2")

# Join ----

acreditados_econom2 = inner_join(estudiantes_econometria2_full, estudiantes_econometria2_pruebas, by = c("CORREO"))

# Placebos ----

placebo_econom2 = anti_join(estudiantes_econometria2_pruebas, estudiantes_econometria2_full, by = c("CORREO"))

# Info adicional placebos ----

setwd(matriculados_path)

# Matriculas
matricula_2020_1 = read_xlsx("Matriculados_fce_2020-1S.xlsx")
matricula_2020_2 = read_xlsx("Matriculados_fce_2020-2S.xlsx")
matricula_2021_1 = read_xlsx("Matriculados_fce_2021-1S.xlsx")
matricula_2021_2 = read_xlsx("Matriculados_fce_2021-2S.xlsx")
matricula_2022_1 = read_xlsx("Matriculados_fce_2022-1S.xlsx")
matricula_2022_2 = read_xlsx("Matriculados_fce_2022-2S.xlsx")
matricula_2023_1 = read_xlsx("Matriculados_fce_2023-1S.xlsx")
matricula_2023_2 = read_xlsx("Matriculados_fce_2023-2S.xlsx")


matriculados = bind_rows(matricula_2020_1, matricula_2020_2, matricula_2021_1, matricula_2021_2, 
                         matricula_2022_1, matricula_2022_2) %>% 
  distinct(CORREO, .keep_all = TRUE)

info_adicional_placebos = inner_join(placebo_econom2, matriculados, by = c("CORREO"))

info_adicional_placebos_trim = info_adicional_placebos %>% 
  select(CORREO, PROGRAMA_CURRICULAR)

# Escritura de información placebos ----
setwd(adicionales)

placebo_lst = list("info_test" = placebo_econom2, "info_carrera" = info_adicional_placebos_trim, "info_total" = info_adicional_placebos)

write_xlsx(placebo_lst, "placebo_econom2.xlsx")