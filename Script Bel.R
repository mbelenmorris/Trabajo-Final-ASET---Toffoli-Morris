library(eph)
library(tidyverse)
library(dplyr)

Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I")
EPH2019_23=get_microdata(year=2019:2023, period=3,  type = "individual", vars = Variables)

#de acá en adelante nombro EPH2019_23 a la base, cambiar al nombre definitivo - *"base_eph1923_acotada"*

#variable nueva: Grupos etarios, ámbito del establecimiento, lugar de nacimiento, nivel_educativo_completo

EPH2019_23=EPH2019_23 %>%
  mutate (grupos_etarios= factor(case_when(
    CH06 %in% 16:30 ~ "16 a 30", 
    CH06 %in% 31:50 ~ "31 a 50", 
    CH06 >= 51 ~ "51 o más"), levels = c("16 a 30", "31 a 50", "51 o más")),
    ambito_establecimiento = case_when(PP04A == 1 ~ "Estatal", PP04A ==2 ~ "Privado", PP04A == 3 ~ "Otro"), 
    lugar_nacimiento = case_when(CH15 %in% c (1,2) ~ "Esta provincia", CH15 == 3 ~ "Otra provincia", CH15 == 4 ~ "País limítrofe", CH15 == 5 ~ "Otro país", CH15 == 9  ~ NA))

EPH2019_23=EPH2019_23 %>%
  mutate (nivel_educativo_completo= factor(case_when(NIVEL_ED %in% c (1,7) ~ "Menor a primario", NIVEL_ED %in% c (2,3) ~ "Primario", NIVEL_ED %in% c (4,5) ~ "Secundario", NIVEL_ED == 6~"Universitario"), levels = c("Menor a primario","Primario","Secundario", "Universitario")))

table(EPH2019_23$grupos_etarios)
table(EPH2019_23$ambito_establecimiento)
table(EPH2019_23$lugar_nacimiento)
table(EPH2019_23$nivel_educativo_completo)

#PARA RECODIFICAR VARIABLE CAES (PP04B_COD)

base_recodCAES=organize_caes(EPH2019_23) #crea 8 nuevas variables con el CAES con diferente extensión (a un dígito, a dos, etc)


library(dplyr)
library(stringr)

EPH2019_23 <- EPH2019_23 %>% #RECORDAR CAMBIAR NOMBRE DE BASE
  mutate(
    Sector_actividad = case_when(
      grepl("^0[1-9]", PP04B_COD) ~ "Agricultura y minería", 
      grepl("^1[0-9]|^2[0-9]|^3[0-3]", PP04B_COD) ~ "Industria manufacturera",
      grepl("^3[5-9]", PP04B_COD) ~ "Suministro de gas, agua, electricidad y otros", 
      grepl("^40", PP04B_COD) ~ "Construcción",
      grepl("^4[5-8]|^5[5-6]", PP04B_COD) ~ "Comercio, hotelería y gastronomía",
      grepl("^4[9-9]|^5[0-3]", PP04B_COD) ~ "Transporte",
      grepl("^5[8-9]|^6[0-3]", PP04B_COD) ~ "Información y Comunicación",
      grepl("^6[4-8]", PP04B_COD) ~ "Actividades financieras, seguros, inmobiliarias",
      grepl("^6[9-9]|^7[0-5]", PP04B_COD) ~ "Actividades Profesionales, Científicas y Técnicas",
      grepl("^7[7-9]|^8[0-2]", PP04B_COD) ~ "Actividades Administrativas y Servicios de Apoyo",
      grepl("^8[3-4]", PP04B_COD) ~ "Administración Pública y Defensa",
      grepl("^8[5]", PP04B_COD) ~ "Enseñanza",
      grepl("^8[6-8]", PP04B_COD) ~ "Salud Humana y Servicios Sociales",
      grepl("^9[0-3]", PP04B_COD) ~ "Artes, Entretenimiento y Recreación",
      grepl("^9[4-6]", PP04B_COD) ~ "Otras Actividades de Servicios",
      TRUE ~ "Otros"
    )
  )


#PARA TRABAJAR PLURIEMPLEO

pluriempleo = EPH2019_23 %>%
  filter(ESTADO == 1, CAT_OCUP == 3)  %>%
  summarise(pluriempleados=sum (PONDERA [PP03C == 2]),
            unico_empleo=sum (PONDERA [PP03C ==1]))
summary(EPH2019_23)