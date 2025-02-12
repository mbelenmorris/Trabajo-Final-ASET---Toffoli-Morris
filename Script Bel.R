library(eph)
library(tidyverse)
library(dplyr)

Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I")
EPH2019_23=get_microdata(year=2019:2023, period=3,  type = "individual", vars = Variables)

#de acá en adelante nombro EPH2019_23 a la base, cambiar al nombre definitivo

#variable nueva: Grupos etarios, ámbito del establecimiento, lugar de nacimiento, nivel_educativo_completo

EPH2019_23=EPH2019_23 %>%
  mutate (grupos_etarios= factor(case_when(
    CH06 %in% 16:30 ~ "16 a 30", 
    CH06 %in% 31:50 ~ "31 a 50", 
    CH06 >= 51 ~ "51 o más"), levels = c("16 a 30", "31 a 50", "51 o más"))
    ambito_establecimiento = case_when(PP04A == 1 ~ "Estatal", PP04A ==2 ~ "Privado", PP04A == 3 ~ "Otro"), 
    lugar_nacimiento = case_when(CH15 %in% c (1,2) ~ "Esta provincia", CH15 == 3 ~ "Otra provincia", CH15 == 4 ~ "País limítrofe", CH15 == 5 ~ "Otro país", CH15 == 9  ~ NA))

EPH2019_23=EPH2019_23 %>%
  mutate (nivel_educativo_completo= factor(case_when(NIVEL_ED %in% c (1,7) ~ "Menor a primario", NIVEL_ED %in% c (2,3) ~ "Primario", NIVEL_ED %in% c (4,5) ~ "Secundario", NIVEL_ED == 6~"Universitario"), levels = c("Menor a primario","Primario","Secundario", "Universitario")))

table(EPH2019_23$grupos_etarios)
table(EPH2019_23$ambito_establecimiento)
table(EPH2019_23$lugar_nacimiento)
table(EPH2019_23$nivel_educativo_completo)



pluriempleo = EPH2019_23 %>%
  filter(ESTADO == 1, CAT_OCUP == 3)  %>%
  summarise(pluriempleados=sum (PONDERA [PP03C == 2]),
            unico_empleo=sum (PONDERA [PP03C ==1]))
summary(EPH2019_23)