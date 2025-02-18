library(eph)
library(tidyverse)
library(dplyr)
library(janitor)
library(srvyr)

Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "PP03I", "PP03J", "P21", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I", "DECOCUR", "PONDIIO")
EPH2019_2023CAES=get_microdata(year=2019:2023, period = 1,type = "individual", vars = Variables)

saveRDS(EPH2019_2023CAES, file = "EPH2019_2023CAES.rds")
EPH2019_2023CAES <- EPH2019_2023CAES %>% eph::organize_labels()


#Variables nuevas y recodificadas (SEXO, PP04B_COD (CAES), PP04C(CANTIDAD EMPLEADOS), Grupos etarios, ámbito del establecimiento, región, lugar de nacimiento, nivel_educativo_completo)


#SEXO
EPH2019_2023CAES=EPH2019_2023CAES %>% mutate(CH04=case_when(
  CH04==1 ~ "Varón",
  CH04==2 ~ "Mujer",
  TRUE~ "Otro"))

EPH2019_2023CAES=EPH2019_2023CAES %>% rename(SEXO=CH04)
EPH2019_2023CAES=EPH2019_2023CAES %>% rename(CANT.EMPLEADOS=PP04C)

# REGION

EPH2019_2023CAES = EPH2019_2023CAES %>% 
  mutate (region_etiqueta= factor(case_when(
    REGION ==01 ~ "AMBA", 
    REGION ==43 ~ "Centro",
    REGION ==40 ~ "NOA",
    REGION ==41 ~ "NEA",
    REGION ==42 ~ "Cuyo",
    REGION ==44 ~ "Patagonia"), 
    levels=c("AMBA", "Centro", "NEA", "Cuyo", "Patagonia", "NOA")))


#CANTIDAD DE EMPLEADOS
EPH2019_2023CAES <- EPH2019_2023CAES %>%
    mutate(CANT.EMPLEADOS = as.factor(as.numeric(CANT.EMPLEADOS))) %>%  # Convierte a numérico puro
  mutate(CANT.EMPLEADOS = case_when(
    CANT.EMPLEADOS == 0 ~ "0",
    CANT.EMPLEADOS %in% 1:6 ~ "1 a 10",
    CANT.EMPLEADOS %in% c(7,8) ~ "11 a 40",
    CANT.EMPLEADOS %in% c(9,10) ~ "41 a 200",
    CANT.EMPLEADOS %in% c(11,12)  ~ "Más de 200",
    CANT.EMPLEADOS == 99 ~ "Ns./Nr.",
    TRUE ~ NA_character_  # Captura otros valores como NA
  )) %>%
  mutate(CANT.EMPLEADOS = factor(CANT.EMPLEADOS, 
                                 levels = c("0", "1 a 10", "11 a 40", "41 a 200", "Más de 200", "Ns./Nr."), 
                                 ordered = TRUE))

class(EPH2019_2023CAES$CANT.EMPLEADOS)# "ordered" "factor" 

summary(EPH2019_2023CAES$CANT.EMPLEADOS) #no me aparecían registros para más del valor 5 porque no estaba incluida una categoría para el valor 0. 

table(EPH2019_2023CAES$CANT.EMPLEADOS)


#GRUPOS ETARIOS, AMBITO_ESTABLECIMIENTO, LUGAR_NACIMIENTO, NIVEL_EDUCATIVO,

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(
    grupos_etarios = factor(case_when(
      CH06 %in% 16:30 ~ "16 a 30", 
      CH06 %in% 31:50 ~ "31 a 50", 
      CH06 >= 51 ~ "51 o más"), 
      levels = c("16 a 30", "31 a 50", "51 o más")
    ),
    
    
    ambito_establecimiento = factor(case_when(
      PP04A == 1 ~ "Estatal", 
      PP04A == 2 ~ "Privado", 
      PP04A == 3 ~ "Otro"
    ), levels = c("Estatal", "Privado", "Otro")),
    
    lugar_nacimiento = case_when(
      CH15 %in% c(1,2) ~ "Esta provincia", 
      CH15 == 3 ~ "Otra provincia", 
      CH15 == 4 ~ "País limítrofe", 
      CH15 == 5 ~ "Otro país", 
      CH15 == 9 ~ NA_character_
    )
  )   
EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (nivel_educativo_completo= factor(case_when(NIVEL_ED %in% c (1,7) ~ "Menor a primario", NIVEL_ED %in% c (2,3) ~ "Primario", NIVEL_ED %in% c (4,5) ~ "Secundario", NIVEL_ED == 6~"Universitario"), levels = c("Menor a primario","Primario","Secundario", "Universitario")))

table(EPH2019_2023CAES$grupos_etarios)
table(EPH2019_2023CAES$ambito_establecimiento)
table(EPH2019_2023CAES$lugar_nacimiento)
table(EPH2019_2023CAES$nivel_educativo_completo)

#-Descripción de la población de la muestra 

#SEXO Y AÑO
poblacion_x_año<- EPH2019_2023CAES %>% group_by (ANO4) %>% summarize(sum(PONDERA))
sexo_año<- EPH2019_2023CAES %>% group_by (SEXO,ANO4) %>% summarize(sum(PONDERA))

#distribución y porcentajes por sexo, estado, categoría ocupacional, categoría inactividad

#sexo
sexo_año_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#grupos etarios

Muestra_Gruposetarios1=EPH2019_2023CAES %>%
  filter(!is.na(grupos_etarios)) %>%  # Filtra los valores NA
  tabyl(grupos_etarios) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

#lugar de nacimiento
Muestra_lugarnacimiento=EPH2019_2023CAES %>%
  group_by(ANO4) %>% 
  tabyl(lugar_nacimiento) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

#cant.empleados --tamaño del establecimiento
cant_empleados_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, CANT.EMPLEADOS) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#sector de actividad
sector.de.actividad_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, SECTOR.DE.ACTIVIDAD) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#condición de actividad
cond_actividad_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, ESTADO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría ocupacional:notar que hay un valor 0 que no figura en el diseño. segun el anexo: El código 0 identifica los casos a los cuales no les corresponde la secuencia analizada. ACÁ SERÍAN LOS INACTIVOS Y MENORES DE 10
categ_ocupacional_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))
#categoría de inactividad
unique(EPH2019_2023CAES$CAT_INAC)
cat_inactividad_pond <- EPH2019_2023CAES %>%  filter(!is.na(CAT_INAC)& CAT_INAC!=0 ) %>% 
  group_by(ANO4, CAT_INAC) %>% 
  summarize(casos = sum(PONDERA),.groups = "drop") %>% group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Tasas básicas del MT. Notar que utiliza la PP03j, que es la pregunta por la búsqueda de otro empleo además del que ya se tiene. Por eso uso el df con todas las variables.  

Datos_MT_1923<- base3T_19_23 %>%  
  group_by(ANO4) %>% 
  summarize(
    Poblacion          = sum(PONDERA),
    
    Ocupados          = sum(PONDERA[ESTADO == 1]),
    
    Desocupados       = sum(PONDERA[ESTADO == 2]),
    
    PNEA= sum(PONDERA[ESTADO %in% c(3, 4)]), 
    
    PEA               = Ocupados + Desocupados,
    
    Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J == 1]),
    
    Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1]),
    
    Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2, 9)]),
    
    Subocupados       = Suboc_demandante + Suboc_no_demand,
    'Tasa Actividad' = (PEA / Poblacion) * 100,
    'Tasa Inactividad' = (PNEA / Poblacion) * 100,
    'Tasa Empleo' = (Ocupados / Poblacion) * 100,
    'Tasa Desocupacion' = (Desocupados / PEA) * 100,
    'Tasa ocupados demandantes' = (Ocupados_demand / PEA) * 100,
    'Tasa Subocupación' = (Subocupados / PEA) * 100,
    'Tasa Subocupación demandante' = (Suboc_demandante / PEA) * 100,
    'Tasa Subocupación no demandante' = (Suboc_no_demand / PEA) * 100
  )  %>% # Redondear las columnas de tasas a 1 decimal
  mutate(across(starts_with("Tasa"), ~ round(., 1)))

TasasMT_1923 <- Datos_MT_1923 %>% select(ANO4, starts_with("Tasa"))


#VARIABLE PRECARIEDAD 

##CONDICIONES DE TRABAJO

EPH2019_2023CAES <- EPH2019_2023CAES %>% filter (ESTADO == 1, CAT_OCUP == 3) %>%  #ocupados asalariados
  mutate(
    Preca_cond_lab = ifelse (PP07H==2, yes = 10,no= 0)+ #descuento jubilatorio (ojo porque hay valores NA Y 0)EN este caso y en el que sigue asigné los mismos valores del indicador
      ifelse (PP07G1==2, yes = 4,no= 0)+ #VACACIONES PAGAS
      ifelse (PP07G2==2, yes = 4,no= 0)+ #aguinaldo
      ifelse (PP07G3==2, yes = 4,no= 0)+ #LICENCIAS MÉDICAS
      ifelse (PP07G4==2, yes = 8,no= 0)) #Obra social
unique(EPH2019_2023CAES$Preca_cond_lab)  #0 30 12 10  4 22 26 18  8 16 14 20
calculate_tabulates (EPH2019_2023CAES, "ANO4", "Preca_cond_lab", add.percentage = "col", "PONDERA")
summary (EPH2019_2023CAES$Preca_cond_lab) # promedio: 10 puntos de precariedad


##FORMAS DE CONTRATACIÓN

EPH2019_2023CAES <- EPH2019_2023CAES %>% filter (ESTADO == 1, CAT_OCUP == 3) %>%  #ocupados asalariados
  mutate(
    Preca_forma_contrat = ifelse (PP07C==1, yes = 10,no= 0))
calculate_tabulates (EPH2019_2023CAES, "ANO4", " Preca_forma_contrat", add.percentage = "col", "PONDERA")    
summary(EPH2019_2023CAES$ Preca_forma_contrat)


#Preca.cuentaprop (TAREA: chequear si la pp03j se cruza con cat_ocup cuentapropistas)

EPH2019_2023CAES <- EPH2019_2023CAES %>% 
  mutate(
    preca.cuentaprop = ifelse(CAT_OCUP == 2 & PP05H %in% c(1,2,3,4), 10, 0)  # Se aplica solo a CAT_OCUP == 2
  )


##PRECARIEDAD POR INGRESOS 

table(EPH2019_2023CAES$DECOCUR)
table(EPH2019_2023CAES$P21)

EPH2019_2023CAES = EPH2019_2023CAES %>%
  filter(ESTADO==1 & CAT_OCUP %in% c(2, 3)) %>%  # Filtrar empleados asalariados y cuentapropistas
  mutate(Ingreso_para_imputar = case_when(
    P21 == -9 ~ NA_real_,  # Reemplazar -9 con NA
    P21 == 0 ~ NA_real_,   # Reemplazar 0 con NA
    TRUE ~ P21             # Mantener los demás valores
  ))



#Imputo un ingreso a todos los NA (0,-9)
EPH2019_2023CAES <- EPH2019_2023CAES %>%
  filter(ESTADO == 1 & CAT_OCUP %in% c(2, 3)) %>%
  group_by(across(c(ANO4, sexo, grupos_etarios, region_etiqueta, caes_eph_cod))) %>%
  mutate(P21_imputado_AC = ifelse(is.na(Ingreso_para_imputar), mean(Ingreso_para_imputar, na.rm = TRUE), Ingreso_para_imputar)) %>%
  ungroup()


EPH2019_2023CAES %>% 
  select(sexo, ANO4, P21, P21_imputado_AC, DECOCUR_imputado, Preca_ingresos_deciles) -> base_chiquita
view(base_chiquita)


#Divido en deciles la variable P21_imputado: 

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  group_by(ANO4) %>%  # Si los deciles deben calcularse por año
  mutate(DECOCUR_imputado = cut(P21_imputado_AC,
                                breaks = quantile(P21_imputado, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                                labels = 1:10,
                                include.lowest = TRUE)) %>%
  ungroup()

#agrupo deciles en la variable Preca_ingresos_deciles

EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_ingresos_deciles= factor(case_when(
    DECOCUR_imputado %in% 1:3 ~ "Ingresos bajos", 
    DECOCUR_imputado %in% 4:8 ~ "Ingresos medios", 
    DECOCUR_imputado %in% 9:10 ~ "Ingresos altos"), 
    levels = c("Ingresos bajos", "Ingresos medios", "Ingresos altos"))
  )

round(prop.table(table(EPH2019_2023CAES$Preca_ingresos_deciles)) * 100, 2)

#asigno valores numéricos a la variable de ingresos

EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_ingresos_deciles_N= as.numeric(case_when (
    Preca_ingresos_deciles == "Ingresos bajos" ~ 30,
    Preca_ingresos_deciles == "Ingresos medios" ~ 15,
    Preca_ingresos_deciles == "Ingresos altos" ~ 0) 
  ))


#creo variable para "busqueda de otro empleo" que llamo Preca_ingresos_buscarotrotrabajo


unique(EPH2019_2023CAES$PP03J)
EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_ingresos_buscarotrotrabajo= as.numeric(case_when (
    PP03J == 1 ~ 10,
    PP03J == 2 ~ 0,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_ingresos_buscarotrotrabajo)

##INTENSIDAD EN LA JORNADA LABORAL. Creo variables pluriempleo (Preca_intensidad_pluriempleo) 

unique(EPH2019_2023CAES$PP03C)
class(EPH2019_2023CAES$PP03C)

EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_intensidad_pluriempleo= as.numeric(case_when (
    PP03C == 1 ~ 0,
    PP03C == 2 ~ 10,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_intensidad_pluriempleo)

#y sobreocupado (Preca_intensidad_sobreocup)

unique(EPH2019_2023CAES$INTENSI)
class(EPH2019_2023CAES$INTENSI)
EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_intensidad_sobreocup= as.numeric(case_when (
    INTENSI == 3 ~ 10,
    INTENSI %in% c("1", "2", "4") ~ 0,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_intensidad_sobreocup)



#creo variables por tipo de precarización y una que agrupa tres categorías de precariedad: 

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(
    Preca_ingresos = as.numeric(Preca_ingresos_deciles_N +
                                  Preca_ingresos_buscarotrotrabajo), 
    Preca_intensidad = as.numeric(Preca_intensidad_sobreocup +
                                    Preca_intensidad_pluriempleo), 
    Niveles_precariedad_total = case_when(
      precariedad_total %in% 0:30  ~ "Precariedad baja",
      precariedad_total %in% 31:60 ~ "Precariedad media",
      precariedad_total %in% 61:100 ~ "Precariedad alta",
      TRUE ~ "Otro"  # Para evitar valores NA
    )
  )


