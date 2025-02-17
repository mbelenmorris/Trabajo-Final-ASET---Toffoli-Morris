library(eph)
library(tidyverse)
library(dplyr)
library(janitor)
library(srvyr)

Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "PP03I", "PP03J", "P21", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I", "DECOCUR", "PONDIIO")
EPH2019_2023CAES=get_microdata(year=2019:2023, period = 1,type = "individual", vars = Variables)

EPH2019_2023CAES = EPH2019_2023CAES

#variable nueva: Grupos etarios, ámbito del establecimiento, lugar de nacimiento, nivel_educativo_completo

EPH2023_1 <- EPH2023_1 %>%
  mutate(
    grupos_etarios = factor(case_when(
      CH06 %in% 16:30 ~ "16 a 30", 
      CH06 %in% 31:50 ~ "31 a 50", 
      CH06 >= 51 ~ "51 o más"), 
      levels = c("16 a 30", "31 a 50", "51 o más")
    ),
    
    sexo = case_when(
      CH04 == 1 ~ "Hombre", 
      CH04 == 2 ~ "Mujer"
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
EPH2023_1=EPH2023_1 %>%
  mutate (nivel_educativo_completo= factor(case_when(NIVEL_ED %in% c (1,7) ~ "Menor a primario", NIVEL_ED %in% c (2,3) ~ "Primario", NIVEL_ED %in% c (4,5) ~ "Secundario", NIVEL_ED == 6~"Universitario"), levels = c("Menor a primario","Primario","Secundario", "Universitario")))

table(EPH2023_1$grupos_etarios)
table(EPH2023_1$ambito_establecimiento)
table(EPH2023_1$lugar_nacimiento)
table(EPH2023_1$nivel_educativo_completo)

#acá me fijé si había planes sociales de empleo entre los ingresos laborales 
unique(EPH2019_23$PP04B_COD)
"8300" %in% EPH2019_23$PP04B_COD

#cuartiles de ingresoslaborales 
summary(EPH2023_1$P21)
quantile(EPH2023_1$P21, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = FALSE) #esto me mostraba la distribución con 0

base_2023etiquetada=organize_labels(EPH2023_1, type = "individual")


EPH2023_1 %>% 
  filter(P21 >0) %>%
  summarise(Min = min(P21, na.rm = TRUE),
            
            Q1 = quantile(P21, 0.25, na.rm = TRUE),
            Mediana = median(P21, na.rm = TRUE),
            Q3 = quantile(P21, 0.75, na.rm = TRUE),
            Max = max(P21, na.rm = TRUE)
  )


EPH2023_1 <- EPH2023_1 %>%
  mutate(DECOCUR = as.numeric(DECOCUR)) #para calcular cuartiles tengo que hacerla numérica

EPH2023_1 %>% 
  filter(P21 >0) %>%
  summarise(Min = min(DECOCUR, na.rm = TRUE),
            
            Q1 = quantile(DECOCUR, 0.25, na.rm = TRUE),
            Mediana = median(DECOCUR, na.rm = TRUE),
            Q3 = quantile(DECOCUR, 0.75, na.rm = TRUE),
            Max = max(DECOCUR, na.rm = TRUE)
  )

# Calcular deciles excluyendo 0 y -9
quantile(EPH2023_1$P21 [!EPH2023_1$P21 %in% c(0, -9)], probs = seq(0, 1, by = 0.1), na.rm = TRUE)

library(janitor)
library(dplyr)

# Tabla bivariada con conteo
EPH2023_1%>% 
  tabyl(CAT_OCUP, PP07H) %>%
  adorn_totals("both")  # Agrega totales



#-Descripción de la población de la muestra 
#(sexo, edad, estado, cat. ocupacional, cat.inac. lugar de nacimiento, 
#tasa de actividad, empleo y desocupación)

#grupos etarios
library(janitor)
library(scales)

Muestra_Gruposetarios1=EPH2019_23 %>%
  filter(!is.na(grupos_etarios)) %>%  # Filtra los valores NA
  tabyl(grupos_etarios) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

Muestra_Gruposetarios2=calculate_tabulates(base = EPH2019_23,
                                           x = "grupos_etarios",
                                           add.totals = "row",
                                           add.percentage = "col")


#lugar de nacimiento
Muestra_lugarnacimiento=EPH2019_23 %>%
  group_by(ANO4) %>% 
  tabyl(lugar_nacimiento) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

#ESTADO
class(EPH2019_23$ESTADO)
unique(EPH2019_23$ESTADO)

#recodifico variable

EPH2023_1 <- EPH2023_1 %>%
  mutate(ESTADO_recod = case_when(
    ESTADO == "1" ~ "Ocupado",
    ESTADO == "2" ~ "Desocupado",
    ESTADO %in% c("3", "4") ~ "Inactivo",
    ESTADO == "4" ~ "Menor de 10 años", 
    ESTADO == "0" ~ NA_character_,
  ))

#pido resultado 
Muestra_ESTADO=EPH2023_1 %>%
  tabyl(ESTADO_recod) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

Muestra_ESTADO2=EPH2023_1 %>%
  group_by(ANO4) %>% 
  count(ESTADO_recod) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  arrange(ANO4)  # Opcional: Ordena por año

cantidades=  EPH2023_1 %>% 
  group_by(ANO4) %>%
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]), 
            Inactivos       = sum(PONDERA[ESTADO == 3]))


EPH2023_1 <- EPH2023_1 %>%
  mutate(ESTADO = case_when(
    ESTADO == 0 ~ NA_real_,  # Cambia 0 a NA
    TRUE ~ as.numeric(ESTADO)         # Mantiene los demás valores
  ))

ESTADO_PorAno=calculate_tabulates(EPH2023_1, x= "ANO4", y= "ESTADO", weights = "PONDERA", digits = 1, affix_sign = T, add.percentage = "row")
colnames(ESTADO_PorAno) <- c("AÑO", "Ocupado", "Desocupado", "Inactivo", "Menor de 10 años")           


#categoría ocupacional 


Muestra_CAT_OCUP=EPH2023_1 %>% 
  group_by(ANO4) %>% 
  filter(ESTADO==1) %>% 
  count(CAT_OCUP) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  arrange(ANO4)  # Opcional: Ordena por año


EPH2023_1 <- EPH2023_1 %>%
  mutate(CAT_OCUP = case_when(
    CAT_OCUP == 9 ~ NA_real_,  # Cambia 9 a NA
    TRUE ~ as.numeric(CAT_OCUP)         # Mantiene los demás valores
  ))


#ESTE ES EL QUE MÁS ME GUSTA!: 
Categoria_ocupacional_PorAno=calculate_tabulates(EPH2023_1, x= "ANO4", y= "CAT_OCUP", weights = "PONDERA", digits = 1, affix_sign = T, add.percentage = "row")
colnames(Categoria_ocupacional_PorAno) <- c("AÑO", "Patrón", "Cuenta propia", "Obrero o empleado", "Trabajador familiar sin remuneración", "NS/NR")           


#Tasas del MT



tasas <- EPH2023_1 %>% 
  group_by(ANO4) %>% 
  summarise(
    Poblacion = sum(PONDERA),
    Ocupados = sum(PONDERA[ESTADO == 1]),
    Desocupados = sum(PONDERA[ESTADO == 2]),
    Inactivos = sum(PONDERA[ESTADO == 3]),
    Poblacion_mayor_10 = sum(PONDERA[ESTADO %in% c(1, 2, 3)]),
    PEA = Ocupados + Desocupados,
    
    # Primero calculamos Ocupados_demand, y las otras variables en el mismo paso
    Ocupados_demand = sum(PONDERA[ESTADO == 1 & PP03J == 1]),
    Suboc_demandante = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1]),
    Suboc_no_demand = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2, 9)]),
    Subocupados = Suboc_demandante + Suboc_no_demand,
    
    # Luego calculamos las tasas
    'Tasa Actividad' = (PEA / Poblacion) * 100,
    'Tasa Empleo' = (Ocupados / Poblacion) * 100,
    'Tasa Desocupacion' = (Desocupados / PEA) * 100,
    'Tasa ocupados demandantes' = (Ocupados_demand / PEA) * 100,
    'Tasa Subocupación' = (Subocupados / PEA) * 100,
    'Tasa Subocupación demandante' = (Suboc_demandante / PEA) * 100,
    'Tasa Subocupación no demandante' = (Suboc_no_demand / PEA) * 100,
    'Tasa Inactividad' = (Inactivos / Poblacion_mayor_10) * 100
  )

unique(EPH2023_1$DECOCUR)


#VARIABLE PRECARIEDAD 

#Precariedad por ingresos 

table(EPH2023_1$DECOCUR)
table(EPH2023_1$P21)

EPH2023_1 = EPH2023_1 %>%
  filter(ESTADO==1 & CAT_OCUP %in% c(2, 3)) %>%  # Filtrar empleados asalariados y cuentapropistas
  mutate(Ingreso_para_imputar = case_when(
    P21 == -9 ~ NA_real_,  # Reemplazar -9 con NA
    P21 == 0 ~ NA_real_,   # Reemplazar 0 con NA
    TRUE ~ P21             # Mantener los demás valores
  ))

#genero variables para imputar (sexo, grupos_etarios, region_etiqueta, sector de actividad). La que me faltaba era región: 

unique(EPH2023_1$REGION)
EPH2023_1 = EPH2023_1 %>% 
  mutate (region_etiqueta= factor(case_when(
    REGION ==01 ~ "AMBA", 
    REGION ==43 ~ "Centro",
    REGION ==40 ~ "NOA",
    REGION ==41 ~ "NEA",
    REGION ==42 ~ "Cuyo",
    REGION ==44 ~ "Patagonia"), 
    levels=c("AMBA", "Centro", "NEA", "Cuyo", "Patagonia", "NOA")))

EPH2019_2023CAES = organize_caes(EPH2023_1)

#de acá en adelante, empiezo a trabajar con la base EPH_2023_1_CAES porque es la que tiene estas etiquetas




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

#agrupo deciles en la variable Preca_ingresos_deciles, me falta asignarle 30 a bajos, 15 a medios, 0 a altos.
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


#creo variable para "buscó trabajar más horas" que llamo Preca_ingresos_trabajarmashoras

unique(EPH2019_2023CAES$PP03I)
class(EPH2019_2023CAES$PP03I)

EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_ingresos_trabajarmashoras= as.numeric(case_when (
    PP03I == 1 ~ 10,
    PP03I == 2 ~ 0,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_ingresos_trabajarmashoras)

#por si nos parece mejor, creo variable "busqueda de otro empleo" (tiene más casos el "si" que en "buscó trabajar más horas; me quedaría con esta)

unique(EPH2019_2023CAES$PP03J)
EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_ingresos_buscarotrotrabajo= as.numeric(case_when (
    PP03J == 1 ~ 10,
    PP03J == 2 ~ 0,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_ingresos_buscarotrotrabajo)

#INTENSIDAD EN LA JORNADA LABORAL. Creo variables pluriempleo (Preca_intensidad_pluriempleo) 

unique(EPH2019_2023CAES$PP03C)
class(EPH2019_2023CAES$PP03C)

EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_intensidad_pluriempleo= as.numeric(case_when (
    PP03C == 1 ~ 0,
    PP03C == 2 ~ 10,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_intensidad_pluriempleo)

#y sobreocupado (preca_intensidad_sobreocup)

unique(EPH2019_2023CAES$INTENSI)
class(EPH2019_2023CAES$INTENSI)
EPH2019_2023CAES=EPH2019_2023CAES %>%
  mutate (Preca_intensidad_sobreocup= as.numeric(case_when (
    INTENSI == 3 ~ 10,
    INTENSI %in% c("1", "2", "4") ~ 0,
    TRUE ~ NA_real_) 
  ))
table(EPH2019_2023CAES$Preca_intensidad_sobreocup)


#empiezo a crear variable de resumen:

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(precariedad_total = as.numeric(Preca_ingresos_deciles_N +
                                          Preca_ingresos_buscarotrotrabajo +
                                          Preca_intensidad_sobreocup +
                                          Preca_intensidad_pluriempleo))
unique(EPH2019_2023CAES$precariedad_total)
table(EPH2019_2023CAES$precariedad_total)


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


#calculo cantidad de asalariados precarios por categoria


asalariados_precarios <- EPH2019_2023CAES %>% 
  filter(ESTADO == 1, CAT_OCUP == 3) %>% 
  summarise(
    Preca_ingresos = sum(PONDERA[Preca_ingresos_deciles_N ==30], na.rm = TRUE) + 
      sum(PONDERA[Preca_ingresos_buscarotrotrabajo > 0], na.rm = TRUE),
    Preca_intensidad = sum(PONDERA[Preca_intensidad_pluriempleo > 0], na.rm = TRUE) + 
      sum(PONDERA[Preca_intensidad_sobreocup > 0], na.rm = TRUE), 
    Asalariados = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3], na.rm = TRUE)
  )
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_ingresos = (Preca_ingresos / Asalariados)*100)
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_intensidad = (Preca_intensidad / Asalariados)*100)


colnames(EPH2019_2023CAES)

tabulados <- list(
  g_etarios_x_precariedad = calculate_tabulates(EPH2019_2023CAES, 
                                                x = "grupos_etarios", 
                                                y = "Niveles_precariedad_total", 
                                                weights = "PONDERA", 
                                                add.percentage = "row"),
  
  ambito_x_precariedad = calculate_tabulates(EPH2019_2023CAES, 
                                             x = "ambito_establecimiento", 
                                             y = "Niveles_precariedad_total", 
                                             weights = "PONDERA", 
                                             add.percentage = "row"),
  
  sexo_x_precariedad = calculate_tabulates(EPH2019_2023CAES, 
                                           x = "CH04", 
                                           y = "Niveles_precariedad_total", 
                                           weights = "PONDERA", 
                                           add.percentage = "row"),
  
  lugarnacimiento_x_precariedad = calculate_tabulates(EPH2019_2023CAES, 
                                                      x = "lugar_nacimiento", 
                                                      y = "Niveles_precariedad_total", 
                                                      weights = "PONDERA", 
                                                      add.percentage = "row"),
  
  ambito_x_precaingresos = calculate_tabulates(EPH2019_2023CAES, 
                                               x = "ambito_establecimiento", 
                                               y = "Preca_ingresos", 
                                               weights = "PONDERA", 
                                               add.percentage = "row"), 
  ambito_x_precaintensidad = calculate_tabulates(EPH2019_2023CAES, 
                                                 x = "ambito_establecimiento", 
                                                 y = "Preca_intensidad", 
                                                 weights = "PONDERA", 
                                                 add.percentage = "row")
  
  
)


rm(wb)
wb <- createWorkbook()
for (ambito_x_precariedad  in names(tabulados)) {addWorksheet(wb, "g_etarios_x_precariedad") 
  writeData(wb, "g_etarios_x_precariedad", tabulados[[g_etarios_x_precariedad]])  
}
for (g_etarios_x_precariedad in names(tabulados)) {addWorksheet(wb, "ambito_x_precariedad") 
  writeData(wb, "ambito_x_precariedad", tabulados[[ambito_x_precariedad]])  
}

for (sexo_x_precariedad in names(tabulados)) {addWorksheet(wb, "sexo_x_precariedad") 
  writeData(wb, "sexo_x_precariedad", tabulados[[sexo_x_precariedad]])  
}

for (lugarnacimiento_x_precariedad in names(tabulados)) {addWorksheet(wb, "lugarnacimiento_x_precariedad") 
  writeData(wb, "lugarnacimiento_x_precariedad", tabulados[[lugarnacimiento_x_precariedad]])  
}

for (ambito_x_precaingresos in names(tabulados)) {addWorksheet(wb, "ambito_x_precaingresos") 
  writeData(wb, "ambito_x_precaingresos", tabulados[[ambito_x_precaingresos]])  
}
for (ambito_x_precaintensidad in names(tabulados)) {addWorksheet(wb, "ambito_x_precaintensidad") 
  writeData(wb, "ambito_x_precaintensidad", tabulados[[ambito_x_precaintensidad]])  
}

#Guardo resultados: 


saveWorkbook(wb, "tabulados_resultados.xlsx", overwrite = TRUE)



#pruebas anteriores con pluriempleo (ver utilizacion de pondera)

pluriempleo = EPH2019_2023CAES %>%
  filter(ESTADO == 1, CAT_OCUP == 3)  %>%
  summarise(pluriempleados=sum (PONDERA [PP03C == 2]),
            unico_empleo=sum (PONDERA [PP03C ==1]))
summary(EPH2019_23)





#pruebo modificar la variable de CAES: 


base_recodCAES=organize_caes(EPH2019_23) #crea 8 nuevas variables con el CAES con diferente extensión (a un dígito, a dos, etc)


library(dplyr)
library(stringr)

EPH2019_23 <- EPH2019_23 %>%
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


cantidad_empleos=calculate_tabulates(base= EPH2019_23, x="PP03D", weights = "PONDERA")
