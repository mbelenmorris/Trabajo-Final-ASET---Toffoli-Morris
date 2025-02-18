# unificar nombre base. ES EPH_2023_1_CAES?

#Precariedad por condiciones laborales
base_eph1923_acotada <- base_eph1923_acotada %>% filter (ESTADO == 1, CAT_OCUP == 3) %>%  #ocupados asalariados
  mutate(
    preca.cond.lab = ifelse (PP07H==2, yes = 10,no= 0)+ #descuento jubilatorio (ojo porque hay valores NA Y 0)EN este caso y en el que sigue asigné los mismos valores del indicador
      ifelse (PP07G1==2, yes = 4,no= 0)+ #VACACIONES PAGAS
      ifelse (PP07G2==2, yes = 4,no= 0)+ #aguinaldo
      ifelse (PP07G3==2, yes = 4,no= 0)+ #LICENCIAS MÉDICAS
      ifelse (PP07G4==2, yes = 8,no= 0)) #Obra social
unique(base_eph1923_acotada$preca.cond.lab)  #0 30 12 10  4 22 26 18  8 16 14 20
calculate_tabulates (base_eph1923_acotada, "ANO4", "preca.cond.lab", add.percentage = "col", "PONDERA")
summary (base_eph1923_acotada$preca.cond.lab) # promedio: 10 puntos de precariedad


#Precariedad por formas de contratación
base_eph1923_acotada <- base_eph1923_acotada %>% filter (ESTADO == 1, CAT_OCUP == 3) %>%  #ocupados asalariados
  mutate(
    preca.forma.contrat = ifelse (PP07C==1, yes = 10,no= 0))
calculate_tabulates (base_eph1923_acotada, "ANO4", "preca.forma.contrat", add.percentage = "col", "PONDERA")    
summary(base_eph1923_acotada$preca.forma.contrat)


#Preca.cuentaprop
base_eph1923_acotada <- base_eph1923_acotada %>% 
  mutate(
    preca.cuentaprop = ifelse(CAT_OCUP == 2 & PP05H %in% c(1,2,3,4), 10, 0)  # Se aplica solo a CAT_OCUP == 2
  )

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
                                          Preca_intensidad_pluriempleo+
                                          Preca_cond_lab+
                                          Preca_forma_contrat+
                                        ))
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


