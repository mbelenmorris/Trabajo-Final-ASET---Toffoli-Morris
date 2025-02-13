library(eph)
library(tidyverse)
base3T_19_23 <- get_microdata(year = 2019:2023, trimester = 3, type = "individual")
saveRDS(base3T_19_23, file = "base3T_19_23.rds")
base_eph1923_acotada=base3T_19_23 %>% select(CODUSU, ANO4,TRIMESTRE, REGION,AGLOMERADO, PONDERA,CH04,CH06,PP04A,PP04B_COD, CH15, NIVEL_ED,PP04C, ESTADO,CAT_OCUP, CAT_INAC, PP03C,PP03G, INTENSI, PP07C, PP07E, PP07G1, PP07G2, PP07G3, PP07G4, PP07H,PP07I) 
saveRDS(base_eph1923_acotada, file = "base_eph1923_acotada.rds")

#recodificar valores: SEXO, PP04B_COD (CAES), PP04C(CANTIDAD EMPLEADOS)
#base_eph1923_acotada %>% eph::organize_labels (base_eph1923_acotada) ----si interesa agregar etiquetas
base_eph1923_acotada=base_eph1923_acotada %>% mutate(CH04=case_when(
  CH04==1 ~ "Varón",
  CH04==2 ~ "Mujer",
  TRUE~ "Otro"))

base_eph1923_acotada=base_eph1923_acotada %>% rename(SEXO=CH04)
base_eph1923_acotada=base_eph1923_acotada %>% rename(CANT.EMPLEADOS=PP04C)
base_eph1923_acotada=base_eph1923_acotada %>% rename(SECTOR.DE.ACTIVIDAD=PP04B_COD)
# Verifica la cantidad de valores únicos en cada base
summary(base3T_19_23$PP04C)
summary(base_eph1923_acotada$CANT.EMPLEADOS)
unique(base3T_19_23$PP04C) # 7  0  5 NA  2  9 99  1  6  3 10  4 12  8 11
# Verifica los valores únicos antes de la recategorización
unique(base_eph1923_acotada$CANT.EMPLEADOS) #7  0  5 NA  2  9 99  1  6  3 10  4 12  8 11


base_eph1923_acotada=base_eph1923_acotada %>% mutate(CANT.EMPLEADOS=case_when(CANT.EMPLEADOS==0~ "Sin empleados",
                                                                               CANT.EMPLEADOS==1 ~ "1",
                                                                               CANT.EMPLEADOS==2 ~ "2",
                                                                               CANT.EMPLEADOS==3 ~ "3",
                                                                               CANT.EMPLEADOS==4 ~ "4",
                                                                               CANT.EMPLEADOS==5 ~ "5",
                                                                               CANT.EMPLEADOS==6 ~ "6 a 10",
                                                                               CANT.EMPLEADOS==7 ~ "11 a 25",
                                                                               CANT.EMPLEADOS==8 ~ "26 a 40",
                                                                               CANT.EMPLEADOS==9 ~ "41 a 100",
                                                                               CANT.EMPLEADOS==10 ~ "101 a 200",
                                                                               CANT.EMPLEADOS==11~ "201 a 500",
                                                                               CANT.EMPLEADOS==12~ "Más de 500",
                                                                               CANT.EMPLEADOS==99~ "Ns./Nr.")) %>%
  mutate(CANT.EMPLEADOS = factor(CANT.EMPLEADOS, 
                                 levels = c("1", "2", "3", "4", "5", "6 a 10", 
                                            "11 a 25", "26 a 40", "41 a 100", 
                                            "101 a 200", "201 a 500", "Más de 500", "Ns./Nr.","Sin empleados"), 
                                 ordered = TRUE))

class(base_eph1923_acotada$CANT.EMPLEADOS)# "ordered" "factor" 
                                                                          
summary(base_eph1923_acotada$CANT.EMPLEADOS) #no me aparecían registros para más del valor 5 porque no estaba incluida una categoría para el valor 0. 

table(base_eph1923_acotada$CANT.EMPLEADOS)

base_eph1923_acotada <- base_eph1923_acotada %>%
  mutate(
    SECTOR.DE.ACTIVIDAD = case_when(
      grepl("^0[1-9]", SECTOR.DE.ACTIVIDAD) ~ "Agricultura y minería", 
      grepl("^1[0-9]|^2[0-9]|^3[0-3]", SECTOR.DE.ACTIVIDAD) ~ "Industria manufacturera",
      grepl("^3[5-9]", SECTOR.DE.ACTIVIDAD) ~ "Suministro de gas, agua, electricidad y otros", 
      grepl("^40", SECTOR.DE.ACTIVIDAD) ~ "Construcción",
      grepl("^4[5-8]|^5[5-6]", SECTOR.DE.ACTIVIDAD) ~ "Comercio, hotelería y gastronomía",
      grepl("^4[9-9]|^5[0-3]", SECTOR.DE.ACTIVIDAD) ~ "Transporte",
      grepl("^5[8-9]|^6[0-3]", SECTOR.DE.ACTIVIDAD) ~ "Información y Comunicación",
      grepl("^6[4-8]", SECTOR.DE.ACTIVIDAD) ~ "Actividades financieras, seguros, inmobiliarias",
      grepl("^6[9-9]|^7[0-5]", SECTOR.DE.ACTIVIDAD) ~ "Actividades Profesionales, Científicas y Técnicas",
      grepl("^7[7-9]|^8[0-2]", SECTOR.DE.ACTIVIDAD) ~ "Actividades Administrativas y Servicios de Apoyo",
      grepl("^8[3-4]", SECTOR.DE.ACTIVIDAD) ~ "Administración Pública y Defensa",
      grepl("^8[5]", SECTOR.DE.ACTIVIDAD) ~ "Enseñanza",
      grepl("^8[6-8]",SECTOR.DE.ACTIVIDAD) ~ "Salud Humana y Servicios Sociales",
      grepl("^9[0-3]", SECTOR.DE.ACTIVIDAD) ~ "Artes, Entretenimiento y Recreación",
      grepl("^9[4-6]", SECTOR.DE.ACTIVIDAD) ~ "Otras Actividades de Servicios",
      TRUE ~ "Otros"
    )
  )
base3T_19_23%>% 
  filter(P21 >0) %>% group_by(ANO4) %>% 
  summarise(Min = min(P21, na.rm = TRUE),
            
            Q1 = quantile(P21, 0.25, na.rm = TRUE),
            Mediana = median(P21, na.rm = TRUE),
            Q3 = quantile(P21, 0.75, na.rm = TRUE),
            Max = max(P21, na.rm = TRUE)
  )


