library(eph)
library(tidyverse)
base3T_19_23 <- get_microdata(year = 2019:2023, trimester = 3, type = "individual")
saveRDS(base3T_19_23, file = "base3T_19_23.rds")
base_eph1923_acotada=base3T_19_23 %>% select(CODUSU, ANO4,TRIMESTRE, REGION,AGLOMERADO, PONDERA,CH04,CH06,PP04A,PP04B_COD, CH15, NIVEL_ED,PP04C, PP05H, ESTADO,CAT_OCUP, CAT_INAC, PP03C,PP03G, INTENSI, PP07C, PP07E, PP07G1, PP07G2, PP07G3, PP07G4, PP07H,PP07I) 
saveRDS(base_eph1923_acotada, file = "base_eph1923_acotada.rds")

#recodificar valores: SEXO, PP04B_COD (CAES), PP04C(CANTIDAD EMPLEADOS)
base_eph1923_acotada <- base_eph1923_acotada %>% eph::organize_labels()
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
str(base_eph1923_acotada$CANT.EMPLEADOS)

library(haven)  # Necesario si las variables son de tipo "labelled"

base_eph1923_acotada <- base_eph1923_acotada %>%
  mutate(CANT.EMPLEADOS = as.numeric(as_factor(CANT.EMPLEADOS))) %>%  # Convierte a numérico puro
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

#acoto los valores posibles para que no haya tanta dispersión y podamos identificar, microempresas, pequeñas, medianas y grandes empresas

summary(base_eph1923_acotada$CANT.EMPLEADOS) #no me aparecían registros para más del valor 5 porque no estaba incluida una categoría para el valor 0. 

#PP04B_COD (CAES)

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
organize_caes(base3T_19_23)

caes_año <- base3T_19_23%>% 
  group_by(ANO4, PP04B_COD) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

base_eph1923_acotada <- base_eph1923_acotada %>% eph::organize_labels()

#códigos para el análisis exploratorio

poblacion_x_año<- base_eph1923_acotada %>% group_by (ANO4) %>% summarize(Poblacion=sum(PONDERA))

#calculo distribución y porcentajes por sexo, estado, categoría ocupacional, categoría inactividad

#sexo
#SEXO Y AÑO
sexo_año_pond <- base_eph1923_acotada %>% 
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))
#cant.empleados --tamaño del establecimiento
cant_empleados_pond <- base_eph1923_acotada %>% 
  filter(!is.na(CANT.EMPLEADOS)) %>%
  group_by(ANO4, CANT.EMPLEADOS) %>% 
  summarize(Casos = sum(PONDERA)) %>%
  group_by(ANO4) %>%  # Aquí agrupamos solo por ANO4 para calcular el total por año
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>% 
  ungroup()  # Deshacemos el group_by para no dejarlo para los siguientes cálculos

#sector de actividad
sector.de.actividad_pond <- base_eph1923_acotada %>% 
  filter(!is.na(SECTOR.DE.ACTIVIDAD)) %>%  group_by(ANO4, SECTOR.DE.ACTIVIDAD) %>% 
  summarize(casos = sum(PONDERA)) %>%  group_by(ANO4) %>%
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))
#condición de actividad
unique(base_eph1923_acotada$ESTADO)
cond_actividad_pond <- base_eph1923_acotada %>% 
  filter(!is.na(ESTADO)& ESTADO != 0) %>%  # Excluir NA y valores 0
  group_by(ANO4, ESTADO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>% group_by(ANO4) %>%
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#en población inactiva, tener en cuenta que despueés hay que sumar los casos de valor 4 que son menores de 10 años. 
#Si no, pareciera que los ocupados superan a los inactivos

#categoría ocupacional:notar que hay un valor 0 que no figura en el diseño. segun el anexo: El código 0 identifica los casos a los cuales no les corresponde la secuencia analizada. ACÁ SERÍAN LOS INACTIVOS Y MENORES DE 10

unique(base_eph1923_acotada$CAT_OCUP) # ES CLAVE VER QUÉ VALORES ASUME LA VARIABLE Y FILTRAR PORQUE SI NO SALEN TODOS LOS PORCENTAJES DISTORSIONADOS

categ_ocupacional_pond <- base_eph1923_acotada %>% 
  filter(!is.na(CAT_OCUP)& CAT_OCUP!=0 ) %>% group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA),.groups = "drop") %>% group_by(ANO4) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#SOLO CALCULO POR CONSOLA PARA CHEQUEAR QUÉ CONDICIÓN DE ACTIVIDAD ASUME VALOR 0 EN CAT_OCUP, para ver qué significa ese valor.. Se destina para inactivos, menores de 10 años, parte de desocupados 
calculate_tabulates(
  base_eph1923_acotada,
  x = "ESTADO",
  y = "CAT_OCUP",
  weight = "PONDERA",
  add.totals = "row", add.percentage = "col")

#categoría de inactividad
unique(base_eph1923_acotada$CAT_INAC)
cat_inactividad_pond <- base_eph1923_acotada %>%  filter(!is.na(CAT_INAC)& CAT_INAC!=0 ) %>% 
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

#probando imprimir tabla
install.packages("webshot")  # Para tomar capturas de HTML
install.packages("kableExtra")  # Para mejorar la visualización de tablas
webshot::install_phantomjs()  # Necesario solo la primera vez

library(kableExtra)
library(webshot)

# Crear un archivo HTML temporal
html_file <- tempfile(fileext = ".html")

# Crear y guardar la tabla en HTML
TasasMT_1923 %>%
  kable(format = "html", digits = 1) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable(html_file)

# Convertir el HTML a PNG
webshot(html_file, file = "TasasMT_1923.png", selector = "table")



##tasas básicas por sexo
tasas_sexo_1923<- base3T_19_23 %>%  
  group_by(ANO4, CH04) %>% 
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
  mutate(across(starts_with("Tasa"), ~ round(., 1))) %>% select(ANO4, CH04, starts_with("Tasa"))
#imprimir tabla
html_file2 <- tempfile(fileext = ".html")

# Crear y guardar la tabla en HTML
tasas_sexo_1923 %>%
  kable(format = "html", digits = 1) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable(html_file2)

# Convertir el HTML a PNG
webshot(html_file2, file = "tasas_sexo_1923.png", selector = "table")

#VARIABLES DUMMY

unique (base_eph1923_acotada$PP07H)
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
    preca.cuentaprop = ifelse(CAT_OCUP == 2 & PP05H %in% c(1,2,3,4), 10, 0)  # Se aplica solo a CAT_OCUP == 2; revisar categoría 9 q es no sabe no contesta y catalogarla como NA
  )


calculate_tabulates (base_eph1923_acotada, "ANO4", "preca.forma.contrat", add.percentage = "col", "PONDERA")    
summary(base_eph1923_acotada$preca.forma.contrat)


    