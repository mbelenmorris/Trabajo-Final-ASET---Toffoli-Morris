library(eph)
library(tidyverse)
library(dplyr)
library(janitor)
library(srvyr)
library(haven)
library(ggplot2)
library(scales)
library(dplyr)
library(ggthemes)
library(haven)  # Para manejar labelled
library(RColorBrewer)




Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "PP03I", "PP03J", "P21", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I", "DECOCUR", "PONDIIO", "PP05H")
EPH2019_2023CAES=get_microdata(year=2019:2023, period = 1,type = "individual", vars = Variables)


EPH2019_2023CAES <- EPH2019_2023CAES %>% eph::organize_labels()
EPH2019_2023CAES=organize_caes(EPH2019_2023CAES)

TABLAP21 <- calculate_tabulates(EPH2019_2023CAES, "ANO4", "P21", "PONDERA")#lo hice para ver la distribución original de p21

#Variables nuevas y recodificadas (SEXO, PP04B_COD (CAES), PP04C(CANTIDAD EMPLEADOS), Grupos etarios, ámbito del establecimiento, región, lugar de nacimiento, nivel_educativo_completo)

#SEXO

EPH2019_2023CAES=EPH2019_2023CAES %>% mutate(SEXO=case_when(
  CH04==1 ~ "Varón",
  CH04==2 ~ "Mujer"))

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
colnames(EPH2019_2023CAES)

#CANTIDAD DE EMPLEADOS


EPH2019_2023CAES=EPH2019_2023CAES %>% rename(CANT.EMPLEADOS=PP04C)
summary(EPH2019_2023CAES$CANT.EMPLEADOS)

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(CANT.EMPLEADOS = as_factor(CANT.EMPLEADOS)) %>%  # Convertir a factor para trabajar con etiquetas
  mutate(CANT.EMPLEADOS = case_when(
    CANT.EMPLEADOS == "0" ~ 0,
    CANT.EMPLEADOS == "1 persona" ~ 1,
    CANT.EMPLEADOS == "2 personas" ~ 2,
    CANT.EMPLEADOS == "3 personas" ~ 3,
    CANT.EMPLEADOS == "4 personas" ~ 4,
    CANT.EMPLEADOS == "5 personas" ~ 5,
    CANT.EMPLEADOS == "6 a 10 personas" ~ 6,
    CANT.EMPLEADOS == "11 a 25 personas" ~ 11,
    CANT.EMPLEADOS == "26 a 40 personas" ~ 26,
    CANT.EMPLEADOS == "de 41 a 100 personas" ~ 41,
    CANT.EMPLEADOS == "de 101 a 200 personas" ~ 101,
    CANT.EMPLEADOS == "de 201 a 500 personas" ~ 201,
    CANT.EMPLEADOS == "mas de 500 personas" ~ 501,
    CANT.EMPLEADOS == "Ns./Nr." ~ 99,
    TRUE ~ NA_real_  # Asignar NA a otros valores
  )) %>%
  mutate(CANT.EMPLEADOS = case_when(
    is.na(CANT.EMPLEADOS) ~ NA_character_,
    CANT.EMPLEADOS == 0 ~ "0",
    CANT.EMPLEADOS %in% 1:6 ~ "1 a 10",
    CANT.EMPLEADOS %in% 11:25 ~ "11 a 40",
    CANT.EMPLEADOS %in% 26:40 ~ "41 a 200",
    CANT.EMPLEADOS %in% 41:500 ~ "Más de 200",
    CANT.EMPLEADOS == 99 ~ "Ns./Nr.",
    TRUE ~ NA_character_
  )) %>%
  mutate(CANT.EMPLEADOS = factor(CANT.EMPLEADOS, 
                                 levels = c("0", "1 a 10", "11 a 40", "41 a 200", "Más de 200", "Ns./Nr."), 
                                 ordered = TRUE))

# Verificar el resultado
table(EPH2019_2023CAES$CANT.EMPLEADOS, useNA = "always")



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

saveRDS(EPH2019_2023CAES, file = "EPH2019_2023CAES.rds")

##Caracterización de la muestra y población: sexo, estado, categoría ocupacional, categoría inactividad

##SEXO Y AÑO
poblacion_x_año<- EPH2019_2023CAES %>% group_by (ANO4) %>% summarize(sum(PONDERA))
sexo_año<- EPH2019_2023CAES %>% group_by (SEXO,ANO4) %>% summarize(sum(PONDERA))

graf_sexoyaño <- ggplot(EPH2019_2023CAES, aes(x = ANO4, y = PONDERA, fill = SEXO)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Set3")+
  labs(title = "Sexo de la población por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(size = "none")
print(graf_sexoyaño)


#grupos etarios y año

unique(EPH2019_2023CAES$grupos_etarios)
gruposetarios_año_pond <- EPH2019_2023CAES %>% filter (!is.na(grupos_etarios)) %>% 
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

# Convertir variables labelled a formatos adecuados
gruposetarios_año_pond <- gruposetarios_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    grupos_etarios = as_factor(grupos_etarios)  # Convertir la variable de grupos etarios a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

# Crear gráfico de barras apiladas. no logro generar con etiquetas de porcentaje, revisar
graf_gruposetarios <- ggplot(gruposetarios_año_pond, aes(x = ANO4, y = porcentaje, fill = grupos_etarios)) + 
  geom_bar(stat = "identity", ) +  
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores
  labs(title = "Distribución de grupos etarios por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  
  guides(size = "none")+
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
            position = position_stack(vjust = 0.5),  # Centrar las etiquetas dentro de las pilas
            color = "black", fontface = "bold", size = 3)
print(graf_gruposetarios)

#Lugar de nacimiento 

Muestra_lugarnacimiento=EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4) %>% 
  tabyl(lugar_nacimiento) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

lugarnacimiento_año_pond <- EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4, lugar_nacimiento) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))


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

gruposetarios_año_pond <- EPH2019_2023CAES %>% filter(!is.na(grupos_etarios)) %>% 
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))


#lugar de nacimiento
unique(EPH2019_2023CAES$lugar_nacimiento)

Muestra_lugarnacimiento=EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4) %>% 
  tabyl(lugar_nacimiento) %>%
  adorn_totals() %>% 
  adorn_pct_formatting()

lugarnacimiento_año_pond <- EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4, lugar_nacimiento) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

# Convertir variables adecuadas y calcular proporciones dentro de cada año
lugarnacimiento_año_pond <- lugarnacimiento_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    lugar_nacimiento = as_factor(lugar_nacimiento)  # Convertir la variable lugar_nacimiento a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

# Crear gráfico de barras apiladas
graf_lugar_nacimiento <- ggplot(lugarnacimiento_año_pond, aes(x = ANO4, y = porcentaje, fill = lugar_nacimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Greens") +  # Paleta de colores
  labs(title = "Lugar de nacimiento por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")
print(graf_lugar_nacimiento)

# Guardar imagen del gráfico
ggsave(filename = "graf_lugar_nacimiento.jpg", plot = graf_lugar_nacimiento, width = 8, height = 6, dpi = 300)

#nivel educativo 

nivel_educativo_año_pond <- EPH2019_2023CAES %>% filter(!is.na(nivel_educativo_completo)) %>% 
  group_by(ANO4, nivel_educativo_completo) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

# Convertir variables adecuadas y calcular proporciones dentro de cada año
nivel_educativo_año_pond <- nivel_educativo_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    nivel_educativo_completo = as_factor(nivel_educativo_completo)  # Convertir la variable lugar_nacimiento a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

# Crear gráfico de barras apiladas
graf_nivel_educativo <- ggplot(nivel_educativo_año_pond, aes(x = ANO4, y = porcentaje, fill = nivel_educativo_completo)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores
  labs(title = "Nivel educativo completo por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")+
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)
print(graf_nivel_educativo)


#cant.empleados/tamaño del establecimiento  
cant_empleados_pond <- EPH2019_2023CAES %>% filter(!is.na(CANT.EMPLEADOS) & CANT.EMPLEADOS !=0)%>%
  group_by(ANO4, CANT.EMPLEADOS) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1)) 

#sector de actividad
sector.de.actividad_pond <- EPH2019_2023CAES %>% 
    group_by(ANO4, caes_eph_label) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#condición de actividad
cond_actividad_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, ESTADO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría ocupacional:notar que hay un valor 0 que no figura en el diseño. segun el anexo: El código 0 identifica los casos a los cuales no les corresponde la secuencia analizada. ACÁ SERÍAN LOS INACTIVOS Y MENORES DE 10
categ_ocupacional_pond = EPH2019_2023CAES %>% 
  filter(ESTADO == 1) %>% 
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA, na.rm = TRUE)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría de inactividad
unique(EPH2019_2023CAES$CAT_INAC)
cat_inactividad_pond <- EPH2019_2023CAES %>%  filter(!is.na(CAT_INAC)& CAT_INAC!=0 ) %>% 
  group_by(ANO4, CAT_INAC) %>% 
  summarize(casos = sum(PONDERA),.groups = "drop") %>% group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Tasas básicas del MT. Notar que utiliza la PP03j, que es la pregunta por la búsqueda de otro empleo además del que ya se tiene. 


Datos_MT_1923<- EPH2019_2023CAES %>%  
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

#en lo que sigue, incorporamos cruces entre variables para descripción de la población de la muestra

colnames(EPH2019_2023CAES)

#ESTADO y combinaciones con sexo y edad
unique(EPH2019_2023CAES$ESTADO)
#condición de actividad y sexo
Estado_sexo <-  EPH2019_2023CAES %>%  
  filter(!is.na(ESTADO) & ESTADO != 0) %>%  # Filtrar NA y ESTADO =/= a 0
  group_by(ANO4, SEXO, ESTADO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Población ocupada y sexo
Estado1_sexo <-  EPH2019_2023CAES %>%  
  filter(!is.na(ESTADO) & ESTADO == 1) %>%  # Filtrar NA y ESTADO = 1
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#POblación desocupada y sexo
Estado2_sexo <-  EPH2019_2023CAES %>%  
  filter(!is.na(ESTADO) & ESTADO == 2) %>%  # Filtrar NA y ESTADO =   2
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Población desocupada y edad
Estado2_gruposetarios <- EPH2019_2023CAES %>%  
  filter(!is.na(ESTADO) & ESTADO == 2 & !is.na(grupos_etarios)) %>%  # Excluir NA en ESTADO y grupos_etarios
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Población inactiva y sexo
Estado3y4_sexo <-  EPH2019_2023CAES %>%  
  filter(!is.na(ESTADO) & ESTADO %in% c(3, 4)) %>%  # Filtrar NA y quedarnos con estado 3 y 4
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Población ocupada por sexo y edad
Estado1_sexo_edad <- EPH2019_2023CAES %>%  
  filter(!is.na(grupos_etarios) & ESTADO == 1) %>%  # Filtrar NA en grupos_etarios y quedarnos con ESTADO == 1
  group_by(ANO4, SEXO, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Población desocupada por sexo y edad
Estado2_sexo_gruposetarios <- EPH2019_2023CAES %>%  
  filter(!is.na(grupos_etarios) & ESTADO == 2) %>%  # Filtrar NA en grupos_etarios y quedarnos con ESTADO == 2
  group_by(ANO4, SEXO, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#CAT_OCUP, sexo y edad

unique(EPH2019_2023CAES$CAT_OCUP)
unique(EPH2019_2023CAES$grupos_etarios)
#vemos cómo se comportan las cat ocupacionales según sexo y grupo etario. 
#creé varios objetos nuevos porque se me complica la visualización, seguro podamos sacar otras conclusiones luego cuando mejoremos las tablas/gráficos

Catocup1_sexo<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 1) %>%  # QUEDARME SOLO CON PATRONES
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup2_sexo<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 2) %>%  # QUEDARME SOLO CON cuenta propia
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup3_sexo<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 3) %>%  # QUEDARME SOLO CON asalariados
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup4_sexo<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 4) %>%  # QUEDARME SOLO CON trabajadores familiares
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup1_grupoetarios<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 1 & !is.na(grupos_etarios)) %>%  # QUEDARME SOLO CON PATRONES y valores válidos en grupos etarios
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup2_grupoetarios<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 2 & !is.na(grupos_etarios)) %>%  # QUEDARME SOLO CON cuenta propia y valores válidos en grupos etarios
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup3_grupoetarios<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 3 & !is.na(grupos_etarios)) %>%  # QUEDARME SOLO CON asalariades y valores válidos en grupos etarios
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup4_grupoetarios<- EPH2019_2023CAES %>%  
  filter(CAT_OCUP == 4 & !is.na(grupos_etarios)) %>%  # QUEDARME SOLO CON trab familiares y valores válidos en grupos etarios
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup_sexo_edad <- EPH2019_2023CAES %>%  
  filter(!is.na(grupos_etarios) & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  # Filtrar NA en grupos_etarios y excluir CAT_OCUP == 0 e ==9
  group_by(ANO4, CAT_OCUP, SEXO, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Categoría ocupacional por sector de actividad, tamaño de establecimiento 


#categoría ocupacional por sector de actividad

Catocup_CAES<- EPH2019_2023CAES %>%  #este código está bueno para generar una visual pero no es práctico como table para leer
  filter(!is.na(caes_eph_label) & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  
  group_by(ANO4, CAT_OCUP, caes_eph_label) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría ocupacional por tamaño de establecimiento

Catocup_cantempleados<- EPH2019_2023CAES %>%  
  filter(!is.na(CANT.EMPLEADOS) & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  
  group_by(ANO4, CAT_OCUP, CANT.EMPLEADOS) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría ocupacional por ámbito- NO LLEGUÉ A ANALIZAR
Catocup_ambito<- EPH2019_2023CAES %>%  
  filter(!is.na(PP04A) & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  
  group_by(ANO4, CAT_OCUP, PP04A) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#categoría ocupacional por procedencia migrante-- este es porque me engolosiné
unique(EPH2019_2023CAES$CH15)
Catocup_migrante<- EPH2019_2023CAES %>% 
  filter(CH15 ==4 & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Catocup_argentines<- EPH2019_2023CAES %>% 
  filter((CH15 ==1|CH15 ==2|CH15 ==3) & CAT_OCUP != 0 & CAT_OCUP != 9 ) %>%  
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))


#####Construimos la VARIABLE PRECARIEDAD 

##CONDICIONES DE TRABAJO

EPH2019_2023CAES <- EPH2019_2023CAES %>%   #ocupados asalariados. uso ifelse dentro de mutate en lugar de filter, si no se peierden todos los casos q no son ocup asalariados
  mutate(
    Preca_cond_lab = ifelse (ESTADO==1& CAT_OCUP==3,
      (ifelse (PP07H==2, yes = 10,no= 0)+ #descuento jubilatorio (ojo porque hay valores NA Y 0)EN este caso y en el que sigue asigné los mismos valores del indicador
      ifelse (PP07G1==2, yes = 4,no= 0)+ #VACACIONES PAGAS
      ifelse (PP07G2==2, yes = 4,no= 0)+ #aguinaldo
      ifelse (PP07G3==2, yes = 4,no= 0)+ #LICENCIAS MÉDICAS
      ifelse (PP07G4==2, yes = 8,no= 0)), #Obra social
      0)) # Asigna 0 a los que no cumplen la condición   (si pongo NA en la suma de Precariedad_total me da NA)

unique(EPH2019_2023CAES$Preca_cond_lab)
table(EPH2019_2023CAES$Preca_cond_lab)

##FORMAS DE CONTRATACIÓN

EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(
    Preca_forma_contrat = ifelse(ESTADO == 1 & CAT_OCUP == 3, 
                                 ifelse(PP07C == 1, 10, 
                                        ifelse(PP07C == 9, 0, 0)),  # PP07C == 9 asigna 0
                                 0)  # Asigna NA a los que no cumplen la condición
  )

calculate_tabulates(EPH2019_2023CAES, "ANO4", "Preca_forma_contrat", weight= "PONDERA", add.percentage = "col")

table(EPH2019_2023CAES$Preca_forma_contrat)

#Preca.cuentaprop 


#pruebo la distribución de cat_ocup ,desaparecieron todas las categorías ocupacionales menos la 3. Esto es porque usé filter en la creación de las variables anteriores, y perdí las obs para otras cat ocup. ya está resuelto
calculate_tabulates(EPH2019_2023CAES, "ANO4", "CAT_OCUP", weight= "PONDERA", add.percentage = "col")

#(TAREA: chequear si la pp05h se cruza con cat_ocup cuentapropistas--sí se cruza:
estabilidadcuentaprop <- EPH2019_2023CAES %>%  filter(CAT_OCUP == 2 & ESTADO== 1 ) %>%  
  group_by(ANO4, CAT_OCUP, PP05H) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))


EPH2019_2023CAES <- EPH2019_2023CAES %>% 
  mutate(
    preca_cuentapropista = ifelse(CAT_OCUP == 2, 
                              ifelse(PP05H %in% c(1,2,3,4), 10, 
                                     ifelse(PP05H == 9, 99, 0)), #99 son los cuenta propia que responden 
                              0)  # 0 para los que no son CAT_OCUP == 2
  )



calculate_tabulates(EPH2019_2023CAES, "ANO4", "preca.cuentaprop", weight= "PONDERA", add.percentage = "col")

##PRECARIEDAD POR INGRESOS 

class(EPH2019_2023CAES$P21)
unique(EPH2019_2023CAES$P21)

#redefino -9 y 0 de ocupados (asalariados y cuentapropistas) como NA, para dps imputar


EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(P21 = as.numeric(unclass(P21)))  # Removes "labelled" class


#Imputo un ingreso a todos los valores 0,-9

eph_filtrada <- EPH2019_2023CAES %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(2, 3)) %>% 
  group_by(ANO4, SEXO, grupos_etarios, region_etiqueta, caes_eph_cod) %>%
  mutate(
    P21_imputado_AC = case_when( P21<=0~ mean(P21, na.rm = TRUE), 
                                 TRUE ~ P21))

sum(is.na(eph_filtrada$P21_imputado_AC))


#Divido en deciles la variable P21_imputado_AC: 

eph_filtrada<- eph_filtrada %>%
  group_by(ANO4) %>%
  mutate(
    Decil_imputado = cut(P21_imputado_AC, 
                         breaks = quantile(P21_imputado_AC, probs = 0:10 / 10, na.rm = TRUE), 
                         labels = 1:10, 
                         include.lowest = TRUE)
  ) %>%
  ungroup()



table(eph_filtrada$ANO4, eph_filtrada$Decil_imputado)

#agrupo deciles en la variable Preca_ingresos_deciles

eph_filtrada=eph_filtrada %>%
  mutate (Preca_ingresos_deciles= factor(case_when(
    Decil_imputado %in% 1:3 ~ "Ingresos bajos", 
    Decil_imputado %in% 4:8 ~ "Ingresos medios", 
    Decil_imputado %in% 9:10 ~ "Ingresos altos"), 
    levels = c("Ingresos bajos", "Ingresos medios", "Ingresos altos"))
  )


round(prop.table(table(eph_filtrada$Preca_ingresos_deciles)) * 100, 2)


#asigno valores numéricos a la variable de ingresos

eph_filtrada=eph_filtrada %>%
  mutate (Preca_ingresos_deciles_N= as.numeric(case_when (
    Preca_ingresos_deciles == "Ingresos bajos" ~ 30,
    Preca_ingresos_deciles == "Ingresos medios" ~ 15,
    Preca_ingresos_deciles == "Ingresos altos" ~ 0) 
  ))

sum(is.na(eph_filtrada %>% 
            filter(ESTADO == 1 & CAT_OCUP %in% c(2, 3)) %>% 
            pull(Preca_ingresos_deciles_N)))


#creo variable para "busqueda de otro empleo" que llamo Preca_ingresos_buscarotrotrabajo


unique(eph_filtrada$PP03J)
eph_filtrada=eph_filtrada %>%
  mutate (Preca_ingresos_buscarotrotrabajo= as.numeric(case_when (
    PP03J == 1 ~ 10,
    PP03J == 2 ~ 0) 
  ))
table(eph_filtrada$Preca_ingresos_buscarotrotrabajo)

##INTENSIDAD EN LA JORNADA LABORAL. Creo variables pluriempleo (Preca_intensidad_pluriempleo) 

unique(eph_filtrada$PP03C)
class(eph_filtrada$PP03C)

eph_filtrada=eph_filtrada %>%
  mutate (Preca_intensidad_pluriempleo= as.numeric(case_when (
    PP03C %in% 0:1 ~0,
    PP03C == 2 ~ 10) 
  ))
table(eph_filtrada$Preca_intensidad_pluriempleo)


#y sobreocupado (Preca_intensidad_sobreocup)

eph_filtrada=eph_filtrada %>%
  mutate (Preca_intensidad_sobreocup= as.numeric(case_when (
    INTENSI == 3 ~ 10,
    INTENSI %in% c("1", "2", "4") ~ 0) 
  ))
table(eph_filtrada$Preca_intensidad_sobreocup)
colnames(eph_filtrada)

#variables por tipo de precarización y una que agrupa tres categorías de precariedad: 

eph_filtrada <- eph_filtrada %>%
  mutate(
    Preca_ingresos = as.numeric(Preca_ingresos_deciles_N +
                                  Preca_ingresos_buscarotrotrabajo), 
    Preca_intensidad = as.numeric(Preca_intensidad_sobreocup +
                                    Preca_intensidad_pluriempleo))

#VARIABLE DE RESUMEN: PRECARIEDAD TOTAL ASALARIADOS

eph_filtrada <- eph_filtrada %>%
  mutate(precariedad_total_asalariados = as.numeric(Preca_ingresos +
                                        Preca_intensidad +
                                        Preca_cond_lab+
                                        Preca_forma_contrat
  ))

eph_filtrada <- eph_filtrada %>%
  mutate(precariedad_total_cuentapropistas = as.numeric(Preca_ingresos +
                                          Preca_intensidad +
                                          preca_cuentapropista
  ))
colnames(eph_filtrada)
unique(eph_filtrada$precariedad_total_cuentapropistas)
table(eph_filtrada$precariedad_total_cuentapropistas)





eph_filtrada <- eph_filtrada %>% 
  mutate(
   Niveles_precariedad_total = case_when(
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 0:20   ~ "Precariedad baja",
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 21:40  ~ "Precariedad media",
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 41:70 ~ "Precariedad alta",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 0:30   ~ "Precariedad baja",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 31:60  ~ "Precariedad media",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 61:100 ~ "Precariedad alta",
    TRUE ~ NA_character_  # Para otros valores de CAT_OCUP
  ) %>% factor(levels = c("Precariedad baja", "Precariedad media", "Precariedad alta"
                          )))

table(eph_filtrada$Niveles_precariedad_total) 



Precariedad_por_ano <- eph_filtrada %>%
  filter(ESTADO == 1 & CAT_OCUP %in% c(2, 3)) %>% 
  group_by(ANO4, Niveles_precariedad_total) %>%
  summarise(Cantidad = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Niveles_precariedad_total, values_from = Cantidad, values_fill = 0)


Tabla_precariedad_por_ano <- Precariedad_por_ano %>%
  kable(booktabs = TRUE,          
        caption = "<b>Niveles de precariedad laboral según año. Total de ocupados asalariados y cuentapropistas (31 aglomerados urbanos). 3T-2019-2023</b>", 
        align = c('l','c','c','c', 'c'), 
        col.names = c("Año", 
                      "Precariedad baja", 
                      "Precariedad media", 
                      "Precariedad alta", 
                      "NA"
                      )) %>%   
  kable_styling(bootstrap_options = c("striped", "hover", "bordered"), 
                full_width = T, 
                position = "center") %>% 
  row_spec(0, bold = TRUE, background = "#e3e5e8", color = "blue") %>%
  row_spec(seq(1, nrow(Precariedad_por_ano), 2), background = "#F5F5F5") %>% 
  collapse_rows(columns = 1, valign = "middle") %>%  
  column_spec(1, bold = TRUE, color = "blue" 
              ) %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")
  
Tabla_precariedad_por_ano
precariedad_cuentaprop <- eph_filtrada %>% 
  filter(ESTADO == 1 & CAT_OCUP == 2) %>%
  group_by(ANO4, Preca_cond_lab, Preca_ingresos, Preca_forma_contrat, Preca_intensidad) %>%  
  summarise(casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#Tipos de precariedad por año (ACA ME QUEDE)

library(ggplot2)
library(dplyr)
library(tidyr)

# Convertimos los datos a formato largo
eph_long <- eph_filtrada %>%
  select(ANO4, Preca_cond_lab, Preca_ingresos, Preca_forma_contrat, Preca_intensidad) %>%
  pivot_longer(cols = c(Preca_cond_lab, Preca_ingresos, Preca_forma_contrat, Preca_intensidad), 
               names_to = "Tipo_de_precariedad", 
               values_to = "Valor")

# Crear el gráfico de líneas
graf_tipo_precariedad <- ggplot(eph_long, aes(x = factor(ANO4), y = Valor, color = Tipo_de_precariedad, group = Tipo_de_precariedad)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  # Opcional: agrega puntos en las líneas
  scale_color_brewer(palette = "Oranges") +  
  labs(title = "Tipo de precariedad por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Valor de precariedad",
       color = "Tipo de precariedad") +
  theme_minimal()

# Mostrar el gráfico
print(graf_tipo_precariedad)



# PUNTO 3: 


#calculo cantidad de asalariados precarios por categoria


asalariados_precarios <- eph_filtrada %>% 
  group_by(ANO4) %>% 
  filter(ESTADO == 1, CAT_OCUP == 3) %>% 
  summarise(
    Preca_ingresos = sum(PONDERA[Preca_ingresos_deciles_N ==30], na.rm = TRUE) + 
      sum(PONDERA[Preca_ingresos_buscarotrotrabajo > 0], na.rm = TRUE),
    Preca_intensidad = sum(PONDERA[Preca_intensidad_pluriempleo > 0], na.rm = TRUE) + 
      sum(PONDERA[Preca_intensidad_sobreocup > 0], na.rm = TRUE), 
    Preca_cond_lab = sum(PONDERA[Preca_cond_lab > 0], na.rm = TRUE), 
    Preca_forma_contrat = sum (PONDERA [Preca_forma_contrat> 0], na.rm = TRUE),
    Asalariados = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3], na.rm = TRUE)
  )
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_ingresos = (Preca_ingresos / Asalariados)*100)
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_intensidad = (Preca_intensidad / Asalariados)*100)
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_con_lab = (Preca_cond_lab / Asalariados)*100)
asalariados_precarios <- asalariados_precarios %>%
  mutate(Proporcion_precariedad_contrat = (Preca_forma_contrat / Asalariados)*100)

cuentapropistas_precarios <- eph_filtrada %>% 
  group_by(ANO4) %>% 
  filter(ESTADO == 1, CAT_OCUP == 2) %>% 
  summarise(
    Preca_ingresos = sum(PONDERA[Preca_ingresos_deciles_N ==30], na.rm = TRUE) + 
      sum(PONDERA[Preca_ingresos_buscarotrotrabajo > 0], na.rm = TRUE),
    Preca_intensidad = sum(PONDERA[Preca_intensidad_pluriempleo > 0], na.rm = TRUE) + 
      sum(PONDERA[Preca_intensidad_sobreocup > 0], na.rm = TRUE), 
    preca.cuentaprop = sum(PONDERA[preca.cuentaprop==10], na.rm = TRUE),
   Cuentapropistas = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 2], na.rm = TRUE)
  )

cuentapropistas_precarios <- cuentapropistas_precarios %>%
  mutate(Proporcion_precariedad_ingresos = (Preca_ingresos / Cuentapropistas)*100)
cuentapropistas_precarios <- cuentapropistas_precarios %>%
  mutate(Proporcion_precariedad_intensidad = (Preca_intensidad / Cuentapropistas)*100)
cuentapropistas_precarios <- cuentapropistas_precarios %>%
  mutate(Proporcion_precariedad_cuentapropista = (preca.cuentaprop / Cuentapropistas)*100)




colnames(eph_filtrada)

tabulados <- list(
  g_etarios_x_precariedad = calculate_tabulates(eph_filtrada, 
                                                x = "grupos_etarios", 
                                                y = "Niveles_precariedad_total", 
                                                weights = "PONDERA", 
                                                add.percentage = "row"),
  
  ambito_x_precariedad = calculate_tabulates(eph_filtrada, 
                                             x = "ambito_establecimiento", 
                                             y = "Niveles_precariedad_total", 
                                             weights = "PONDERA", 
                                             add.percentage = "row"),
  
  sexo_x_precariedad = calculate_tabulates(eph_filtrada, 
                                           x = "SEXO", 
                                           y = "Niveles_precariedad_total", 
                                           weights = "PONDERA", 
                                           add.percentage = "row"),
  
  lugarnacimiento_x_precariedad = calculate_tabulates(eph_filtrada, 
                                                      x = "lugar_nacimiento", 
                                                      y = "Niveles_precariedad_total", 
                                                      weights = "PONDERA", 
                                                      add.percentage = "row"),
  
  ambito_x_precaingresos = calculate_tabulates(eph_filtrada, 
                                               x = "ambito_establecimiento", 
                                               y = "Preca_ingresos", 
                                               weights = "PONDERA", 
                                               add.percentage = "row"), 
  ambito_x_precaintensidad = calculate_tabulates(eph_filtrada, 
                                                 x = "ambito_establecimiento", 
                                                 y = "Preca_intensidad", 
                                                 weights = "PONDERA", 
                                                 add.percentage = "row"))