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
library(kableExtra)




Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "PP03I", "PP03J", "P21", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I", "DECOCUR", "PONDIIO", "PP05H")
EPH2019_2023CAES=get_microdata(year=2019:2023, period = 1,type = "individual", vars = Variables)


EPH2019_2023CAES <- EPH2019_2023CAES %>% eph::organize_labels()
EPH2019_2023CAES=organize_caes(EPH2019_2023CAES)


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

gruposetarios_año_pond <- EPH2019_2023CAES %>% filter (!is.na(grupos_etarios)) %>% 
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

gruposetarios_año_pond <- gruposetarios_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    grupos_etarios = as_factor(grupos_etarios)  # Convertir la variable de grupos etarios a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

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
            position = position_stack(vjust = 0.5),  # Centrar las etiquetas en las pilas
            color = "black", fontface = "bold", size = 3)

print(graf_gruposetarios)

#Lugar de nacimiento 

lugarnacimiento_año_pond <- EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4, lugar_nacimiento) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

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
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores
  labs(title = "Lugar de nacimiento por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")+
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)
print(graf_lugar_nacimiento)


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
  mutate(Porcentaje = round((casos / sum(casos)), 2),
         Condición_actividad = case_when(
           ESTADO== 1 ~ "Ocupado/a",
           ESTADO == 2 ~ "Desocupado/a",
           ESTADO == 3 ~ "Inactivo/a",
           ESTADO == 4 ~ "Menor de 10 anos",
           TRUE ~ "Otro"  # Para manejar cualquier otro valor posible
         )) 
cond_actividad_pond_acotada = cond_actividad_pond %>% 
  select(-casos, -ESTADO) 

graf_cond_actividad <- ggplot(cond_actividad_pond_acotada, aes(x = ANO4, y = Porcentaje, fill = Condición_actividad)) + 
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores
  labs(title = "Codición de actividad por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +  # Mostrar en porcentaje
  guides(size = "none")+
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)
print(graf_cond_actividad)


#categoría ocupacional:notar que hay un valor 0 que no figura en el diseño. segun el anexo: El código 0 identifica los casos a los cuales no les corresponde la secuencia analizada. ACÁ SERÍAN LOS INACTIVOS Y MENORES DE 10
categ_ocupacional_pond = EPH2019_2023CAES %>% 
  filter(ESTADO == 1) %>% 
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA, na.rm = TRUE)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 2),
         Categoría_ocupacional = case_when(
           CAT_OCUP== 1 ~ "Patrón",
           CAT_OCUP == 2 ~ "Cuenta propia",
           CAT_OCUP == 3 ~ "Obrero o empleado",
           CAT_OCUP == 4 ~ "Trabajador familiar sin remuneración",
           TRUE ~ NA  
         ))

categ_ocupacional_pond_filtrada <- categ_ocupacional_pond %>%
  filter(!is.na(Categoría_ocupacional))

graf_cat_ocup <- ggplot(categ_ocupacional_pond_filtrada, aes(x = ANO4, y = Porcentaje, fill = Categoría_ocupacional)) + 
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Categoría ocupacional por año (Total ocupados)",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +  # Mostrar en porcentaje
  guides(size = "none")

print(graf_cat_ocup)

# Ámbito del establecimiento 

ambitoestablecimiento_año_pond <- EPH2019_2023CAES %>% 
  filter(!is.na(ambito_establecimiento)) %>% 
  group_by(ANO4, ambito_establecimiento) %>% 
  summarize(casos = sum(PONDERA))

ambitoestablecimiento_año_pond <- ambitoestablecimiento_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  
    casos = as.numeric(casos),  
    ambito_establecimiento = as_factor(ambito_establecimiento)  
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  
  ungroup()

graf_ambitoestablecimiento <- ggplot(ambitoestablecimiento_año_pond, aes(x = ANO4, y = porcentaje, fill = ambito_establecimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Ambito del establecimiento de trabajo por año (Total ocupados)",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")+
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)
print(graf_ambitoestablecimiento)


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

# Tasas desagregadas
# sexo

TASAS_MT_SEXO_1923 <- EPH2019_2023CAES %>%  
  group_by(ANO4, SEXO) %>% 
  summarize(
    Poblacion          = sum(PONDERA),
    Ocupados          = sum(PONDERA[ESTADO == 1]),
    Desocupados       = sum(PONDERA[ESTADO == 2]),
    PNEA               = sum(PONDERA[ESTADO %in% c(3, 4)]), 
    PEA                = Ocupados + Desocupados,
    Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J == 1]),
    Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1]),
    Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2, 9)]),
    Subocupados       = Suboc_demandante + Suboc_no_demand,
    'Tasa Actividad'  = (PEA / Poblacion) * 100,
    'Tasa Inactividad' = (PNEA / Poblacion) * 100,
    'Tasa Empleo'     = (Ocupados / Poblacion) * 100,
    'Tasa Desocupacion' = (Desocupados / PEA) * 100,
    'Tasa ocupados demandantes' = (Ocupados_demand / PEA) * 100,
    'Tasa Subocupación' = (Subocupados / PEA) * 100,
    'Tasa Subocupación demandante' = (Suboc_demandante / PEA) * 100,
    'Tasa Subocupación no demandante' = (Suboc_no_demand / PEA) * 100
  ) %>%
  # Redondear las tasas a 1 decimal
  mutate(across(starts_with("Tasa"), ~ round(., 1)))

TasasMT_SEXO_SELECTED <- TASAS_MT_SEXO_1923 %>% select(ANO4, SEXO, 'Tasa Actividad', 'Tasa Inactividad', 'Tasa Empleo','Tasa Desocupacion','Tasa Subocupación')
TablaTasasMT_SEXO <- TasasMT_SEXO_SELECTED %>%
  kable(booktabs = TRUE,          
        caption = "<b>Principales tasas del mercado de trabajo según sexo. Total de 31 aglomerados urbanos. 3T-2019-2023</b>", 
        align = c('l','c','c','c'), 
        col.names = c("Año", 
                      "Sexo",
                      "Tasa de actividad", 
                      "Tasa de inactividad", 
                      "Tasa de empleo", 
                      "Tasa de desocupación", 
                      "Tasa de subocupación")) %>%   
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), 
                full_width = T, 
                position = "center") %>% 
  row_spec(0, bold = TRUE, background = "#4dd686", color = "white") %>%
  row_spec(seq(1, nrow(TasasMT_SEXO_SELECTED), 2), background = "#F5F5F5") %>% 
  collapse_rows(columns = 1, valign = "middle") %>%  
  column_spec(1, bold = TRUE) %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")
TablaTasasMT_SEXO 



# Grupos etarios

TASAS_MT_gruposetarios_1923 <- EPH2019_2023CAES %>% filter(!is.na(grupos_etarios)) %>%  
  group_by(ANO4, grupos_etarios) %>% 
  summarize(
    Poblacion          = sum(PONDERA),
    Ocupados          = sum(PONDERA[ESTADO == 1]),
    Desocupados       = sum(PONDERA[ESTADO == 2]),
    PNEA               = sum(PONDERA[ESTADO %in% c(3, 4)]), 
    PEA                = Ocupados + Desocupados,
    Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J == 1]),
    Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1]),
    Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2, 9)]),
    Subocupados       = Suboc_demandante + Suboc_no_demand,
    'Tasa Actividad'  = (PEA / Poblacion) * 100,
    'Tasa Inactividad' = (PNEA / Poblacion) * 100,
    'Tasa Empleo'     = (Ocupados / Poblacion) * 100,
    'Tasa Desocupacion' = (Desocupados / PEA) * 100,
    'Tasa ocupados demandantes' = (Ocupados_demand / PEA) * 100,
    'Tasa Subocupación' = (Subocupados / PEA) * 100,
    'Tasa Subocupación demandante' = (Suboc_demandante / PEA) * 100,
    'Tasa Subocupación no demandante' = (Suboc_no_demand / PEA) * 100
  ) %>%
  # Redondear las tasas a 1 decimal
  mutate(across(starts_with("Tasa"), ~ round(., 1)))

#TABLA PARA TASAS DEL MT DESAGREGADAS POR GRUPOS ETARIOS
TasasMT_GRUPOSETARIOS_SELECTED <- TASAS_MT_gruposetarios_1923 %>% select(ANO4, grupos_etarios, 'Tasa Actividad', 'Tasa Inactividad', 'Tasa Empleo','Tasa Desocupacion','Tasa Subocupación')

TablaTasasMT_GRUPOSETARIOS <- TasasMT_GRUPOSETARIOS_SELECTED %>%
  kable(booktabs = TRUE,          
        caption = "<b>Principales tasas del mercado de trabajo según grupos etarios. Total de 31 aglomerados urbanos. 3T-2019-2023</b>", 
        align = c('l','c','c','c'), 
        col.names = c("Año", 
                      "Grupos etarios",
                      "Tasa de actividad", 
                      "Tasa de inactividad", 
                      "Tasa de empleo", 
                      "Tasa de desocupación", 
                      "Tasa de subocupación")) %>%   
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), 
                full_width = T, 
                position = "center") %>% 
  row_spec(0, bold = TRUE, background = "#fcba03", color = "white") %>%
  row_spec(seq(1, nrow(TasasMT_SEXO_SELECTED), 2), background = "#F5F5F5") %>% 
  collapse_rows(columns = 1, valign = "middle", latex_hline = "none") %>%  
  column_spec(1, extra_css = "font-weight: bold;") %>%  
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")

TablaTasasMT_GRUPOSETARIOS

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

Ocupados_sexo_edad = Estado1_sexo_edad %>%
  kable(booktabs = TRUE,          
        caption = "<b>Distribución ocupados de acuerdo a sexo y grupos etarios</b>", 
        align = c('l', 'c', 'c', 'c', 'r'), 
        col.names = c("Año", 
                      "Sexo", 
                      "Grupo etario", 
                      "Casos", 
                      "Porcentaje")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), 
                full_width = TRUE, 
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#a503fc", color = "white") %>%
  row_spec(seq(1, nrow(Estado1_sexo_edad), 2), background = "#F5F5F5") %>% 
  collapse_rows(columns = 1, valign = "middle", latex_hline = "none") %>%  
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")

Ocupados_sexo_edad 

#Población desocupada por sexo y edad
Estado2_sexo_gruposetarios <- EPH2019_2023CAES %>%  
  filter(!is.na(grupos_etarios) & ESTADO == 2) %>%  # Filtrar NA en grupos_etarios y quedarnos con ESTADO == 2
  group_by(ANO4, SEXO, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Estado2_sexo_gruposetarios <- EPH2019_2023CAES %>%  
  filter(!is.na(grupos_etarios) & ESTADO == 2) %>%  # Filtrar NA en grupos_etarios y quedarnos con ESTADO == 2
  group_by(ANO4, SEXO, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA), .groups = "drop") %>%  
  group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

Desocupados_sexo_gruposetarios = Estado2_sexo_gruposetarios %>% 
  kable(booktabs = TRUE,          
        caption = "Distribución de desocupados de acuerdo a sexo y grupos etarios", 
        align = c('l', 'c', 'c', 'c', 'r'), 
        col.names = c("Ano", 
                      "Sexo", 
                      "Grupo etario", 
                      "Casos", 
                      "Porcentaje")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), 
                full_width = TRUE, 
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#035efc", color = "white") %>%
  row_spec(seq(1, nrow(Estado2_sexo_gruposetarios), 2), background = "#F5F5F5") %>%  
  collapse_rows(columns = 1, valign = "middle", latex_hline = "none") %>%  
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")
Desocupados_sexo_gruposetarios

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


EPH2019_2023CAES <- EPH2019_2023CAES %>% 
  mutate(
    Preca_cuentaprop = ifelse(CAT_OCUP == 2, 
                              ifelse(PP05H %in% c(1,2,3,4), 10, 
                                     ifelse(PP05H == 9, 99, 0)), #99 son los cuenta propia que responden 
                              0)  # 0 para los que no son CAT_OCUP == 2
  )


calculate_tabulates(EPH2019_2023CAES, "ANO4", "Preca_cuentaprop", weight= "PONDERA", add.percentage = "col")

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
                                          Preca_cuentaprop
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

#Tipos de precariedad por año 

library(ggplot2)
library(dplyr)
library(tidyr)

# ASALARIADOS
asalariados_precarios <- eph_filtrada %>% 
  group_by(ANO4) %>% 
  filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
  summarise(
    Preca_ingresos = sum(PONDERA[Preca_ingresos %in% c(30, 40)], na.rm = TRUE),
    
    Preca_intensidad = sum(PONDERA[Preca_intensidad %in% c(10, 20)], na.rm = TRUE),
    
    Preca_cond_lab = sum(PONDERA[Preca_cond_lab == 30], na.rm = TRUE), 
    Preca_forma_contrat = sum (PONDERA [Preca_forma_contrat == 10], na.rm = TRUE),
    Asalariados = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3], na.rm = TRUE),
    Proporcion_precariedad_ingresosA = (Preca_ingresos / Asalariados)*100,
    Proporcion_precariedad_intensidadA = (Preca_intensidad / Asalariados)*100,
    Proporcion_precariedad_cond_labA = (Preca_cond_lab / Asalariados)*100,
    Proporcion_precariedad_contratA = (Preca_forma_contrat / Asalariados)*100)

#gráfico 
asalariados_precarios_select <- asalariados_precarios %>%
  select(ANO4, Proporcion_precariedad_ingresosA, Proporcion_precariedad_intensidadA, Proporcion_precariedad_cond_labA, Proporcion_precariedad_contratA)

asalariados_precarios_select <- asalariados_precarios_select %>%
  rename_with(~ gsub("Proporcion_precariedad_", "", .), starts_with("Proporcion_precariedad"))


asalariados_precarios_long <- asalariados_precarios_select %>%
  pivot_longer(cols = c("ingresosA", 
                        "intensidadA", 
                        "cond_labA", 
                        "contratA"),  # Especificando las columnas
               names_to = "Tipo_de_precariedad", 
               values_to = "Proporcion")



# Crear gráfico de líneas
Grafico_asalariados_precarios_ano= ggplot(asalariados_precarios_long, aes(x = factor(ANO4), y = Proporcion, color = Tipo_de_precariedad, group = Tipo_de_precariedad)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +  # Agregar puntos en las líneas
  scale_color_brewer(palette = "Dark2", 
                     name = "Tipo de precariedad", 
                     labels = c("Precariedad en condiciones laborales", "Precariedad en forma de contratación","Precariedad por ingresos","Precariedad por intensidad de jornada laboral")) +  # Cambiar los nombres
  labs(title = "Asalariados/as precarios/as según tipo de precariedad",
       subtitle = "Terceros trimestres, 2019-2023",
       x = "Año",
       y = "Proporción de precariedad") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Proporcion, 2))), # Etiqueta con el valor de la proporción redondeado
            vjust = -0.5, hjust = 0.5, size = 2, color = "black") +  # Ajuste de posición de las etiquetas
  theme(legend.position = "top",  # Mueve la leyenda al top
        legend.text = element_text(size = 8),  # Ajusta el tamaño del texto de la leyenda
        legend.title = element_text(size = 10))+
  guides(color = guide_legend(nrow = 2))

print(Grafico_asalariados_precarios_ano)

# CUENTAPROPISTAS 

Cuentapropistas_precarios = eph_filtrada %>% 
  group_by(ANO4) %>% 
  filter(ESTADO == 1 & CAT_OCUP == 2) %>% 
  summarise(
    Cuentapropistas = sum(PONDERA [ESTADO == 1 & CAT_OCUP == 2], na.rm = TRUE), 
    Preca_ingresos = sum(PONDERA[Preca_ingresos %in% c(30, 40)], na.rm = TRUE),
    
    Preca_intensidad = sum(PONDERA[Preca_intensidad %in% c(10, 20)], na.rm = TRUE),
    
    Preca_cuentaprop = sum (PONDERA [Preca_cuentaprop ==10], na.rm = TRUE),
    Proporcion_precariedad_ingresosC = (Preca_ingresos / Cuentapropistas)*100, 
    Proporcion_precariedad_intensidadC = (Preca_intensidad / Cuentapropistas)*100,
    Proporcion_precariedad_cuentapropC = (Preca_cuentaprop / Cuentapropistas)*100)

#grafico

cuentapropistas_precarios_select <- Cuentapropistas_precarios %>%
  select(ANO4, Proporcion_precariedad_ingresosC, Proporcion_precariedad_intensidadC, Proporcion_precariedad_cuentapropC) %>% 
  rename_with(~ gsub("Proporcion_precariedad_", "", .), starts_with("Proporcion_precariedad"))

cuentapropistas_precarios_long <- cuentapropistas_precarios_select %>%
  pivot_longer(cols = c("ingresosC", 
                        "intensidadC", 
                        "cuentapropC"),  # Especificando las columnas
               names_to = "Tipo_de_precariedad", 
               values_to = "Proporcion")


Grafico_cuentaprop_precarios_ano= ggplot(cuentapropistas_precarios_long, aes(x = factor(ANO4), y = Proporcion, color = Tipo_de_precariedad, group = Tipo_de_precariedad)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +  # Agregar puntos en las líneas
  scale_color_brewer(palette = "Dark2", 
                     name = "Tipo de precariedad", 
                     labels = c("Precariedad por inestabilidad laboral","Precariedad por ingresos", "Precariedad por intensidad de jornada laboral")) +  # Cambiar los nombres
  labs(title = "Cuentapropistas precarios/as según tipo de precariedad",
       subtitle = "Terceros trimestres, 2019-2023",
       x = "Año",
       y = "Proporción de precariedad") +
  theme_minimal() +
  geom_text(aes(label = round(Proporcion, 2)), # Etiqueta con el valor de la proporción redondeado
            vjust = -0.5, hjust = 0.5, size = 2, color = "black") +  # Ajuste de posición de las etiquetas
  theme(legend.position = "top",  # Mueve la leyenda al top
        legend.text = element_text(size = 8),  # Ajusta el tamaño del texto de la leyenda
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(nrow = 2))  # Organiza la leyenda en 2 filas

print(Grafico_cuentaprop_precarios_ano)

# precariedad por sexo

Precariedad_sexo_GE <- eph_filtrada %>% 
  filter(!is.na(Niveles_precariedad_total) & !is.na(grupos_etarios)) %>% 
  group_by(SEXO, grupos_etarios, Niveles_precariedad_total) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = (casos / sum(casos)))

graf_Precariedad_sexo_GE=ggplot (Precariedad_sexo_GE, aes(x = interaction(SEXO, grupos_etarios), y = Porcentaje, fill = Niveles_precariedad_total)) +
  geom_bar(stat = "identity") +  # Graficar como barras apiladas
  labs(
    title = "Niveles de Precariedad según Sexo y Grupo Etario",
    subtitle = "Proporción de precariedad según sexo y grupo etario (2019-2023)",
    x = "Sexo y Grupo Etario",
    y = "Porcentaje",
    fill = "Niveles de Precariedad"
  ) +
  scale_fill_brewer(palette = "Greens") +  # Colores para los niveles de precariedad
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para mayor legibilidad
  )+
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)


print(graf_Precariedad_sexo_GE)

# precaredad x lugar de nacimiento 

Precariedad_lugar_nacimiento <- eph_filtrada %>% 
  filter(!is.na(Niveles_precariedad_total) & !is.na(lugar_nacimiento))  %>% 
  group_by(lugar_nacimiento, Niveles_precariedad_total) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = (casos / sum(casos)))

graf_precariedad_lugarnacimiento = ggplot(Precariedad_lugar_nacimiento, aes(x = lugar_nacimiento, y = Porcentaje, fill = Niveles_precariedad_total)) +
  geom_bar(stat = "identity") +  # Graficar como barras apiladas
  labs(
    title = "Niveles de Precariedad según lugar de nacimiento",
    subtitle = "Terceros trimestres, 2019-2023",
    x = "Lugar de nacimiento",
    y = "Porcentaje",
    fill = "Niveles de Precariedad"
  ) +
  scale_fill_brewer(palette = "PuBuGn") +  # Colores para los niveles de precariedad
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para mayor legibilidad
  )+
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)

print(graf_precariedad_lugarnacimiento)

# precariedad x nivel educativo 

Precariedad_nivel_educativo <- eph_filtrada %>% 
  filter(!is.na(Niveles_precariedad_total) & !is.na(nivel_educativo_completo))  %>% 
  group_by(nivel_educativo_completo, Niveles_precariedad_total) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = (casos / sum(casos)))

graf_precariedad_nivel_educativo = ggplot(Precariedad_nivel_educativo, aes(x = nivel_educativo_completo, y = Porcentaje, fill = Niveles_precariedad_total)) +
  geom_bar(stat = "identity") +  # Graficar como barras apiladas
  labs(
    title = "Niveles de Precariedad según nivel educativo",
    subtitle = "Terceros trimestres, 2019-2023",
    x = "Nivel educativo",
    y = "Porcentaje",
    fill = "Niveles de Precariedad"
  ) +
  scale_fill_brewer(palette = "RdPu") +  # Colores para los niveles de precariedad
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para mayor legibilidad
  )+
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),  
            color = "#313332", fontface = "bold", size = 2.5)

print(graf_precariedad_nivel_educativo) 

# precariedad por tipo de establecimiento

Precariedad_establecimiento <- eph_filtrada %>%
  filter(!is.na(Niveles_precariedad_total) & !is.na(ambito_establecimiento)) %>% 
  group_by(ambito_establecimiento, Niveles_precariedad_total) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  group_by(ambito_establecimiento) %>% 
  mutate(Porcentaje = (casos / sum(casos)) * 100) %>% 
  ungroup()


Tabla_precariedad_por_ambitoestablecimiento <- Precariedad_establecimiento %>%
  kable(booktabs = F,          
        caption = "<b>Niveles de precariedad laboral según ámbito de establecimiento. Total de ocupados asalariados y cuentapropistas (31 aglomerados urbanos). 3T-2019-2023</b>", 
        align = c('l','c','c','c'), 
        col.names = c("Ambito", 
                      "Tipo de precariedad", 
                      "Casos", 
                      "Proporción" 
                      
        )) %>%   
  kable_styling(bootstrap_options = c("striped", "hover", "bordered"), 
                full_width = T, 
                position = "center") %>% 
  row_spec(0, bold = TRUE, background = "#e3e5e8", color = "blue") %>%
  row_spec(seq(1, nrow(Precariedad_establecimiento), 2), background = "#F5F5F5") %>% 
  collapse_rows(columns = 1, valign = "middle") %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC")

Tabla_precariedad_por_ambitoestablecimiento

# tipos de precariedad según tipo de establecimiento

#asalariados 

Tipo_precariedad_establecimiento <- eph_filtrada %>% 
  filter(ESTADO == 1 & CAT_OCUP == 3 & !is.na(ambito_establecimiento)) %>% 
  group_by(ambito_establecimiento) %>%  # Agrupar por ámbito de establecimiento
  summarise(
    Preca_ingresos = sum(PONDERA[Preca_ingresos %in% c(30, 40)], na.rm = TRUE),
    
    Preca_intensidad = sum(PONDERA[Preca_intensidad %in% c(10, 20)], na.rm = TRUE),
    
    Preca_cond_lab = sum(PONDERA[Preca_cond_lab == 30], na.rm = TRUE), 
    
    Preca_forma_contrat = sum(PONDERA[Preca_forma_contrat == 10], na.rm = TRUE),
    
    Asalariados = sum(PONDERA, na.rm = TRUE),  # Total de asalariados
    
    Proporcion_precariedad_ingresosA = (Preca_ingresos / Asalariados),
    Proporcion_precariedad_intensidadA = (Preca_intensidad / Asalariados),
    Proporcion_precariedad_cond_labA = (Preca_cond_lab / Asalariados),
    Proporcion_precariedad_contratA = (Preca_forma_contrat / Asalariados)
  )

# Transformar el dataframe a formato largo
Tipo_precariedad_establecimiento_long <- Tipo_precariedad_establecimiento %>%
  pivot_longer(cols = starts_with("Proporcion_precariedad"), 
               names_to = "Tipo_de_precariedad", 
               values_to = "Porcentaje")

# Crear el gráfico de barras
ggplot(Tipo_precariedad_establecimiento_long, aes(x = ambito_establecimiento, 
                                                  y = Porcentaje, 
                                                  fill = Tipo_de_precariedad)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barra lado a lado (no apilada)
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar en formato porcentaje
  labs(
    title = "Tipo de Precariedad por Ámbito de Establecimiento",
    subtitle = "Total asalariados. Terceros trimestres 2019-2023",
    x = "Ámbito de Establecimiento",
    y = "Porcentaje",
    fill = "Tipo de Precariedad"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Proporcion_precariedad_cond_labA" = "#66c2a5", 
               "Proporcion_precariedad_contratA" = "#fc8d62", 
               "Proporcion_precariedad_ingresosA" = "#8da0cb", 
               "Proporcion_precariedad_intensidadA" = "#e78ac3"),
    labels = c("Proporcion_precariedad_cond_labA" = "Precariedad en condiciones laborales", 
               "Proporcion_precariedad_contratA" = "Precariedad en forma de contratación",
               "Proporcion_precariedad_ingresosA" = "Precariedad por ingresos", 
               "Proporcion_precariedad_intensidadA" = "Precariedad por intensidad de jornada laboral")
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje X
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)), 
            position = position_dodge(width = 0.8), 
            color = "black", fontface = "bold", size = 3)

# cuentapropistas 

Tipo_precariedad_establecimiento_cuentaprop <- eph_filtrada %>% 
  filter(ESTADO == 1 & CAT_OCUP == 2 & ambito_establecimiento == "Privado") %>% 
  group_by(ambito_establecimiento) %>%  # Agrupar por ámbito de establecimiento
  summarise(
    Cuentapropistas = sum(PONDERA [ESTADO == 1 & CAT_OCUP == 2], na.rm = TRUE),
    Preca_ingresos = sum(PONDERA[Preca_ingresos %in% c(30, 40)], na.rm = TRUE),
    
    Preca_intensidad = sum(PONDERA[Preca_intensidad %in% c(10, 20)], na.rm = TRUE),
    
    Preca_cuentaprop = sum (PONDERA [Preca_cuentaprop ==10], na.rm = TRUE),
    Proporcion_precariedad_ingresosC = (Preca_ingresos / Cuentapropistas), 
    Proporcion_precariedad_intensidadC = (Preca_intensidad / Cuentapropistas),
    Proporcion_precariedad_cuentapropC = (Preca_cuentaprop / Cuentapropistas))


# Transformar el dataframe a formato largo
Tipo_precariedad_establecimiento_cuentaprop_long <- Tipo_precariedad_establecimiento_cuentaprop %>%
  pivot_longer(cols = starts_with("Proporcion_precariedad"), 
               names_to = "Tipo_de_precariedad", 
               values_to = "Porcentaje")

# Crear el gráfico de barras
ggplot(Tipo_precariedad_establecimiento_cuentaprop_long, aes(x = ambito_establecimiento, 
                                                             y = Porcentaje, 
                                                             fill = Tipo_de_precariedad)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barra lado a lado (no apilada)
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar en formato porcentaje
  labs(
    title = "Tipo de Precariedad por Ámbito de Establecimiento",
    subtitle = "Total cuentapropistas. Terceros trimestres 2019-2023",
    x = "Ámbito de Establecimiento",
    y = "Porcentaje",
    fill = "Tipo de Precariedad"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Proporcion_precariedad_cuentapropC" = "#fc8d62", 
               "Proporcion_precariedad_ingresosC" = "#8da0cb", 
               "Proporcion_precariedad_intensidadC" = "#e78ac3"),
    labels = c("Proporcion_precariedad_cuentapropC" = "Precariedad por inestabilidad laboral", 
               "Proporcion_precariedad_ingresosA" = "Precariedad por ingresos", 
               "Proporcion_precariedad_intensidadA" = "Precariedad por intensidad de jornada laboral")
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje X
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)), 
            position = position_dodge(width = 0.8), 
            color = "black", fontface = "bold", size = 3)

# precariedad por sector de actividad

names(eph_filtrada)
Precariedad_sectoractividad <- eph_filtrada %>%
  filter(ESTADO == 1 & CAT_OCUP %in% c(2,3) & !is.na(Niveles_precariedad_total) & !is.na(caes_eph_label)) %>% 
  group_by(caes_eph_label, Niveles_precariedad_total) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  group_by(caes_eph_label) %>% 
  mutate(Porcentaje = (casos / sum(casos)) * 100) %>% 
  ungroup()



# Gráfico de barras apiladas horizontales (ajustado)
Grafico_Precariedad_sectoractividad=ggplot(Precariedad_sectoractividad, aes(x = casos, 
                                        y = reorder(caes_eph_label, casos), 
                                        fill = Niveles_precariedad_total)) +
  geom_bar(stat = "identity", position = "fill") +  # Apilado proporcional
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar en porcentaje
  labs(
    title = "Distribución de Precariedad por Sector de Actividad",
    subtitle = "Total de ocupados asalariados y cuentapropistas (2019-2023)",
    x = "Porcentaje",
    y = "Sector de Actividad",
    fill = "Nivel de Precariedad"
  ) +
  theme_minimal(base_size = 12) +  # Fuente más grande
  theme(
    legend.position = "bottom",  # Leyenda debajo
    legend.title = element_text(face = "bold"),  # Título en negrita
    legend.key.size = unit(1, "cm"),  # Tamaño de la leyenda
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Centrar título
    plot.subtitle = element_text(size = 10, hjust = 0.5),  # Centrar subtítulo
    axis.text.y = element_text(size = 10)  # Tamaño de texto del eje Y
  ) +
  scale_fill_brewer(palette = "Set2") +  # Colores diferenciados
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "black", size = 4)  # Etiquetas dentro de las barras
print(Grafico_Precariedad_sectoractividad)
ggsave("Precariedad_por_Sector.png", plot = Grafico_Precariedad_sectoractividad, width = 12, height = 8, dpi = 300, bg = "white")

eph_filtrada=eph_filtrada %>% 
  mutate(algo_precariedad= case_when(precariedad_total_asalariados==0 ~ "No", 
                                     precariedad_total_asalariados>0 ~ "Si", 
                                     precariedad_total_cuentapropistas== 0 ~ "No", 
                                     precariedad_total_cuentapropistas>0~ "Si"))

Algoprecarios_sector <- eph_filtrada %>%
  filter(ESTADO == 1 & CAT_OCUP %in% c(2,3) & !is.na(algo_precariedad) & !is.na(caes_eph_label)) %>% 
  group_by(caes_eph_label, algo_precariedad) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  group_by(caes_eph_label) %>% 
  mutate(Porcentaje = (casos / sum(casos)) * 100) %>% 
  ungroup()

Grafico_algoprecariedadsectoractividad=ggplot(Algoprecarios_sector, aes(x = casos, 
                                                                        y = reorder(caes_eph_label, casos), 
                                                                        fill = algo_precariedad)) +
  geom_bar(stat = "identity", position = "fill") +  # Apilado proporcional
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar en porcentaje
  labs(
    title = "Distribución de ocupados/as precarios por Sector de Actividad",
    subtitle = "Total de ocupados asalariados y cuentapropistas (2019-2023)",
    x = "Porcentaje",
    y = "Sector de Actividad",
    fill = "Algo de precariedad"
  ) +
  theme_minimal(base_size = 12) +  # Fuente más grande
  theme(
    legend.position = "bottom",  # Leyenda debajo
    legend.title = element_text(face = "bold"),  # Título en negrita
    legend.key.size = unit(1, "cm"),  # Tamaño de la leyenda
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Centrar título
    plot.subtitle = element_text(size = 10, hjust = 0.5),  # Centrar subtítulo
    axis.text.y = element_text(size = 10)  # Tamaño de texto del eje Y
  ) +
  scale_fill_brewer(palette = "Set2") +  # Colores diferenciados
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "black", size = 4)  # Etiquetas dentro de las barras
print(Grafico_algoprecariedadsectoractividad)
ggsave("Algo_de_Precariedad_por_Sector.png", plot = Grafico_algoprecariedadsectoractividad, width = 12, height = 8, dpi = 300, bg = "white")

