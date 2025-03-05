library(eph)
library(tidyverse)
library(dplyr)
library(janitor)
library(srvyr)
library(kableExtra)
webshot::install_phantomjs()
library(ggplot2)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(knitr)
install.packages("webshot")
library(webshot)
library(rmarkdown)


Variables=c("CODUSU","ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA", "CH04","CH06","ESTADO", "CAT_OCUP", "CAT_INAC",
            "PP04A", "PP04B_COD", "CH15", "NIVEL_ED", "PP04C", "PP03C", "PP03G", "PP03I", "PP03J", "P21", "INTENSI",  "PP07C", "PP07E", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07H", "PP07I", "DECOCUR", "PONDIIO", "PP05H")
EPH2019_2023CAES=get_microdata(year=2019:2023, period = 1,type = "individual", vars = Variables)

saveRDS(EPH2019_2023CAES, file = "EPH2019_2023CAES.rds")
EPH2019_2023CAES <- EPH2019_2023CAES %>% eph::organize_labels()
EPH2019_2023CAES=organize_caes(EPH2019_2023CAES)

TABLAP21 <- calculate_tabulates(EPH2019_2023CAES, "ANO4", "P21", "PONDERA")#lo hice para ver la distribución original de p21

#Variables nuevas y recodificadas (SEXO, PP04B_COD (CAES), PP04C(CANTIDAD EMPLEADOS), Grupos etarios, ámbito del establecimiento, región, lugar de nacimiento, nivel_educativo_completo)

#SEXO
EPH2019_2023CAES=EPH2019_2023CAES %>% mutate(SEXO=case_when(
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
colnames(EPH2019_2023CAES)

#CANTIDAD DE EMPLEADOS

summary(EPH2019_2023CAES$CANT.EMPLEADOS)
calculate_tabulates(EPH2019_2023CAES, "CANT.EMPLEADOS")

unique(EPH2019_2023CAES$CANT.EMPLEADOS)
class(EPH2019_2023CAES$CANT.EMPLEADOS)
table(as_factor(EPH2019_2023CAES$CANT.EMPLEADOS), useNA = "always")


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

#-Descripción de la población de la muestra CON GRÁFICOS 

#SEXO Y AÑO
poblacion_x_año<- EPH2019_2023CAES %>% group_by (ANO4) %>% summarize(sum(PONDERA))
sexo_año<- EPH2019_2023CAES %>% group_by (SEXO,ANO4) %>% summarize(sum(PONDERA))

# install.packages("ggplot2" y otros complementarios)

library(ggplot2)
library(scales)
library(dplyr)
library(ggthemes)
library(haven)  # Para manejar labelled
library(RColorBrewer)

# Convertir variables labelled a formatos adecuados para trabajar con ggplot2
EPH2019_2023CAES <- EPH2019_2023CAES %>% # ACÁ USÉ la base eph2019_2023 CAES. CHEQUEAR QUE NO TRAIGA PROBLEMAS MÁS ADELANTE
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    PONDERA = as.numeric(PONDERA)  # Asegurar que PONDERA sea numérico
  )

# Calcular proporciones dentro de cada año
EPH2019_2023CAES <- EPH2019_2023CAES %>%
  group_by(ANO4, SEXO) %>%
  mutate(porcentaje = PONDERA / sum(PONDERA))

# Crear gráfico
display.brewer.all()# en plots te muestra paletas de colores

graf_sexoyaño <- ggplot(EPH2019_2023CAES, aes(x = ANO4, y = PONDERA, fill = SEXO)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Set1")+
  labs(title = "Sexo de la población por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(size = "none")
print(graf_sexoyaño)

graf_sexoyaño_alternativo <- ggplot(EPH2019_2023CAES, aes(x = ANO4, y = PONDERA, fill = SEXO)) + 
  geom_bar(stat = "identity", position = "stack") + #cambié para probar cómo queda sin homogeneizar todas las barras a 100%
  scale_fill_brewer(palette="Set1")+
  labs(title = "Sexo de la población por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::comma_format()) + 
  guides(size = "none")
print(graf_sexoyaño_alternativo)


# Guardar imagen asegurando tamaño adecuado
ggsave(filename = "graf_sexoyaño.jpg", plot = graf_sexoyaño, width = 8, height = 6, dpi = 300)


#grupos etarios

unique(EPH2019_2023CAES$grupos_etarios)
gruposetarios_año_pond <- EPH2019_2023CAES %>% filter (!is.na(grupos_etarios)) %>% 
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#gráfico grupos etarios

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(ggthemes)


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
  guides(size = "none")

# Guardar imagen del gráfico
ggsave(filename = "graf_gruposetarios.jpg", plot = graf_gruposetarios, width = 8, height = 6, dpi = 300)

#distribución y porcentajes por sexo, estado, categoría ocupacional, categoría inactividad

#sexo
sexo_año_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, SEXO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#grupos etarios

gruposetarios_año_pond <- EPH2019_2023CAES %>% filter(!is.na(grupos_etarios)) %>% 
  group_by(ANO4, grupos_etarios) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#lugar de nacimiento
unique(EPH2019_2023CAES$lugar_nacimiento)

lugarnacimiento_año_pond <- EPH2019_2023CAES %>% filter(!is.na(lugar_nacimiento)) %>% 
  group_by(ANO4, lugar_nacimiento) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#gráfico lugar de nacimiento. 

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(ggthemes)

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

# Guardar imagen del gráfico
ggsave(filename = "graf_lugar_nacimiento.jpg", plot = graf_lugar_nacimiento, width = 8, height = 6, dpi = 300)

# Mostrar gráfico
print(graf_lugar_nacimiento)

#cant.empleados/tamaño del establecimiento  
cant_empleados_pond <- EPH2019_2023CAES %>% filter(!is.na(CANT.EMPLEADOS) & CANT.EMPLEADOS !=0)%>%
  group_by(ANO4, CANT.EMPLEADOS) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1)) 

#omito gráfico de cant de empleados. 

#sector de actividad
sector.de.actividad_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, caes_eph_label) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#omito gráfico de sector de activ

#condición de actividad 
cond_actividad_pond <- EPH2019_2023CAES %>% 
  group_by(ANO4, ESTADO) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))

#omito gráfico porque después graficamos las tasas del mt

#categoría ocupacional:notar que hay un valor 0 que no figura en el diseño. segun el anexo: El código 0 identifica los casos a los cuales no les corresponde la secuencia analizada. ACÁ SERÍAN LOS INACTIVOS Y MENORES DE 10
unique(EPH2019_2023CAES$CAT_OCUP)
categ_ocupacional_pond <- EPH2019_2023CAES %>% 
  filter(CAT_OCUP != 0 & CAT_OCUP !=9 ) %>%  # Excluir casos donde CAT_OCUP es 0 y 9 (NS/NR)
  group_by(ANO4, CAT_OCUP) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  # Normalizar el porcentaje para que sume 100% por cada año
  group_by(ANO4) %>% 
  mutate(Porcentaje =casos / sum(casos)) %>% # Calcular proporciones y no porcentajes permite que no se multiplique 2 veces por 100. Reacomodar si es que se desea hacer una tabla con estos datos, porque quedaría muy poco legible
  ungroup()  # Desagrupar después del cálculo

#gráfico de categoría ocupacional

# Crear gráfico de barras para la distribución de categorías ocupacionales por año.
graf_categ_ocupacional <- ggplot(categ_ocupacional_pond, aes(x = factor(ANO4), y = Porcentaje, fill = factor(CAT_OCUP))) + 
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas por cada categoría ocupacional
  scale_fill_brewer(palette = "Set3", name= "Categoría ocupacional") +  # Colores de las barras según CAT_OCUP
  labs(title = "Distribución de las categorías ocupacionales por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población")+
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar porcentaje en el eje y
  guides(size = "false")  # Eliminar leyenda para tamaño
# Mostrar gráfico
print(graf_categ_ocupacional)
# Guardar imagen del gráfico
ggsave(filename = "graf_categ_ocupacional.jpg", plot = graf_categ_ocupacional, width = 8, height = 6, dpi = 300)


#Ambito establecimiento, nivel educativo


ambitoestablecimiento_año_pond <- EPH2019_2023CAES %>% 
  filter(!is.na(ambito_establecimiento)) %>% 
  group_by(ANO4, ambito_establecimiento) %>% 
  summarize(casos = sum(PONDERA))

ambitoestablecimiento_año_pond <- ambitoestablecimiento_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    ambito_establecimiento = as_factor(ambito_establecimiento)  # Convertir la variable lugar_nacimiento a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

graf_ambitoestablecimiento <- ggplot(ambitoestablecimiento_año_pond, aes(x = ANO4, y = porcentaje, fill = ambito_establecimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "PuRd") +  # Paleta de colores
  labs(title = "Ambito del establecimiento de trabajo por año (Total ocupados)",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Población") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")
print(graf_ambitoestablecimiento)

#pruebo si es total ocupados o si es solo para ocupados asalariados (es para total de ocupados)

calculate_tabulates(
  base = EPH2019_2023CAES,
  x = "ambito_establecimiento", y = "CAT_OCUP",
  weights = "PONDERA"
)

ggsave(filename = "graf_ambitoestablecimiento.jpg", plot = graf_ambitoestablecimiento, width = 8, height = 6, dpi = 300)


#nivel educativo

niveleducativo_año_pond <- EPH2019_2023CAES %>% 
  filter(!is.na(nivel_educativo_completo)) %>% 
  group_by(ANO4, nivel_educativo_completo) %>% 
  summarize(casos = sum(PONDERA))

niveleducativo_año_pond <- niveleducativo_año_pond %>%
  mutate(
    ANO4 = as_factor(ANO4),  # Convertir año a factor
    casos = as.numeric(casos),  # Asegurar que casos sea numérico
    nivel_educativo_completo = as_factor(nivel_educativo_completo)  # Convertir la variable lugar_nacimiento a factor
  ) %>%
  group_by(ANO4) %>%
  mutate(porcentaje = casos / sum(casos)) %>%  # Calcular proporciones dentro de cada año
  ungroup()

graf_niveleducativo <- ggplot(niveleducativo_año_pond, aes(x = ANO4, y =porcentaje, fill = nivel_educativo_completo)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "PuBu") +  # Paleta de colores
  labs(title = "Nivel educativo completo por año (Total ocupados)",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Nivel educativo") +
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Mostrar en porcentaje
  guides(size = "none")
print(graf_niveleducativo)

ggsave(filename = "graf_niveleducativo.jpg", plot = graf_niveleducativo, width = 8, height = 6, dpi = 300)



#categoría de inactividad
unique(EPH2019_2023CAES$CAT_INAC)
cat_inactividad_pond <- EPH2019_2023CAES %>%  filter(!is.na(CAT_INAC)& CAT_INAC!=0 ) %>% 
  group_by(ANO4, CAT_INAC) %>% 
  summarize(casos = sum(PONDERA),.groups = "drop") %>% group_by(ANO4) %>%  
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))


#Tasas básicas del MT. Notar que utiliza la PP03j, que es la pregunta por la búsqueda de otro empleo además del que ya se tiene. 
# Calcular las tasas y formatearlas como porcentaje
library(scales)

Datos_MT_1923 <- EPH2019_2023CAES %>%  
  group_by(ANO4) %>% 
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

# Verificar el resultado
head(Datos_MT_1923)

         
library(knitr)
install.packages("webshot")
library(webshot)
library(kableExtra)
library(rmarkdown)
TasasMT_1923 <- Datos_MT_1923 %>% select(ANO4, 'Tasa Actividad', 'Tasa Inactividad', 'Tasa Empleo','Tasa Desocupacion','Tasa Subocupación')
ncol(TasasMT_1923)
names(TasasMT_1923)

# Tabla de tasas del Mercado de trabajo 2019-2023
ncol(TasasMT_1923)
colnames(TasasMT_1923)

TablaTasasMT <- TasasMT_1923 %>%
  kable(booktabs = TRUE,          
        caption = "<b>Principales tasas del mercado de trabajo. Total de 31 aglomerados urbanos. 3T-2019-2023</b>", 
        align = c('l','c','c','c'), 
        col.names = c("Año", 
                      "Tasa de actividad", 
                      "Tasa de inactividad", 
                      "Tasa de empleo", 
                      "Tasa de desocupación", 
                      "Tasa de ocupados demandantes",
                      "Tasa de subocupación", 
                      "Tasa de subocupación demandante", 
                      "Tasa de subocupación no demandante")) %>%   
  kable_material(c("striped", "hover")) %>%  
  kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = FALSE, position = "center") %>%  
  column_spec(1, bold = TRUE) %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC") %>% 
  save_kable(file = "TablaTasasMT.html", self_contained = TRUE) #esto último para que no precise recursos externos al html
webshot::webshot("TablaTasasMT.html", file = "TablaTasasMT.jpg", vwidth = 800, vheight = 600)


#TASAS DE MERCADO DE TRABAJO DESAGREGADAS
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

#TABLA PARA TASAS DEL MT DESAGREGADAS POR SEXO
TasasMT_SEXO_SELECTED <- TASAS_MT_SEXO_1923 %>% select(ANO4, SEXO, 'Tasa Actividad', 'Tasa Inactividad', 'Tasa Empleo','Tasa Desocupacion','Tasa Subocupación')
ncol(TasasMT_1923)
names(TasasMT_1923)

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
  kable_material(c("striped", "hover")) %>%  
  kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = FALSE, position = "center") %>%  
  column_spec(1, bold = TRUE) %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC") %>% 
  save_kable(file = "TablaTasasMT_SEXO.html", self_contained = TRUE) #esto último para que no precise recursos externos al html
webshot::webshot("TablaTasasMT_SEXO.html", file = "TablaTasasMT_SEXO.jpg", vwidth = 800, vheight = 600)

#TABLA PARA TASAS DEL MT DESAGREGADAS POR GRUPOS ETARIOS
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
ncol(TasasMT_1923)
names(TasasMT_1923)

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
  kable_material(c("striped", "hover")) %>%  
  kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = FALSE, position = "center") %>%  
  column_spec(1, bold = TRUE) %>% 
  footnote(symbol = "Elaboración propia en base a EPH-INDEC") %>% 
  save_kable(file = "TablaTasasMT_GRUPOSETARIOS.html", self_contained = TRUE) #esto último para que no precise recursos externos al html
webshot::webshot("TablaTasasMT_GRUPOSETARIOS.html", file = "TablaTasasMT_GRUPOSETARIOS.jpg", vwidth = 800, vheight = 600)


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

unique(EPH2019_2023CAES$PP07C)
EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(
    Preca_forma_contrat = ifelse(ESTADO == 1 & CAT_OCUP == 3, 
                                 ifelse(PP07C == 1, 10, 
                                        ifelse(PP07C == 9, 0, 0)),  # PP07C == 9 asigna 0
                                 0)  
  )
table(is.na(EPH2019_2023CAES$Preca_forma_contrat))#compruebo que no hay valores NA en la variable

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

unique(EPH2019_2023CAES$PP05H) #NA, 0, 6, 5, 2, 4, 3, 1, 9


EPH2019_2023CAES <- EPH2019_2023CAES %>% 
  mutate(
    Preca_cuentaprop = case_when(
      ESTADO == 1 & CAT_OCUP == 2 & PP05H %in% c(1,2,3,4) ~ 10,
      ESTADO == 1 & CAT_OCUP == 2 & PP05H %in% c(5,6) ~ 0,
      TRUE ~ 0  # Valor por defecto para todos los demás casos
    )
  )
table(is.na(EPH2019_2023CAES$Preca_cuentaprop))#compruebo que no hay valores NA en la variable

unique (EPH2019_2023CAES$Preca_cuentaprop)
  



calculate_tabulates(EPH2019_2023CAES, "ANO4", "Preca_cuentaprop", weight= "PONDERA", add.percentage = "col")

##PRECARIEDAD POR INGRESOS (ACA NOS TRABAMOS)

class(EPH2019_2023CAES$P21)
unique(EPH2019_2023CAES$P21)

#redefino -9 y 0 de ocupados (asalariados y cuentapropistas) como NA, para dps imputar


EPH2019_2023CAES <- EPH2019_2023CAES %>%
  mutate(P21 = as.numeric(unclass(P21)))  # Removes "labelled" class


#Imputo un ingreso a todos los valores 0,-9

eph_filtrada <- EPH2019_2023CAES %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(2, 3)) %>% #nos quedamos con asalariados y cuentapropistas
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

table(is.na(eph_filtrada$Preca_ingresos_deciles)) 
#FALSE  102475 me aseguro de que no haya valores na

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
eph_filtrada <- eph_filtrada %>%
  mutate(Preca_ingresos_buscarotrotrabajo = as.numeric(case_when(
    PP03J == 1 ~ 10,
    PP03J == 2 ~ 0,
    TRUE ~ 0  # Ahora correctamente dentro de case_when() --agrego esto para que no nos afecten el calculo los NA 26/2
  )))

table(eph_filtrada$Preca_ingresos_buscarotrotrabajo)
sum(is.na (eph_filtrada$Preca_ingresos_buscarotrotrabajo))

##INTENSIDAD EN LA JORNADA LABORAL. Creo variables pluriempleo (Preca_intensidad_pluriempleo) 

unique(eph_filtrada$PP03C)
class(eph_filtrada$PP03C)

eph_filtrada=eph_filtrada %>%
  mutate (Preca_intensidad_pluriempleo= as.numeric(case_when (
    PP03C %in% 0:1 ~0,
    PP03C == 2 ~ 10) 
  ))
table(eph_filtrada$Preca_intensidad_pluriempleo)
sum(is.na (eph_filtrada$Preca_intensidad_pluriempleo)) #0


#y sobreocupado (Preca_intensidad_sobreocup)

unique(eph_filtrada$INTENSI)
class(eph_filtrada$INTENSI)
eph_filtrada=eph_filtrada %>%
  mutate (Preca_intensidad_sobreocup= as.numeric(case_when (
    INTENSI == 3 ~ 10,
    INTENSI %in% c("1", "2", "4") ~ 0) 
  ))
table(eph_filtrada$Preca_intensidad_sobreocup)
sum(is.na (eph_filtrada$Preca_intensidad_sobreocup)) #0



eph_filtrada <- eph_filtrada %>%
  mutate(
    Preca_ingresos = as.numeric(Preca_ingresos_deciles_N +
                                  Preca_ingresos_buscarotrotrabajo), 
    Preca_intensidad = as.numeric(Preca_intensidad_sobreocup +
                                    Preca_intensidad_pluriempleo))
sum(is.na(eph_filtrada$Preca_ingresos)) #0
sum(is.na(eph_filtrada$Preca_intensidad)) #0

#Preca_cuentaprop -antigüedad en el empleo para cuentapropistas. la agrego a eph_filtrada que no estaba

eph_filtrada <- eph_filtrada %>% 
mutate(
  Preca_cuentaprop = case_when(
    ESTADO == 1 & CAT_OCUP == 2 & PP05H %in% c(1,2,3,4) ~ 10,
    ESTADO == 1 & CAT_OCUP == 2 & PP05H %in% c(5,6) ~ 0,
    TRUE ~ 0  # Valor por defecto para todos los demás casos
  )
)
sum(is.na(eph_filtrada$Preca_cuentaprop)) #0
#Preca_forma_contrat-tiempo de finalización del empleo. para asalariados

eph_filtrada <- eph_filtrada  %>%
  mutate(
    Preca_forma_contrat = ifelse(ESTADO == 1 & CAT_OCUP == 3, 
                                 ifelse(PP07C == 1, 10, 
                                        ifelse(PP07C == 9, 0, 0)),  # PP07C == 9 asigna 0
                                 0)  
  )
sum(is.na(eph_filtrada$Preca_forma_contrat)) #0

#VARIABLE DE RESUMEN: PRECARIEDAD TOTAL ASALARIADOS
colnames(eph_filtrada)

eph_filtrada <- eph_filtrada %>%
  mutate(precariedad_total_asalariados = as.numeric(Preca_ingresos +
                                                      Preca_intensidad +
                                                      Preca_cond_lab+
                                                      Preca_forma_contrat 
  ))
sum(is.na(eph_filtrada$precariedad_total_asalariados))#0

eph_filtrada <- eph_filtrada %>%
  mutate(precariedad_total_cuentapropistas = as.numeric(Preca_ingresos +
                                                          Preca_intensidad +
                                                          Preca_cuentaprop  
  ))

sum(is.na(eph_filtrada$precariedad_total_cuentapropistas))#0

unique(eph_filtrada$precariedad_total_cuentapropistas)
table(eph_filtrada$precariedad_total_cuentapropistas)


#variables por tipo de precarización y una que agrupa tres categorías de precariedad: 

#eph_filtrada <- eph_filtrada %>% 
 # mutate (Niveles_precariedad_total = factor(case_when(
  #  precariedad_total%in% 0:30  ~ "Precariedad baja",
   # precariedad_total%in% 31:60 ~ "Precariedad media",
   # precariedad_total%in% 61:100 ~ "Precariedad alta"
 # ), levels = c("Precariedad baja", "Precariedad media", "Precariedad alta")))

#EPH FILTRADA 2 ES LA QUE TIENE LA VARIABLE NIVELES DE PRECARIEDAD TOTAL
eph_filtrada2 <- eph_filtrada %>% 
  mutate(Niveles_precariedad_total = case_when(
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 0:20   ~ "Precariedad baja",
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 21:40  ~ "Precariedad media",
    CAT_OCUP == 2 & precariedad_total_cuentapropistas %in% 41:70 ~ "Precariedad alta",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 0:30   ~ "Precariedad baja",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 31:60  ~ "Precariedad media",
    CAT_OCUP == 3 & precariedad_total_asalariados %in% 61:100 ~ "Precariedad alta",
    TRUE ~ NA_character_  # Para otros valores de CAT_OCUP
  ) %>% factor(levels = c("Precariedad baja", "Precariedad media", "Precariedad alta"
  )))
sum(is.na(eph_filtrada2$Niveles_precariedad_total))#0
table(eph_filtrada2$Niveles_precariedad_total) 
colnames(eph_filtrada2)

Precariedad_por_ano <- eph_filtrada2 %>% #cambié el nombre de la base 
  group_by(ANO4, Niveles_precariedad_total) %>%
  count() %>%
  rename(Cantidad = n)

eph_filtrada %>% 
  select(SEXO, ANO4, P21, P21_imputado_AC, Decil_imputado, Preca_ingresos_deciles) -> base_chiquita
view(base_chiquita)

eph_filtrada2 %>% 
  select(P21_imputado_AC, Preca_ingresos, Preca_cond_lab, Preca_forma_contrat, Preca_intensidad, Niveles_precariedad_total) -> base_precariedad
view(base_precariedad)
unique(eph_filtrada$Niveles_precariedad_total)

precariedad_cuentaprop <- eph_filtrada %>% 
  filter(ESTADO == 1 & CAT_OCUP == 2) %>%
  group_by(ANO4, Preca_cond_lab, Preca_ingresos, Preca_forma_contrat, Preca_intensidad) %>%  
  summarise(casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))



print(Precariedad_por_ano)
# PUNTO 3: 

#GRÁFICO DE niveles de precariedad total por año PARA ASALARIADOS Y CUENTAPROPISTAS 

graf_precariedadxcatocup <- eph_filtrada2 %>%
  filter(!is.na(CAT_OCUP)) %>% ggplot( aes(x = factor(ANO4), fill = Niveles_precariedad_total)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~ CAT_OCUP, drop = TRUE) +  # Separar por categorías ocupacionales
  scale_fill_brewer(palette = "Oranges")+ #es parte de las paletas secuenciales que permiten ver gradualidad
  labs(title = "Niveles de precariedad por categoría ocupacional y año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Frecuencia") +
  theme_calc()+
  guides(fill = guide_legend(title = "Niveles de precariedad"))

print(graf_precariedadxcatocup)
ggsave(filename = "graf_precariedadxcatocup.jpg", plot = graf_precariedadxcatocup, width = 8, height = 6, dpi = 300)

# seg

precariedadxingresos_catocup2y3 <-  eph_filtrada %>% 
  group_by(ANO4, CAT_OCUP, Preca_ingresos) %>% 
  summarize(casos = sum(PONDERA)) %>% 
  mutate(Porcentaje = round((casos / sum(casos)) * 100, 1))



graf_nivelesprecariedadtotal <- ggplot(eph_filtrada2, aes(x = factor(ANO4), fill = Niveles_precariedad_total)) + 
  geom_bar(position = "fill") +  # Removí stat = "identity" ya que no es necesario
  guides(fill = guide_legend(title = "Niveles de precariedad")) +
  scale_fill_brewer(palette = "Set3", labels = c("Alta", "Media", "Baja")) +  # Moví labels aquí
  labs(title = "Nivel de precariedad total por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Porcentaje") +  # Cambio "Frecuencia" por "Porcentaje"
  theme_calc() +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +  # Mostrar porcentaje en el eje y
  guides(size = "none")  # Corrige el error de "false"

# Mostrar gráfico
print(graf_nivelesprecariedadtotal)
# Guardar imagen del gráfico
ggsave(filename = "graf_nivelesprecariedadtotal.jpg", plot = graf_nivelesprecariedadtotal, width = 8, height = 6, dpi = 300)
#otro formato-creo que mejor
graf2_nivelesprecariedadtotal <- ggplot(eph_filtrada2, aes(x = factor(ANO4), fill = Niveles_precariedad_total)) + 
  geom_bar(position = "dodge") +  # "dodge" agrupa las barras una al lado de la otra
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Niveles de precariedad por año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año", y = "Frecuencia",
       fill = "Nivel de precariedad") +
  theme_calc()
ggsave(filename = "graf2_nivelesprecariedadtotal.jpg", plot = graf2_nivelesprecariedadtotal, width = 8, height = 6, dpi = 300)





colnames(eph_filtrada)

tabulados <- list(
  g_etarios_x_precariedad = calculate_tabulates(eph_filtrada2, 
                                                x = "grupos_etarios", 
                                                y = "Niveles_precariedad_total", 
                                                weights = "PONDERA", 
                                                add.percentage = "row"),
 
  ambito_x_precariedad = calculate_tabulates(eph_filtrada2, 
                                             x = "ambito_establecimiento", 
                                             y = "Niveles_precariedad_total", 
                                             weights = "PONDERA", 
                                             add.percentage = "row"),
  
  sexo_x_precariedad = calculate_tabulates(eph_filtrada2, 
                                           x = "SEXO", 
                                           y = "Niveles_precariedad_total", 
                                           weights = "PONDERA", 
                                           add.percentage = "row"),
  
  lugarnacimiento_x_precariedad = calculate_tabulates(eph_filtrada2, 
                                                      x = "lugar_nacimiento", 
                                                      y = "Niveles_precariedad_total", 
                                                      weights = "PONDERA", 
                                                      add.percentage = "row"),
  
  ambito_x_precaingresos = calculate_tabulates(eph_filtrada2, 
                                               x = "ambito_establecimiento", 
                                               y = "Preca_ingresos", 
                                               weights = "PONDERA", 
                                               add.percentage = "row"), 
  ambito_x_precaintensidad = calculate_tabulates(eph_filtrada2, 
                                                 x = "ambito_establecimiento", 
                                                 y = "Preca_intensidad", 
                                                 weights = "PONDERA", 
                                                 add.percentage = "row"))


#GRÁFICOS PARA VER PRECARIEDAD SEGÚN NUESTRAS VARS DE INTERES:grupos etarios, sexo, lugar de nacimiento

#grupos etarios
sum (is.na)
graf_precariedadxgruposetarios <- eph_filtrada2 %>%
  filter(!is.na(grupos_etarios)) %>% ggplot( aes(x = factor(ANO4), fill = Niveles_precariedad_total)) +
  geom_bar(position = "dodge") +  # Cambia a "fill" para ver proporciones
  facet_wrap(~ grupos_etarios, drop = TRUE) +  # Separar por grupos etarios 
  scale_fill_brewer(palette = "Paired")+
  labs(title = "Niveles de precariedad por grupo etario y año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Frecuencia") +
  theme_calc()+
guides(fill = guide_legend(title = "Niveles de Precariedad"))

print(graf_precariedadxgruposetarios)
ggsave(filename = "graf_precariedadxgruposetarios.jpg", plot = graf_precariedadxgruposetarios, width = 8, height = 6, dpi = 300)
# sexo

graf_precariedadxsexo <- eph_filtrada2 %>%
  filter(!is.na(grupos_etarios)) %>% ggplot( aes(x = factor(ANO4), fill = Niveles_precariedad_total)) +
  geom_bar(position = "dodge") +  
  facet_wrap(~ SEXO, drop = TRUE) +  # Separar por sexo
  scale_fill_brewer(palette = "BuGn")+
  labs(title = "Niveles de precariedad por sexo y año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Frecuencia") +
  theme_calc()+
  guides(fill = guide_legend(title = "Niveles de Precariedad"))

print(graf_precariedadxgruposetarios)
ggsave(filename = "graf_precariedadxsexo.jpg", plot = graf_precariedadxsexo, width = 8, height = 6, dpi = 300)

#lugar de nacimiento: no queda un gráfico muy bueno
graf_precariedadxlugarnac <- eph_filtrada2 %>%
  filter(!is.na(lugar_nacimiento)) %>% ggplot( aes(x = factor(ANO4), fill = Niveles_precariedad_total)) +
  geom_bar(position = "dodge") +  
  facet_wrap(~ lugar_nacimiento, drop = TRUE) +  # Separar por sexo
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Niveles de precariedad por lugar de nacimiento y año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Frecuencia") +
  theme_calc()+
  guides(fill = guide_legend(title = "Niveles de Precariedad"))
display.brewer.all()
print(graf_precariedadxlugarnac)
ggsave(filename = "graf_precariedadxlugarnac.jpg", plot = graf_precariedadxlugarnac, width = 8, height = 6, dpi = 300)

#GRAFICOS INTERACTIVOS

library(ggplot2)
library(plotly)
library(htmlwidgets)

# gráfico de precariedad total
colnames (eph_filtrada2)
unique (eph_filtrada2$Niveles_precariedad_total)

TasasPrecariedad_1923 <- eph_filtrada2 %>%  #calculo tasas para que me aparezcan mejor los valores de porcentajes en el grafico posterior
  group_by(ANO4) %>% 
  summarize(
    Poblacion = sum(PONDERA),
    Ocupados = sum(PONDERA[ESTADO == 1]),
    Precarios_bajos = sum(PONDERA[Niveles_precariedad_total == "Precariedad baja"]),
    Precarios_medios = sum(PONDERA[Niveles_precariedad_total == "Precariedad media"]),
    Precarios_altos = sum(PONDERA[Niveles_precariedad_total == "Precariedad alta"]),
    'Tasa de precariedad baja' = (Precarios_bajos / Ocupados) * 100,
    'Tasa de precariedad media' = (Precarios_medios / Ocupados) * 100,
    'Tasa de precariedad alta' = (Precarios_altos / Ocupados) * 100
  ) %>%  # El pipe debe ir después de summarize
  mutate(across(starts_with("Tasa"), ~ round(., 1)))  # Redondear las tasas a 1 decimal

TasasPrecariedad_1923_long <- TasasPrecariedad_1923 %>%
  pivot_longer(cols = starts_with("Tasa"), #La función pivot_longer() convierte las columnas de las tasas de precariedad (Tasa de precariedad baja, Tasa de precariedad media, etc.) en una única columna llamada Tasa_tipo, con los valores correspondientes en la columna Valor_tasa.
               names_to = "Tasa_tipo", 
               values_to = "Valor_tasa")
# Crear gráfico con ggplot
grafico_Tasasprecariedad <- ggplot(TasasPrecariedad_1923_long, aes(x = factor(ANO4), y = Valor_tasa, fill = Tasa_tipo)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Tasas de Precariedad por Año",
       subtitle = "Total de 31 aglomerados. Terceros trimestres de 2019-2023",
       x = "Año",
       y = "Tasa (%)",
       fill = "Tipo de tasa") + 
  scale_fill_brewer(palette = "Reds")+
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1, big.mark = ".", decimal.mark = ","))+
  aes(text = paste("Año: ", ANO4, #incluí esta línea para que aparezca el nombre de la variable en la etiqueta (x ej Año en lugar de ANO4), pero no hay caso  
                   "<br>Tasa: ", round(Valor_tasa, 1), "%", 
                   "<br>Tipo de tasa: ", Tasa_tipo)) 

# Convertir a gráfico interactivo y personalizar las etiquetas emergentes
grafinteractivo_precatotal <- ggplotly(grafico_Tasasprecariedad, 
                                       tooltip = c("x", "y", "fill"))  # Especifica las variables

# Mostrar el gráfico interactivo
grafinteractivo_precatotal

# Guardar el gráfico estático como PNG
ggsave("grafico_precariedad.png", plot = grafinteractivo_precatotal, width = 8, height = 6, dpi = 300)

# Guardar el gráfico interactivo como HTML
saveWidget(graf_interactivo, "grafico_precariedad.html", selfcontained = TRUE)
