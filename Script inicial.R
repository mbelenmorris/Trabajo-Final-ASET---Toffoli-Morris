library(eph)
prueba_get_microdata <- get_microdata(year = 2019, trimester = 3, type = "individual")
prueba_get_microdata2 <- get_microdata(year = 2019:2023, trimester = 3, type = "individual")
summary(prueba_get_microdata2)
min(prueba_get_microdata2$ANO4)
max(prueba_get_microdata2$ANO4)
library(openxlsx)
  