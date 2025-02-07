library(eph)
base3T_19_23 <- get_microdata(year = 2019:2023, trimester = 3, type = "individual")
min(base3T_19_23$ANO4)
max(base3T_19_23$ANO4)
saveRDS(base3T_19_23, file = "base3T_19_23.rds")
base3T_19_23=readRDS("base3T_19_23.rds")


