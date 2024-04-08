# rm(list = ls())
# library(readr)
# library(dplyr)
# 
# DEVOLUCOES <- read_delim("C:/Users/Multipel-Supervisor/Desktop/TESTE PND/DEVOLUCOES.csv", 
#                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
# colnames(DEVOLUCOES) <-  c("CODCLI", "VL_DEV", "QT_DEV")
# 
# DATA <- read_delim("C:/Users/Multipel-Supervisor/Desktop/TESTE PND/DATA.csv", 
#                    delim = ";", escape_double = FALSE, col_types = cols(CODCLI = col_integer(), 
#                                                                         DATA = col_date(format = "%d/%m/%Y")), 
#                    locale = locale(decimal_mark = ",", grouping_mark = "."), 
#                    trim_ws = TRUE)
# 
# 
# 
# 
# dados <- DATA %>% group_by(CODCLI, DATA) %>% summarise(VENDAS = sum(VLATEND),
#                                                        NUMITENS = sum(NUMITENS),
#                                                        LIMCRED = mean(LIMCRED))
# 
# rm(DATA)
# 
# temp <- dados %>% arrange(DATA) %>% filter(DATA >= as.Date("2024-02-01")) %>% group_by(CODCLI) %>%
#   summarise(ultcomp = max(DATA),
#             primcomp_depoisjanela = min(DATA))
# 
# x <- dados %>% arrange(DATA) %>% filter(DATA < as.Date("2024-02-01")) %>% group_by(CODCLI) %>%
#   summarise(dcadastro_dias = as.Date("2024-01-31") - min(DATA),
#             d1 = rev(diff(DATA))[1],
#             d2 = rev(diff(DATA))[2],
#             d3 = rev(diff(DATA))[3],
#             dmedia = mean(c(d1,d2,d3)),
#             ddesvio = sd(c(d1,d2,d3)),
#             freq = n(),
#             tgasto = sum(VENDAS),
#             ultcompjanela = max(DATA),
#   ) %>% filter(freq > 3)
# 
# 
# rfv <- tibble(r = as.numeric(as.Date("2024-01-31") - x$ultcompjanela),
#               f = x$freq,
#               v = x$tgasto,
#               CODCLI = x$CODCLI)
# 
# 
# 
# x <- x %>% left_join(temp)
# x$dia_primcompultcomp <- x$primcomp_depoisjanela - x$ultcompjanela
# x <- x %>% select(-c("ultcomp", "primcomp_depoisjanela", "ultcompjanela"))
# x <- x %>% left_join(DEVOLUCOES, by = 'CODCLI')
# 
# 
# x <- as_tibble(lapply(x, as.numeric))
# 
# 
# x <- x %>% mutate(QT_DEV = ifelse(is.na(QT_DEV), 0, QT_DEV),
#                   VL_DEV = ifelse(is.na(VL_DEV), 0, VL_DEV),
#                   COMPROU = ifelse(is.na(dia_primcompultcomp), "N", "S"))
# 
# colnames(x)[10] = "Y"
# 
# 
# rfv$r[rfv$r < quantile(rfv$r)[2]] = 4
# rfv$r[rfv$r >= quantile(rfv$r)[2] & rfv$r < quantile(rfv$r)[3]] = 3
# rfv$r[rfv$r >= quantile(rfv$r)[3] & rfv$r < quantile(rfv$r)[4]] = 2
# rfv$r[rfv$r >= quantile(rfv$r)[4]] = 1
# 
# rfv$f[rfv$f < quantile(rfv$f)[2]] = 1
# rfv$f[rfv$f >= quantile(rfv$f)[2] & rfv$f < quantile(rfv$f)[3]] = 2
# rfv$f[rfv$f >= quantile(rfv$f)[3] & rfv$f < quantile(rfv$f)[4]] = 3
# rfv$f[rfv$f >= quantile(rfv$f)[4]] = 4
# 
# rfv$v[rfv$v < quantile(rfv$v)[2]] = 1
# rfv$v[rfv$v >= quantile(rfv$v)[2] & rfv$v < quantile(rfv$v)[3]] = 2
# rfv$v[rfv$v >= quantile(rfv$v)[3] & rfv$v < quantile(rfv$v)[4]] = 3
# rfv$v[rfv$v >= quantile(rfv$v)[4]] = 4
# 
# rfv <- rfv %>% mutate(rfv = r + f + v)
# 
# rfv$cluster[rfv$rfv < quantile(rfv$rfv)[2]] = 'bronze customers'
# rfv$cluster[rfv$rfv >= quantile(rfv$rfv)[2] & rfv$rfv < quantile(rfv$rfv)[3]] = 'silver customers'
# rfv$cluster[rfv$rfv >= quantile(rfv$rfv)[3] & rfv$rfv < quantile(rfv$rfv)[4]] = 'gold customers'
# rfv$cluster[rfv$rfv >= quantile(rfv$rfv)[4]] = 'diamond customers'
# 
# x <- x %>% left_join(rfv)
# 
# 
# #write.table(x, "base_tratada.csv", sep = ";", row.names = F)
# 

library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)
library(patchwork)
library(ggcorrplot)
library(baguette)
library(poissonreg)
tidymodels_prefer()

dados_censurados <- base_tratada %>%
  filter_all(any_vars(is.na(.)))

dados <- base_tratada %>%
  filter_all(all_vars(!is.na(.)))

dados1 <- dados %>% select(c("rfv", "dmedia", "ddesvio", "dcadastro_dias", "tgasto", "freq", "VL_DEV", "QT_DEV", "Y"))