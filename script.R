rm(list = ls())
gc()
library(dplyr)
library(readr)
dados <- read_delim("/content/drive/MyDrive/TCC ARQUIVOS/raw data.csv", delim = ";",
                    escape_double = FALSE, col_types = cols(CODCLI = col_integer(),
                                                            DATA = col_date(format = "%d/%m/%Y")),
                    locale = locale(decimal_mark = ",", grouping_mark = "."),
                    trim_ws = TRUE)

head(dados)

colnames(dados) <- c('CODCLI', 'VLATEND', 'DATA')
old <- dados


dados <- dados %>% group_by(CODCLI, DATA) %>% summarise(VENDAS = sum(VLATEND))


temp <- dados %>% arrange(DATA) %>% filter(DATA >= as.Date("2024-01-01")) %>% group_by(CODCLI) %>%
  summarise(ultcomp = max(DATA),
            primcomp_depoisjanela = min(DATA))


x <- dados %>% arrange(DATA) %>% filter(DATA < as.Date("2024-01-01")) %>% group_by(CODCLI) %>%
  summarise(dcadastro = min(DATA), #data do cadastro
            dcadastro_dias = as.Date("2023-12-31") - min(DATA),
            d1 = rev(diff(DATA))[1],
            d2 = rev(diff(DATA))[2],
            d3 = rev(diff(DATA))[3],
            dmedia = mean(c(d1,d2,d3)),
            ddesvio = sd(c(d1,d2,d3)),
            freq = n(),
            tgasto = sum(VENDAS),
            ultcompjanela = max(DATA),
  ) %>% filter(freq > 3)



dados <- x %>% left_join(temp)

dados$dia_ultcomp_primcomp <- dados$primcomp_depoisjanela - dados$ultcompjanela

rfv <- tibble(r = as.numeric(as.Date("2023-12-31") - dados$ultcompjanela),
              f = dados$freq,
              v = dados$tgasto,
              CODCLI = dados$CODCLI)


((ggplot(dados, aes(x = rfv$r)) +
    geom_histogram(bins = 45, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Histograma da Recência das Compras",
         x = "Valores",
         y = "Frequência") + xlim(0, 300)) +
    
    (ggplot(dados, aes(x = rfv$f)) +
       geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
       labs(title = "Histograma da Frequência de Compra",
            x = "Valores",
            y = "Frequência")) +
    
    (ggplot(rfv, aes(x = v)) +
       geom_histogram(bins = 35, fill = "skyblue", color = "black", alpha = 0.7) +
       labs(title = "Histograma do Valor Gasto",
            x = "Valores",
            y = "Frequência") + xlim(0, 100000)) )


rfv$r[rfv$r < quantile(rfv$r)[2]] = 4
rfv$r[rfv$r >= quantile(rfv$r)[2] & rfv$r < quantile(rfv$r)[3]] = 3
rfv$r[rfv$r >= quantile(rfv$r)[3] & rfv$r < quantile(rfv$r)[4]] = 2
rfv$r[rfv$r >= quantile(rfv$r)[4]] = 1

rfv$f[rfv$f < quantile(rfv$f)[2]] = 1
rfv$f[rfv$f >= quantile(rfv$f)[2] & rfv$f < quantile(rfv$f)[3]] = 2
rfv$f[rfv$f >= quantile(rfv$f)[3] & rfv$f < quantile(rfv$f)[4]] = 3
rfv$f[rfv$f >= quantile(rfv$f)[4]] = 4

rfv$v[rfv$v < quantile(rfv$v)[2]] = 1
rfv$v[rfv$v >= quantile(rfv$v)[2] & rfv$v < quantile(rfv$v)[3]] = 2
rfv$v[rfv$v >= quantile(rfv$v)[3] & rfv$v < quantile(rfv$v)[4]] = 3
rfv$v[rfv$v >= quantile(rfv$v)[4]] = 4

rfv <- rfv %>% mutate(rfv = r + f + v)

rfv$grupo2[rfv$rfv < quantile(rfv$rfv)[2]] = 'bronze customers'
rfv$grupo2[rfv$rfv >= quantile(rfv$rfv)[2] & rfv$rfv < quantile(rfv$rfv)[3]] = 'silver customers'
rfv$grupo2[rfv$rfv >= quantile(rfv$rfv)[3] & rfv$rfv < quantile(rfv$rfv)[4]] = 'gold customers'
rfv$grupo2[rfv$rfv >= quantile(rfv$rfv)[4]] = 'diamond customers'

dados <- dados %>% left_join(rfv)


dados <- dados %>% select(-c("dcadastro", "ultcompjanela", "ultcomp", "primcomp_depoisjanela"))

dados_censurados <- dados %>%
  filter_all(any_vars(is.na(.)))

dados <- dados %>%
  filter_all(all_vars(!is.na(.)))


write.table(dados, "/content/drive/MyDrive/TCC ARQUIVOS/data.csv", sep = ";")