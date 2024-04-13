rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

#setwd("/Users/marceloferreira/Dropbox/DE-UFPB/disciplinas/2023-2/TCC II/Eraldo")
setwd("C:/Users/erald/Desktop/tcc")


DEVOLUCOES <- read_delim("DEVOLUCOES.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(DEVOLUCOES) <-  c("CODCLI", "VL_DEV", "QT_DEV")

DATAdf <- read_delim("DATA.csv",
                     delim = ";", escape_double = FALSE,
                     col_types = cols(CODCLI = col_integer(),
                                      DATA = col_date(format = "%d/%m/%Y")),
                     locale = locale(decimal_mark = ",", grouping_mark = "."),
                     trim_ws = TRUE)


dados <- DATAdf %>% group_by(CODCLI, DATA) %>% summarise(VENDAS = sum(VLATEND),
                                                         NUMITENS = sum(NUMITENS),
                                                         LIMCRED = mean(LIMCRED))

rm(DATAdf)

temp <- dados %>%
  arrange(DATA) %>%
  filter(DATA >= as.Date("2024-02-01")) %>%
  group_by(CODCLI) %>%
  summarise(ultcomp = max(DATA),
            primcomp_depoisjanela = min(DATA))

x <- dados %>%
  arrange(desc(DATA)) %>%
  filter(DATA < as.Date("2024-02-01")) %>%
  group_by(CODCLI) %>%
  summarise(dcadastro_dias = as.Date("2024-01-31") - min(DATA),
            d1 = abs(diff(DATA)[1]),
            d2 = abs(diff(DATA)[2]),
            d3 = abs(diff(DATA)[3]),
            dmedia = mean(c(d1,d2,d3)),
            ddesvio = sd(c(d1,d2,d3)),
            r = as.numeric(as.Date("2024-01-31") - max(DATA)),
            f = n(),
            v = sum(VENDAS),
            ultcompjanela = max(DATA)
  ) %>% filter(f > 3)

# rfv <- tibble(r = as.numeric(as.Date("2024-01-31") - x$ultcompjanela),
#               f = x$freq,
#               v = x$tgasto,
#               CODCLI = x$CODCLI)

x <- x %>% left_join(temp, by = "CODCLI")
x$Y <- x$primcomp_depoisjanela - x$ultcompjanela
x <- x %>% select(-c("ultcomp", "primcomp_depoisjanela", "ultcompjanela"))
x <- x %>% left_join(DEVOLUCOES, by = 'CODCLI')


x <- as_tibble(lapply(x, as.numeric))


x <- x %>% mutate(QT_DEV = ifelse(is.na(QT_DEV), 0, QT_DEV),
                  VL_DEV = ifelse(is.na(VL_DEV), 0, VL_DEV),
                  COMPROU = ifelse(is.na(Y), "N", "S"))

x %>% ggplot() +
  geom_histogram(aes(x = r))

x %>%
  filter(r <= 100) %>%
  ggplot() +
  geom_histogram(aes(x = r))

x %>%
  filter(r <= 200) %>%
  ggplot() +
  geom_histogram(aes(x = r))

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

source("teste/elbow_method.R")

# Clustering recency

set.seed(1313)
elbow.plot(as.matrix(x$r))

set.seed(1313)
fit.recency <- kmeans(as.matrix(x$r), centers = 4, iter.max = 100, nstart = 30)
table(fit.recency$cluster)

# Clustering frequency

set.seed(1313)
elbow.plot(as.matrix(x$f))

set.seed(1313)
fit.frequency <- kmeans(as.matrix(x$f), centers = 4, iter.max = 100, nstart = 30)
table(fit.frequency$cluster)

# Clustering value

set.seed(1313)
elbow.plot(as.matrix(x$v))

set.seed(1313)
fit.value <- kmeans(as.matrix(x$v), centers = 4, iter.max = 100, nstart = 30)
table(fit.value$cluster)

cl.r <- fit.recency$cluster
cl.f <- fit.frequency$cluster
cl.v <- fit.value$cluster

table(cl.r,cl.f)
table(cl.r,cl.v)
table(cl.f,cl.v)

cl_df <- tibble(r=x$r,f=x$f,v=x$v,
                clr=as.factor(cl.r),clf=as.factor(cl.f),clv=as.factor(cl.v))
p1 <- cl_df %>%
  ggplot() +
  geom_point(aes(x = r, y = f, color = clr))
p2 <- cl_df %>%
  ggplot() +
  geom_point(aes(x = r, y = f, color = clf))
p3 <- cl_df %>%
  ggplot() +
  geom_point(aes(x = r, y = v, color = clr))
p4 <- cl_df %>%
  ggplot() +
  geom_point(aes(x = r, y = v, color = clv))

(p1 + p2) / (p3 + p4)

x %>% ggplot() +
  geom_point(aes(x = r, y = f))

x %>% ggplot() +
  geom_point(aes(x = r, y = v))

x %>% ggplot() +
  geom_point(aes(x = f, y = v))

tmp = tibble(r = fit.recency$cluster, f = fit.frequency$cluster, v = fit.value$cluster)

tmp %>% ggplot() +
  geom_point(aes(x = r, y = f))

tmp %>% ggplot() +
  geom_point(aes(x = r, y = v))

tmp %>% ggplot() +
  geom_point(aes(x = f, y = v))

cl_df %>%
  group_by(clr) %>%
  summarise(mu = mean(r))

cl_df %>%
  group_by(clf) %>%
  summarise(mu = mean(f))

cl_df %>%
  group_by(clv) %>%
  summarise(mu = mean(v))

x_original = x

cl_df <- tibble(r=x$r,f=x$f,v=x$v,
                clr=cl.r,clf=cl.f,clv=cl.v)

cl_df$clr = cl_df$clr + 4
cl_df$clf = cl_df$clf + 4
cl_df$clv = cl_df$clv + 4

cl_df$clr[cl_df$clr == 5] <- 0
cl_df$clr[cl_df$clr == 7] <- 1
cl_df$clr[cl_df$clr == 6] <- 2
cl_df$clr[cl_df$clr == 8] <- 3

cl_df$clf[cl_df$clf == 7] <- 0
cl_df$clf[cl_df$clf == 5] <- 1
cl_df$clf[cl_df$clf == 8] <- 2
cl_df$clf[cl_df$clf == 6] <- 3

cl_df$clv[cl_df$clv == 5] <- 0
cl_df$clv[cl_df$clv == 7] <- 1
cl_df$clv[cl_df$clv == 6] <- 2
cl_df$clv[cl_df$clv == 8] <- 3

x$clr <- cl_df$clr
x$clf <- cl_df$clf
x$clv <- cl_df$clv
x$rfv <- cl_df$clr + cl_df$clf + cl_df$clv

x %>% ggplot() +
  geom_histogram(aes(x = rfv), bins = 4)

quantile(x$rfv)

x$cluster[x$rfv <= quantile(x$rfv)[2]] <- 1
x$cluster[x$rfv > quantile(x$rfv)[2] & x$rfv <= quantile(x$rfv)[4]] <- 2
x$cluster[x$rfv > quantile(x$rfv)[4]] <- 3

x$cluster <- factor(x$cluster,
                    levels = 1:3,
                    labels = c("low value costumers","mid value costumers","high value costumers"),
                    ordered = TRUE)

table(x$cluster)

write_csv(x, "base_tratada_v2_kmeans.csv")

p1 <- x %>%
  ggplot() +
  geom_point(aes(x = r, y = f, color = cluster))
p2 <- x %>%
  ggplot() +
  geom_point(aes(x = r, y = v, color = cluster))
p3 <- x %>%
  ggplot() +
  geom_point(aes(x = f, y = v, color = cluster))

p1 / p2 / p3
