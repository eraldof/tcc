library(ggplot2)
library(e1071) # Fuzzy K-médias
library(factoextra)

# Função que obtém o WSS para o método hierárquico
get_wss <- function(d, cluster){
  d <- stats::as.dist(d)
  cn <- max(cluster)
  clusterf <- as.factor(cluster)
  clusterl <- levels(clusterf)
  cnn <- length(clusterl)
  
  if (cn != cnn) {
    warning("cluster renumbered because maximum != number of clusters")
    for (i in 1:cnn) cluster[clusterf == clusterl[i]] <- i
    cn <- cnn
  }
  cwn <- cn
  # Compute total within sum of square
  dmat <- as.matrix(d)
  within.cluster.ss <- 0
  for (i in 1:cn) {
    cluster.size <- sum(cluster == i)
    di <- as.dist(dmat[cluster == i, cluster == i])
    within.cluster.ss <- within.cluster.ss + sum(di^2)/cluster.size
  }
  within.cluster.ss
}

# Função que contrói o "gráfico do cotovelo" e aponta o K ótimo
elbow.plot <- function(x, kmax = 15, alg = "kmeans", niter = 100, nstarts = 30) {
  # alg = c("kmeans", "cmeans", "hclust")
  wss <- c()
  if (alg == "kmeans") {
    for (i in 1:kmax) {
      set.seed(13)
      tmp <- kmeans(x, i, iter.max = niter, nstart = nstarts)
      # wss[i] <- get_wss(dist(x), tmp$cluster)
      wss[i] <- tmp$tot.withinss
    }
    tmp <- data.frame(k = 1:kmax, wss)
    max_k <- max(tmp$k)
    max_k_wss <- tmp$wss[which.max(tmp$k)]
    max_wss <- max(tmp$wss)
    max_wss_k <- tmp$k[which.max(tmp$wss)]
    max_df <- data.frame(x = c(max_wss_k, max_k), y = c(max_wss, max_k_wss))
    tmp_lm <- lm(max_df$y ~ max_df$x)
    d <- c()
    for(i in 1:kmax) {
      d <- c(d, abs(coef(tmp_lm)[2]*i - tmp$wss[i] + coef(tmp_lm)[1]) /
               sqrt(coef(tmp_lm)[2]^2 + 1^2))
    }
    tmp$d <- d
    ggplot(data = tmp, aes(k, wss)) +
      geom_line() +
      geom_segment(aes(x = k[1], y = wss[1],
                       xend = max(k), yend = wss[which.max(k)]),
                   linetype = "dashed") +
      geom_point(aes(size = (d == max(d)), color = (d == max(d))),
                 show.legend = FALSE) +
      scale_size_manual(values = c(2,5)) +
      scale_color_manual(values = c("black", "red")) +
      labs(x = "Number of clusters",
           y = "Total within-cluster sum of squares",
           title = "Elbow plot for the K-means method") +
      theme_bw()
  }
  else if (alg == "cmeans") {
    for (i in 1:kmax) {
      if (i == 1) {
        wss[i] <- get_wss(dist(x), rep(1, nrow(x)))
      }
      else {
        set.seed(13)
        tmp <- cmeans(x, i, iter.max = niter)
        wss[i] <- get_wss(dist(x), tmp$cluster)
        # wss[i] <- tmp$sumsqrs$tot.within.ss
      }
    }
    tmp <- data.frame(k = 1:kmax, wss)
    max_k <- max(tmp$k)
    max_k_wss <- tmp$wss[which.max(tmp$k)]
    max_wss <- max(tmp$wss)
    max_wss_k <- tmp$k[which.max(tmp$wss)]
    max_df <- data.frame(x = c(max_wss_k, max_k), y = c(max_wss, max_k_wss))
    tmp_lm <- lm(max_df$y ~ max_df$x)
    d <- c()
    for(i in 1:kmax) {
      d <- c(d, abs(coef(tmp_lm)[2]*i - tmp$wss[i] + coef(tmp_lm)[1]) /
               sqrt(coef(tmp_lm)[2]^2 + 1^2))
    }
    tmp$d <- d
    ggplot(data = tmp, aes(k, wss)) +
      geom_line() +
      geom_segment(aes(x = k[1], y = wss[1],
                       xend = max(k), yend = wss[which.max(k)]),
                   linetype = "dashed") +
      geom_point(aes(size = (d == max(d)), color = (d == max(d))),
                 show.legend = FALSE) +
      scale_size_manual(values = c(2,5)) +
      scale_color_manual(values = c("black", "red")) +
      labs(x = "Number of clusters",
           y = "Total within-cluster sum of squares",
           title = "Elbow plot for the fuzzy K-means method") +
      theme_bw()
  }
  else if (alg == "hclust") {
    for (i in 1:kmax) {
      set.seed(13)
      tmp <- hcut(x, i)
      wss[i] <- get_wss(dist(x), tmp$cluster)
    }
    tmp <- data.frame(k = 1:kmax, wss)
    max_k <- max(tmp$k)
    max_k_wss <- tmp$wss[which.max(tmp$k)]
    max_wss <- max(tmp$wss)
    max_wss_k <- tmp$k[which.max(tmp$wss)]
    max_df <- data.frame(x = c(max_wss_k, max_k), y = c(max_wss, max_k_wss))
    tmp_lm <- lm(max_df$y ~ max_df$x)
    d <- c()
    for(i in 1:kmax) {
      d <- c(d, abs(coef(tmp_lm)[2]*i - tmp$wss[i] + coef(tmp_lm)[1]) /
               sqrt(coef(tmp_lm)[2]^2 + 1^2))
    }
    tmp$d <- d
    ggplot(data = tmp, aes(k, wss)) +
      geom_line() +
      geom_segment(aes(x = k[1], y = wss[1],
                       xend = max(k), yend = wss[which.max(k)]),
                   linetype = "dashed") +
      geom_point(aes(size = (d == max(d)), color = (d == max(d))),
                 show.legend = FALSE) +
      scale_size_manual(values = c(2,5)) +
      scale_color_manual(values = c("black", "red")) +
      labs(x = "Number of clusters",
           y = "Total within-cluster sum of squares",
           title = "Elbow plot for the hierarchical method") +
      theme_bw()
  }
}

