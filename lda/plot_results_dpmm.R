setwd("/Users/yulia/Documents/=Courses=/CSC2506/iir/lda/")
source("helpers.R")

#Document 9
#corpus=10788, vocabSize=26261, wordsInDoc=328, T=100, a=0.500000, b=0.500000
#-383 p=3192.655654 clusters=5 big_clusters=5
#Document 9
#Topics: [97, 5, 20, 99, 82]
#Counts: [86, 106, 54, 54, 28]

colors = c("blue", "red", "darkgreen", "gold", "black", "magenta", "orange")
plot_perplexity_over_iterations <- function(pattern = "dpmm_alpha0.5.*.log", ylim=c(3000, 4500)) {
  plot(0:1,0:1,type="n",xlim=c(0,1000), ylim = ylim, bty='L', xlab="Iterations", ylab="Perplexity")
  
  # Plot perplexity
  i = 1
  legend <- c()
  for (f in list.files(path="convergence/", pattern = pattern, full.names =T)) {
    list[alpha, beta, perplex, clusters, big_clusters, topics, counts] <- processFile(f)
    lines(smooth(perplex),  type="l", col=colors[i], lwd=2)
    legend <- c(legend, get(metric))
    i = i+1
  }
  order_legend <- order(as.numeric(legend))
  legend("topright", title = metric, legend=legend[order_legend], fill=colors[order_legend], horiz = T,  bty="n")
}

plot_n_clusters_over_iterations <- function(pattern = "dpmm_alpha0.5.*.log", ymax=60) {
  # Plot number of clusters
  plot(0:1,0:1,type="n",xlim=c(0,1000), ylim=c(0,ymax), bty='L', xlab="Iterations", ylab="Clusters")
  i = 1
  legend <- c()
  for (f in list.files(path="convergence/", pattern = pattern, full.names =T)) {
    list[alpha, beta, perplex, clusters, big_clusters, topics, counts] <- processFile(f)
    lines(smooth(clusters), col=colors[i], type="l", lwd=2)
    legend <- c(legend, get(metric))
    i = i+1
  }
  order_legend <- order(as.numeric(legend))
  legend("topright", title = metric, legend=legend[order_legend], fill=colors[order_legend], horiz = T,  bty="n")
}
  
plot_n_big_clusters_over_iterations <- function(pattern = "dpmm_alpha0.5.*.log", ymax=10) {
  # Plot number of clusters
  plot(0:1,0:1,type="n",xlim=c(0,1000), ylim=c(0,ymax), bty='L', xlab="Iterations", ylab="Clusters")
  i = 1
  legend <- c()
  for (f in list.files(path="convergence/", pattern = pattern, full.names =T)) {
    list[alpha, beta, perplex, clusters, big_clusters, topics, counts] <- processFile(f)
    lines(smooth(big_clusters), col=colors[i], type="l", lwd=2)
    legend <- c(legend, get(metric))
    i = i+1
  }
  order_legend <- order(as.numeric(legend))
  legend("topright", title = metric, legend=legend[order_legend], fill=colors[order_legend], horiz = T, bty="n")
}

compare_word_distr <- function() {
  pdf("../../project/dpmm_word_counts.pdf", width = 11, height = 5)
  par(cex=1.5)
  list[alpha, beta, perplex, clusters, big_clusters, topics_dpmm, counts_dpmm] <- processFile("convergence//dpmm_alpha0.5_beta0.05.log")
  counts_100_topics <- rep(0, 100)
  counts_100_topics[topics_dpmm+1] <- counts_dpmm
  barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  dev.off()
  
  pdf("../../project/lda_word_counts_alpha05_beta5.pdf", width = 11, height = 5)
  par(cex=1.5)
  list[alpha, beta, perplex, clusters, big_clusters, topics_dpmm, counts_dpmm] <- processFile("convergence//lda_alpha0.5_beta5.log")
  counts_100_topics <- rep(0, 100)
  counts_100_topics[topics_dpmm+1] <- counts_dpmm
  barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  dev.off()
  
  pdf("../../project/lda_word_counts_alpha5_beta05.pdf", width = 11, height = 5)
  par(cex=1.5)
  list[alpha, beta, perplex, clusters, big_clusters, topics_dpmm, counts_dpmm] <- processFile("convergence//lda_alpha5_beta0.5.log")
  counts_100_topics <- rep(0, 100)
  counts_100_topics[topics_dpmm+1] <- counts_dpmm
  barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  dev.off()
  
  pdf("../../project/lda_word_counts_alpha1_beta05.pdf", width = 11, height = 5)
  par(cex=1.5)
  list[alpha, beta, perplex, clusters, big_clusters, topics_dpmm, counts_dpmm] <- processFile("convergence//lda_alpha1_beta0.5.log")
  counts_100_topics <- rep(0, 100)
  counts_100_topics[topics_dpmm+1] <- counts_dpmm
  barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  dev.off()
  
  pdf("../../project/lda_word_counts_alpha05_beta05.pdf", width = 5, height = 1*4)
  par(mfrow=c(4,1), mar=c(1, 4, 0.5, 1))
  for (f in list.files(pattern="lda_alpha0.5_beta.*.log", path="convergence/", full.names =T)[1:4]) {
    list[alpha, beta, perplex, clusters, big_clusters, topics_lda, counts_lda] <- processFile(f)
    counts_100_topics <- rep(0, 100)
    counts_100_topics[topics_lda+1] <- counts_lda
    barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  }
  dev.off()
  
  pdf("../../project/dpmm_word_counts_stability.pdf", width = 5, height = 1*4)
  par(mfrow=c(4,1), mar=c(1, 4, 0.5, 1))
  for (f in list.files(pattern="dpmm.*_beta0.05.log", path="convergence/", full.names =T)) {
    list[alpha, beta, perplex, clusters, big_clusters, topics_dpmm, counts_dpmm] <- processFile(f)
    counts_100_topics <- rep(0, 100)
    counts_100_topics[topics_dpmm+1] <- counts_dpmm
    barplot(counts_100_topics, main="", xlab="Topics", ylab = "Word counts")
  }
  dev.off()
}

save_plots <- function() {
  metric = "alpha"
  pdf("../../project/dpmm_variable_alpha_perplex.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_perplexity_over_iterations("dpmm_alpha.*_beta0.1.log", ylim = c(3000, 4000))
  dev.off()
  
  metric = "beta"
  pdf("../../project/dpmm_variable_beta_perplex.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_perplexity_over_iterations(pattern = "dpmm_alpha0.5.*.log", ylim = c(3000, 6000))
  dev.off()
  
  pdf("../../project/dpmm_variable_beta_n_clusters.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_n_clusters_over_iterations(pattern = "dpmm_alpha0.5.*.log", ymax=70)
  dev.off()
  
  pdf("../../project/dpmm_variable_beta_n_big_clusters.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_n_big_clusters_over_iterations(pattern = "dpmm_alpha0.5.*.log", ymax= 15)
  dev.off()
  
  
  metric = "alpha"
  pdf("../../project/lda_variable_alpha_perplex.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_perplexity_over_iterations("lda_alpha.*_beta0.5.log", ylim = c(3000, 15000))
  dev.off()
  
  metric = "beta"
  pdf("../../project/lda_variable_beta_perplex.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_perplexity_over_iterations(pattern = "lda_alpha0.5.*.log", ylim = c(3000, 5000))
  dev.off()
  
  pdf("../../project/lda_variable_beta_n_big_clusters.pdf", width = 11, height = 5)
  par(cex=1.5)
  plot_n_big_clusters_over_iterations(pattern = "lda_alpha0.5.*.log", ymax=15)
  dev.off()
}