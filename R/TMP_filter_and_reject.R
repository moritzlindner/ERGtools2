# # Create a histogram using ggplot2
# cor_histogram <- ggplot(cor_df, aes(x = Correlation)) +
#   geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
#   facet_wrap(~Variable1, scales = "free") +
#   labs(
#     x = "Correlation",
#     y = "Frequency",
#     title = "Correlation Histograms"
#   )
# cor_histogram
#
#
# autoreject.distance<-function(x,threshold=1){
#   dist_matrix <- dist(t(scale(dat)), method = "euclidean")
#   dist_df <- as.matrix(dist_matrix)
#   reject <- scale(apply(dist_df, 1, mean)) > 1
#   return(reject)
# }
#
#
#
# tmp<-dat[,!reject]
# plot(apply(tmp,1,var))
#
# tmp$x<-1:nrow(tmp)
# tmp <- tidyr::gather(tmp, key = "key", value = "value",-x)
# ggplot(data = tmp,aes(y=value,x=x, colour=key))+
#   geom_line()
#
# Measurements(X)
# Y<-Subset(X, Channel = "ERG_Flicker",Step=9,Eye="RE")
# dat<-Y@Data[[1]]@Data
# t<-TimeTrace(Y@Data[[1]])
# freq<-1/set_units(unique(diff(t)),s)
# dat<-as.data.frame(dat)
# single<-dat[,1]
# plot(single)
#
#
#
#
# data <- data.frame(keep = apply(dat[autoreject.function],1,mean),drop = apply(dat[,!autoreject.function],1,mean))
# data$count <- 1:nrow(data)
#
# require(ggplot2)
# ggplot(data, aes(x=count)) +
#   geom_line(aes(y=keep),colour="green") +
#   geom_line(aes(y=drop),colour="red")
