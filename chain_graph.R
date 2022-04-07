df_tick <- read.csv('./data/netlogo-results-tick.csv')

png(file="./plots/ex_price.png",
    width=600, height=350)
plot(df_tick$ticks,df_tick$current.price,main="Current Price")
dev.off()

png(file="./plots/ex_vol.png",
    width=600, height=350)
plot(df_tick$ticks,df_tick$vol,main="Volume")
dev.off()

png(file="./plots/ex_info.png",
    width=600, height=350)
plot(df_tick$ticks,df_tick$current.reddit,col="red",main="Info Stream")
points(df_tick$ticks,df_tick$current.info)
legend("topright",col=c("black","red"),legend=c("main","reddit"),lty=c(0,0),pch=c(1,1))
dev.off()

library(dplyr)
library(greta)
library(causact)
graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability by Car",label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_node("Car Model","x",
           data = carModelDF$carModel,
           child = "y") %>%
  dag_plate("Car Model","x",
            data = carModelDF$carModel,
            nodeLabels = "theta")
graph %>% dag_render()
gretaCode = graph %>% dag_greta(mcmc=FALSE)
## Not run:
## default functionality returns a data frame
# below requires Tensorflow installation
# png("./plots/ex_graph.png")
drawsDF = graph %>% dag_greta()
# dev.off()
# png("./plots/ex_est.png")
drawsDF %>% dagp_plot()
# dev.off()