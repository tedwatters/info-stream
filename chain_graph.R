library(dplyr)
library(greta)
library(causact)
library(reshape)
library(tidyr)
library(ggplot2)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
library(stringr)

# get list of the data files
file_names <- list.files(path = "./data",pattern="tick", full.names = TRUE)

# loop through the files generating plots
for (itr in seq(1:length(file_names))){
  # read the file
  df_tick <- read.csv(file_names[itr])

  # extract the scenario name
  scenario_name <- str_trim(sub("*.csv","",sub(".*RR","RR",file_names[itr])))

  png(file=paste0("./plots/price",scenario_name,".png"),
      width=600, height=350)
  plot(
    df_tick$ticks,
    df_tick$current.price,
    main=paste0("Current Price: ",scenario_name)
  )
  dev.off()

  png(file=paste0("./plots/vol",scenario_name,".png"),
      width=600, height=350)
  plot(df_tick$ticks,df_tick$vol,main=paste0("Volume: ",scenario_name))
  dev.off()

  if (itr == 1){
    png(file=paste0("./plots/info",scenario_name,".png"),
        width=600, height=350)
    plot(
      df_tick$ticks,
      df_tick$current.reddit,col="red",
      main=paste0("Info Stream: ",scenario_name)
    )
    points(df_tick$ticks,df_tick$current.info)
    legend("topright",col=c("black","red"),legend=c("main","reddit"),lty=c(0,0),pch=c(1,1))
    dev.off()
  }
  

}

# define the function that we know how traders make decisions
decision_function <- function(rho,betar,er,em,theta){
  
  # trigger if reddit trader will take action
  reddit_trigger <- function(rho,betar,er,em,theta){
    if ( isTRUE( rho>0.5) ){
      if ((betar*er+(1-betar)*em) < -1 * theta ){
        ans <- TRUE
      } else if ((betar*er+(1-betar)*em) > theta){
        ans <- TRUE
      } else {
        ans <- FALSE
      } 
    } else {
      ans <- FALSE
    }
    return(ans)
    
  }
  
  # trigger if main user will take action
  main_trigger <- function(rho,em,theta){
    if ( isTRUE(rho<0.5)){
      if (em < -1 * theta){
        ans <- TRUE
      } else if (em > 1 * theta){
        ans <- TRUE
      } else {
        ans <- FALSE
      } 
    } else {
      ans <- FALSE
    }
    return(ans)
    
  }
  
  # set trading action based on triggers and thresholds
  if (reddit_trigger(rho,betar,er,em,theta) == TRUE){
    if ((betar*er+(1-betar)*em) > 0){
      ans2 <- 1
    } else {
      ans2 <- -1
    }
  } else if (main_trigger(rho,em,theta) == TRUE){
    if (em > 0){
      ans2 <- 1
    } else {
      ans2 <- -1
    }
  } else {
    ans2 <- 0
  }
  return( greta_array(ans2) )
}

# loop through files to generate inferences
for (itr in seq(1:length(file_names))){
  # read the file
  df_tick <- read.csv(file_names[itr])
  
  # extract the scenario name
  scenario_name <- str_trim(sub("*.csv","",sub(".*RR","RR",file_names[itr])))
  
  # get trader decisions in order
  trader_decisions <- melt(
    df_tick, 
    id=c("ticks","current.price","g","vol","current.reddit","current.info")
  )
  trader_decisions <- trader_decisions[complete.cases(trader_decisions),]
  trader_decisions <- trader_decisions[!grepl("thresh",trader_decisions$variable),]
  trader_decisions$variable <- gsub("\\D+","",trader_decisions$variable)
  
  trader_tick_phi <- df_tick[, !names(df_tick) %in% c("ticks","current.price","g","vol","current.reddit","current.info","X")] 
  trader_tick_phi <- trader_tick_phi[,!grepl("thresh",colnames(trader_tick_phi))]
  colnames(trader_tick_phi) <- gsub("\\D+","",colnames(trader_tick_phi))
  trader_tick_phi <- t(trader_tick_phi)
  
  # make the graph
  graph = dag_create() %>%
    dag_node("Decision","phi_o",
             rhs = uniform(-1,1),
             # rhs = uniform(phi[tp,ip]-0.1,phi[tp,ip]+0.1),
             data = trader_tick_phi,
             keepAsDF = TRUE
    ) %>%
    dag_node("Trader Decision","phi",
             rhs = decision_function(rho[ip],betar,epsilon_reddit[tp],epsilon_main[tp],theta[ip]),
             # rhs = uniform(-1,1),
             # data = trader_decisions$value,
             dec = TRUE,
             child = "phi_o") %>%
    dag_node(descr = "Trader Classification",label = "rho",
             rhs = alpha+z,
             child = "phi") %>%
    dag_node(descr = "Proportion of Reddit Traders",label = "alpha",
             rhs = uniform(0,0.5),
             child = "rho") %>%
    dag_node(descr = "Trader Threshold", label = "theta",
             rhs = beta(2,2),
             child = "phi") %>%
    dag_node(descr =  "Reddit Importance Factor",label = "betar",
             rhs = uniform(0,1),
             child = "phi") %>%
    dag_node("Info Main","epsilon_main",
             data = trader_decisions$current.info,
             child = "phi") %>%
    dag_node("Info Reddit","epsilon_reddit",
             data = trader_decisions$current.reddit,
             child = "phi") %>%
    dag_node("Tick","t",
             data = trader_decisions$ticks,
             child="phi") %>%
    dag_node("Trader","i",
             data = as.numeric(trader_decisions$variable),
             child="phi") %>%
    dag_node("Rho_cut","z",
             rhs = uniform(0,0.5),
             child="rho") %>%
    dag_plate("Trader","ip",
              nodeLabels = c("rho","theta","i","phi","phi_o","z"),
              data = trader_decisions$variable) %>%
    dag_plate("Tick Plate","tp",
              nodeLabels = c("epsilon_reddit","epsilon_main","t","phi","phi_o"),
              data = trader_decisions$tick)
  if (itr == 1){
    graph %>% dag_render() %>%
      export_svg %>% charToRaw %>% 
      rsvg_png(paste0("./plots/graph_",scenario_name,".png"))
    # png(file=paste0("./plots/graph_",scenario_name,".png"))
    # dag_render(graph = graph)
    # dev.off()
  }

  
  gretaCode = graph %>% dag_greta(mcmc=FALSE)
  drawsDF = graph %>% dag_greta()
  
  # png(file=paste0("./plots/param_est_",scenario_name,".png"))
  drawsDF %>% dagp_plot()
  +ggtitle(paste("Parameter Estimate: ",scenario_name))
  ggsave(paste0("./plots/param_est_",scenario_name,".png"))
  # dagp_plot(drawsDF = drawsDF)
  # dev.off()
  
  write.csv(drawsDF,paste0('./plots/drawsDF_',scenario_name,'.csv'))
}





#test
# trader_decisions$value <- abs(trader_decisions$value)
# trader_decisions$variable <- gsub("\\D+","",trader_decisions$variable)
# trader_decisions <- transform(trader_decisions, variable = as.numeric(variable))
# trader_decisions <- transform(trader_decisions, ticks = as.character(ticks))
# decision_test <- function(x,z){
#   ans <- ceiling(x-z)
# }



# rho_function <- function(alpha){
#   distribution(y) <- alpha
# }
# 
# alpha <- beta(2,2)
# marginalise(rho_function,bernoulli(alpha))
# 
# weights <- uniform(0,1,dim=2)
# rates <- beta(0.05,0.05)
# distribution(rho) <- mixture(
#   bernoulli(rates),
#   binomial(1,0),
#   weights = weights
# )
# 
# start_rho <- as_data(rbern(100,0.2))

# beta_marg_pdf <- function(a,b,rho_s){
#   ans <- beta(a+sum(rho_s),b+length(rho_s)-sum(rho_s))/beta(a,b)
#   return(ans)
# }
# 
# blerp <- function(a,b){
#   return(paste0("normal(",a,",",b,")"))
# }

# graph = dag_create() %>%
#   # dag_node("Price","p",
#   #          data = trader_decisions$current.price) %>%
#   dag_node("Trader Decision","phi",
#            # rhs = decision_function(rho,betar,epsilon_reddit,epsilon_main,theta),
#            rhs = normal(theta,0),
#            data = trader_decisions$value) %>%
#   # dag_node(descr =  "Reddit Importance Factor",label = "betar",
#   #          rhs = beta(2,2),
#   #          child = "phi") %>%
#   dag_node(descr = "Trader Classification",label = "rho",
#            rhs = beta(alpha,2),
#            child = "phi") %>%
#   dag_node(descr = "Proportion of Reddit Traders",label = "alpha",
#            rhs = beta(2,2),
#            child = "rho") %>%
#   dag_node(descr = "Trader Threshold", label = "theta",
#            rhs = beta(2,2),
#            child = "phi") %>%
#   dag_node("Info Main","epsilon_main",
#            data = trader_decisions$current.info,
#            child = "phi") %>%
#   dag_node("Info Reddit","epsilon_reddit",
#            data = trader_decisions$current.reddit,
#            child = "phi") %>%
#   dag_node("Tick","t",
#            data = trader_decisions$ticks,
#            child="phi") %>%
#   dag_node("Trader","i",
#            data = trader_decisions$variable,
#            child="phi") %>%
#   dag_plate("Trader","ip",
#             nodeLabels = c("rho","theta","phi","i"),
#             data = trader_decisions$variable) %>%
#   dag_plate("Tick Plate","tp",
#             nodeLabels = c("epsilon_reddit","epsilon_main","phi","t"),
#             data = trader_decisions$tick)
# 
# graph %>% dag_render()
# 
# gretaCode = graph %>% dag_greta(mcmc=FALSE)
# # Not run:
# # default functionality returns a data frame
# # below requires Tensorflow installation
# png("./plots/ex_graph.png")
# drawsDF = graph %>% dag_greta()
# dev.off()
# 
# 
#   
# 
# # multiple plate example
# library(dplyr)
# poolTimeGymDF = gymDF %>%
#   mutate(stretchType = ifelse(yogaStretch == 1,
#                               "Yoga Stretch",
#                               "Traditional")) %>%
#   group_by(gymID,stretchType,yogaStretch) %>%
#   summarize(nTrialCustomers = sum(nTrialCustomers),
#             nSigned = sum(nSigned))
# graph = dag_create() %>%
#   dag_node("Cust Signed","k",
#            rhs = binomial(n,p),
#            data = poolTimeGymDF$nSigned) %>%
#   dag_node("Probability of Signing","p",
#            rhs = beta(2,2),
#            child = "k") %>%
#   dag_node("Trial Size","n",
#            data = poolTimeGymDF$nTrialCustomers,
#            child = "k") %>%
#   dag_plate("Yoga Stretch","x",
#             nodeLabels = c("p"),
#             data = poolTimeGymDF$stretchType,
#             addDataNode = TRUE) %>%
#   dag_plate("Observation","i",
#             nodeLabels = c("x","k","n")) %>%
#   dag_plate("Gym","j",
#             nodeLabels = "p",
#             data = poolTimeGymDF$gymID,
#             addDataNode = TRUE)
# graph %>% dag_render()
# 
# graph = dag_create() %>%
#   dag_node("Get Card","y",
#            rhs = bernoulli(theta),
#            data = carModelDF$getCard) %>%
#   dag_node(descr = "Card Probability by Car",label = "theta",
#            rhs = beta(2,2),
#            child = "y") %>%
#   dag_node("Car Model","x",
#            data = carModelDF$carModel,
#            child = "y") %>%
#   dag_plate("Car Model","x",
#             data = carModelDF$carModel,
#             nodeLabels = "theta")
# graph %>% dag_render()
# 
# schools_dat <- data.frame(y = c(28, 8, -3, 7, -1, 1, 18, 12),
#                           sigma = c(15, 10, 16, 11, 9, 11, 10, 18), schoolName = paste00("School",1:8))
# graph = dag_create() %>%
#   dag_node("Treatment Effect","y",
#            rhs = normal(theta, sigma),
#            data = schools_dat$y) %>%
#   dag_node("Std Error of Effect Estimates","sigma",
#            data = schools_dat$sigma,
#            child = "y") %>%
#   dag_node("Exp. Treatment Effect","theta",
#            child = "y",
#            rhs = avgEffect + schoolEffect) %>%
#   dag_node("Pop Treatment Effect","avgEffect",
#            child = "theta",
#            rhs = normal(0,30)) %>%
#   dag_node("School Level Effects","schoolEffect",
#            rhs = normal(0,30),
#            child = "theta") %>%
#   dag_plate("Observation","i",nodeLabels = c("sigma","y","theta")) %>%
#   dag_plate("School Name","school",
#             nodeLabels = "schoolEffect",
#             data = schools_dat$schoolName,
#             addDataNode = TRUE)
# graph %>% dag_render()
# ## Not run:
# # below requires Tensorflow installation
# graph %>% dag_greta(mcmc=TRUE)
# DrawsDF %>% dagp_plot()
# 
# 
# epsilon_main <- as_data(trader_decisions$current.info)       #DATA
# epsilon_reddit <- as_data(trader_decisions$current.reddit)   #DATA
# t <- as.factor(trader_decisions$ticks)                       #DIM
# i <- as.factor(trader_decisions$variable)                    #DIM
# phi <- as_data(trader_decisions$value)                       #DATA
# i_dim <- length(unique(i))                                   #DIM
# t_dim <- length(unique(t))                                   #DIM
# alpha  <- beta(shape1 = 2, shape2 = 2)                     #PRIOR
# theta  <- beta(shape1 = 2, shape2 = 2, dim = i_dim)       #PRIOR
# rho    <- beta(shape1 = alpha, shape2 = 2, dim = i_dim)   #PRIOR
# distribution(phi) <- normal(mean = theta[i], sd = 0, dim = c(i_dim,t_dim))   #LIKELIHOOD
# gretaModel  <- model(rho,alpha,theta)   #MODEL
# meaningfulLabels(graph)
# draws       <- mcmc(gretaModel)              #POSTERIOR
# drawsDF     <- replaceLabels(draws) %>% as.matrix() %>%
#   dplyr::as_tibble()           #POSTERIOR
# tidyDrawsDF <- drawsDF %>% addPriorGroups()  #POSTERIOR
# 
# 
# 
# x <- runif(100)
# y <- vector(length=length(x)*1000)
# y <- matrix(data=y,nrow=length(x))
# for (itr in seq(1:length(x))){
#   y[itr,] <- rbinom(1000,1,x[itr])
# }
# hist(y)
