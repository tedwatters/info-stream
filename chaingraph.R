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
  
  if(ans2==-1){
    return(greta_array(c(1,0,0)))
  }
  if(ans2==0){
    return(greta_array(c(0,1,0)))
  }
  if(ans2==1){
    return(greta_array(c(0,0,1)))
  }
  
  
  # return( greta_array(ans2) )
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
             # rhs = uniform(-1,1),
             rhs = normal(10,10),
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
              nodeLabels = c("rho","theta","i","phi","z","phi_o"),
              data = trader_decisions$variable) %>%
    dag_plate("Tick Plate","tp",
              nodeLabels = c("epsilon_reddit","epsilon_main","t","phi","phi_o"),
              data = trader_decisions$tick)
  if (itr == 1){
    graph %>% dag_render() %>%
      export_svg %>% charToRaw %>% 
      rsvg_png(paste0("./plots/graph",scenario_name,".png"))
    # png(file=paste0("./plots/graph_",scenario_name,".png"))
    # dag_render(graph = graph)
    # dev.off()
  }

  
  gretaCode = graph %>% dag_greta(mcmc=FALSE)
  # drawsDF = graph %>% dag_greta()
  gretaCode <- str_replace(
    gretaCode, 
    "mcmc\\(gretaModel\\)", 
    "mcmc\\(gretaModel, n_samples = 100000\\)"
    )
  gretaCode <- str_replace(
    gretaCode, 
    "normal\\(mean = 10, sd = 10, dim = c\\(ip_dim,tp_dim\\)\\)", 
    "mixture\\(normal\\(-1,0.1\\), normal\\(0,0.1\\), normal\\(1,0.1\\),weights=phi\\)"
    )
  eval(parse(text=gretaCode))
  
  # png(file=paste0("./plots/param_est_",scenario_name,".png"))
  plot_p <- drawsDF %>% dagp_plot() 
  plot_p$labels$title <- paste("Parameter Estimate: ",scenario_name)
  ggsave(paste0("./plots/paramEst",scenario_name,".png"))
  # dagp_plot(drawsDF = drawsDF)
  # dev.off()
  
  write.csv(drawsDF,paste0('./plots/drawsDF',scenario_name,'.csv'))
}

# Reads from csvs
# get list of the data files
file_names <- list.files(path = "./plots",pattern="drawsDF", full.names = TRUE)

# loop through files to generate inferences
for (itr in seq(1:length(file_names))){
  # read the file
  df_par <- read.csv(file_names[itr])
  
  # extract the scenario name
  scenario_name <- str_trim(sub("*.csv","",sub(".*RR","RR",file_names[itr])))
  
  df_par_subset_alpha<- data.frame(df_par$alpha * 2)
  colnames(df_par_subset_alpha) <- paste0("alpha_",scenario_name)
  df_par_subset_betar<- data.frame(df_par$betar)
  colnames(df_par_subset_betar) <- paste0("betar_",scenario_name)

  if (itr == 1){
    df_par_summary_alpha <- df_par_subset_alpha
    df_par_summary_betar <- df_par_subset_betar
    
  } else {
    df_par_summary_alpha <- cbind(df_par_summary_alpha,df_par_subset_alpha)
    df_par_summary_betar <- cbind(df_par_summary_betar,df_par_subset_betar)
  }
}

plot_alpha <- drawsDF %>% dagp_plot() 
plot_alpha$labels$title <- "Summary of Alpha"
ggsave(paste0("./plots/alpha_summary.png"))

plot_betar <- drawsDF %>% dagp_plot() 
plot_betar$labels$title <- "Summary of Beta"
ggsave(paste0("./plots/beta_summary.png"))