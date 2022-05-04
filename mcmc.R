#################

# General description

################

# What's random at project initiation
## Each trader threshold
## Each trader type


# What's random at each time step
## value of main info-stream
## value of reddit info-stream

# What depends on the RV's at each time step
## Each trader decision
### Volume
### Price

#################

# Initiation

################
library(stringr)


# Key assumptions
set.seed(42)

alpha <- 0.2
beta_val <- 0.2
lambda <- 75
sd <- 0.036
price <- 100

trader_names <- vector(length=100)
for (itr in seq(1:100)){
  trader_names[itr] = paste0("trader_",itr)
}

# This function generates trader parameters
generate_trader_df <- function(alpha,trader_names){
  rho_i <- rbinom(100,1,alpha)
  theta_i <- runif(100)
  df_trader = data.frame(rho_i,theta_i,trader_names)
  return(df_trader)
}

# This function generates initial tick df (i.e. the time series)
generate_tick_df <- function(
  sd,
  trader_names,
  info_stream_main = rnorm(n=1,mean=0,sd=sd),
  info_stream_reddit = rnorm(n=1,mean=0,sd=sd),
  price = 100
  ){
  vol <- 0
  buys <- 0
  sells <- 0
  df_tick <- data.frame(info_stream_reddit,info_stream_main,vol,price)
  df_tick[trader_names] <- 0
  return(df_tick)
}

# This function is what defines a trader decision
# Each trader is an "i"
# Each time step is a "t"
phi_i_t <- function(
  rho_i, # class
  theta_i, #threshold
  beta_val, #reddit importance
  info_stream_main_t,
  info_stream_reddit_t
  ){
    
    # trigger if reddit trader will take action
    reddit_trigger <- function(
      rho_i,
      beta_val,
      info_stream_reddit_t,
      info_stream_main_t,
      theta_i
      ){
      if ( rho_i == 1 ){
        if ((beta_val*info_stream_reddit_t
             +(1-beta_val)*info_stream_main_t) < -1 * theta_i ){
          ans <- TRUE
        } else if ((beta_val*info_stream_reddit_t+(1-beta_val)
                    *info_stream_main_t) > theta_i){
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
    main_trigger <- function(rho_i,info_stream_main_t,theta_i){
      if ( rho_i == 0){
        if (info_stream_main_t < -1 * theta_i){
          ans <- TRUE
        } else if (info_stream_main_t > 1 * theta_i){
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
    if (reddit_trigger(
      rho_i,
      beta_val,
      info_stream_reddit_t,
      info_stream_main_t,
      theta_i
      ) == TRUE){
      if ((beta_val*info_stream_reddit_t+(1-beta_val)*info_stream_main_t) > 0){
        ans2 <- 1
      } else {
        ans2 <- -1
      }
    } else if (main_trigger(rho_i,info_stream_main_t,theta_i) == TRUE){
      if (info_stream_main_t > 0){
        ans2 <- 1
      } else {
        ans2 <- -1
      }
    } else {
      ans2 <- 0
    }
    return( ans2 )
}

# This function updates the price
update_price <- function(df_tick_t,trader_names,lambda){
  z <- sum(df_tick_t[trader_names]) # This is the sum of all buy/ sell decisions
  g <- (z/length(trader_names))/lambda # constant that describes price movement
  ans <- max(exp(g)*df_tick_t$price,1) # calculates next price using current
  return(ans)
}

# This function defines conditional pdf of info_stream_reddit
update_info_stream_reddit <- function(val_main,val_reddit,sd){
  r_update <- rnorm(n=1,mean=0,sd=sd)
  update1 <- min(val_reddit-0.3*val_main+r_update,sd*2) #sets upper bound
  update2 <- max(update1,-0.3*sd) # sets lower bound
  return(update2)
}

# This function defines conditional pdf of info_stream_main
update_info_stream_main <- function(val_main,sd){
  # sets boundary
  r_update <- rnorm(n=1,mean=0,sd=sd)
  if (abs(val_main)>(sd*1.2)){
    ans <- -0.01
  } else {
    ans <- val_main + r_update
  }
  return(ans)
}

# Gibbs Sampling Step
gibbs_sample_step <- function(
  df_tick,
  df_trader,
  trader_names,
  beta_val,
  lambda,
  sd,
  update_price,
  update_info_stream_main,
  update_info_stream_reddit
  ){
  # See Computational Statistics Lecture 6B for procedure

  tick <- dim(df_tick)[1]

  # Step 2
    # Start with the traders
    for (trader in trader_names){
      df_tick[tick+1,trader] <- phi_i_t(
        rho_i = df_trader[which(trader == df_trader$trader_names),"rho_i"],
        theta_i = df_trader[which(trader == df_trader$trader_names),"theta_i"],
        beta_val = beta_val,
        info_stream_main_t = df_tick[tick,"info_stream_main"],
        info_stream_reddit_t = df_tick[tick,"info_stream_reddit"]
      )
    }
    # Then update volume
    df_tick[tick+1,"vol"] <- sum(abs(df_tick[tick+1,trader_names]))
    # Then update buy/ sells
    df_tick[tick+1,"buys"] <- sum(df_tick[tick+1,trader_names] > 0)
    df_tick[tick+1,"sells"] <- abs(sum(df_tick[tick+1,trader_names] < 0))
    # Then update price
    df_tick[tick+1,"price"] <- update_price(df_tick[tick,],trader_names,lambda)
    # Then update info streams
    df_tick[tick+1,"info_stream_main"] <- update_info_stream_main(
      val_main = df_tick[tick,"info_stream_main"],
      sd = sd
      )
    df_tick[tick+1,"info_stream_reddit"] <- update_info_stream_reddit(
      val_main = df_tick[tick,"info_stream_main"],
      val_reddit = df_tick[tick,"info_stream_reddit"],
      sd = sd
    )
  
    return(df_tick)  
  
}

# Full Gibbs Sampling Procedure
gibbs_sample_procedure <- function(gibbs_sample_step,  
                                   df_tick,
                                   df_trader,
                                   trader_names,
                                   beta_val,
                                   lambda,
                                   sd,
                                   update_price,
                                   update_info_stream_main,
                                   update_info_stream_reddit,
                                   runs){
  # Step 1 from Computational Statistics Lecture 6B already accomplished
  # Step 2
  for (itr in seq(2:runs)){
    df_tick <- gibbs_sample_step(
        df_tick,
        df_trader,
        trader_names,
        beta_val,
        lambda,
        sd,
        update_price,
        update_info_stream_main,
        update_info_stream_reddit
    )
  }
  
  return(df_tick)
  
}

#################

# Calculate initial runs

################

# Each tick df has a randomly generated info_stream starting point
# Each trader df has a randomly generated threshold and class for each trader
# Generate 10 of them to see how sensitive things are to starting point
runs <- 1000
main = seq(from = -0.5, to = 0.5, by = 0.5)*sd
reddit = seq(from = 0.5, to = -0.5, by = -0.5)*sd
start_points <- length(main)
alpha_list <- c(0,0.05,0.1,0.2,0.3)
beta_list <- c(0,0.1,0.2,0.5,0.7)
best_start_points <- vector(length=length(alpha_list)*length(beta_list))

count_main <- 1
for (alpha_par in alpha_list){
  for (beta_par in beta_list){
    alpha <- alpha_par
    beta_val <- beta_par
    
    price_matrix <- matrix(nrow=start_points^2,ncol=runs)
    vol_matrix <- matrix(nrow=start_points^2,ncol=runs)
    buys_matrix <- matrix(nrow=start_points^2,ncol=runs)
    sells_matrix <- matrix(nrow=start_points^2,ncol=runs)
    
    count <- 1
    for (itr in seq(1:start_points)){
      for (itr2 in seq(1:start_points)){
        df_tick <- generate_tick_df(sd,
                                    trader_names,
                                    info_stream_main = main[itr],
                                    info_stream_reddit = reddit[itr2],
                                    price = price
        )
        df_trader <- generate_trader_df(alpha,trader_names)
        
        df_tick <- gibbs_sample_procedure(gibbs_sample_step,  
                                          df_tick,
                                          df_trader,
                                          trader_names,
                                          beta_val,
                                          lambda,
                                          sd,
                                          update_price,
                                          update_info_stream_main,
                                          update_info_stream_reddit,
                                          runs)
        price_matrix[count,] <- df_tick$price
        vol_matrix[count,] <- df_tick$vol
        buys_matrix[count,] <- df_tick$buys
        sells_matrix[count,] <- df_tick$sells
        cat("on itr: ",count,"\n")
        count <- count + 1
      }
      }
    
    
    # Check if mean is within 20% of starting price. If so, call stable.
    stable_measure = vector(length=start_points^2)
    mean_dif = vector(length=start_points^2)
    count <- 1
    for (itr in seq(1:start_points^2)){

      mean_price <- mean(price_matrix[itr,])
      mean_dif[itr] <- abs(price - mean_price)/price
      if (mean_dif[itr] < 0.2){
        stable_measure[count] <- "black"
      }
      else{
        stable_measure[count] <- "red"
      }
      count <- count + 1
    }
    
    best_start_points[count_main] <- which.min(mean_dif)
    
    # Make legend
    legend_entries = vector(length=start_points^2)
    count <- 1
    for (itr in seq(1:start_points)){
      for (itr2 in seq(1:start_points)){
        
      legend_entries[count] <- paste0("Main: ",main[itr],",Reddit: ",reddit[itr2])
      count <- count + 1
      }
    }
    
    png(file=paste0("./plots/trace",alpha,beta_val,".png"),
        width=600, height=350)
    # Trace plots
    plot(
      seq(1:runs),
      price_matrix[1,],
      type="l",
      ylim=c(0,200),
      ylab = "price",
      xlab = "iteration",
      main = paste0("Trace Plot for alpha=",alpha,", beta=",beta_val),
      col = stable_measure[1]
    )
    for (itr in seq(2:start_points^2)){
      lines(seq(1:runs),price_matrix[itr,],col=stable_measure[itr])
    }
    legend("topleft",legend=legend_entries,col=stable_measure,lty = 1)
    dev.off()
    
    
    # Cumulative Sum
    price_cumulative_sum = matrix(nrow=start_points^2,ncol=runs)
    for (itr in seq(1:start_points^2)){
      for (itr2 in seq(1:runs)){
        if (itr2 == 1){
          price_cumulative_sum[itr,itr2] <- price_matrix[itr,itr2] - price
        } else{
          price_cumulative_sum[itr,itr2] <- price_matrix[itr,itr2] + price_cumulative_sum[itr,itr2-1] - price
        }
      }
    }
    png(file=paste0("./plots/sum",alpha,beta_val,".png"),
        width=600, height=350)
    plot(
      seq(1:runs),
      price_cumulative_sum[1,],
      type="l",
      ylim=c(-10000,10000),
      ylab = "price",
      xlab = "iteration",
      main = paste0("Cumulative Sum Plot for alpha=",alpha,", beta=",beta_val),
      col = stable_measure[1]
    )
    for (itr in seq(2:start_points^2)){
      lines(seq(1:runs),price_cumulative_sum[itr,],col=stable_measure[itr])
    }
    legend("topleft",legend=legend_entries,col=stable_measure,lty = 1)
    dev.off()
    
    
    # Autocorrelation
    count <- 1
    for (itr in seq(1:start_points)){
      for (itr2 in seq(1:start_points)){
        
      png(file=paste0("./plots/acf",alpha,beta_val,count,".png"),
          width=600, height=350)
      acf(
        price_matrix[count,],
        lag.max=100,
        main = paste0(
          "Trace Plot for alpha=",
          alpha,
          ", beta=",
          beta_val,
          ", Main: ",
          main[itr],
          ",Reddit: ",
          reddit[itr2]
        )
      )
      dev.off()
      count < count + 1
    }
    
    }
    count_main <- count_main+1
  }
}

write.csv(best_start_points,'./plots/best_starting_points.csv')


# Now that we have some graphs and the best starting points...
# Start doing the long runs to see if the distribtuion matches the ABM output.

best_start_points <- read.csv('./plots/best_starting_points.csv')


burn <- 500
runs <- 10000

count_main <-1
for (alpha_par in alpha_list){
  for (beta_par in beta_list){
    
    alpha <- alpha_par
    beta_val <- beta_par
    cat("alpha: ",alpha,"beta: ",beta_val,"\n")
    
    df_tick <- generate_tick_df(sd,
                                trader_names,
                                info_stream_main = main[max(floor(best_start_points[count_main,"x"]/length(main)),1)],
                                info_stream_reddit = reddit[best_start_points[count_main,"x"] %% length(main)+1],
                                price = price
    )
    df_trader <- generate_trader_df(alpha,trader_names)
    
    df_tick <- gibbs_sample_procedure(gibbs_sample_step,  
                                      df_tick,
                                      df_trader,
                                      trader_names,
                                      beta_val,
                                      lambda,
                                      sd,
                                      update_price,
                                      update_info_stream_main,
                                      update_info_stream_reddit,
                                      burn+runs)
    
    df_tick <- df_tick[burn:(burn+runs),]
    png(file=paste0("./plots/histogramMCMC",alpha,beta_val,".png"),
        width=600, height=350)
    hist(
      df_tick$price,
      freq = FALSE,
      main = paste0("Density of MCMC, Alpha: ",alpha,", Beta: ",beta_val),
      xlab = "Price",
      xlim = c(0,200)
    )
    dev.off()
    
    count_main <- count_main+1
  }
}

# Now generate histograms for the ABM:
# get list of the data files
file_names <- list.files(path = "./data",pattern="tick", full.names = TRUE)

# loop through the files generating plots
for (itr in seq(1:length(file_names))){
  # read the file
  df_tick <- read.csv(file_names[itr])
  
  # extract the scenario name
  scenario_name <- str_trim(sub("*.csv","",sub(".*RR","RR",file_names[itr])))
  
  png(file=paste0("./plots/histogramABM",scenario_name,".png"),
      width=600, height=350)
  hist(
    df_tick$current.price,
    freq = FALSE,
    main = paste0("Density of ABM: ", scenario_name),
    xlab = "Price",
    xlim = c(0,200)
  )
  dev.off()
}

