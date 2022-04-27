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

# Key assumptions
set.seed(42)

alpha <- 0.6
beta_val <- 0.6
lambda <- 15
sd <- 0.036

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
generate_tick_df <- function(sd,trader_names){
  info_stream_main <- rnorm(n=1,mean=0,sd=sd)
  info_stream_reddit <- rnorm(n=1,mean=0,sd=sd)
  vol <- 0
  price <- 100
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
runs <- 5000
start_points <- 10

price_matrix <- matrix(nrow=start_points,ncol=runs)
vol_matrix <- matrix(nrow=start_points,ncol=runs)
buys_matrix <- matrix(nrow=start_points,ncol=runs)
sells_matrix <- matrix(nrow=start_points,ncol=runs)

for (itr in seq(1:start_points)){
  df_tick <- generate_tick_df(sd,trader_names)
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
  price_matrix[itr,] <- df_tick$price
  vol_matrix[itr,] <- df_tick$vol
  buys_matrix[itr,] <- df_tick$buys
  sells_matrix[itr,] <- df_tick$sells
  cat("on itr: ",itr,"\n")
}




