###############################################
######## Moving average_vectorized ############
# df should have date in column 1, indicator in other columns
# Returns date in column 1, moving average in other columns

ma_v <-
  function(df, days, lag) {
    # days= number of days (periods) in average, lag= days to lag
    
    m_a <- matrix(nrow = length(df[, 2]), ncol = length(df) - 1) #storage
    
    for (i in (days + lag):length(df[, 2]))
      m_a[i, ] = apply(df[(i - days - lag + 1):(i - lag), -1], 2, mean) #calculate moving average
    
    temp <-
      as.data.frame(cbind(df[-(1:(days + lag - 1)), 1], m_a[-(1:(days + lag -
                                                                   1)), ]))
    
    # if(lag!=0) ma_name<-paste(names(df)[2],"_ma",as.character(days),"_lag",as.character(lag),sep="")
    # else ma_name<-paste(names(df)[2],"_ma",as.character(days),sep="")
    
    names(temp) <- names(df)
    
    return(temp)
  } # end ma_v fuction

############################################
######## Momentum_vectorized ###############
# df should have date in column 1, indicator in other columns
# Returns date in column 1, moving average in other columns

momentum_v <-
  function(df, days, lag) {
    # days= number of days (periods) prior used for comparison, lag= days to lag
    
    mom <- matrix(nrow = length(df[, 2]), ncol = length(df) - 1) #storage
    
    for (i in (days + lag):length(df[, 2]))
      mom[i, ] <-
        as.matrix(((df[(i - lag), -1] / df[(i - days - lag + 1), -1]) - 1) * 100) #calculate % change
    
    temp <-
      as.data.frame(cbind(df[-(1:(days + lag - 1)), 1], mom[-(1:(days + lag -
                                                                   1)), ]))
    
    # if(lag!=0) mom_name<-paste(names(df)[2],"_mom",as.character(days),"_lag",as.character(lag),sep="")
    # else mom_name<-paste(names(df)[2],"_mom",as.character(days),sep="")
    
    names(temp) <- names(df)
    
    return(temp)
  } # end momentum_v function

###############################################
####### Moving average ratio_vectorized #######
mar_v <-
  function(df, mindays, maxdays) {
    # mindays= smaller number of days (periods) in ratio, maxdays= larger number of days
    
    ma_ratio <- matrix(nrow = length(df[, 2]), ncol = length(df) - 1) #storage
    
    for (i in maxdays:length(df[, 2]))
      ma_ratio[i, ] = apply(df[(i - mindays + 1):i, -1], 2, mean) / apply(df[(i -
                                                                                maxdays + 1):i, -1], 2, mean)
    
    temp <-
      as.data.frame(cbind(df[-(1:(maxdays - 1)), 1], ma_ratio[-(1:(maxdays - 1)), ])) #create data.frame with date and ma ratio
    
    names(temp) <- names(df)
    return(temp)
  } # end mar_v

#########################################
####### Date adjuster ###################
dateadjuster.function2 <- function(filename, delay) {
  filename[, 1] <- filename[, 1] + delay
  filename$day <- filename[, 1] %% 7
  
  filename[, 1][filename$day <= 1] = filename[, 1][filename$day <= 1] + 2 - filename$day[filename$day <= 1]
  
  return(filename[, 1])
  
} # end date adjuster

######################################################
###### Rolling windows on multiple indicators ########
# Prep function
rowSds <- function (x)
{
  y <- rowVars(x)
  y <- sqrt(y)
  return(y)
}

rowVars <- function (x)
{
  n <- !is.na(x)
  n <- rowSums(n)
  n[n <= 1] <- NA
  center <- rowMeans(x)
  x <- x - center
  x <- x * x
  x <- rowSums(x)
  x <- x / (n - 1)
  return(x)
}

# Main function
rolling_window_v <- function(dataset, mindays, maxdays, countrylist) {
  temp1 <- subset(dataset, select = c("DATE", countrylist))
  temp1 <- na.omit(temp1)
  temp <- subset(temp1, select = countrylist)
  dimen <- dim(temp)
  print(dimen)
  
  average_z <- matrix(0, nrow = dimen[1], ncol = dimen[2])
  stddev_z <- matrix(0, nrow = dimen[1], ncol = dimen[2])
  
  average_zz <- matrix(0, nrow = dimen[1], ncol = dimen[2])
  stddev_zz <- matrix(0, nrow = dimen[1], ncol = dimen[2])
  
  # z calculation
  
  average_z <- rowMeans(temp)
  #storage for means
  stddev_z <- rowSds(temp)
  #storage for standard deviations
  
  dataset_z <- (temp - average_z) / stddev_z
  
  dataset_z <- cbind(temp1$DATE, dataset_z)
  names(dataset_z)[1] <- "DATE"
  
  # z of z calculation
  
  for (i in 1:dimen[1]) {
    if (i < mindays) {
      # if1
      stddev_zz[i, ] <- NA
      average_zz[i, ] <- NA
    } # end if1
    else           {
      # else1
      if (i < maxdays) {
        # if2
        stddev_zz[i, ] <- sapply(temp[1:i, ], sd)
        average_zz[i, ] <- colMeans(temp[1:i, ])
      } # end if2
      else           {
        # else2
        stddev_zz[i, ] <- sapply(temp[(i - maxdays + 1):i, ], sd)
        average_zz[i, ] <- colMeans(temp[(i - maxdays + 1):i, ])
      } # end else2
    } # end else1
  } #end i loop
  
  dataset_zz <- (temp - average_zz) / stddev_zz
  
  average_zz <- rowMeans(dataset_zz) #storage for means
  stddev_zz <- rowSds(dataset_zz) #storage for standard deviations
  
  dataset_zz <-
    (dataset_zz - average_zz) / stddev_zz #X-section of z-scores
  dataset_zz <- cbind(temp1$DATE, dataset_zz)
  names(dataset_zz)[1] <- "DATE"
  
  return(list(z = dataset_z, zz = dataset_zz))
  
  
} # end main function

##########################################################
######## FORWARD RETURN GENERATION FUNCTION ##############
generate_returns <- function(index, periods)
  
  c(index[-(1:periods)] / index[-((length(index) - periods + 1):length(index))] -
      1, rep(NA, periods))


calculate_returns <- function(indexdf) {
  return_1w <- generate_returns(indexdf[, 2], 5)
  
  return_4w <- generate_returns(indexdf[, 2], 20)
  
  return_9w <- generate_returns(indexdf[, 2], 45)
  
  return_13w <- generate_returns(indexdf[, 2], 65)
  
  return_26w <- generate_returns(indexdf[, 2], 130)
  
  return_52w <- generate_returns(indexdf[, 2], 260)
  
  
  returns <-
    data.frame(return_1w,
               return_4w,
               return_9w,
               return_13w,
               return_26w,
               return_52w)
  
  
  names(returns) <-
    c(
      paste(names(indexdf)[2], "_1w", sep = ""),
      paste(names(indexdf)[2], "_4w", sep = ""),
      paste(names(indexdf)[2], "_9w", sep = ""),
      paste(names(indexdf)[2], "_13w", sep = ""),
      paste(names(indexdf)[2], "_26w", sep = ""),
      paste(names(indexdf)[2], "_52w", sep = "")
    )
  
  return(returns)
} # end cal returns

generate_returns_all <- function(multi) {
  df <- multi[, 1] #DATE column
  for (i in 2:length(multi))
    df <- cbind(df, calculate_returns(multi[, c(1, i)]))
  names(df)[1] <- "DATE"
  
  return(df)
} # end gen all

##########################################
######## FILTER RIGHT DATES ##############
filter_right_dates <-
  function(indicator_list, day_of_month = 22, con_list) {
    min_date <- 0
    max_date <- 999999 #initialize variables
    
    # indicator_list should be of class "list". Each member of the list has 1+length(con) columns
    
    number_of_countries <- length(con_list)
    number_of_indicators <- length(indicator_list)
    
    combined <-
      rep(NA, 1000 * (1 + number_of_countries)) #DATE in column 1, indicators in other columns
    dim(combined) <-
      c(1000, (1 + number_of_countries)) #start combined with 1000 rows
    
    combined <- as.data.frame(combined)
    names(combined)[1] <- "DATE"
    
    for (i in 1:number_of_indicators) {
      if (min(indicator_list[[i]][, 1]) > min_date)
        min_date <- min(indicator_list[[i]][, 1])
      if (max(indicator_list[[i]][, 1]) < max_date)
        max_date <- max(indicator_list[[i]][, 1])
    }
    # find days that have right day of month
    # start at row 1 and build up one row at a time
    
    rows <- 1
    for (i in min_date:max_date) {
      if (as.numeric(substring(as.Date(i, origin = '1899-12-30'), 9, 10)) == day_of_month)
      {
        if (i %% 7 <= 1)
          combined[rows, 1] <- i - (i %% 7) - 1
        else
          combined[rows, 1] <- i
        rows <- rows + 1
      } # end if
    } # end for
    
    # delete unused rows
    
    last_row <- rows - 1
    print(last_row)
    combined <- combined[1:last_row, ]
    
    # Stack indicators. Rows should be equal to no of observations times number of countries
    
    Stacked_combined <-
      rep(NA, last_row * number_of_countries * (number_of_indicators + 2))
    dim(Stacked_combined) <-
      c(last_row * number_of_countries, (number_of_indicators + 2))
    Stacked_combined <- as.data.frame(Stacked_combined)
    
    # Column 1 in final dataset
    
    stack_date <- rep(combined[, 1], number_of_countries)
    dim(stack_date) <- c(last_row * number_of_countries, 1)
    
    ### for exporting unstacked data outside this functions
    # data_for_new_factor <- list()
    
    # use dates found above and find latest value of indicators that was available at that time
    
    for (j in 1:number_of_indicators) {
      for (i in 1:last_row) {
        # finds last row for which indicator$DATE <= DATE in combined[i,]
        
        temp <- sum(indicator_list[[j]][, 1] <= combined[i, 1])
        combined[i, 2:(number_of_countries + 1)] <-
          indicator_list[[j]][temp, 2:(number_of_countries + 1)]
      } # end i
      
      temp <- combined[, 2:(number_of_countries + 1)]
      
      ### exporting data from function to global table
      
      data_for_new_factor[[j]] <<-
        combined[, 1:(number_of_countries + 1)]
      names(data_for_new_factor[[j]]) <<- c("DATE", con_list)
      
      #  stacking dataset, adding data to stacked dataset
      names(temp) <- c(con_list)
      temp <- stack(temp)
      Stacked_combined[, j + 2] <- temp[, 1]
    } #end j
    
    Stacked_combined[, 1] <- stack_date # add date to first column
    Stacked_combined[, 2] <-
      temp[, 2] # add country name to second column
    
    Stacked_combined <- na.omit(Stacked_combined) #strip out NAs
    return(Stacked_combined) ### return stacked dataset
    
  }#end filter_right_dates

######################################################
################## stack_corr ########################
# This Function is for pulling out right data for return between start and end date and calculate relative returns wrt some index
# Indicator List should be provided as list
# first data in the list should be returns data
# name_list should be the name for return and factors
# This routine calls the filter_right_dates routine
# day of month = 22 (default)
# main subroutine for function ic
# calculates correlation between stacked factor and stack return relative to world
stack_corr <-
  function(indicator_list,
           name_list,
           con_list,
           day_of_month = 22,
           weeks,
           world = "World",
           start_date,
           end_date) {
    # choose needed columns of R according to conuntries and weeks
    
    R_sub <-
      subset(indicator_list[[1]], select = c("DATE", paste(con_list, weeks, sep =
                                                             "")))
    R_sub <-
      R_sub[as.Date(R_sub$DATE, origin = "1899-12-30") >= start_date &
              as.Date(R_sub$DATE, origin = "1899-12-30") <= end_date, ]
    
    # choose world column of R according to world index and weeks
    
    World_sub <-
      subset(indicator_list[[1]], select = c("DATE", paste(world, weeks, sep =
                                                             "")))
    World_sub <-
      World_sub[as.Date(World_sub$DATE, origin = "1899-12-30") >= start_date &
                  as.Date(World_sub$DATE, origin = "1899-12-30") <= end_date, ]
    
    # Calculate relative returns
    
    R_sub[, -1] <- R_sub[, -1] - World_sub[, 2]
    
    # Replace first dataset in the indicator list with relative returns dataset
    
    indicator_list[[1]] <- R_sub
    
    # calling function filter right dates and get the stacked columns
    
    stack_combi <-
      filter_right_dates(indicator_list, day_of_month, con_list)
    names(stack_combi) <- c("DATE", "Sector", name_list)
    
    # Calculate correlations between world return and countries relative returns
    
    len = (length(indicator_list) - 1)
    corr <- matrix(0, nrow = len, ncol = 1)
    
    for (i in 1:len) {
      corr[i] = cor(stack_combi[, 3], stack_combi[, i + 3], method = "spearman")
    } # end for
    
    print(corr)
    return(stack_combi)
  }
# end stack_corr

######################################################
############### Rolling Grinold_kahn #################
# this function should be run once you have the stacked dataset
# In addition you need to have unstacked dataset as well
# ind_list = list of indicators combined
rolling_grinold_kahn <-
  function(stacked_dataset,
           country_list,
           unstacked_data,
           ind_list,
           mindays,
           maxdays) {
    len_data <-
      dim(stacked_dataset)[1] / length(country_list) # getting length of dataset
    len_con <- length(country_list)
    numberofindicators <- length(ind_list[-1])
    
    weights <-
      matrix(NA, nrow = len_data, ncol = numberofindicators) # for weights
    new_factor <-
      matrix(0, nrow = len_data, ncol = (len_con + 1)) # New factor no of countries and one for date
    
    for (j in 1:len_data) {
      tempdate <-
        as.numeric(stacked_dataset[j, 1]) # Getting date one by one
      
      if (j < mindays) {
        # Do nothing here
        
        new_factor[j, ] <- NA
        weights[j, ] <- NA
      } # end if
      
      else {
        if (j < maxdays) {
          # Expanding window
          
          temp_stacked_data <-
            stacked_dataset[which(stacked_dataset[, 1] <= tempdate), ] # get data with date less than the tempdate (DATE, country, R, factors)
          
        } # end if
        
        else{
          mintempdate <-
            as.numeric(stacked_dataset[j - maxdays + 1, 1]) # you need mintemp date for rollingwindow
          temp_stacked_data <-
            stacked_dataset[which((stacked_dataset[, 1] <= tempdate) &
                                    (stacked_dataset[, 1] >= mintempdate)), ] # Subsetting based on #mintempdate and tempdate
        } # end else
        
        rho <-
          cor(temp_stacked_data[, -(1:2)]) # Getting correlation matrix
        IC <-
          as.matrix(rho[, 1])[-1]  # ICs of individual variables (1st column is returns)
        rho1 <-
          rho[-1, -1] # Delete first row and column to get Correlation matrix of factors
        omega = sd(temp_stacked_data[, 3]) # standard deviation of returns
        
        weights[j, ] <-
          omega * t(IC) %*% solve(rho1) # Applying GK for getting weights
        weights[j, ] <-
          weights[j, ] / sum(weights[j, ]) # normalizing weights
        
        for (i in 1:numberofindicators) {
          new_factor[j, 2:(len_con + 1)] = new_factor[j, 2:(len_con + 1)] + as.matrix(weights[j, i] *
                                                                                        unstacked_data[[i + 1]][which(unstacked_data[[i + 1]][, 1] == tempdate), -1]) # getting new factors-- one factor at a time,
          # find right date, pull z scores from unstacked data and multiply by GK weight for that factor
          
        } # end i
        
      } # end else
      
      ################## if ( j>= mindays){} # end if ######################
      
      new_factor[j, 1] <- tempdate
      
    } # end j
    
    glob_weights <<- as.data.frame(weights)
    glob_new_factor <<- as.data.frame(new_factor)
    
  } # end rolling_grinold_kahn

############################################################
############## Rolling GK _ equal weights ##################
# this is a copy of rolling_grinold-kahn, but uses equal weights instead of GK weights
# this function should be run once you have the stacked dataset
# In addition you need to have unstacked dataset as well
# ind_list = list of indicators combined
equal_weights <-
  function(stacked_dataset,
           country_list,
           unstacked_data,
           ind_list,
           mindays,
           maxdays) {
    len_data <-
      dim(stacked_dataset)[1] / length(country_list) # getting length of dataset
    len_con <- length(country_list)
    numberofindicators <- length(ind_list)
    
    weights <-
      matrix(NA, nrow = len_data, ncol = numberofindicators) ### for weights
    new_factor <-
      matrix(0, nrow = len_data, ncol = (len_con + 1)) ###  New factor no of countries and one for date
    
    for (j in 1:len_data) {
      tempdate <-
        as.numeric(stacked_dataset[j, 1]) # Getting date one by one
      
      if (j < mindays) {
        # Do nothing here
        
        new_factor[j, ] <- NA
        weights[j, ] <- NA
      } # end if
      
      else {
        if (j < maxdays) {
          # Expanding window
          
          temp_stacked_data <-
            stacked_dataset[which(stacked_dataset[, 1] <= tempdate), ] # get data with date less than the tempdate
        } # end if
        
        else{
          mintempdate <-
            as.numeric(stacked_dataset[j - maxdays + 1, 1]) # you need mintemp date for rollingwindow
          temp_stacked_data <-
            stacked_dataset[which((stacked_dataset[, 1] <= tempdate) &
                                    (stacked_dataset[, 1] >= mintempdate)), ] # Subsetting based on mintempdate and tempdate
        } # end else
        
        weights[j, ] <- 1 / numberofindicators ### equal weights
        for (i in 1:numberofindicators) {
          new_factor[j, 2:(len_con + 1)] = new_factor[j, 2:(len_con + 1)] + as.matrix(weights[j, i] *
                                                                                        unstacked_data[[i + 1]][which(unstacked_data[[i + 1]][, 1] == tempdate), -1]) ### getting new factors
        } # end i
        
      } # end else
      
      ########### if ( j>= mindays){} # end if ##########
      
      new_factor[j, 1] <- tempdate
      
    } # end j
    
    glob_weights <<- as.data.frame(weights)
    glob_new_factor <<- as.data.frame(new_factor)
    
  } # equal_weights

######################################################
############### Backtesting ##########################
# This code is for backtesting strategy for individual variable
# Return data to be provided under R
# Country list to be provided under con_list
# forward return period to be provided under weeks
# To be tested indicator under factors
# Two investment strategy under method
# 1. "LS" = Long Short, count = number of assets Long or Short, threshold = NULL to initialize
# 2. "threshold" = invest iff above threshold or below -threshold, threshold = threshold value, count = NULL to initialize
# Start_date is analysis start date
# end_date is analysis end date

## BACK TESTING ##
backtesting_ind <-
  function(factors,
           R = Sectorindex_R,
           day_of_month = 22,
           con_list = sector_list,
           weeks = "_4w",
           method = "LS",
           count = 2,
           threshold = .5,
           start_date,
           end_date)
  {
    if (deparse(substitute(factors)) == "glob_new_factor") {
      # Store backtest parameters setup
      
      setup <-
        list(
          test_var = paste(name_list[-1]),
          Indexreturn = deparse(substitute(R)),
          day_of_month = day_of_month,
          investing_universe = con_list,
          fwd_period = weeks,
          start_date = start_date,
          end_date = end_date
        )
    } # end if
    
    else{
      # Store backtest parameters setup
      
      setup <-
        list(
          test_var = factorname,
          Indexreturn = deparse(substitute(R)),
          day_of_month = day_of_month,
          investing_universe = con_list,
          fwd_period = weeks,
          start_date = start_date,
          end_date = end_date
        )
      
    } # end else
    
    # initialize variables
    
    min_date <- 0
    max_date <- 999999
    
    number_of_countries <- length(con_list)
    
    combined <-
      rep(NA, 1000 * (1 + number_of_countries)) # DATE in column 1, indicators in other columns
    dim(combined) <-
      c(1000, (1 + number_of_countries)) # start combined with 1000 rows
    combined <- as.data.frame(combined)
    names(combined)[1] <- "DATE"
    
    
    factors <- na.omit(factors) # get rid of NAs
    
    min_date <- min(factors[, 1])
    max_date <- max(factors[, 1])
    print("min_date")
    print(min_date)
    print("max_date")
    print(max_date)
    
    rows <- 1 #start at row 1 and build up one row at a time
    
    for (i in min_date:max_date) {
      if (as.numeric(substring(as.Date(i, origin = '1899-12-30'), 9, 10)) == day_of_month)
        
      {
        # right day of month, put date into DATE column of combined, if weekend, move back to Friday
        
        if (i %% 7 <= 1)
          combined[rows, 1] <- i - (i %% 7) - 1
        else
          combined[rows, 1] <- i
        rows <- rows + 1
      }
    } # end for loop
    
    last_row <-
      rows - 1 #because adding 1 to rows in above loop, ends up one too high
    
    combined <- combined[1:last_row, ] #delete unused rows
    
    # use dates found above and find latest value of indicators that was available at that time
    
    for (i in 1:last_row) {
      # finds last row for which indicator$DATE <= DATE in combined[i,]
      
      temp <- sum(factors[, 1] <= combined[i, 1])
      combined[i, 2:(number_of_countries + 1)] <-
        factors[temp, 2:(number_of_countries + 1)] # store value in combined
      
    }#end i
    
    # print(combined[(last_row-20):last_row,])
    
    temp <-
      combined[, 1:(number_of_countries + 1)] # Filtered dataset for the particular factor
    names(temp) <- c("DATE", con_list) # Providing names
    
    returndata <-
      subset(R, select = c("DATE", paste(con_list, weeks, sep = ""))) # subsetting data for weeks
    
    return_factor <-
      merge(temp, returndata, by = "DATE") # Merge dataset with the return data by dates
    
    # return_factor <- na.omit(return_factor) # omit NAs
    # return factor can have NAs
    
    # subsetting data for start and end date
    
    return_factor <-
      return_factor[as.Date(return_factor[, 1], origin = "1899-12-30") >= start_date &
                      as.Date(return_factor[, 1], origin = "1899-12-30") <= end_date, ]
    len <- length(con_list)
    dimen <- dim(return_factor)
    
    # ranking cross sectionally
    
    Rank_var <- matrix(0, nrow = dimen[1], ncol = len)
    Rank_var <- t(apply(return_factor[, 2:(len + 1)], 1, rank)) # Get ranks
    
    # for getting FWD Returns
    
    fwd_return <- matrix(0, nrow = dimen[1], ncol = 1)
    
    # Get dates in advance and export to global table
    
    investing_date <<- return_factor[, 1]
    
    
    ########### check ###########
    # check<<-return_factor
    #############################
    
    ## Strategy long-short ##
    
    if (method == "LS") {
      # if LS
      
      # store backtesting strategy info
      
      setup <- c(setup, strategy = paste(method, "_", count, sep = ""))
      
      country_index <-
        matrix(0, nrow = dimen[1], ncol = 2 * count) # for getting index corresponding to investing countries
      
      # calculate all longs
      
      for (j in 1:count) {
        for (i in (len + 2):(len + 2 + len - 1)) {
          fwd_return <-
            fwd_return + return_factor [, i] * as.integer(Rank_var[, i - len - 1] == (len +
                                                                                        1 - j)) / count # getting long fwd return
          
          country_index[, j] <-
            country_index[, j] + (i - len - 1) * as.integer(Rank_var[, i - len - 1] == (len +
                                                                                          1 - j)) # getting Long
          
        } # end i
      } # end j
      
      # calculate all shorts
      
      for (j in (count + 1):(count + count)) {
        for (i in (len + 2):(len + 2 + len - 1)) {
          fwd_return <-
            fwd_return - return_factor [, i] * as.integer(Rank_var[, i - len - 1] == (j -
                                                                                        count)) / count # getting short fwd return
          
          country_index[, j] <-
            country_index[, j] + (i - len - 1) * as.integer(Rank_var[, i - len - 1] == (j -
                                                                                          count)) # getting short
          
        } # end i
      } # end j
      
      # exporting investing  countries to global table
      
      investing_position <<- country_index
      
    } # end if LS
    
    
    ## Strategy threshold ##
    
    else{
      if (method == "threshold") {
        # if threshold
        
        # store backtesting strategy info
        
        setup <- c(setup, strategy = paste(method, "_", threshold, sep = ""))
        
        # store number of countries to long and short
        
        #countlong <- rep(NA,dimen[1])
        #countshort <- rep(NA,dimen[1])
        #dim(countlong) <- c(dimen[1],1)
        #dim(countshort) <- c(dimen[1],1)
        country_index <-
          matrix(NA, nrow = dimen[1], ncol = number_of_countries)
        colnames(country_index) <- con_list
        
        for (j in 1:dimen[1]) {
          # write down one row each time
          
          countlong <- sum(return_factor [j, 2:(len + 1)] >= threshold)
          countshort <- sum(return_factor [j, 2:(len + 1)] <= -threshold)
          
          if (countlong > 0 &
              countshort > 0) {
            # generate fwd return iff both long and short
            
            fwd_return[j] <-
              sum(return_factor [j, (len + 2):(len + 2 + len - 1)] * as.integer(return_factor [j, 2:(len +
                                                                                                       1)] >= threshold) / countlong)  - sum(return_factor [j, (len + 2):(len +
                                                                                                                                                                            2 + len - 1)] * as.integer(return_factor [j, 2:(len + 1)] <= -threshold) /
                                                                                                                                               countshort)
            
            country_index[j, ] <-
              as.integer(return_factor [j, 2:(len + 1)] >= threshold) / countlong - as.integer(return_factor [j, 2:(len +
                                                                                                                      1)] <= -threshold) / countshort
            
          } # end if
          
        } # end j
        
        # exporting investing  countries to global table
        
        investing_position <<- country_index
        
      } # end if threshold
    } # end else
    
    # Exporting backtest parameters setup
    
    backtest_setup <<- setup
    
    # Exporting Fwd returns to global tables
    
    forward_return <<- fwd_return
    
    # annualized for IR calculation
    
    fwd_return_annual <- (1 + fwd_return) ^ 12 - 1
    # fwd_return_annual <- fwd_return*12
    fwd_return_annual <- na.omit(fwd_return_annual)
    
    # Getting Information Ratio
    
    IR <-
      mean(as.vector(fwd_return_annual)) / sd(as.vector(fwd_return_annual))
    
    # Getting Hit Ratio
    
    HR = sum(as.integer(na.omit(fwd_return) > 0)) / dimen[1]
    
    value <- matrix(0, nrow = dimen[1] + 1, ncol = 1)
    value[1] <- 100
    
    for (i in 1:dimen[1]) {
      # Generate portfolio value
      
      value[i + 1] = value[i] * (1 + fwd_return[i])
      
    } # end for
    
    # Monthly ICs Calculation
    
    IC_monthly <- rep(0, dimen[1]) # store Monthly ICs
    
    for (i in 1:dimen[1]) {
      # Calculating Monthly ICs
      
      IC_monthly[i] <-
        cor(t(return_factor[i, 2:(len + 1)]), t(return_factor[i, (len + 2):(len + 2 + len - 1)]), method = "spearman")
      
    } # end for
    
    IC_month <<- IC_monthly # Exporting ICs
    
    print("Backtest setup")
    print(setup)
    print("Performance stats")
    print("Avgreturn")
    print(mean(as.vector(na.omit(fwd_return))))
    print("Standard Deviation")
    print(sd(as.vector(na.omit(fwd_return))))
    print("IR")
    print(IR)
    print("HR")
    print(HR)
    #print(fwd_return[1:20])
    #print(IC_monthly)
    
    return(value)
    
  } # end backtesting_ind function

##############################################################
############# Pairwise_backtest_ind ##########################
# This code is for backtesting strategy for individual variables
# Return data to be provided under R
# Country list to be provided under con_list
# forward return period to be provided under weeks
# To be tested indicator under factors
Pairwise_backtest_ind <-
  function(factors,
           R = MSCI_LOC,
           day_of_month = 22,
           con_list = all_countries,
           weeks = "_4w") {
    # Store backtest parameters setup
    
    setup <-
      list(
        test_var = factorname,
        Indexreturn = deparse(substitute(R)),
        day_of_month = day_of_month,
        investing_universe = con_list,
        fwd_period = weeks
      )
    
    
    min_date <- 0
    max_date <- 999999 # initialize variables
    
    number_of_countries <- length(con_list)
    
    combined <-
      rep(NA, 1000 * (1 + number_of_countries)) # DATE in column 1, indicators in other columns
    dim(combined) <-
      c(1000, (1 + number_of_countries)) # start combined with 1000 rows
    
    combined <- as.data.frame(combined)
    
    names(combined)[1] <- "DATE"
    
    min_date <- min(factors[, 1])
    max_date <- max(factors[, 1])
    
    rows <- 1 # start at row 1 and build up one row at a time
    
    
    for (i in min_date:max_date) {
      if (as.numeric(substring(as.Date(i, origin = '1899-12-30'), 9, 10)) == day_of_month)
        
      {
        # right day of month, put date into DATE column of combined, if weekend, move back to Friday
        
        if (i %% 7 <= 1)
          combined[rows, 1] <- i - (i %% 7) - 1
        else
          combined[rows, 1] <- i
        rows <- rows + 1
      }
    } # end for loop
    
    last_row <-
      rows - 1 #because adding 1 to rows in above loop, ends up one too high
    
    combined <- combined[1:last_row, ] #delete unused rows
    
    # use dates found above and find latest value of indicators that was available at that time
    
    for (i in 1:last_row) {
      # finds last row for which indicator$DATE <= DATE in combined[i,]
      
      temp <- sum(factors[, 1] <= combined[i, 1])
      combined[i, 2:(number_of_countries + 1)] <-
        factors[temp, 2:(number_of_countries + 1)] # store value in combined
      
    } # end i
    
    print(combined[(last_row - 20):last_row, ])
    
    temp <-
      combined[, 1:(number_of_countries + 1)] # Filtered dataset for the particular factor
    names(temp) <- c("DATE", con_list) # Providing names
    
    returndata <-
      subset(R, select = c("DATE", paste(con_list, weeks, sep = ""))) # subsetting data for weeks
    
    return_factor <-
      merge(temp, returndata, by = "DATE") # Merge dataset with the return data by dates
    
    return_factor <- na.omit(return_factor) # omit NAs
    # return factor can not have NAs
    
    # subsetting data for start and end date
    
    return_factor <-
      return_factor[as.Date(return_factor[, 1], origin = "1899-12-30") >= start_date &
                      as.Date(return_factor[, 1], origin = "1899-12-30") <= end_date, ]
    len <- length(con_list)
    dimen <- dim(return_factor)
    
    pair_wise_data <- matrix(NA, nrow = len, ncol = len)
    fwd_return <- matrix(0, nrow = dimen[1], ncol = 1)
    
    for (i in 2:(len + 1)) {
      for (j in (i + 1):(len + 1)) {
        if (j <= (len + 1)) {
          fwd_return  <-
            (
              return_factor[, (i + len)] * as.integer(return_factor[, i] > return_factor[, j]) +
                return_factor[, (j + len)] * as.integer(return_factor[, j] > return_factor[, i]) -
                return_factor[, (i + len)] * as.integer(return_factor[, i] < return_factor[, j]) -
                return_factor[, (j + len)] * as.integer(return_factor[, j] < return_factor[, i])
            )  # Getting Fwd _return for 2 countries at a time
          
          value <- matrix(0, nrow = dimen[1] + 1, ncol = 1)
          value[1] <- 100
          
          for (m in 1:dimen[1]) {
            # Backtesting
            
            value[m + 1] = value[m] * (1 + fwd_return[m])
          } # end m
          
          if (value[(dimen[1] + 1)] > 100) {
            pair_wise_data[i - 1, j - 1] = 1 # assigning value as 1 if backtesting gives a positive result
          } # end if
          
          else{
            pair_wise_data[i - 1, j - 1] = 0 # assigning value as 0 o/w
          } # end else
          # print(1)
        } # end if
      } # end j
    } # end i
    
    pairwise <<- pair_wise_data # exporting pairwise results globally
    print("Backtest setup")
    print(setup)
    print(pair_wise_data)
    
  } # end Pairwise_backtest_ind

##############################################################
############## regression_based_prediction_ind ###############
# This code is for Predicting values for countries based on global factor
# global Factor data to be provided under factors
# Return data to be provided uner R
# Country list to be provided under con_list
# forward return period to be provided under weeks

## Regression based Prediction ##
regression_based_prediction_ind <-
  function(factors,
           variable_name,
           R = MSCI_LOC_R,
           con_list,
           day_of_month = 22,
           weeks) {
    # initialize variables
    
    min_date <- 0
    max_date <- 999999
    
    
    combined <-
      rep(NA, 1000 * 2) # DATE in column 1, indicators in other columns
    dim(combined) <- c(1000, 2) # start combined with 1000 rows
    
    combined <- as.data.frame(combined)
    
    names(combined)[1] <- "DATE"
    
    min_date <- min(factors[, 1])
    max_date <- max(factors[, 1])
    
    rows <- 1 # start at row 1 and build up one row at a time
    
    for (i in min_date:max_date) {
      if (as.numeric(substring(as.Date(i, origin = '1899-12-30'), 9, 10)) == day_of_month)
      {
        # right day of month
        if (i %% 7 <= 1)
          combined[rows, 1] <-
            i - (i %% 7) - 1 #put date into DATE column of combined, if weekend, move back to Friday
        else
          combined[rows, 1] <- i
        rows <- rows + 1
      } # end if
    } # end for
    
    last_row <-
      rows - 1 # because adding 1 to rows in above loop, ends up one too high
    
    combined <- combined[1:last_row, ] # delete unused rows
    
    
    # use dates found above and find latest value of indicators that was available at that time
    
    for (i in 1:last_row) {
      temp <-
        sum(factors[, 1] <= combined[i, 1]) # finds last row for which indicator$DATE <= DATE in combined[i,]
      combined[i, 2] <- factors[temp, 2] # store value in combined
    } # end i
    
    temp <-
      combined[, 1:2] # Getting latest value for factor available  as of date
    print(temp[1:5, ])
    print(Variable_name)
    
    names(temp) <- c("DATE", Variable_name)
    
    print(temp[1:5, ])
    
    returndata <-
      subset(R, select = c("DATE", paste(con_list, weeks, sep = "")))
    
    return_factor <-
      merge(temp, returndata, by = "DATE") # Merge based on Date
    
    return_factor <- na.omit(return_factor)
    
    
    len <- length(con_list)
    dimen <- dim(return_factor)
    
    Predicted_value_req <-
      matrix(0, nrow = dimen[1], ncol = len) # for predicted return values
    betas <- matrix(0, nrow = dimen[1], ncol = len) # for Betas
    alphas <- matrix(0, nrow = dimen[1], ncol = len) # for alphas
    
    for (i in 1:dimen[1]) {
      # for each observation
      
      temp <- return_factor[1:i, ] # getting data on a rolling basis
      
      Predicted_value <- matrix(0, nrow = i, ncol = len)
      
      for (j in 1:len) {
        # For each country
        
        fit <-
          lm(temp[, j + 2] ~ temp[, 2], data = temp) # Running Regression
        Predicted_value[, j] <-
          coefficients(fit)[1] + coefficients(fit)[2] * temp[, 2] # Predict based on Linear Regression coefficients
        betas[i, j] <- coefficients(fit)[2] # storing Betas
        alphas[i, j] <- coefficients(fit)[1] # storing alphas
        
      } # end j
      
      Predicted_value_req[i, ] <-
        Predicted_value[i, ] # Getting last oservation from predicted and stoing it
      
    } # end i
    
    Predicted_value_req <-
      cbind(temp[, 1], Predicted_value_req) # Combine date and predicted values
    
    names(Predicted_value_req) <- c("DATE", con_list)
    names(betas) <- con_list
    betas <<- betas
    alphas <<- alphas
    
    return(Predicted_value_req) # returning Predicted values
    
  } # end regression_based_prediction_ind

#########################################################################
############ CART specific functions ###################################

## WINNER/LOSER FUNCTION ##
# Output #
# the function will return 1 if the candidate is considered winner and -1 if loser. For the case with neutral candidate, it will be marked 0, if so

# Parameter #
# 1. data_R: original crossectional return data

# 2. method:
# default value: method=1
# find winner/loser by ranking (only winner and loser):             method <- 1
# find winner/loser by ranking (winner, loser and neutral):        method <- 2
# find winner/loser by fixed threshold:                                       method <- 3
# find winner/loser by value of benchmark variable:                 method <- 4

# 3.threshold.w & threshold.l: for method 3, need to set threshold value to decide winner and loser
# default value: threshold.w=1, threshold.l=0
# the value would not matter if choosing other methods
# candidates with cross-sectional z-score of return greater than threshold.w will be regarded as winner
# candidates with cross-sectional z-score of return less than threshold.l will be regarded as loser
# candidates with cross-sectional z-score of return between the two threshold values will be regarded as neutral

# 4.benchmark.val: for method 4, need to set benchmark variable for comparison. Benchmark variable should be available in data_R
# eg. in the case of country rotation model, benchmark.val = "ACWI"

# 5. neutral.number: for method 2, need to set number of candidates expected to be indifferent towards, and number of winner and loser will be split evenly from the rest
# the number would not matter if any other method is chosen
# default value: neutral.number=2

# 6. candidate.list: candidates for recoginition of winner and loser
# eg. in the case of country rotation model, candidate.list = c("USD","GBP","EUR","JPY","AUD","CAD","CHF","EM")

# 7. fwd.week: fwd.week of return expected
# assuming the original dataset has varibale names such as: ACWI_1w.ACWI_26w...
# eg. in the case of country rotation model, fwd.week = "26w"

# 8. label.var: variable in dataset for labeling data points (ie. for the convenience of tracking data)
# default value: label.var="DATE"){

winner.loser <-
  function(data_R,
           method = 1,
           threshold.w = 1,
           threshold.l = 0,
           benchmark.var,
           neutral.number = 2,
           candidate.list,
           fwd.week,
           label.var = "DATE") {
    ll = length(candidate.list)
    fwd.return.all <-
      subset(data_R, select = c(label.var, paste(candidate.list, "_", fwd.week, sep =
                                                   "")))
    #fwd.return.all <- na.omit(fwd.return.all)
    fwd.return <-
      subset(fwd.return.all, select = c(paste(candidate.list, "_", fwd.week, sep =
                                                "")))
    performance.indicator <- rep(NA, ncol(fwd.return) * nrow(fwd.return))
    performance.indicator <-
      matrix(performance.indicator, nrow = nrow(fwd.return))
    
    if (method == 1) {
      # poisition where to split winner and loser based on number of candidate
      # if number of candidate is odd, number of winner will be less than that of loser
      thresh = as.integer(ll / 2)
      for (i in 1:nrow(fwd.return)) {
        if (any(as.character(fwd.return[i, ]) == "NA")) {
          performance.indicator[i, ] = rep(NA, ncol(fwd.return))
        } else {
          # rank the values at each time point to compare
          temp = sort(fwd.return[i, ], decreasing = TRUE)
          # winner will be marked 1, while loser will be marked -1
          performance.indicator[i, ] = 2 * as.integer(fwd.return[i, ] >=
                                                        as.numeric(temp[thresh])) - 1
        }
        performance.indicator = data.frame(performance.indicator)
        names(performance.indicator) = c(paste(candidate.list, "_", fwd.week, sep =
                                                 ""))
      }
    }
    
    if (method == 2) {
      # poisition where to split winner and loser based on number of candidate and expected neutral candidate
      # if number of non-neutral candidate is odd, number of winner will be less than that of loser
      thresh_w = as.integer((ll - neutral.number) / 2)
      thresh_l  = thresh_w + neutral.number + 1
      for (i in 1:nrow(fwd.return)) {
        if (any(as.character(fwd.return[i, ]) == "NA")) {
          performance.indicator[i, ] = rep(NA, ncol(fwd.return))
        } else {
          # rank the values at each time point to compare
          temp = sort(fwd.return[i, ], decreasing = TRUE)
          # winner will be marked 1, loser will be marked -1 and neutral will be marked 0
          performance.indicator[i, ] = as.integer(fwd.return[i, ] > as.numeric(temp[thresh_l]) &
                                                    fwd.return[i, ] < as.numeric(temp[thresh_w])) + 2 * as.integer(fwd.return[i, ] >=
                                                                                                                     as.numeric(temp[thresh_w])) - 1
        }
      }
      performance.indicator = data.frame(performance.indicator)
      names(performance.indicator) = c(paste(candidate.list, "_", fwd.week, sep =
                                               ""))
    }
    
    if (method == 3) {
      # 'return.z.score' is a function to calculate cross-section z-score at each time point
      return.z.score <- function(dataset) {
        mean.target <- rowMeans(dataset)
        var.target <- apply(dataset, 1, var)
        z.score <- (dataset - mean.target) / sqrt(var.target)
        return(z.score)
      }
      # calcualte z-score at each time point
      fwd.return_z <- return.z.score(fwd.return)
      # apply the two threshold value and compare the z-scores with them to recoginze winner, loser and neutral
      # candidate with value greater than or equal to the winner threshold is considered winner
      # candidate with value less than or equal to the loser threshold is considered loser
      # the rest will be regarded neutral
      performance.temp <-
        matrix((
          as.integer(fwd.return_z > threshold.l &
                       fwd.return_z < threshold.w) + 2 * as.integer(fwd.return_z >= threshold.w) -
            1
        ), nrow = nrow(fwd.return_z))
      performance.indicator <- data.frame(performance.temp)
      names(performance.indicator) <-
        c(paste(candidate.list, "_", fwd.week, sep = ""))
    }
    
    if (method == 4) {
      benchmark <-
        subset(data_R, select = c(paste(benchmark.var, "_", fwd.week, sep = "")))
      # compare each candidate with the benchmark value and decide if it is winner or loser
      # candidate with value greater than benchmark value is considered winner
      performance.indicator <-
        apply(fwd.return, 2, function(x)
          2 * (x > benchmark) - 1)
      performance.indicator = data.frame(performance.indicator)
    }
    
    
    winner.loser.indicator = data.frame(subset(data_R, select = label.var), performance.indicator)
    return(winner.loser.indicator)
  }



## FILTER RIGHT DATES CART##
filter_right_dates_cart <-
  function(indicator_list, day_of_month = 22, con_list) {
    min_date <- 0
    max_date <- 999999 #initialize variables
    
    number_of_indicators <-
      length(indicator_list) #indicator_list should be of class "list". Each member of the list has 2 columns, with DATE in 1st column
    number_of_countries <- length(con_list)
    
    combined <-
      rep(NA, 1000 * (1 + number_of_countries)) #DATE in column 1, indicators in other columns
    dim(combined) <-
      c(1000, (1 + number_of_countries)) #start combined with 1000 rows
    
    combined <- as.data.frame(combined)
    
    
    names(combined)[1] <- "DATE"
    
    for (i in 1:number_of_indicators) {
      if (min(indicator_list[[i]][, 1]) > min_date)
        min_date <-
          min(indicator_list[[i]][, 1]) #finding highest starting date
      if (max(indicator_list[[i]][, 1]) < max_date)
        max_date <- max(indicator_list[[i]][, 1]) #finding lowest ending date
    }
    #find days that have right day of month
    rows <- 1 #start at row 1 and build up one row at a time
    for (i in min_date:max_date) {
      if (as.numeric(substring(as.Date(i, origin = '1899-12-30'), 9, 10)) == day_of_month) {
        #right day of month
        if (i %% 7 <= 1)
          combined[rows, 1] <-
            i - (i %% 7) - 1 #put date into DATE column of combined, if weekend, move back to Friday
        else
          combined[rows, 1] <- i
        rows <- rows + 1
      }
    }
    last_row <-
      rows - 1 #because adding 1 to rows in above loop, ends up one too high
    
    combined <- combined[1:last_row, ] #delete unused rows
    
    Stacked_combined <-
      rep(NA, last_row * number_of_countries * (number_of_indicators + 2))
    dim(Stacked_combined) <-
      c(last_row * number_of_countries, (number_of_indicators + 2))
    Stacked_combined <- as.data.frame(Stacked_combined)
    
    
    stack_date <- rep(combined[, 1], number_of_countries)
    dim(stack_date) <- c(last_row * number_of_countries, 1)
    
    #use dates found above and find latest value of indicators that was available at that time
    
    data_for_new_factor <- list()
    
    
    for (j in 1:number_of_indicators) {
      for (i in 1:last_row) {
        temp <-
          sum(indicator_list[[j]][, 1] <= combined[i, 1]) #finds last row for which indicator$DATE <= DATE in combined[i,]
        combined[i, 2:(number_of_countries + 1)] <-
          indicator_list[[j]][temp, 2:(number_of_countries + 1)] #store value in combined
      }#end i
      temp <- combined[, 2:(number_of_countries + 1)]
      
      data_for_new_factor[[j]] <<-
        combined[, 1:(number_of_countries + 1)]
      
      names(temp) <- c(con_list)
      temp <- stack(temp)
      Stacked_combined[, j + 2] <- temp[, 1]
    } #end j
    
    Stacked_combined[, 1] <- stack_date #add date to first column
    Stacked_combined[, 2] <-
      temp[, 2]#add country name to second column
    
    Stacked_combined <- na.omit(Stacked_combined) #strip out NAs
    return(Stacked_combined)
  }#end filter_right_dates

## STACK DATA ##
stack_corr_cart <-
  function(indicator_list,
           name_list,
           con_list,
           weeks,
           day_of_month = 22,
           world = "World") {
    #main subroutine for function ic
    #calculates correlation between stacked factor and stack return relative to world
    
    R_sub <-
      subset(indicator_list[[1]], select = c("DATE", paste(con_list, weeks, sep =
                                                             "")))#choose needed columns of R
    
    print(R_sub[1:5, ])
    
    #World_sub<-subset(indicator_list[[1]],select=c("DATE",paste(world,weeks,sep="")))#choose World column of R
    
    #R_sub[,-1]<- R_sub[,-1]- World_sub[,2]
    
    print(R_sub[1:5, ])
    
    indicator_list[[1]] <- R_sub
    
    stack_combi <-
      filter_right_dates_cart(indicator_list, day_of_month, con_list)
    
    names(stack_combi) <- c("DATE", "Sectors", name_list)
    
    len = (length(indicator_list) - 1)
    corr <- matrix(0, nrow = len, ncol = 1)
    
    for (i in 1:len)
    {
      corr[i] = cor(stack_combi[, 3], stack_combi[, i + 3])
    }
    print(corr)
    return(stack_combi)
  }



## Step 7. ROLLING WINDOW BACK-TESTING ##

## ROLLING WINDOW BACK-TESTING FUNCTION ##
# NOTE: the function can be only applied to monthly data
# the function will return: 1) first three splits of rolling window trees; 2) investment decision based on the tree; 3) investment result

# Parameter #
# 1. window.width: width of rolling window for back testing in month
# default value: window.width = 18

# 2. tree.data: dataset used for generating tree

# 3. test.data: dataset used for back testing

# 4. candidate.list: candidates for recoginition of winner and loser
# eg. in the case of country rotation model, candidate.list = c("USD","GBP","EUR","JPY","AUD","CAD","CHF","EM")

# 5. predictor.list: independent variable in CART
# eg. in the case of country rotation model, predictor.list=c("PE","PB","EVSALES","EVCE","ROCE","FCFY","mom260_lag22")
# default value: predictor.list="all". This means all varibales expect exclude.var and response.var  will be considered as independent variables in CART

# 6. exclude.var: variables except response.var to be excluded from CART. This is parameter will be used especially when choose "all" for predictor.list
# eg. in the case of country rotation model, exclude.var = c("DATE","Country")

# 7. response.var: dependent variable in CART
# eg. in the case of country rotation model, response variable is the winner/loser indicator and response.var = "wEM_m0"

#8. label.var: variable used to recognize rolling window
# eg. in the case of country rotation model, label.var = "DATE"
# default value: label.var = "DATE"

#9. tree.fwd.month: forward month of return used for tree generation
# eg. If 26-week forward return is used for generating tree, then tree.fwd.month = 6
# default value: tree.fwd.month = 6

#10. tree.control: parameter control for rpart function
# default value: tree.control = c(minbucket=30,maxcompete=3,maxsurrogate=1,usesurrogate=2,xval=10,surrogatestyle=0,maxdepth=2)

#11. graph.all: if to plot all the tree during rolling window
# default value: graph.all = 0. This means the function will not plot the trees.

#12. expand: whether use expanding window or rolling window
# default value is rolling window with expand = 0

#13. num_tree: number of the trees to be plotted at the end
# default value is the last 5 trees in the whole periods

RW_Backtesting <-
  function(window.width,
           tree.data,
           test.data,
           candidate.list,
           predictor.list = "all",
           exclude.var,
           response.var,
           label.var = "DATE",
           tree.fwd.month = 1,
           tree.control = c(
             minbucket = 30,
             maxcompete = 3,
             maxsurrogate = 1,
             usesurrogate = 2,
             xval = 10,
             surrogatestyle = 0,
             maxdepth = 3
           ),
           expand = 0,
           num_tree = 5,
           graph.all = 0) {
    library(rpart)
    library(rpart.plot)
    # get date information from tree generating data and add them into dataset
    data.year = as.POSIXlt(as.Date(tree.data$DATE, origin = "1899-12-30"))$year + 1900
    data.month = as.POSIXlt(as.Date(tree.data$DATE, origin = "1899-12-30"))$mon + 1
    date.col <- which(names(tree.data) == label.var)
    all.dates <- unique(tree.data[, date.col])
    dayCount = rep(c(1:length(all.dates)), length(candidate.list))
    data.new <-
      data.frame(DayCount = dayCount,
                 YEAR = data.year,
                 MONTH = data.month,
                 tree.data)
    
    # get date information from test data and add them into dataset
    test.year = as.POSIXlt(as.Date(test.data$DATE, origin = "1899-12-30"))$year + 1900
    test.month = as.POSIXlt(as.Date(test.data$DATE, origin = "1899-12-30"))$mon + 1
    test.date.col <- which(names(test.data) == label.var)
    test.all.dates <- unique(test.data[, test.date.col])
    test.dayCount = rep(c(1:length(test.all.dates)), length(candidate.list))
    test.data.new <-
      data.frame(DayCount = test.dayCount,
                 YEAR = test.year,
                 MONTH = test.month,
                 test.data)
    names(test.data.new)[6] <- "Return"
    
    # create matrix to record first-split factor
    tree.last.end = max(test.dayCount) - tree.fwd.month
    tree.last.start = tree.last.end - window.width
    first.split = matrix(rep(NA, 4 * tree.last.start), nrow = tree.last.start)
    inv.return = rep(NA, (tree.last.start + 1))
    inv.return[1] = 100
    inv.decision = matrix(rep(NA, length(candidate.list) * tree.last.start), nrow = tree.last.start)
    long.count = rep(NA, tree.last.start)
    short.count = rep(NA, tree.last.start)
    
    # get names of independent variables
    var.list = names(tree.data)
    if (predictor.list[1] == "all") {
      predictor.list = var.list[-which(var.list == response.var)]
      if (length(exclude.var) > 0) {
        for (k in 1:length(exclude.var)) {
          if (length(which(predictor.list == exclude.var [k])) == 0) {
            var.print <- bquote(.(exclude.var [k]))
            stop(paste(var.print, "not found!"))
          }
          if (length(which(predictor.list == exclude.var [k])) != 0) {
            predictor.list = predictor.list[-which(predictor.list == exclude.var [k])]
          }
        }
      }
    }
    
    for (i in 1:tree.last.start) {
      endDay = dayCount[i] + window.width - 1
      row.start  = which(data.new$DayCount == dayCount[i])
      row.end  = which(data.new$DayCount == endDay)
      start.year = unique(data.new[row.start, ]$YEAR)
      end.year = unique(data.new[row.end, ]$YEAR)
      
      if (expand == 1) {
        dataSubset <-
          data.new[which(data.new$DayCount >= 1 &
                           data.new$DayCount <= endDay), c(response.var, predictor.list)]
      }
      else {
        dataSubset <-
          data.new[which(data.new$DayCount >= dayCount[i] &
                           data.new$DayCount <= endDay), c(response.var, predictor.list)]
      }
      ct <-
        rpart(
          as.formula(paste(names(dataSubset[response.var]), "~.")),
          data = dataSubset,
          method = "class",
          control = tree.control
        )
      
      # record first split
      paste(start.year, "-", end.year, sep = "") -> first.split[i, 1]
      as.vector(head(ct$frame$var))[1:3] -> first.split[i, 2:4]
      
      # graph tree
      if (graph.all != 0 | i > tree.last.start - num_tree) {
        plotTitle <-
          bquote(paste("Tree - From years: ", .(start.year), " to ", .(end.year)))
        windows()
        prp(
          ct,
          nn = TRUE,
          extra = 102,
          box.col = c("burlywood4", "lawngreen", "lightseagreen")[ct$frame$yval],
          branch.type = 5,
          faclen = 8
        )
        title(
          plotTitle,
          col.main = "blue",
          cex.main = 2,
          font.main = 3
        )
      }
      
      # predict
      row  = which(data.new$DayCount == endDay)
      predict.month = unique(data.new[row, ]$MONTH) + tree.fwd.month
      predict.year = unique(data.new[row, ]$YEAR)
      if (predict.month > 12) {
        predict.month = predict.month - 12
        predict.year = predict.year + 1
      }
      predict.sub <- subset(test.data.new, YEAR == predict.year)
      predict.sub <- subset(predict.sub, MONTH == predict.month)
      
      if (i == 1) {
        temp.col <- which(names(predict.sub) == label.var)
        plot.starting.date.1 = predict.sub[1, temp.col]
        temp.row <- as.integer(rownames(predict.sub[1, ]))
        temp.row.0 <- which(rownames(test.data) == temp.row)
        temp.col.0 <- which(names(test.data) == label.var)
        plot.starting.date.0 = test.data[temp.row.0, temp.col.0]
      }
      
      long.return = 0
      short.return = 0
      for (j in 1:length(candidate.list)) {
        if ((as.numeric(
          predict(ct, predict.sub[j, ], type = "class", na.action = na.pass)
        )) == "2")
          inv.decision[i, j] = 1
        if ((as.numeric(
          predict(ct, predict.sub[j, ], type = "class", na.action = na.pass)
        )) == "1")
          inv.decision[i, j] = -1
        if (inv.decision[i, j] == 1)
          long.return = long.return + (predict.sub[j, ]$Return)
        if (inv.decision[i, j] == -1)
          short.return = short.return + (predict.sub[j, ]$Return)
        
      }
      long.count[i] = length(which(inv.decision[i, ] == 1))
      short.count[i] = length(which(inv.decision[i, ] == -1))
      if (long.count[i] > 0 &
          long.count[i] < length(candidate.list))
        inv.return[i + 1] = inv.return[i] * ((long.return / long.count[i] - short.return /
                                                short.count[i]) / 1 + 1)
      if (long.count[i] == 0 |
          long.count[i] == length(candidate.list))
        inv.return[i + 1] = inv.return[i]
    }
    
    inv.decision <- data.frame(inv.decision)
    names(inv.decision) <- candidate.list
    
    # plot investment outcome
    date.return <-
      as.POSIXlt(as.Date(test.all.dates [c(temp.row.0:length(test.all.dates))], origin =
                           "1899-12-30"))
    # date.return <- c(as.POSIXlt(as.Date(plot.starting.date.0,origin="1899-12-30")),date.return)
    invTitle <-
      bquote(paste("Back-testing with ", .(window.width), "-month Rolling Window"))
    windows()
    plot(
      date.return,
      inv.return,
      type = "l",
      xlab = "Time",
      ylab = "Wealth"
    )
    title(invTitle, col.main = "blue",  cex.main = 1.5)
    
    output = list()
    
    # Store first plit information
    output[[1]] <- first.split
    
    # Store investment decisions in each time period
    output[[2]] <-
      cbind(as.Date(date.return[-length(date.return)]), inv.decision)
    names(output[[2]])[1] <- label.var
    
    # Store cumulative investment returns
    output[[3]] <- cbind(as.Date(date.return), inv.return)
    
    names(output) <-
      c("First Splits", "Investment Decision", "Investment Return")
    
    # Return output
    return(output)
  }
