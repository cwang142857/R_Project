## set directory to store output ##
setwd("/sector model/GK output")

## Backtest for combined factor ##
# initialize indicator list
# modify the factors needed to tested in the name list
name_list <-
  c(
    "Sectorindex_R",
    "BB12_zz",
    "E2EV_z",
    "BETA_6M_z",
    "EYG1_zz",
    "SY12M_z",
    "FCFY_z",
    "IT_zz",
    "NPM_z",
    "PRICEMOMENTUM_12M_z",
    "ROCE_z",
    "TRUEYIELD_z"
  )

#### stack_corr<-function(indicator_list,name_list,con_list,day_of_month = 22,weeks,world="World",start_date,end_date)

indicator_list <- mlist[name_list]

start_date <- "1960-01-01"
end_date <- "2012-12-31"
data_for_new_factor <-
  list()  # to get the unstacked dataset for Grinold Kahn

stacked_column <-
  stack_corr(indicator_list,
             name_list,
             sector_list,
             22,
             "_4w",
             "SPX",
             start_date,
             end_date)

cor(stacked_column[, -(1:2)])

#### rolling_grinold_kahn <- function(stacked_dataset,country_list,unstacked_data,ind_list,mindays,maxdays)
glob_weights <- 0
glob_new_factor <- 0

rolling_grinold_kahn(stacked_column,
                     sector_list,
                     data_for_new_factor,
                     indicator_list,
                     5,
                     2600)

names(glob_new_factor) <- c("DATE", sector_list)

glob_new_factor <-
  na.omit(glob_new_factor) # combined factor using rolling grinold khan weights

## Backtesting GK factor ##
# This code is for backtesting strategy for GK factor
# Return data to be provided under SectorReturns_R
# Country/Sector list to be provided under con_list/sector_list
# forward return period to be provided under weeks
# To be tested indicator under factors
# Two investment strategy under method
# 1. "LS" = Long Short, count = number of assets Long or Short, threshold = NULL to initialize
# 2. "threshold" = invest iff above threshold or below -threshold, threshold = threshold value, count = NULL to initialize
# Start_date is analysis start date
# end_date is analysis end date

#### backtesting_ind<-function(factors,R = Sectorindex_R, day_of_month=22,con_list = sector_list,weeks = "_4w",method = "LS",count = 2, threshold = .5,start_date, end_date)
backtest_setup <- 0 # For getting backtest parameters setup
forward_return <- 0 # For getting forward returns
investing_position <- 0 # For getting countriest o invest
investing_date <- 0 # Dates on which to invest
IC_month <- 0 # Information coefficient for each month

value <-
  backtesting_ind(
    glob_new_factor,
    Sectorindex_R,
    22,
    sector_list,
    "_4w",
    "threshold",
    NULL ,
    .5,
    start_date,
    end_date
  )

avgmr <- mean(as.vector(na.omit(forward_return)))
Std <- sd(as.vector(na.omit(forward_return)))
IR <-
  mean(as.vector(na.omit((1 + forward_return) ^ 12 - 1))) / sd(as.vector(na.omit((1 +
                                                                                    forward_return) ^ 12 - 1)))
HR = sum(as.integer(na.omit(forward_return) > 0)) / length(forward_return)
avgIC <- mean(as.vector(na.omit(IC_month)))

# calculate monthly return
Back_value <- rep(NA, length(forward_return))

Back_value[1] <- 100 * (1 + forward_return[1])

for (i in 1:length(forward_return)) {
  Back_value[i + 1] <- Back_value[i] * (1 + forward_return[i + 1])
  
}

Back_value <- Back_value[-(length(Back_value))]

# Plot Backtesting value return
plot(as.Date(investing_date, origin = '1899-12-30'),
     Back_value,
     type = "l")


# calculate max drawdown
Back_value <- as.vector(na.omit(Back_value))

Drawdown <- rep(NA, length(Back_value))

Drawdown[1] <- 0

for (j in 2:length(Back_value)) {
  # check one month at a time
  
  Drawdown[j] <- Back_value[j] / max(Back_value[1:(j - 1)]) - 1
  
} # end j
max_draw <- min(Drawdown)

cbind(
  Start_date = investing_date[1],
  avgmr = avgmr,
  IR = IR,
  HR = HR,
  avgIC = avgIC,
  Maxdraw = max_draw
)

## output each backtesting result to a individual csv file ##
# write.csv(cbind(investing_date,forward_return,investing_position,IC_month), "GK.csv")

## individual variable backtesting ##
# This code is for backtesting strategy for individual variable
# Return data to be provided under SectorReturns_R
# Country/Sector list to be provided under con_list/Sector_list
# forward return period to be provided under weeks
# To be tested indicator under factors
# Two investment strategy under method
# 1. "LS" = Long Short, count = number of assets Long or Short, threshold = NULL to initialize
# 2. "threshold" = invest iff above threshold or below -threshold, threshold = threshold value, count = NULL to initialize
# Start_date is analysis start date
# end_date is analysis end date

#### backtesting_ind<-function(factors,R = Secotrindex_R, day_of_month=22,con_list = sector_list,weeks = "_4w",method = "LS",count = 2, threshold = .5,start_date, end_date)
Performance <-
  data.frame(
    Start_Date = numeric(0),
    Avg_Mon_Return = numeric(0),
    IR = numeric(0),
    HR = numeric(0),
    Avg_IC = numeric(0),
    Max_Draw = numeric(0)
  )

for (n in 1:(length(mlist) - 1)) {
  # Example - DY_zz
  
  factorname <- names(mlist)[n]
  factor_test <- mlist[[factorname]]
  
  # Initializing
  
  start_date <- "1960-01-01"
  end_date <- "2012-12-31"
  backtest_setup <- 0 # For getting backtest parameters setup
  filename <-
    paste(factorname, ".csv", sep = "") # For getting file name of the backtest output
  forward_return <- 0 # For getting forward returns
  investing_position <- 0 # For getting countriest o invest
  investing_date <- 0 # Dates on which to invest
  IC_month <- 0 # Information coefficient for each month
  
  value <-
    backtesting_ind(
      factor_test,
      Sectorindex_R,
      22,
      sector_list,
      "_4w",
      "LS",
      4 ,
      NULL,
      start_date,
      end_date
    )
  
  avgmr <- mean(as.vector(na.omit(forward_return)))
  Std <- sd(as.vector(na.omit(forward_return)))
  IR <-
    mean(as.vector(na.omit((1 + forward_return) ^ 12 - 1))) / sd(as.vector(na.omit((1 +
                                                                                      forward_return) ^ 12 - 1)))
  HR = sum(as.integer(na.omit(forward_return) > 0)) / length(forward_return)
  avgIC <- mean(as.vector(na.omit(IC_month)))
  
  # calculate monthly return
  
  Back_value <- rep(NA, length(forward_return))
  
  Back_value[1] <- 100 * (1 + forward_return[1])
  
  for (i in 1:length(forward_return)) {
    Back_value[i + 1] <- Back_value[i] * (1 + forward_return[i + 1])
    
  }
  
  Back_value <- Back_value[-(length(Back_value))]
  
  # Plot Backtesting value return
  
  plot(as.Date(investing_date, origin = '1899-12-30'),
       Back_value,
       type = "l")
  
  # calculate max drawdown
  
  Back_value <- as.vector(na.omit(Back_value))
  
  Drawdown <- rep(NA, length(Back_value))
  
  Drawdown[1] <- 0
  
  for (j in 2:length(Back_value)) {
    # check one month at a time
    
    Drawdown[j] <- Back_value[j] / max(Back_value[1:(j - 1)]) - 1
    
  } # end j
  max_draw <- min(Drawdown)
  
  Performance[n, ] <-
    cbind(investing_date[1], avgmr, IR, HR, avgIC, max_draw)
  row.names(Performance) <- names(mlist)[1:n]
  
  ## output each backtesting result to a individual csv file
  # write.csv(cbind(investing_date,forward_return,investing_position,IC_month), filename)
  
} # end for
