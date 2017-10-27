## Step 1. SET OUTPUT STORING FOLDER AND SECTOR LIST ##
# Set directory to store output
setwd(
  "/Sector model/Tree ouput"
)

# All sectors
all_sectors <-
  c(
    "AUCO",
    "BANKX",
    "CPGS",
    "COMS",
    "CODU",
    "HOTR",
    "DIVF",
    "ENRSX",
    "FDBT",
    "FDSR",
    "HCES",
    "HOUS",
    "INSU",
    "MATRX",
    "MEDA",
    "PHRM",
    "REAL",
    "RETL",
    "SSEQX",
    "SFTW",
    "TECH",
    "TELSX",
    "TRAN",
    "UTILX"
  )

# Ex-sectors
ex_sectors <- c("REAL", "SSEQX", "BANKX")

# Sectors to be tested after leaving out the ex-sectors
sector_list <- all_sectors[!all_sectors %in% ex_sectors]

## Step 2. DECID WINNER AND LOSER ##
# Notes:
#     1) m<-1: find winner/loser by ranking (only winner and loser)
#     2) m<-2: find winner/loser by ranking (winner, loser and neutral)
#                    - for this method, need to set value to ?neutral? for number of neutral opinion
#                    - of winner and loser will be split evenly from the rest
#     3) m<-3: find winner/loser by fixed threshold
#                    - for this method, need to set value to ?threshold.win? and ?threshold.lose? (for cross-sectional z-score)
#     4) m<-4: find winner/loser by value of benchmark variable
#                    - for this method, need to set value to ?benchmark?
# Set value of parameters in rolling winner/loser function
data.file <- mlist$Sectorindex_R
m <- 4
neutral = 2
threshold.win <- 0.5
threshold.lose <- -0.5
benchmark <- "SPX"

# Decide winner or loser with different window length

winner.loser_1w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "1w",
  label.var = "DATE"
)

winner.loser_4w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "4w",
  label.var = "DATE"
)

winner.loser_9w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "9w",
  label.var = "DATE"
)

winner.loser_13w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "13w",
  label.var = "DATE"
)

winner.loser_26w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "26w",
  label.var = "DATE"
)

winner.loser_52w_m1 = winner.loser(
  data_R = data.file,
  method = m,
  threshold.w = threshold.win,
  threshold.l = threshold.lose,
  benchmark.var = benchmark,
  neutral.number = neutral,
  candidate.list = sector_list,
  fwd.week = "52w",
  label.var = "DATE"
)

# Store all winner and loser in one dataframe
Data_winner.loser_m4 = cbind(
  winner.loser_1w_m1,
  winner.loser_4w_m1[-1],
  winner.loser_9w_m1[-1],
  winner.loser_13w_m1[-1],
  winner.loser_26w_m1[-1],
  winner.loser_52w_m1[-1]
)

# Add winner and loser information into master list
masterlist <- mlist

masterlist[[length(masterlist) + 1]] <- Data_winner.loser_m4

# Name the winner and loser data
names(masterlist)[length(masterlist)] <- "WL_M4"

## Step 3. STACK DATA FOR TREE MODEL AND BACKTESTING ##
# Select candidate factors to be fit into the tree models
factor_list <-
  c(
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
#factor_list <- c("E2EV_z","EYG1_zz","FCFY_z","IT_zz","NPM_z","PRICEMOMENTUM_12M_z","ROCE_z")

## Stack data for tree model
# Add winner and loser data into the list
name_list <- c("WL_M4", factor_list)

indicator_list <- masterlist[name_list]

data_for_new_factor <- list()

data_m1_4w <-
  stack_corr_cart(indicator_list, name_list, sector_list, "_4w", 22, "SPX")

## Stack data for backtesting
# Add sector return data into the list
name_list <- c("Sectorindex_R", factor_list)

indicator_list <- masterlist[name_list]

test_m1_4w <-
  stack_corr_cart(indicator_list, name_list, sector_list, "_4w", 22, "SPX")

## Step 4. ROLLING WINDOW BACK-TESTING ##

# Load rpart library
library(rpart)
library(rpart.plot)

## Do the backtesting
# backTest[[1]]: first split information
# backTest[[2]]: investment decisions in each time period
# backTest[[3]]: cumulative investment returns

backTest <-
  RW_Backtesting(
    window.width = 24,
    tree.data = data_m1_4w,
    test.data = test_m1_4w,
    candidate.list = sector_list,
    predictor.list = factor_list,
    exclude.var = c("DATE", "Sectors"),
    response.var = "WL_M4",
    label.var = "DATE",
    tree.control = c(
      minbucket = 30,
      maxcompete = 3,
      maxsurrogate = 1,
      usesurrogate = 2,
      xval = 10,
      surrogatestyle = 0,
      maxdepth = 3
    ),
    tree.fwd.month = 1,
    expand = 0,
    num_tree = 6,
    graph.all = 0
  )

# Calculate performance criteria
test_return <-
  as.data.frame(cbind(backTest[[3]], c(0, backTest[[3]][-1, 2] / backTest[[3]][-length(backTest[[3]][, 2]), 2] -
                                         1)))
names(test_return) <- c("DATE", "Cum_Return", "Mon_Return")

Avg_return <- mean(as.vector(na.omit(test_return$Mon_Return)))
Std_return <- sd(as.vector(na.omit(test_return$Mon_Return)))
IR <-
  mean(as.vector(na.omit((
    1 + test_return$Mon_Return
  ) ^ 12 - 1))) / sd(as.vector(na.omit((
    1 + test_return$Mon_Return
  ) ^ 12 - 1)))

# Calculate maximum drawdown
Drawdown <- c(0, rep(NA, length(test_return$Cum_Return) - 1))


for (j in 2:length(test_return$Cum_Return)) {
  # check one month at a time
  
  Drawdown[j] <-
    -test_return$Cum_Return[j] / max(test_return$Cum_Return[1:(j - 1)]) + 1
  
} # end j
MaxDD <- max(Drawdown)

Avg_return
Std_return
IR
MaxDD
