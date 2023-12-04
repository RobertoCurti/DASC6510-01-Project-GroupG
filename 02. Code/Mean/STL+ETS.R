##############################################################################
############################## Usual Bookkeeping #############################
##############################################################################

# Load Libraries
pkg_list <- c("fpp3")

# Install packages if needed
for (pkg in pkg_list)
{
  ## Try loading the library.
  if ( ! library(pkg, logical.return=TRUE, character.only=TRUE) )
  {
    ### If the library cannot be loaded, install it; then load.
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
}

# Get the path of the current script file
script_path <- rstudioapi::getSourceEditorContext()$path

# Extract the directory path from the script path
dir_path <- dirname(script_path)

# set the current working directory as the working directory
setwd(dir_path)

# Construct file path to the Files folder one level up
file_path <- file.path(dirname(dirname(dir_path)),"Files")


##############################################################################
############################ Global and Functions ############################
##############################################################################

start.date <- '2017-01-01'
split.date <- '2022-10-31'
end.date <- '2022-12-31'
f.days.m <- 22
f.days.y <- 252

# Sign Correlation Function 
rho.cal<-function(X){
  rho.hat<-cor(sign(X-mean(X)), X-mean(X))
  return(rho.hat)
}

# Generate and Fit several ARIMA models given p, d, q
generate_and_fit_arima_models <- function(p_max, d_max, q_max, data) {
  model_calls <- c()
  for (p in 0:p_max) {
    for (d in 0:d_max) {
      for (q in 0:q_max) {
        model_name <- paste0("arima", p, d, q)
        model_spec <- paste0(model_name, " = ARIMA(BVSP.Adjusted ~ 1 + pdq(", p, ", ", d, ", ", q, "))")
        model_calls <- c(model_calls, model_spec)
      }
    }
  }
  full_call <- paste("model(data, ", paste(model_calls, collapse = ", "), ")", sep = "")
  return(eval(parse(text=full_call)))
}

##############################################################################
#################################### Data ####################################
##############################################################################

# Load the data
price.BVSP <- read.csv(file.path(file_path,"Price_BVSP.csv"))

# Transform into a tsibble
price.BVSP <- as_tsibble(price.BVSP, index = t.day)

# Split the data into train and test
price.BVSP %>%
  filter(Index <= as.Date(split.date)) -> BVSP.train

price.BVSP %>%
  filter(Index > as.Date(split.date)) -> BVSP.test

##############################################################################
#################################### Code ####################################
##############################################################################

# Decomposition of the time series
BVSP.comb <- BVSP.train %>%
  model(ETS = ETS(BVSP.Adjusted),
        ARIMA = ARIMA(BVSP.Adjusted ~ pdq(3, 1, 3))
        ) 
BVSP.comb.fc <- BVSP.comb %>%
  mutate(combination = (ETS + ARIMA)/2
         ) %>%
  forecast(h = nrow(BVSP.test))

# Combination accuracy
BVSP.comb.fc %>% accuracy(BVSP.test)

