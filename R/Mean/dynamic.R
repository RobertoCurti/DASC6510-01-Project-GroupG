##############################################################################
############################## Usual Bookkeeping #############################
##############################################################################

# Load Libraries
pkg_list <- c("fabletools", "fpp3")

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

# Model Fits with a single covariate
dynamic.BVSP.single <- BVSP.train %>%
  model('CDI' = ARIMA(BVSP.Adjusted ~ CDI.d),
        'IPCA' = ARIMA(BVSP.Adjusted ~ IPCA.d),
        'SELIC' = ARIMA(BVSP.Adjusted ~ SELIC),
        'SPREAD' = ARIMA(BVSP.Adjusted ~ SPREAD.d))


## Glance the results to select the best model
dynamic.BVSP.single %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# Model Fits with a two covariates
dynamic.BVSP.double <- BVSP.train %>%
  model('CDI + IPCA' = ARIMA(BVSP.Adjusted ~ CDI.d + IPCA.d),
        'CDI + SELIC' = ARIMA(BVSP.Adjusted ~ CDI.d + SELIC),
        'CDI + SPREAD' = ARIMA(BVSP.Adjusted ~ CDI.d+ SPREAD.d),
        'IPCA + SELIC' = ARIMA(BVSP.Adjusted ~ IPCA.d + SELIC),
        'IPCA + SPREAD' = ARIMA(BVSP.Adjusted ~ IPCA.d + SPREAD.d),
        'SELIC + SPREAD' = ARIMA(BVSP.Adjusted ~ SELIC + SPREAD.d))


## Glance the results to select the best model
dynamic.BVSP.double %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# Model Fits with a three covariates
dynamic.BVSP.triple <- BVSP.train %>%
  model('CDI + IPCA + SELIC' = ARIMA(BVSP.Adjusted ~ CDI.d + IPCA.d + SELIC),
        'CDI + IPCA + SPREAD' = ARIMA(BVSP.Adjusted ~ CDI.d + IPCA.d + SPREAD.d),
        'CDI + SELIC + SPREAD' = ARIMA(BVSP.Adjusted ~ IPCA.d + SELIC + SPREAD.d)
        )


## Glance the results to select the best model
dynamic.BVSP.triple %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# Model Fits with all covariates
dynamic.BVSP.all <- BVSP.train %>%
  model('CDI + IPCA + SELIC + SPREAD' = ARIMA(
    BVSP.Adjusted ~ CDI.d + IPCA.d + SELIC + SPREAD.d)
  )

## Glance the results to select the best model
dynamic.BVSP.all %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# Model Fits with a single lagged covariate
dynamic.BVSP.single.lag <- BVSP.train %>%
  model('CDI no lag' = ARIMA(BVSP.Adjusted ~ CDI.d),
        'CDI lag(1)' = ARIMA(BVSP.Adjusted ~ CDI.d + lag(CDI.d)),
        'CDI lag(2)' = ARIMA(BVSP.Adjusted ~ CDI.d + lag(CDI.d) + lag(CDI.d,2)),
        'CDI lag(3)' = ARIMA(BVSP.Adjusted ~ CDI.d + lag(CDI.d) + lag(CDI.d,2) + lag(CDI.d,3)),
        'CDI lag(4)' = ARIMA(BVSP.Adjusted ~ CDI.d + lag(CDI.d) + lag(CDI.d,2) + lag(CDI.d,3) + lag(CDI.d,4)))

## Glance the results to select the best model
dynamic.BVSP.single.lag %>%
  glance() %>%
  arrange(AICc)


# Fitting best model
dynamic.BVSP.best <- BVSP.train %>%
  model('CDI lag(4)' = ARIMA(BVSP.Adjusted ~ CDI.d + lag(CDI.d) + lag(CDI.d,2) + lag(CDI.d,3) + lag(CDI.d,4)))

# Report from best model
report(dynamic.BVSP.best)

# Residuals from best fit
gg_tsresiduals(dynamic.BVSP.best)

# Produce forecasts of your fitted model. Do the forecasts look reasonable
BVSP.best.future <- new_data(BVSP.train,nrow(BVSP.test))

BVSP.best.future$CDI.d <- mean(BVSP.train$CDI.d)

forecast(dynamic.BVSP.best,BVSP.best.future) %>% autoplot(BVSP.train) +
  xlim(1350, NA)

dynamic.BVSP.best.sim <- dynamic.BVSP.best %>%
  generate(h = nrow(BVSP.test), new_data = BVSP.best.future, times = 5, bootstrap = TRUE)

head(dynamic.BVSP.best.sim)

BVSP.train %>%
  ggplot(aes(x = t.day)) +
  geom_line(aes(y = BVSP.Adjusted)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = dynamic.BVSP.best.sim) +
  xlim(1400, NA) +
  labs(title="Five simulated sample paths based on dynamic regression method with boostrapped residuls", y="Index" ) +
  guides(colour = "none") 