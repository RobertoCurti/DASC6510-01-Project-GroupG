##############################################################################
############################## Usual Bookkeeping #############################
##############################################################################

# Load Libraries
pkg_list <- c("broom", "fabletools", "fpp3", "stringr")

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

# Calculate sign correlation
rho_cal <- rho.cal(price.BVSP$BVSP.Adjusted)

# Print results
cat("Symbol: ^BVSP", "\nSign Correlation:", rho_cal, "\n\n")

# Fit a suitable ARIMA model
ARIMA.BVSP.auto <- BVSP.train %>%
  model(ARIMA(BVSP.Adjusted))

# Extract p, d, q from the auto model model
ARIMA.BVSP.auto.order <- BVSP.train %>%
  model(ARIMA(BVSP.Adjusted)) %>%
  tidy() %>%
  .[,2]

ARIMA.BVSP.auto.order.p <- ARIMA.BVSP.auto.order %>%
  mutate(group = str_extract(term, "^\\D+"),
         number = as.numeric(str_extract(term, "\\d+"))) %>%
  group_by(group) %>%
  summarize(largest = max(number, na.rm = TRUE)) %>%
  .[1,2] %>%
  as.numeric()

ARIMA.BVSP.auto.order.q <- ARIMA.BVSP.auto.order %>%
  mutate(group = str_extract(term, "^\\D+"),
         number = as.numeric(str_extract(term, "\\d+"))) %>%
  group_by(group) %>%
  summarize(largest = max(number, na.rm = TRUE)) %>%
  .[2,2] %>%
  as.numeric()

# Generate a series of ARIMA models
ARIMA.BVSP.all <- generate_and_fit_arima_models(
  ARIMA.BVSP.auto.order.p + 1, 2, ARIMA.BVSP.auto.order.q, BVSP.train)

# Glance the results to select the best model
ARIMA.BVSP.all %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# Fit the best model
ARIMA.BVSP.best <- BVSP.train %>%
  model(ARIMA(BVSP.Adjusted~ pdq(3, 1, 3)))

# Report from best fit
report(ARIMA.BVSP.best)

# Residuals from best fit
gg_tsresiduals(ARIMA.BVSP.best)

# Produce forecasts of your fitted model. Do the forecasts look reasonable
ARIMA.BVSP.best.fc <- ARIMA.BVSP.best %>%
  forecast(h = nrow(BVSP.test))

ARIMA.BVSP.best.fc %>% 
  autoplot(BVSP.train) +
  xlim(1350, NA)


ARIMA.BVSP.best.sim <- ARIMA.BVSP.best %>%
  generate(h = nrow(BVSP.test), times = 5, bootstrap = TRUE)

head(ARIMA.BVSP.best.sim)

BVSP.train %>%
  ggplot(aes(x = t.day)) +
  geom_line(aes(y = BVSP.Adjusted)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = ARIMA.BVSP.best.sim) +
  xlim(1400, NA) +
  labs(title="Five simulated sample paths based on ARIMA method with boostrapped residuls", y="Index" ) +
  guides(colour = "none") 