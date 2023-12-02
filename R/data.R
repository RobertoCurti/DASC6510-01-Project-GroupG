##############################################################################
############################## Usual Bookkeeping #############################
##############################################################################

# Load Libraries
pkg_list <- c("BETS", "fpp3", "quantmod", "rbcb")

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

##############################################################################
#################################### Code ####################################
##############################################################################

start.date <- '2017-01-31'
split.date <- '2022-10-31'
end.date <- '2022-12-31'

# Download the IBOVESPA data from Yahoo Finance.
## Symbols you want to download
symbol <- c("^BVSP")

## Download the selected stocks
getSymbols(symbol, src = "yahoo", from = start.date, to = end.date)

## Extract only the adjusted price for the requested range
Price.BVSP <- na.omit(BVSP[, "BVSP.Adjusted"])

## Create date variable
zoo::fortify.zoo(Price.BVSP) %>%
  mutate(t.day = row_number()) %>%
  as_tsibble(index = t.day) -> Price.BVSP

## Split the data into train and test
Price.BVSP %>%
  filter(Index <= as.Date(split.date)) -> BVSP.train

Price.BVSP %>%
  filter(Index > as.Date(split.date)) -> BVSP.test

# Download Covariates Data
## GDP
data.load <- BETSget(c(4192))

### Convert the ts object to a tsibble
GDP <- as_tsibble(data.load)

## Inflation
data.load <- rbcb::get_series(c(IPCA = 433),
                         start_date = start.date,
                         end_date = end.date,
                         as = "xts")

### Create date variable
zoo::fortify.zoo(data.load) %>%
  mutate(Index = yearmonth(Index)) %>%
  as_tsibble(index = Index) -> IPCA

## Base Federal Interest Rate
data.load <- rbcb::get_series(c(SELIC = 1178),
                          start_date = start.date,
                          end_date = end.date,
                          as = "xts")

### Create date variable
zoo::fortify.zoo(data.load) %>%
  as_tsibble(index = Index) -> SELIC

## Interest Rate Spread
data.load <- rbcb::get_series(c(SPREAD = 20783),
                        start_date = start.date,
                        end_date = end.date,
                        as = "xts")

### Create date variable
zoo::fortify.zoo(data.load) %>%
  mutate(Index = yearmonth(Index)) %>%
  as_tsibble(index = Index) -> SER

## Bank Bond Certificate
### Daily
data.load <- rbcb::get_series(c(CDI.d = 12),
                              start_date = start.date,
                              end_date = end.date,
                              as = "xts")

#### Create date variable
zoo::fortify.zoo(data.load) %>%
  as_tsibble(index = Index) -> CDI.d

### Monthly
data.load <- rbcb::get_series(c(CDI.m = 4391),
                              start_date = start.date,
                              end_date = end.date,
                              as = "xts")

### Create date variable
zoo::fortify.zoo(data.load) %>%
  mutate(Index = yearmonth(Index)) %>%
  as_tsibble(index = Index) -> CDI.m