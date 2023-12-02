##############################################################################
############################## Usual Bookkeeping #############################
##############################################################################

# Load Libraries
pkg_list <- c("BETS", "fpp3", "quantmod", "rbcb", "purrr")

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

start.date <- '2017-01-01'
split.date <- '2022-10-31'
end.date <- '2022-12-31'
f.days.m <- 22
f.days.y <- 252

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

# Download Covariates Data
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

# Create a single tsibble
## Add year and month columns to Price.BVSP
Price.BVSP <- Price.BVSP %>%
  mutate(Year = year(Index), Month = month(Index))

## Count days per month
days_per_month <- Price.BVSP %>%
  count(Year, Month)

## Convert IPCA to a format that can be joined with Price.BVSP
IPCA_join <- IPCA %>%
  mutate(Year = year(Index), Month = month(Index)) %>%
  select(-Index)

## Join IPCA data to Price.BVSP
Price.BVSP <- Price.BVSP %>%
  left_join(IPCA_join, by = c("Year", "Month")) %>%
  left_join(days_per_month, by = c("Year", "Month"))

## Calculate the daily IPCA rate
Price.BVSP <- Price.BVSP %>%
  mutate(IPCA.d = ((1 + IPCA)^(1/n) - 1))

## Convert SER to a format that can be joined with Price.BVSP
SER_join <- SER %>%
  mutate(Year = year(Index), Month = month(Index)) %>%
  select(-Index)

## Join SER data to Price.BVSP
Price.BVSP <- Price.BVSP %>%
  left_join(SER_join, by = c("Year", "Month"))

## Calculate the daily SER rate
Price.BVSP <- Price.BVSP %>%
  mutate(SPREAD.d = ((1 + SPREAD)^(1/n) - 1))

## Drop duplicate columns
Price.BVSP <- Price.BVSP %>%
  select(-c("Month", "IPCA", "Index.y", "n","SPREAD"))

## Join SELIC and CDI.d data to Price.BVSP
Price.BVSP <- Price.BVSP %>%
  left_join(SELIC, by = c("Index")) %>%
  left_join(CDI.d, by = c("Index"))

# Save Price.BVSP to a csv
# Construct the path to the Files folder one level up
file_path <- file.path(dirname(dir_path), "Files", "Price_BVSP.csv")

# Save Price.BVSP to CSV
write.csv(Price.BVSP, file_path, row.names = FALSE)