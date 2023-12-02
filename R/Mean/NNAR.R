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


# NNAR forecasts for GOOG price 

NNAR.BVSP.train <- BVSP.train %>%
  as.data.frame() %>%
  select(c(BVSP.Adjusted)) %>%
  ts() %>%
  as_tsibble()

NNAR.BVSP <- NNAR.BVSP.train %>%
  model(NNETAR(value))

NNAR.BVSP


NNAR.BVSP.fc <- NNAR.BVSP %>%
  forecast(h = nrow(BVSP.test))



NNAR.BVSP.fc %>%
  autoplot(NNAR.BVSP.train) +
  xlim(1400, NA)


NNAR.BVSP.sim <- NNAR.BVSP %>%
  generate(h = nrow(BVSP.test), times = 5, bootstrap = TRUE)

head(NNAR.BVSP.sim)

NNAR.BVSP.train %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = value)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = NNAR.BVSP.sim) +
  xlim(1400, NA) +
  labs(title="Five simulated sample paths based on NNAR method with boostrapped residuals", y="Index" ) +
  guides(colour = "none") 