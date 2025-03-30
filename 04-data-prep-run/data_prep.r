require(fpp3)
require(ClustOfVar)
require(dplyr)
require(tidyr)
require(optparse)

source("utilities.r")

#### create run output directory ####
if (!dir.exists("./outputs")) {
  dir.create("./outputs")
}
#### end #####

#### optparse ####
parser <- OptionParser()

parser <- add_option(
  parser,
  opt_str = "--data_folder",
  action = "store",
  type = "character"
)

args <- parse_args(parser)

#### end ####

#### Load data ####
data <- read.csv("hour.csv") %>% as_tibble()
data_dir <- args$data_folder
path <- file.path(data_dir,"hour.csv")
data <- read.csv(path) %>% as_tibble()

#data <- read.csv("/home/azureuser/cloudfiles/code/bikeshare/02-data-asset/hour.csv") %>% as_tibble()

#### end ####

#### Data Prep ####

# fix date variable
data$dteday <- as_datetime(paste0(data$dteday," ",data$hr,":00:00"))
data <- data %>% as_tsibble(index = dteday)

# create workday variable
data <- data %>%
  mutate(isWorkday = ifelse(data$holiday == 0 & data$workingday == 1,TRUE,FALSE))

data$weathersit %>% table()

# remove useless variables
bikeshare <- data %>%
  select(dteday, isWorkday, weathersit, temp, atemp, hum, windspeed, casual, cnt) %>%
  mutate(isWorkday = factor(isWorkday), weathersit = factor(weathersit))

#scale numeric variables 
class <- sapply(bikeshare, class)
num_v <- colnames(bikeshare)[grep("numeric|integer", class)][-6]

bikeshare[, num_v] <- scale(bikeshare[, num_v])

#### end ####

#### variable selection ####

require(ClustOfVar)

class <- sapply(bikeshare, class)
num_v <- colnames(bikeshare)[grep("numeric|integer", class)][-6]
fact_v <- colnames(bikeshare)[grep("factor", class)]

# inputs should be in matrices
hclust_mod <- hclustvar(
  X.quanti = bikeshare[, num_v] %>% as.matrix(),
  X.quali = bikeshare[, fact_v] %>% as.matrix()
)

# determine number of clusters using dendogram
ClustOfVar::plot.hclustvar(hclust_mod)
nclust <- 5

# k means
kmeans_mod <- kmeansvar(
  X.quanti = bikeshare[, num_v] %>% as.matrix(),
  X.quali = bikeshare[, fact_v] %>% as.matrix(),
  nstart=20, init = nclust
)

# remove redundant variable
kmeans_mod$var

bikeshare <- bikeshare %>%
  select(-atemp,-weathersit)

# redo hclust to see if there are still redudant variable
class <- sapply(bikeshare, class)
num_v <- colnames(bikeshare)[grep("numeric|integer", class)][-5]
fact_v <- colnames(bikeshare)[grep("factor", class)]

# inputs should be in matrices
hclust_mod <- hclustvar(
  X.quanti = bikeshare[, num_v] %>% as.matrix(),
  X.quali = bikeshare[, fact_v] %>% as.matrix()
)

# determine number of clusters using dendogram
ClustOfVar::plot.hclustvar(hclust_mod)
nclust <- 4

# Kmeans
kmeans_mod <- kmeansvar(
  X.quanti = bikeshare[, num_v] %>% as.matrix(),
  X.quali = bikeshare[, fact_v] %>% as.matrix(),
  init = nclust, nstart=20
)

# remove redundant variable
kmeans_mod$var

bikeshare <- bikeshare %>%
  select(-temp)

bikeshare

#### end ####


#### relationships between predictors and forecast ####

require(ggplot2)

a <- data %>%
  ggplot(aes(x = casual, y = cnt)) +
  geom_point() +
  geom_smooth() +
  labs(title = "cnt vs casual")+
  scale_x_continuous(n.breaks = 20)

b <- bikeshare %>%
  ggplot(aes(x = hum, y = cnt)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "cnt vs hum"
  )

ggsave(plot = a, path = "./outputs", filename = "a.png")
ggsave(plot = b , path = "./outputs", filename = "b.png")

mlflow_log_artifact(path = "./outputs/a.png")
mlflow_log_artifact(path = "./outputs/b.png")

#### end ####

#### save the data ####
saveRDS(bikeshare, file = "./outputs/bike_clean.rds")

#### end ####
