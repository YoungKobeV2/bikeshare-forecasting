require(fpp3)
require(ClustOfVar)
require(dplyr)
require(tidyr)

#source("utilities.r")

#### create run output directory ####
if (!dir.exists("./outputs")) {
    dir.create("./outputs")
}
#### end #####

#### Load data ####
#data_dir <- Sys.getenv("AZUREML_DATADIR")
#path <- file.path(data_dir,"bikesharedata","hour.csv")
#data <- read.csv(path)

data <- read.csv("/home/azureuser/cloudfiles/code/bikeshare/02-data-asset/hour.csv") %>% as_tibble()

#### end ####

#### Data Prep ####

# fix date variable
data$dteday <- as_datetime(paste0(data$dteday," ",data$hr,":00:00"))

# create workday variable
data <- data %>%
    mutate(isWorkday = ifelse(data$holiday == 1 | wday(dteday,week_start=1) %in% c(6, 7), FALSE,TRUE)) %>%
    select(-holiday,-weekday)

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
    nstart=10 , init = nclust
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
nclust = 3

# Kmeans
kmeans_mod <- kmeansvar(
    X.quanti = bikeshare[, num_v] %>% as.matrix(),
    X.quali = bikeshare[, fact_v] %>% as.matrix(),
    nstart=20, init = nclust
)

# remove redundant variable
kmeans_mod$var

bikeshare <- bikeshare %>%
    select(-windspeed,-casual)

bikeshare

#### end ####


#### relationships between predictors and forecast ####

require(ggplot2)

bikeshare %>%
    ggplot(aes(x = temp, y = cnt)) +
    geom_point() +
    geom_smooth() +
    labs(title = "cnt vs temp")

bikeshare %>%
    ggplot(aes(x = hum, y = cnt)) +
    geom_point() +
    geom_smooth() +
    labs(
        title = "cnt vs hum",
        subtitle = "Needs at knot at hum=-2"
    )

bikeshare <- bikeshare %>%
    mutate(hum_knot = pmax(hum, -2))

bikeshare
#### end ####

#### save the data ####
#saveRDS(bikeshare, file = "./outputs/bike_clean.rds")

#### end ####
