require(fpp3)
require(azuremlsdk)
require(optparse)
require(mlflow)
require(forecast)
require(dplyr)
require(tidyr)

source("utilities.r")


#### make outputs dir ####

if(!dir.exists("./outputs")){
  dir.create("./outputs")
}

#### end ####

#### Make script Options ####
Options <- list(
  make_option("--data_folder") ## create parameter for this script
) 
opts_vars <- OptionParser(Options) ## creates variable to hold parameter
parse_vars <- parse_args(opts_vars) ## runs the variables , returns values

paste(parse_vars$data_folder)
#### end ####

#### Build Train Model ####

data <- readRDS(file.path(parse_vars$data_folder,"clean_bikeshare.rds"))
summary(data)
tail(data)

## understand the series

acf(data$cnt %>% log(),lag.max = 1000)
pacf(data$cnt %>% log(),lag.max = 1000)

acf(data$cnt %>% log()%>% diff(24),lag.max = 1000)
pacf(data$cnt %>% log() %>% diff(24),lag.max = 1000)

data %>%
  fill_gaps()%>%
  head(1000)%>%
  autoplot(.vars = cnt)

## decomposition

stl_decomp <- data %>%
  mutate(t=row_number())%>%
  update_tsibble(index = t)%>%
  model(
    STL(log(cnt)~season(period=24)+season(period=24*7)+trend(window=300))
  ) %>%
  components()
stl_decomp %>% autoplot()

data <- data %>%
  mutate(trend_comp = stl_decomp$trend)

## train test

train <- data %>%
  filter(yearweek(dteday) <= yearweek("2012 W51"))
test <- data %>%
  filter(yearweek(dteday) > yearweek("2012 W51"))

## model
best_model <- train %>%
  fill_gaps()%>%
  model(
    ARIMA(log(cnt)~trend_comp+pdq(d=0)+PDQ(0,0,0)+
            isWorkday+hum+casual+windspeed+
            fourier(period="day",K=10):isWorkday+
            fourier(period="week",K=8):isWorkday),
  )
train_acc <- best_model %>% 
  accuracy() %>%
  select(-.model,-.type) %>%
  pivot_longer(cols=everything(),names_to = "key")%>%
  mutate(key=paste("Train",key))

mlflow_log_batch(params = train_acc)
# trend model

trend_mod <- train %>%
  mutate(t=row_number())%>%
  update_tsibble(index = t)%>%
  model(
    est=ETS(trend_comp~trend("Ad"))
  )
trend_mod %>% 
  forecast(h=nrow(test))%>% 
  autoplot(data %>% 
             mutate(t=row_number())%>%
             update_tsibble(index = t)%>%
             tail(300))

new_test <- test %>%
  mutate(trend_comp = forecast(trend_mod[,2] , h = nrow(test))$.mean)

# forecast

plot <- forecast(best_model, new_data = new_test %>% fill_gaps()) %>%
  autoplot(data %>%tail(nrow(test))%>% fill_gaps(),linewidth=1)+
  labs(title = "Forecast Plot",
       subtitle = "Blue = Forecast , Black = True")+
  guides(colour=guide_legend())

ggsave(filename = "Forecast Plot.jpeg",
       plot = plot,
       dpi = 500,
       path = "./outputs")

mlflow_log_artifact(path = "./outputs/Forecast Plot.jpeg")

# test accuracy

test_acc <- best_model %>%
  refit(new_data = new_test %>% fill_gaps() ,reestimate=F)%>%
  accuracy()%>%
  select(-.model,-.type)%>%
  pivot_longer(cols=everything(),names_to = "key")%>%
  mutate(key=paste("Test",key))

mlflow_log_batch(params = test_acc)

#### end ####

#### Build Final Model ####

## model

model <- data %>%
  fill_gaps()%>%
  model(
    ARIMA(log(cnt)~Trend_comp+pdq(d=0)+PDQ(0,0,0)+
            isWorking+hum+hum_knot+temp+windspeed+
            fourier(period="day",K=10):isWorking+
            fourier(period="week",K=8):isWorking),
  )

saveRDS(object = model ,file = "./outputs/model.rds")

# trend model
trend_mod <- data %>%
  mutate(t=row_number())%>%
  update_tsibble(index = t)%>%
  model(
    est=ETS(trend_comp~trend("Ad"))
  )

saveRDS(object = trend_mod , file = "./outputs/trend_model.rds")
