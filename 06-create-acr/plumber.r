require(fpp3)
require(plumber)
require(jsonlite)

#* @apiTitle Bikeshare Forecasting
#* @apiDescription Forecast Bike Count

#* Aliveness 
#* @get /alive

function(){
    "alive"
}

#* Readiness
#* @get /ready

function() {
    "ready"
}


#### Load our models ####
mod_dir <- Sys.getenv("AZUREML_MODEL_DIR")

model <- readRDS(file.path(mod_dir, "outputs", "model.rds"))
trend_mod <- readRDS(file.path(mod_dir, "outputs", "trend_model.rds"))

#### end ####

#* @post /score
#* @param data The input dataset as JSON
#* @serializer json

function(data){

    newdata <- fromJSON(req$postBody , flatten = R)
    
    ##### data prep#####
    # fix dteday
    newdata$dteday <- paste(newdata$dteday, " ", newdata$hr, ":00:00", sep = "") %>% as_datetime()
    
    # select variables we will use 
    names_v <- c("dteday","holiday","workingday","weathersit","temp","hum","windspeed","casual","cnt")
    newdata<- newdata[, names_v]
    
    newdata <- newdata %>%
      as_tsibble(index = dteday)
    
    
    ## Normalize the numeric perdictors
    num_v <- c("temp","hum","windspeed","casual")
    newdata[, num_v] <- scale(newdata[, num_v])
    
    ## Create a new variable to indicate workday
    newdata <- newdata %>%
        mutate(isWorkday = ifelse(newdata$holiday == 0 & newdata$workingday == 1,TRUE,FALSE))
    
    newdata <-  newdata %>%
      select(-workingday,-holiday)
    
    #factor character variables
    newdata <- newdata %>%
      mutate(isWorking = as.factor(isWorking),
             weathersit = as.factor(weathersit))
    
    ##### end data prep#####
    
    #### prediction ####
    newdata <- newdata %>% 
      fill_gaps() %>%
      mutate(trend_comp = forecast(trend_model,h=nrow(newdata))$.mean)
    
    forecasts <- forecast(model , newdata %>% fill_gaps())$.mean
    true <- newdata$cnt
    #### end ####
    
    toJSON(list(true , forecasts),auto_unbox = T)
}

