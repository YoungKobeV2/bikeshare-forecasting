require(fpp3)
require(optparse)
require(ggplot2)
require(patchwork)
require(urca)

#### make options ####
parser <- OptionParser()

parser <- add_option(
    parser,
    "--data_folder",
    action = "store",
    type = "character"
)

args <- parse_args(parser)

#### end #####

#### Load data ####
path <- file.path(args$data_folder, "hour.csv")

bikeshare <- read.csv(path)

bikeshare <- bikeshare %>% as_tsibble(index = dteday)

summary(bikeshare)

#### end #####

#### plot time series ####
bikeshare %>%
    fill_gaps() %>%
    filter(yearmonth(dteday) == yearmonth("2011 Jan")) %>%
    ggplot(aes(x = dteday, y = cnt)) +
    geom_line() +
    labs(title = "Time Series for 2011 Jan")

bikeshare %>%
    fill_gaps() %>%
    filter(yearweek(dteday) == yearweek("2011 W2")) %>%
    ggplot(aes(x = dteday, y = cnt)) +
    geom_line() +
    labs(
        title = "Time Series for 2011 W2",
        caption = "Seasonal Pattern change for weekends"
    )

#### end #####

#### explore seasonal patterns ####

# over years 

a <- bikeshare %>%
    fill_gaps() %>%
    filter(year(dteday)==2011)%>%
    gg_season(y = cnt, period = "week") +
    labs(title = "2011")

b <- bikeshare %>%
    fill_gaps() %>%
    filter(year(dteday)==2012)%>%
    gg_season(y = cnt, period = "week") +
    labs(title = "2012")     

a / b + plot_annotation(subtitle = "Patterns change over weekends")

# over year months 

a <- bikeshare %>%
    fill_gaps() %>%
    filter(yearmonth(dteday)==yearmonth("2011 Jan"))%>%
    gg_season(y = cnt, period = "week") +
    labs(title = "2011 Jan")

b <- bikeshare %>%
    fill_gaps() %>%
    filter(yearmonth(dteday)==yearmonth("2012 Jan"))%>%
    gg_season(y = cnt, period = "week") +
    labs(title = "2012 Jan")     

a / b + plot_annotation(subtitle = "Patterns change over weekends")

#### end #####

#### identify seasonal periods ####

acf(bikeshare$cnt %>% log(), lag.max = 100)
title(main = "Difference for lag=24",line = 0)

acf(bikeshare$cnt %>% log() %>% diff(lag=24), lag.max = 1000)
title(main = "No Differencing is needed",line = 0)

acf(bikeshare$cnt %>% log() %>% diff(lag=24), lag.max = 5000)
title(main = "We have Daily , Weekly Seasonality", line = 0)

pacf(bikeshare$cnt %>% log() %>% diff(lag=24), lag.max = 1000)

#### Build Model ####

# stl decomposition

stl_decomp <- bikeshare %>%
    mutate(t = row_number()) %>%
    update_tsibble(index = t) %>%
    model(
        STL(log(cnt) ~ trend(window=1000) +season(period = 24)+season(period = 24*7))
    ) %>%
    components()

stl_decomp %>% autoplot()

bikeshare <- bikeshare %>%
    mutate(trend_comp = stl_decomp$trend)

# model

yearweek(bikeshare$dteday) %>% tail()

train <- bikeshare %>%
    filter(yearweek(dteday) < yearweek("2012 51"))

test <- bikeshare %>%
    filter(yearweek(dteday) >= yearweek("2012 51"))


test_mod <- bikeshare %>%
    fill_gaps() %>%
    model(
        ARIMA(log(cnt)~trend_comp+isWorkday+
        fourier(period="day",K=6):isWorkday+
        fourier(period="week",K=10):isWorkday)
    )
acc <- accuracy(test_mod)


bikeshare %>%
    filter(yearmonth(dteday) == yearmonth("2011 Jan")) %>%
    autoplot(.vars = cnt) +
    autolayer(fitted(test_mod) %>%
        filter(yearmonth(dteday) == yearmonth("2011 Jan")))