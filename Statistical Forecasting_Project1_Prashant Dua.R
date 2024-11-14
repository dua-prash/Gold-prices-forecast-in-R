library(fpp3)
library(tidyverse)
library(knitr)
library(seasonal)
library(lubridate)
library(ggplot2)

#Loaded the dataset in R using Import dataset option. 
Gold_Price<-read_csv("C:/Users/duapr/OneDrive/Documents/Conestoga College Study/Predictive Analytics/Sem 2/STAT8041-Statistical Forecasting/Individual Project/daily gold price/Gold Price.csv")

#Converting ruppes into CAD value
Gold_Price$Open<- Gold_Price$Open/62
Gold_Price$High<- Gold_Price$High/62
Gold_Price$Low<- Gold_Price$Low/62

#Converted dataset in tsibble based on Date index.
goldprices_t1 <- Gold_Price |>
  as_tsibble(index=Date)

# Used filter to filter results on or after 2018 since we are interested in analyzing our dataset for last 5 years 
#We also used rowid_to_column()
#to add sequential indexes which will become final index of the tsibble object to replace 
#gaps in Dates object.
goldprices_t2<- goldprices_t1 |>
  filter(year(Date)>=2018) |>
  rowid_to_column() |>
  update_tsibble(index=rowid, regular = TRUE)

#Code to check the structure of the tsibble to ensure we right datatypes.
str(goldprices_t2)

#Plotting the time series for Opening prices of Gold per day
goldprices_t2 |>
  autoplot(Open) +
    labs(title="Daily Opening Rate of Gold prices for last 5 years",
         y="Opening Price",
         x="Daily rate by year"
    )

head(goldprices_t2)
#Plotting the timeseries for High prices of Gold per day
goldprices_t2 |>
  autoplot(High) +
  labs(title="Daily High prices of Gold for last 5 years",
       y="High Price",
       x="Daily rate by year"
  )

#Plotting the timeseries for Low prices of Gold per day
goldprices_t2 |>
  autoplot(Low) +
  labs(title="Daily Low prices of Gold for last 5 years",
       y="Low Price",
       x="Daily rate by year"
  )


#Code to filter the daily Open prices of gold for further analysis
goldprices_Open <- goldprices_t2 |>
  select(rowid, Date, Open)

#ACF plot of the Open gold prices
goldprices_Open %>% 
  ACF(goldprices_Open$Open, lag_max=24) |>
  autoplot() +
  labs(title="ACF plot of Daily open prices for 2018-2022",
       x="lag"
  )

#Box Cox transformation

#Calculated optimal lambda using guerrero method that will give the right amount of transformation we need.
lambda<- goldprices_Open |>
  features(Open,features = guerrero) |>
  pull(lambda_guerrero)

goldprices_Opent<-goldprices_Open |>
  autoplot(box_cox(Open,lambda)) +
  labs(title="Box Cox transformer for Open prices")

#ACF Plot without box cox transformation
goldprices_Open %>% 
  ACF(Open, lag_max=60) |>
  autoplot() +
  labs(title="ACF plot of Daily open prices for 2018-2022 without box-cox",
       x="lag"
  )

#ACF Plot with box cox transformation
goldprices_Open %>% 
  ACF(box_cox(Open,lambda), lag_max=60) |>
  autoplot() +
  labs(title="ACF plot of Daily open prices for 2018-2022 with box-cox",
       x="lag"
  )

############### CLASSICAL DECOMPOSITION ##################

dcmp_gold<- goldprices_Open |>
  model(
    classical = classical_decomposition(Open ~ season(36), type="additive"),
    )

print(components(dcmp_gold))

classical_comp<- dcmp_gold|> select(classical) |> components(dcmp_gold)

#lookup kable command for classical decomposition to see how to make html table
classical_comp |> tail() |> kable()

classical_comp |> autoplot()


#FORECASTS

#Create Train set for 2021 to train model and test set for 2022 to test performance of trained models
gold_train<- goldprices_Open |> filter(year(Date)==2021)
gold_test<- goldprices_Open |> filter(year(Date)==2022)

#Build a mable with three models to compare accuracy, mean, naive and drift
gold_fit <- gold_train |>
  model(
    Mean = MEAN(Open),
    NaiveM= NAIVE(Open),
    NaiveD = NAIVE(Open~drift())
  )
gold_fit

#Performing forecasts to next 50 periods
gold_fc<-gold_fit |> forecast(h=40)
gold_fc
#Plotting the forecasts for all the 3 fitted models and we have added another layer to the plot to show 
#   confidence intervals of forecasts
goldprices_Open %>% autoplot(Open) +
  autolayer(gold_fc)+
  labs(title="Forecasts using Mean, Naive and Drift models for Open Gold prices")

  
# Evaluate forecast accuracy
accuracy(gold_fc,gold_test) %>% select(.model, RMSE, MAE)

# Evaluate the residuals for the best fitted model and below we take residuals of our most accurate model that is the Naive model
#out of the three models used
naive_fit <- gold_fit |> select(NaiveM)
naive_fit |> gg_tsresiduals() +
  labs(title="Fitted residuals for the Naive forecast model for Open Gold prices")

#Gathers the residual data for each of the forecast observation against the actual observations.
augment(naive_fit) 

#We use ggplot to plot the fitted versus the observed data
augment(naive_fit) |>
  ggplot(aes(x=rowid)) +
  geom_line(aes(y=Open,colour="Data"))+
  geom_line(aes(y=.fitted,color="Fitted")) +
  labs(title="GGplot of fitted versus actual Open Gold prices")


#Fitted the linear regression with trend() as the predictor
fit<- goldprices_Open |>
  model(trend_model= TSLM(Open ~ trend()))

fit |> forecast(h=40) |> autoplot(goldprices_Open)
accuracy(fit,gold_test$Open) %>% select(.model, RMSE, MAE)

fit |> gg_tsresiduals() +
  labs(title="Fitted residuals for the TSLM forecast model for Open Gold prices")


