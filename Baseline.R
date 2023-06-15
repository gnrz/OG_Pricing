

# Preliminaries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)
library(googledrive)

source("functions.R")
#### Assumptions ####
## One product, 
## only two different prices, 
## one site, 
## 1 competitors, 
## no facilities, 
## no time of day, 
## everyone fills the same quantity, 
## no weather, 
## no strategic interaction

## Roadmap

## 1. multiple prices in a range (uniform), per day
## 2. utility_outside is competitor station (uniform prices in same range), assume distance isn't a factor


# Simulate data #
set.seed(0)

sim_data <- create_data(T=90,
                        n_per_day=1000,
                        mean_utility=0.5,
                        price_coefficient=-0.5,
                        high_price=2,
                        low_price=0.4, 
                        price_granularity=1,
                        sites=2)



ggplot(sim_data[[5]])+geom_point(mapping=aes(y=quantity,x=prices, group=site_index, color=site_index))


transactions <-       ggplot(sim_data[[5]],mapping=aes()) + geom_line(aes(x=day_index,y=quantity, group=site_index, color=site_index))
prices_plot <-        ggplot(sim_data[[1]],mapping=aes()) + geom_line(aes(x=day_index, y=prices, group=site_index, color=site_index))

print(transactions /  prices_plot)
          

## Model 1: Observe Purchase Binary, OLS Model

ols <- sim_data[[3]] %>% 
  lm(purchase_binary ~ prices + prices_c,
     data = .) %>%
  summary()

ols

ols_elasticity <- ols$coefficients[2] * mean(sim_data[[3]]$prices) / mean(sim_data[[3]]$purchase_binary)

## Model 2: Observe Purchase Binary, Logit Model

logit <- sim_data[[3]] %>%
  glm(formula = purchase_binary ~ prices + prices_c,
      family = binomial(link = 'logit'),
      data = .) %>%
  summary()

logit

logit_elasticity <- logit$coefficients[2] * (1 - mean(sim_data[[3]]$purchase_binary))*mean(sim_data[[3]]$prices)

## Model 3: Observe Total Quantity

quantity_model <- sim_data[[5]] %>% 
  lm(quantity ~ prices + prices_c,
     data = .) %>%
  summary()

quantity_model

quantity_elasticity <- quantity_model$coefficients[2] * mean(sim_data[[5]]$prices) / mean(sim_data[[5]]$quantity)

#### Comparing Elasticities ####

data.frame(true = sim_data[[4]], 
           ols_binary = ols_elasticity,
           logit_binary = logit_elasticity,
           ols_quantity = quantity_elasticity)


#### Comparing Estimated Demand Curves, and the Implied Profit Functions & Optimal Prices ******WITH COMPETITOR**** ####

demand_curves <- as.data.frame(expand.grid(seq(from = 0, to = 5, by = 0.01),
                                           seq(from = 0, to = 5, by = 0.01))
                              ) %>%
  dplyr::rename(prices=Var1,prices_c=Var2) %>%
  mutate(quantity = quantity_model$coefficients[1] + quantity_model$coefficients[2]*prices + quantity_model$coefficients[3]*prices_c
               ) 

demand_quantity <- demand_curves %>%
    spread(prices_c,quantity) %>%
    select(-prices) %>%
    as.matrix()

demand_prices <- data.matrix(seq(from = 1, to = 5, by = 0.01))
demand_prices_c <- data.matrix(seq(from = 1, to = 5, by = 0.01))

demand_plot <- plot_ly() %>%
                add_trace(
                    x=seq(from = 1, to = 5, by = 0.01),
                    y=seq(from = 1, to = 5, by = 0.01),
                    z=demand_quantity,
                    name='ZED',
                    type='surface',
                    colorbar=list(title='Quantity'),
                    contours=list(
                     
                        x=list(show=TRUE, color='#c4bebe'),
                        y=list(show=TRUE, color='#c4bebe')
                        
                      
                    )
                  ) %>%
          layout(
            scene = list(
              xaxis = list(title='Competitor Price'),
              yaxis = list(title='Own Price'),
              zaxis = list(title='Quantity')
            )
          )
          


demand_plot




profit_curves <- demand_curves %>%
  mutate(profit = quantity * prices)

profit_profit <- profit_curves %>%
  select(-quantity) %>%
  spread(prices_c,profit) %>%
  select(-prices) %>%
  as.matrix()

profit_prices <- data.matrix(seq(from = 0, to = 5, by = 0.01))
profit_prices_c <- data.matrix(seq(from = 0, to = 5, by = 0.01))


profit_maxing_prices <- profit_curves %>%
  select(-quantity) %>%
  group_by(prices_c) %>%
  summarise(max_profit=max(profit), pmax_price=prices[which.max(profit)]) #%>%
 # mutate(id=1:n()) %>%
#  spread(prices_c,max_profit) %>%
 # select(-pmax_price,-id) %>%
#  as.matrix()



profit_plot <- plot_ly() %>%
  add_trace(
    x=seq(from = 0, to = 5, by = 0.01),
    y=seq(from = 0, to = 5, by = 0.01),
    z=profit_profit,
    name='ZED',
    type='surface',
    colorbar=list(title='Profit'),
    contours=list(
      x=list(show=TRUE, color='#c4bebe'),
      y=list(show=TRUE, color='#c4bebe')
    )
  ) %>%
  layout(
    scene = list(
      xaxis = list(title='Competitor Price'),
      yaxis = list(title='Own Price'),
      zaxis = list(title='Profit',
      camera = list(
        center = list(
          x = 10,
          y = 0,
          z = 0
        )
      ))
    )) %>%
     # add_surface(z=~profit_maxing_prices)
  add_trace(
        data=profit_maxing_prices,
        type='scatter3d',
        mode='lines',
       # surfacecolor='grey',
        x=~prices_c,
        y=~pmax_price,
       z=~max_profit,
       opacity=1,
       line=list(width=6,color='5')
       
      )
  



profit_plot


#### Data output ####

day_transactions <- sim_data[[5]] %>%
  inner_join(profit_maxing_prices)


write.csv(day_transactions,"day_transactions.csv",sep=",",dec=".")
drive_upload(
  "day_transactions.csv",
  path="pricing_analytics/day_transactions",
  type="spreadsheet",
  overwrite = TRUE
)

write.csv(profit_maxing_prices,"profit_maxing_prices.csv",sep=",",dec=".")
drive_upload(
  "profit_maxing_prices.csv",
  path="pricing_analytics/profit_maxing_prices",
  type="spreadsheet",
  overwrite=TRUE
)

