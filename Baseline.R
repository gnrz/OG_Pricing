

# Preliminaries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)

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
                        price_granularity=1)



ggplot(sim_data[[5]])+geom_point(mapping=aes(y=quantity,x=prices))


transactions <-       ggplot(sim_data[[5]],mapping=aes()) + geom_line(aes(day_index,quantity))
prices_plot <-        ggplot(sim_data[[1]],mapping=aes()) + geom_line(aes(day_index, prices))

print(transactions /  prices_plot)
          

## Model 1: Observe Purchase Binary, OLS Model

ols <- sim_data[[3]] %>% 
  lm(purchase_binary ~ prices + prices_c,
     data = .) %>%
  summary()

ols

ols_elasticity <- ols$coefficients[2] * mean(df$prices) / mean(df$purchase_binary)

## Model 2: Observe Purchase Binary, Logit Model

logit <- sim_data[[3]] %>%
  glm(formula = purchase_binary ~ prices + prices_c,
      family = binomial(link = 'logit'),
      data = .) %>%
  summary()

logit

logit_elasticity <- logit$coefficients[2] * (1 - mean(df$purchase_binary))*mean(df$prices)

## Model 3: Observe Total Quantity

quantity_model <- sim_data[[5]] %>% 
  lm(quantity ~ prices + prices_c,
     data = .) %>%
  summary()

quantity_model

quantity_elasticity <- quantity_model$coefficients[2] * mean(df_by_day$prices) / mean(df_by_day$quantity)

#### Comparing Elasticities ####

data.frame(true = true_elasticity, 
           ols_binary = ols_elasticity,
           logit_binary = logit_elasticity,
           ols_quantity = quantity_elasticity)


#### Comparing Estimated Demand Curves, and the Implied Profit Functions & Optimal Prices ####

demand_curves <- data.frame(prices = seq(from = 0, to = 3, by = 0.01))

demand_curves %<>%
  mutate(ols = n *
           (ols$coefficients[1] + ols$coefficients[2] * prices),
         logit = n *
           (exp(logit$coefficients[1] + logit$coefficients[2] * prices) / 
              (1 + exp(logit$coefficients[1] + logit$coefficients[2] * prices)))) %>%
          #total = quantity_model$coefficients[1]
  gather(model, quantities, -prices)


demand_plot <- ggplot(data = demand_curves, aes(x = quantities, y = prices, color = model)) + 
  geom_line() + 
  labs(x = 'Quantity', y = 'Price ($)', title = 'Panel A: Estimated Demand Curves') + 
  geom_hline(yintercept = c(low_price, high_price), colour = "#2A73CC", linetype = "longdash") + 
  theme(plot.title = element_text(face = "bold"))

profit_curves <- demand_curves %>%
  mutate(profit = quantities * prices)

profit_curves %>%
  group_by(model) %>%
  filter(profit == max(profit))

profit_plot <- ggplot(data = profit_curves, aes(x = prices, y = profit, color = model)) + 
  geom_line() + 
  labs(x = 'Price ($)', y = 'Profit ($)', title = 'Panel B: Implied Profit Functions') + 
  geom_vline(xintercept = c(low_price, high_price), colour = "#2A73CC", linetype = "longdash") + 
  theme(plot.title = element_text(face = "bold"))

multiplot(demand_plot, profit_plot, cols = 2)

options(repr.plot.width = 10, repr.plot.height = 6)

#### Comparing Estimated Demand Curves, and the Implied Profit Functions & Optimal Prices ******WITH COMPETITOR**** ####

demand_curves <- as.data.frame(expand.grid(seq(from = 1, to = 5, by = 0.01),
                                           seq(from = 1, to = 5, by = 0.01))
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

profit_prices <- data.matrix(seq(from = 1, to = 5, by = 0.01))
profit_prices_c <- data.matrix(seq(from = 1, to = 5, by = 0.01))


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
    x=seq(from = 1, to = 5, by = 0.01),
    y=seq(from = 1, to = 5, by = 0.01),
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
      zaxis = list(title='Profit')
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

