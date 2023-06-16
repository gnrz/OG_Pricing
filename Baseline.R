

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
## Prices are drawn randomly from uniform distribution, Range is specified 
## Multiple sites, identical except for pricing choices, 
## 1 competitor per site, 
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
                        high_price =c(2,6,6),
                        low_price  =c(1,2,0.1), 
                        price_granularity=1,
                        sites=3,
                        coffeemachine=c(0,0,1))



ggplot(sim_data[[5]])+geom_point(mapping=aes(y=quantity,x=prices, group=as.character(site_index), color=as.character(site_index)))

total_data <- sim_data[[5]] %>%
  group_by(site_index) %>%
  summarise(total_sales = sum(quantity), total_profit = sum(quantity*prices),mean_price=mean(prices))


transactions <-       ggplot(sim_data[[5]],mapping=aes()) + geom_line(aes(x=day_index,y=quantity, group=as.character(site_index), color=as.character(site_index))) +
                      labs(color="Site Index",title="Quantity sold per day", x="Day", y="Quantity") + theme_minimal()

mean_price <-         ggplot(total_data,mapping=aes(x=site_index,y=mean_price)) + geom_col() +
                      labs(title="Mean Price", x="Site", y="Price") + theme_minimal() + geom_text(aes(y=mean_price,label=round(mean_price,2)),vjust = 1.5, colour = "white")

prices_plot <-        ggplot(sim_data[[1]],mapping=aes()) + geom_line(aes(x=day_index, y=prices, group=as.character(site_index), color=as.character(site_index))) +
                      labs(color="Site Index",title="Daily prices", x="Day", y="Price") + theme_minimal()

total_sales <-      ggplot(total_data,mapping=aes(x=site_index,y=total_sales)) + geom_col() +
                     labs(title="Total Quantity Sold", x="Site", y="Quantity") + theme_minimal() + geom_text(aes(y=total_sales,label=total_sales),vjust = 1.5, colour = "white")

profit_plot <-      ggplot(sim_data[[5]],mapping=aes()) + geom_line(aes(x=day_index,y=quantity*prices, group=as.character(site_index), color=as.character(site_index))) +
                    labs(color="Site Index",title="Profit per day", x="Day", y="Profit") + theme_minimal()

total_profit <-      ggplot(total_data,mapping=aes(x=site_index,y=total_profit)) + geom_col() +
                    labs(title="Total Profit", x="Site", y="Profit") + theme_minimal() + geom_text(aes(y=total_profit,label=total_profit),vjust = 1.5, colour = "white")

print(prices_plot / mean_price /  transactions / total_sales/ profit_plot / total_profit)
          

## Model 1: Observe Purchase Binary, OLS Model

ols <- sim_data[[3]] %>% 
  lm(purchase_binary ~ prices + prices_c + coffeemachine,
     data = .) %>%
  summary()

ols

ols_elasticity <- ols$coefficients[2] * mean(sim_data[[3]]$prices) / mean(sim_data[[3]]$purchase_binary)

## Model 2: Observe Purchase Binary, Logit Model

logit <- sim_data[[3]] %>%
  glm(formula = purchase_binary ~ prices + prices_c + coffeemachine,
      family = binomial(link = 'logit'),
      data = .) %>%
  summary()

logit

logit_elasticity <- logit$coefficients[2] * (1 - mean(sim_data[[3]]$purchase_binary))*mean(sim_data[[3]]$prices)

## Model 3: Observe Total Quantity

quantity_model <- sim_data[[5]] %>% 
  lm(quantity ~ prices + prices_c + coffeemachine,
     data = .) %>%
  summary()

quantity_model

quantity_model_coeffs <- data.frame(quantity_model$coefficients) %>%
                          slice(2:n()) %>%
                          mutate(Variables = rownames(.)) %>%
                          ggplot(mapping=aes())+geom_col(aes(y=Estimate, x=Variables)) +
                          theme_minimal() + 
                          labs(title="Impact of different variables on quantity sold",y="Sales impact",subtitle="For prices, impact is based on single unit change in price")

quantity_model_coeffs

quantity_elasticity <- quantity_model$coefficients[2] * mean(sim_data[[5]]$prices) / mean(sim_data[[5]]$quantity)

#### Comparing Elasticities ####

data.frame(true = sim_data[[4]], 
           ols_binary = ols_elasticity,
           logit_binary = logit_elasticity,
           ols_quantity = quantity_elasticity)


#### Comparing Estimated Demand Curves, and the Implied Profit Functions & Optimal Prices ******WITH COMPETITOR**** ####

demand_curves <- as.data.frame(expand.grid(seq(from = 0, to = 5, by = 0.01),
                                           seq(from = 0, to = 5, by = 0.01),
                                           c(0,1))
                              ) %>%
  dplyr::rename(prices=Var1,prices_c=Var2,coffeemachine=Var3) %>%
  mutate(quantity = quantity_model$coefficients[1] + 
                    quantity_model$coefficients[2]*prices + 
                    quantity_model$coefficients[3]*prices_c + 
                    quantity_model$coefficients[4]*coffeemachine
               ) 

demand_quantity=c()

demand_quantity[[1]] <- demand_curves %>%
    filter(coffeemachine==0) %>%
    spread(prices_c,quantity) %>%
    select(-prices) %>%
    as.matrix()

demand_quantity[[2]] <- demand_curves %>%
  filter(coffeemachine==1) %>%
  spread(prices_c,quantity) %>%
  select(-prices) %>%
  as.matrix()


demand_prices <- data.matrix(seq(from = 1, to = 5, by = 0.01))
demand_prices_c <- data.matrix(seq(from = 1, to = 5, by = 0.01))

demand_plot=c()

demand_plot[[1]] <- plot_ly() %>%
                add_trace(
                    x=seq(from = 1, to = 5, by = 0.01),
                    y=seq(from = 1, to = 5, by = 0.01),
                    z=demand_quantity[[1]],
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
            ),
            title='Demand curve without coffee machine'
          )

demand_plot[[2]] <- plot_ly() %>%
  add_trace(
    x=seq(from = 1, to = 5, by = 0.01),
    y=seq(from = 1, to = 5, by = 0.01),
    z=demand_quantity[[2]],
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
    ),
    title='Demand curve with coffee machine'
  )
          


demand_plot[[1]]
demand_plot[[2]]

demand_withwithoutcoffee <- demand_curves %>%
  filter(prices_c==3) %>% # hold competitor prices constant (assuming no interaction)
  select(-prices_c) %>%
  ggplot(mapping=aes()) + geom_line(aes(x=quantity, y=prices,group=as.character(coffeemachine), color=as.character(coffeemachine))) +
    theme_minimal() +
    labs(title="Demand with and without a coffee machine",x="Quantity Sold", y="Own Price",colour="Coffee machine")


demand_withwithoutcoffee


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

