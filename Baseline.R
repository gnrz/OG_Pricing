

# Preliminaries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)


# Multiplot, from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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

T <- 90
n_per_day <- 1000
n <- T * n_per_day
day_index <- sort(rep(seq(1,T), n_per_day))

mean_utility <- 0.5
price_coefficient <- -0.5
utility_outside <- 0

high_price = 2
low_price = 0.4
price_granularity = 1

prices <- data.frame(day_index = sort(rep(seq(1,T))),
                     prices = round(runif(T,low_price,high_price),digits=price_granularity))

competitor_prices <- data.frame(day_index = sort(rep(seq(1,T))),
                                prices_c = round(runif(T,low_price,high_price),digits=price_granularity))

df <- data.frame(day_index = day_index,
                  simulatedErrorProduct = rnorm(n = n),       # non_price utility from own product
                  simulatedErrorProduct_c = rnorm(n = n)) %>% # non_price utility from competitor
                  #mutate(prices = c(rep(low_price, n / 2), rep(high_price, n / 2)),
                  left_join(prices)%>%
                  left_join(competitor_prices) %>%
                  mutate(utility_product = mean_utility + simulatedErrorProduct   + price_coefficient * prices,
                         utility_outside = mean_utility + simulatedErrorProduct_c + price_coefficient * prices_c,
                         purchase_binary = (utility_product > utility_outside))
                  

true_elasticity <- (
                pnorm(mean_utility + price_coefficient * high_price) - 
                pnorm(mean_utility + price_coefficient * low_price)
                )   / (high_price - low_price) * mean(df$prices)/mean(df$purchase_binary)
                                                  
## Summary stats about data
df_by_day <- df %>%
                        group_by(day_index) %>%
                        summarise(quantity = sum(purchase_binary), 
                                  prices = mean(prices),
                                  prices_c = mean(prices_c))

ggplot(df_by_day)+geom_point(mapping=aes(y=quantity,x=prices))


transactions <-       ggplot(df_by_day,mapping=aes()) + geom_line(aes(day_index,quantity))
prices_plot <-        ggplot(prices,mapping=aes()) + geom_line(aes(day_index, prices))

print(transactions /  prices_plot)
          

## Model 1: Observe Purchase Binary, OLS Model

ols <- df %>% 
  lm(purchase_binary ~ prices + prices_c,
     data = .) %>%
  summary()

ols

ols_elasticity <- ols$coefficients[2] * mean(df$prices) / mean(df$purchase_binary)

## Model 2: Observe Purchase Binary, Logit Model

logit <- df %>%
  glm(formula = purchase_binary ~ prices + prices_c,
      family = binomial(link = 'logit'),
      data = .) %>%
  summary()

logit

logit_elasticity <- logit$coefficients[2] * (1 - mean(df$purchase_binary))*mean(df$prices)

## Model 3: Observe Total Quantity

quantity_model <- df_by_day %>% 
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

