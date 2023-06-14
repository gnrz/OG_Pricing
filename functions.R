### Useful Functions ###
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

create_data <- function(...,T=90,n_per_day=1000,mean_utility=0.5,price_coefficient=-0.5,high_price=2,low_price=0.4, price_granularity=1) {
  
  n <- T * n_per_day
  day_index <- sort(rep(seq(1,T), n_per_day))
  
  output <- list()
  
  prices <- data.frame(day_index = sort(rep(seq(1,T))),
                       prices = round(runif(T,low_price,high_price),digits=price_granularity))
  
  output[[1]] <- prices
  
  competitor_prices <- data.frame(day_index = sort(rep(seq(1,T))),
                                  prices_c = round(runif(T,low_price,high_price),digits=price_granularity))
  
  output[[2]] <- competitor_prices
  
  df <- data.frame(day_index = day_index,
                   simulatedErrorProduct = rnorm(n = n),       # non_price utility from own product
                   simulatedErrorProduct_c = rnorm(n = n)) %>% # non_price utility from competitor
    #mutate(prices = c(rep(low_price, n / 2), rep(high_price, n / 2)),
    left_join(prices)%>%
    left_join(competitor_prices) %>%
    mutate(utility_product = mean_utility + simulatedErrorProduct   + price_coefficient * prices,
           utility_outside = mean_utility + simulatedErrorProduct_c + price_coefficient * prices_c,
           purchase_binary = (utility_product > utility_outside))
  
  output[[3]] <- df
  
  true_elasticity <- (
    pnorm(mean_utility + price_coefficient * high_price) - 
      pnorm(mean_utility + price_coefficient * low_price)
  )   / (high_price - low_price) * mean(df$prices)/mean(df$purchase_binary)
  
  output[[4]] <- true_elasticity
  
  ## Summary stats about data
  df_by_day <- df %>%
    group_by(day_index) %>%
    summarise(quantity = sum(purchase_binary), 
              prices = mean(prices),
              prices_c = mean(prices_c))
  
  output[[5]] <- df_by_day
  
  return(output)
  
}