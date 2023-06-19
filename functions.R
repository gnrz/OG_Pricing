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

create_data <- function(...,
                        T=90,
                        n_per_day=1000,
                        mean_utility=0.5,
                        price_coefficient=-0.5,
                        coffeemachine_coefficient=0.3,
                       # high_price=c(2,6,6),
                       # low_price=c(0.4,1,0.1), 
                        sim_price_limits = data.frame(),
                        price_granularity=1,
                        sites=3,
                        coffeemachine = c(),
                        profit_maxing=c(),
                        pmax_prices = data.frame(prices_c=c(0),pmax_price=c(0)))
      {
  
  n <- T * n_per_day * sites
  #print(n)
  day_index <- sort(rep(seq(1,T), n_per_day))
  
  site_price_limits_df <- sim_price_limits %>%
                        data.frame() %>%
                        mutate(site_index = rep(seq(0,sites))) %>% # 0 site is competitors
                        rename(high_price =X1, low_price = X2)
  
  output <- list()
  
  prices <- data.frame(
                  expand.grid(
                      site_index=rep(seq(1,sites)),
                      day_index=rep(seq(1,T))
                      )
                  ) %>%
            left_join(site_price_limits_df) %>%
            #rename(site_index=Var1, day_index=Var2) %>%
            mutate(
                  prices =  round(runif(nrow(.),low_price,high_price),digits=price_granularity)
                    
              )
  
  output[[1]] <- prices
  
  competitor_prices <- data.frame(
                      expand.grid(
                        site_index=rep(seq(1,sites)),
                        day_index=rep(seq(1,T)),
                        site_index_comp = 0
                        )
                      ) %>% 
                      inner_join(site_price_limits_df,by=c("site_index_comp"="site_index")) %>%
                      mutate(
                          prices_c = round(runif(nrow(.),low_price,high_price),digits=price_granularity)
                        ) %>%
                      select(-high_price,-low_price)
                      
  output[[2]] <- competitor_prices
  
  site_facilities <- data.frame(
                          site_index=rep(seq(1,sites)),
                          coffeemachine = coffeemachine,
                          profit_maxing = profit_maxing
                    )
  
  df <- data.frame(
                  expand.grid(site_index=rep(seq(1,sites)), day_index=day_index), 
                   simulatedErrorProduct = rnorm(n = n),       # non_price utility from own product
                   simulatedErrorProduct_c = rnorm(n = n)) %>% # non_price utility from competitor
    #mutate(prices = c(rep(low_price, n / 2), rep(high_price, n / 2)),
        left_join(prices)%>%
        left_join(competitor_prices) %>%
        left_join(site_facilities) %>%
        mutate(prices_c_join = as.character(round(prices_c,2))) %>%
        left_join(pmax_prices,by=c("prices_c_join"="prices_c_join"), relationship="many-to-one",suffix = c(".x", ".y"),keep=TRUE) %>%
        mutate(prices_c = prices_c.x) %>%
        mutate(price_actual = 
                  case_when(
                    profit_maxing == 1 ~ pmax_price,
                    profit_maxing == 0 ~ prices
                  )) %>%
        mutate(utility_product = mean_utility + simulatedErrorProduct   + price_coefficient * price_actual + coffeemachine_coefficient * coffeemachine,
               utility_outside = mean_utility + simulatedErrorProduct_c + price_coefficient * prices_c,
               purchase_binary = (utility_product > utility_outside))
  
  output[[3]] <- df
  
  true_elasticity <- 0
  #(
  #  pnorm(mean_utility + price_coefficient * high_price) - 
  #    pnorm(mean_utility + price_coefficient * low_price)
  #)   / (high_price - low_price) * mean(df$prices)/mean(df$purchase_binary)
  
  output[[4]] <- true_elasticity
  
  ## Summary stats about data
  df_by_day <- df %>%
    group_by(day_index,site_index) %>%
    summarise(quantity = sum(purchase_binary), 
              prices = mean(price_actual),
              prices_c = mean(prices_c),
              coffeemachine = mean(coffeemachine)
              )
  
  output[[5]] <- df_by_day
  
  return(output)
  
}
