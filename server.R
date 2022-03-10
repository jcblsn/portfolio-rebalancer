library(shiny)
library(tidyverse)
library(lubridate)

rebalance_server <- function(input, output) {
  
  dat <- reactive({
    req(input$file1)
    tryCatch(
      {
        df <- utils::read.csv(input$file1$datapath,
                              header = T,
                              sep = input$sep,
                              skip = input$nskip
        )
      },
      error = function(e) {stop(safeError(e))}
    )
    
    if(!input$view_og){
      df <- df %>% 
        mutate(Symbol = ifelse(Symbol=="Cash & Cash Investments", "Cash", Symbol)) %>% 
        janitor::clean_names() %>% 
        select(symbol,description,quantity,price,market_value,pct_of_acct = x_of_account) %>% 
        mutate_at(
          .vars = c("quantity","price","market_value","pct_of_acct"), 
          .funs = function(x) {x %>% str_remove_all(., "\\$|,|%|\\+") %>% as.numeric()}
        ) %>% 
        mutate(pct_of_acct = pct_of_acct) %>% 
        filter(symbol != "Account Total", market_value>0) %>% 
        arrange(desc(market_value))
      
    }
    
    return(df)
  })
  
  output$datetime <- renderText({
    
    unparsed <- str_sub(basename(input$file1$name), start = -21, end = -5)
    d <- str_sub(unparsed,start = 1, end = 10)
    t <- str_c(
      str_sub(unparsed,start = -6, end = -5),":",
      str_sub(unparsed,start = -4, end = -3),":",
      str_sub(unparsed,start = -2, end = -1)
    )
    datetime <- str_c(d," at ", t)
    return(datetime)
  })
  
  output$contents <- renderTable({
    return(dat())
  })
  
  dyn_start_props <- reactive({
    df <- dat()
    p <- df$pct_of_acct
    num <- length(dyn_vals$tlist) + length(unique(df$symbol))
    
    if(num>length(p)){
      p <- c(p, rep(0, num-length(p)))
    }
    return(p)
  }) 
  
  dyn_vals <- reactiveValues()
  observe({
    df <- dat()
    if(input$add_ticker > 0){
      dyn_vals$tlist <- c(isolate(dyn_vals$tlist), isolate(input$input_ticker))
    } else {
      dyn_vals$tlist <- df$symbol
    }
  })
  
  output$list<-renderPrint({
    dyn_vals$tlist
  })
  
  output$sliders <- renderUI({
    tickers <- dyn_vals$tlist
    start_props <- dyn_start_props()
    
    lapply(1:length(tickers), function(i) {
      sliderInput(
        inputId = paste0("symbol_", i),
        label = paste(tickers[i]),
        value = start_props[i],
        min = 0, max = 100, step = 1
      )
    })
  })
  
  allocation <- reactive({
    n_assets <- length(dyn_vals$tlist)
    allocation_props <- NULL
    
    for(i in 1:n_assets){
      symbol_name <- paste0("symbol_",i)
      allocation_props <- c(allocation_props,input[[symbol_name]])
    }
    
    return(allocation_props)
  })
  
  output$plot_comparison <- renderPlot({
    new_pcts <- allocation()
    
    validate(
      need(sum(new_pcts)== 100, str_c("Percentages currently sum to ",round(sum(new_pcts),0),
                                      "%. Please adjust so that percentages sum to 100%"))
    )
    
    old <- dat() %>% mutate(v = "Current")
    new <- data.frame(
      symbol = dyn_vals$tlist,
      pct_of_acct = new_pcts
    ) %>% 
      mutate(
        market_value = sum(old$market_value) * pct_of_acct,
        v = "Proposed"
      )
    
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    
    plot_dat <- 
      old %>% 
      bind_rows(
        new
      )
    
    asset_colors <- gg_color_hue(nrow(plot_dat))
    
    plot_dat %>% 
      ggplot(
        aes(
          x = v,
          y = market_value,
          fill = symbol)
      ) +
      geom_bar(
        stat = "identity",
        position="fill",
        width = 1/2
      ) + 
      geom_text(
        aes(
          label = ifelse(
            pct_of_acct>0,
            str_c(pct_of_acct %>% round(1), "%"),
            ""
          )
        ),
        position = position_fill(vjust = 0.5),
        color = "black",
        stat = "identity",
        size = 12 * 5/14
      ) +
      # scale_fill_manual(
      #   values = c(cols_domestic, cols_intl, cols_cash, cols_other)
      # ) +
      # ggtitle(str_c("Data as of ",date_export)) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
  })
  
  output$report_allocation <- renderTable({
    new <- tibble(
      Ticker = dyn_vals$tlist,
      `New allocation` = str_c(allocation(),"%")
    )
    
    return(new)
  })
  
  output$trades <- renderTable({
    old <- dat() %>% mutate(v = "old")
    new <- data.frame(
      symbol = dyn_vals$tlist,
      pct_of_acct = allocation()
    )
    
    
    
    if(input$threshold_option){
      adjusted <- 
        old %>% 
        select(-v) %>% 
        full_join(
          new %>% select(symbol, new_pct_of_acct = pct_of_acct), by = "symbol"
        ) %>% 
        mutate(
          market_value = ifelse(is.na(market_value), 0, market_value),
          pct_of_acct = ifelse(is.na(pct_of_acct), 0, pct_of_acct),
          quantity = ifelse(is.na(quantity), 0, quantity),
          price = ifelse(is.na(price), 0, price),
          
          new_value = (new_pct_of_acct/100)*sum(market_value),
          diff_value = market_value - new_value,
          diff_prop = new_pct_of_acct - pct_of_acct,
          recommendation = ifelse(abs(diff_value) > input$threshold_dollar, "act", "wait"),
          action = case_when(
            recommendation == "act" & diff_value>0 ~ "sell",
            recommendation == "act" & diff_value<0 ~ "buy",
            T ~ "wait"
          )
        )
      
      rec_sell_total <- adjusted %>% filter(action == "sell") %>% pull(diff_value) %>% sum()
      
      readjusted <- 
        adjusted %>% 
        mutate(
          adj_sell = ifelse(diff_value<0, 
                            (abs(adjusted$diff_value)/sum(abs(adjusted$diff_value[adjusted$action=="buy"])))*rec_sell_total,
                            diff_value
          ),
          action = str_to_title(action),
          Recommendation = ifelse(action != "Wait", str_c(action, " $", round(adj_sell,2)), "No action")
        )
      
      rec_output <- 
        readjusted %>% 
        mutate_if(is.numeric, function(x) round(x, 2)) %>% 
        transmute(Ticker = symbol,
                  Description = description,
                  `Quantity held` = quantity,
                  Price = price,
                  `Current market value` = market_value,
                  `Current % of account` = pct_of_acct,
                  `Proposed % of account` = new_pct_of_acct,
                  `Difference ($)` = diff_value,
                  Recommendation)
      
      return(rec_output)
    } else {
      adjusted <- 
        old %>% 
        select(-v) %>% 
        full_join(
          new %>% select(symbol, new_pct_of_acct = pct_of_acct), by = "symbol"
        ) %>% 
        mutate(
          market_value = ifelse(is.na(market_value), 0, market_value),
          pct_of_acct = ifelse(is.na(pct_of_acct), 0, pct_of_acct),
          quantity = ifelse(is.na(quantity), 0, quantity),
          price = ifelse(is.na(price), 0, price),
          
          new_value = (new_pct_of_acct/100)*sum(market_value),
          diff_value = market_value - new_value,
          diff_prop = new_pct_of_acct - pct_of_acct,
          action = case_when(
            diff_value>0 ~ "sell",
            diff_value<0 ~ "buy",
            T ~ "wait"
          ) %>% str_to_title(),
          Recommendation = ifelse(action != "Wait", str_c(action, " $", round(abs(diff_value),2)), "No action")
        )
      
      rec_output <- 
        adjusted %>% 
        mutate_if(is.numeric, function(x) round(x, 2)) %>% 
        transmute(Ticker = symbol,
                  Description = description,
                  Quantity = quantity,
                  Price = price,
                  `Current market value` = market_value,
                  `Current % of account` = pct_of_acct,
                  `Proposed % of account` = new_pct_of_acct,
                  `Difference ($)` = diff_value,
                  `Difference (%)` = diff_prop,
                  Recommendation)
      
      return(rec_output)
    }
    
  })
  
  output$image <- renderImage({
    list(src = "schwab_portfolio_export.png")
  }, deleteFile = F)
  
} 