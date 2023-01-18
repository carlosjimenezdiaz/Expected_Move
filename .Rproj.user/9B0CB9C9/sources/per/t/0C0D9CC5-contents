# Global Variables
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Getting the functions
source(file = "00_scripts/Libraries.R")

# Loading the functions
libraries()

# Starting the Timer
tictoc::tic()

# Local Variables
Ticker_CBOE     <- "SPY"
Expiration_Date <- "2023-02-10"

# Getting the current price
current_price <- getQuote(Ticker_CBOE)$Last

# Getting the Option Chain
tryCatch(
  expr = {

    # Downloading data from CBOE website
    db_Option_Chain <- str_glue("https://cdn.cboe.com/api/global/delayed_quotes/options/{Ticker_CBOE}.json") %>%
      read_json(simplifyVector = TRUE) %>%
      pluck("data", "options") %>%
      as.data.frame() %>%  
      dplyr::mutate(expiry    = str_sub(option, -15, -10) %>% as.Date(format = "%y%m%d"),
                    opt_type  = str_sub(option, -9, -9),
                    strike    = paste0(str_sub(str_sub(option, -8, -1), -8, -4),".",str_sub(str_sub(option, -8, -1), -3, -1)) %>% as.numeric(),
                    days2Exp  = as.Date(expiry) - Sys.Date(),
                    Mid_price = round((bid + ask)/2, 2))

  },
  error = function(e){ 
    print(str_glue("ERROR Getting info from {Ticker_CBOE}"))
  },
  warning = function(w){ 
    print(str_glue("ERROR Getting info from {Ticker_CBOE}"))
  }
)

# Expected Move (either direction)
Data_ATM <- db_Option_Chain %>%
  dplyr::mutate(Flag = case_when(strike < current_price ~ 0, TRUE ~ 1)) %>%
  dplyr::filter(expiry == Expiration_Date %>% as.Date() & Flag == 0) %>%
  tail(n = 2)

Pricing_BID  <- Data_ATM$bid %>% sum()
Pricing_ASK  <- Data_ATM$ask %>% sum()
Pricing_Last <- Data_ATM$last_trade_price %>% sum()

# Selecting the one with data
if(Pricing_BID != 0){
  EM <- Pricing_BID*1.25
}else if(Pricing_ASK != 0){
  EM <- Pricing_ASK*1.25
}else{
  EM <- Pricing_Last*1.25
}

# Generating the Chart
tq_get(Ticker_CBOE,
       from = Sys.Date() - 7,
       to   = Sys.Date()) %>%
  ggplot(aes(x = date, y = adjusted)) + 
  theme_minimal() +
  geom_line(linewidth = 1.3) +
  scale_x_date(limits = c(as.Date(Sys.Date() - 7), as.Date(Expiration_Date) + 3)) +
  geom_hline(yintercept = current_price + EM, linetype = "dotted", color = "steelblue", linewidth = 0.9) +
  geom_hline(yintercept = current_price - EM, linetype = "dotted", color = "steelblue", linewidth = 0.9) +
  geom_vline(xintercept = as.Date(Expiration_Date), linetype = "dotted", color = "red", linewidth = 0.9) +
  labs(title    = "Expected Move - Implied by what is happening in the Option Market",
       subtitle = str_glue("Underlying {Ticker_CBOE} - Expected Move Date: {Expiration_Date}."),  
       caption  = "Data Source: CBOE / Own calculations.",
       x = "Date",
       y = "Underlying Price") +
  theme(legend.title = element_blank()) +
  annotate(geom  = "text", 
           x     = as.Date(Expiration_Date) - 0.5, 
           y     = current_price + EM + 1, 
           label = current_price + EM, 
           color = "red") +
  annotate(geom  = "text", 
           x     = as.Date(Expiration_Date) - 0.5, 
           y     = current_price - EM - 1, 
           label = current_price - EM, 
           color = "red") +
  annotate(geom  = "text", 
           x     = as.Date(Expiration_Date) + 1, 
           y     = current_price, 
           label = as.Date(Expiration_Date), 
           color = "red")

# Stoping the Timer
tictoc::toc()
