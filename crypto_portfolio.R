#Libraries
library(data.table)
library(httr)
library(jsonlite)
library(dplyr)
library(binancer)
library(plotly)
library(geckor)
library(formattable)
library(reticulate)
library(DT)
library(quantmod)
library(priceR)

# Portfolio value and PnL -------------------------------------------------
#Extract data (DeBank)
res <- fromJSON(url("https://openapi.debank.com/v1/user/total_balance?id=0x47aAb99A731aF6AC24Af9E420FD6DacA5Cf29d69"))
doll_value_DeBank <- sum(res[["chain_list"]][["usd_value"]])

#Extract data (Binance)
api_key <- "K1FeJBpBR46DixKqWR1sil6xbIVDfWykJvO9XLiWdPsLmO7XtjAJOvrHxu5dyijR"
secret_key <- "MisxMt9doUE1nahMeRdK5oOwNnnqhLCUeEG9jYq6pRJmoxvVVdoAikV5Wcj6jtgg"
binance_credentials(api_key, secret_key)
doll_value_Binance <- sum(na.omit(binance_balances(usdt = TRUE))$usd)
#Resulting value
load("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/portfolio_value.RData")
doll_value <- round(doll_value_DeBank + doll_value_Binance, 2)
portfolio_value <- rbind(portfolio_value,data.frame(Date = Sys.Date(), Value = doll_value))
#Save results
save(portfolio_value, file = "/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/portfolio_value.RData")
#Portfolio value in RUB
rub_usd <- historical_exchange_rates(from = "USD", to = "RUB", start_date = portfolio_value$Date[1], end_date = Sys.Date())
rub_usd <- rub_usd[rub_usd$date %in% portfolio_value$Date,]
portfolio_value$Value_RUB <- round(portfolio_value$Value*merge(portfolio_value, rub_usd, by.x="Date", by.y="date", all.x = TRUE)$one_USD_equivalent_to_x_RUB,2)
rub_value <- round(doll_value*rub_usd$one_USD_equivalent_to_x_RUB[nrow(rub_usd)]/1000,2)
#PnL plot
PnL <- portfolio_value
PnL$change <- c(0, diff(PnL$Value))
PnL$change_RUB <- c(0, diff(PnL$Value_RUB))
PnL <- aggregate(PnL[,c("change", "change_RUB")], by = list(PnL$Date), FUN = sum)
colnames(PnL) <- c("Date","Change", "Change_RUB")
PnL_plot <- plot_ly(PnL, x = ~ Date, y = ~ Change, 
        marker = list(color = ifelse(PnL$Change>0,"#96d800","#b7241b")), type="bar")
PnL_RUB_plot <- plot_ly(PnL, x = ~ Date, y = ~ Change_RUB, 
        marker = list(color = ifelse(PnL$Change_RUB>0,"#96d800","#b7241b")), type="bar")

#Portfolio Change
portfolio_prev_day_value <- portfolio_value[portfolio_value$Date==unique(portfolio_value$Date)[length(unique(portfolio_value$Date)-1)],]$Value[1]
portfolio_change_perc <- round(PnL$Change[nrow(PnL)]/portfolio_prev_day_value*100,2)
portfolio_change_perc <- ifelse(portfolio_change_perc>0,paste0("+",portfolio_change_perc,"%"),paste0(portfolio_change_perc,"%"))
#rm
rm(res, api_key, doll_value_Binance, doll_value_DeBank, secret_key, portfolio_value, portfolio_prev_day_value)
# Distribution of tokens --------------------------------------------------
#Coins from the wallet
res <- fromJSON(url("https://openapi.debank.com/v1/user/token_list?id=0x47aAb99A731aF6AC24Af9E420FD6DacA5Cf29d69&is_all=true"))
wallet_coins <- res[res$is_verified==TRUE,-c(14,16,17)]
wallet_coins$amount_USD <- wallet_coins$price*wallet_coins$amount
wallet_coins <- subset(wallet_coins, select = -c(amount,price,protocol_id))
#Coins from the protocols
res <- fromJSON(url("https://openapi.debank.com/v1/user/complex_protocol_list?id=0x47aAb99A731aF6AC24Af9E420FD6DacA5Cf29d69&is_all=true"))

extract_protocoles <- function(protocol_list) {
  protocols_table <- data.frame()
  for (i in 1:length(protocol_list$portfolio_item_list))
  {
    protocol <- protocol_list$portfolio_item_list[[i]][["detail"]][["supply_token_list"]]
    for (j in 1:length(protocol))
      protocols_table <- rbind(protocols_table,protocol[[j]])
  }
  protocols_table <- protocols_table[,-c(14)]
  protocols_table$amount_USD <- protocols_table$price*protocols_table$amount
  protocols_table <- subset(protocols_table, select = -c(amount,price,protocol_id))
  return(protocols_table)
}

protocol_coins <- extract_protocoles(res)


#Merge into portfolio_coins
portfolio_coins <- rbind(wallet_coins, protocol_coins)
portfolio_coins %<>% group_by(portfolio_coins[,-c(12)]) %>% mutate(amount_USD = sum(amount_USD)) %>% unique()

portfolio_coins$labels <- as.factor(ifelse(portfolio_coins$amount_USD/sum(portfolio_coins$amount_USD)>0.01,
                 portfolio_coins$optimized_symbol, "Others (<1%)"))

coins_share <- plot_ly(portfolio_coins, 
                     y = ~ labels,
                     x = ~ round(amount_USD, 1),
                     color = ~ optimized_symbol,
type = 'bar', textposition = 'inside', orientation = 'h')  %>% 
  layout(yaxis = list(categoryorder = "total ascending", title = " "),
         xaxis=list(title="Value, USD"),
         barmode = 'stack') %>%
  hide_legend()
#Price Change of tokens  --------------------------------------------------
coins <- supported_coins(max_attempts = 3)
portfolio_symbols <- unique(tolower(c(portfolio_coins$symbol, portfolio_coins$name, c("UOS", "CHICKS","SOLX"))))
coins_sorted <- coins[tolower(coins$symbol) %in% portfolio_symbols |
                        tolower(coins$name) %in% portfolio_symbols,]
coins_table <- data.frame(current_market(coins_sorted$coin_id, vs_currency = "usd", max_attempts = 3))
coins_table <- coins_table[,c("name", "symbol", "current_price", "price_change_percentage_24h",
                              "price_change_percentage_7d_in_currency","price_change_percentage_30d_in_currency",
                              "market_cap")]

colnames(coins_table) <- c("Name", "Symbol", "Price", "24h %","7d %","30d %","Market Cap")
coins_output_table <- coins_table
coins_output_table$Name <- paste0("<a href='coingecko.com/en/coins/",gsub(" ", "-",tolower(coins_output_table$Name)),"'>",coins_output_table$Name,"</a>")
coins_output_table[,c(4,5,6,7)] <- round(coins_output_table[,c(4,5,6,7)], 1)
coins_output_table[,c(3)] <- round(coins_output_table[,c(3)], 3)
#Percentage change
for (i in c(4,5,6)) {
  coins_output_table[,c(i)] <- case_when(
    coins_output_table[,c(i)] > 0 ~ paste0("▴",coins_output_table[,c(i)]),
    coins_output_table[,c(i)] < 0 ~ gsub("-","",coins_output_table[,c(i)]) %>% paste0("▾",.),
    TRUE ~ as.character(coins_output_table[,c(i)])
  )
  coins_output_table[,c(i)] <- ifelse(is.na(coins_output_table[,c(i)])==FALSE, paste0(coins_output_table[,c(i)], "%"), NA)
}

coins_output_table$Price <- paste0("$",coins_output_table$Price)
coins_output_table$`Market Cap` <- case_when(
  coins_output_table$`Market Cap` == 0 ~ "No Data",
  coins_output_table$`Market Cap` < 10^6 ~ paste0("$",round(coins_output_table$`Market Cap`/10^3,2), "k"),
  coins_output_table$`Market Cap` < 10^9 ~ paste0("$",round(coins_output_table$`Market Cap`/10^6,2), "mln"),
  TRUE ~ paste0("$",round(coins_output_table$`Market Cap`/10^9,2), "bn")
)

for (i in c(4:7)) {
  coins_output_table[,c(i)] <- factor(coins_output_table[,c(i)], 
         levels = unique(coins_output_table[,c(i)][order(coins_table[,c(i)], decreasing = TRUE)]))
}


#Construct Coins
change_formatter <-
  formatter("span",
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(grepl("▴", x) == TRUE, "forestgreen", ifelse(grepl("▾", x) == TRUE, "red", "black"))
            ))

coins_output_table <- formattable(
  coins_output_table,
  list(
    `24h %` = change_formatter,
    `7d %`  = change_formatter,
    `30d %` = change_formatter
  )
) %>% as.datatable(escape = FALSE)

#Crypto News
res <- fromJSON(url("https://cryptopanic.com/api/v1/posts/?auth_token=6cc8f1944d54fc8909016473b6a08f4a5a52cb2d&filter=hot"))

df_news <- res$results
news_date <- as.POSIXct(paste0(substr(df_news$published_at,1,10)," ",substr(df_news$published_at,12,16)), format = "%Y-%m-%d %H:%M")
news_date <- as.character.POSIXt(news_date, format = "%d %b %H:%M")
df_news <- data.table(Date = news_date, 
                      IR = df_news$votes$important, 
                      #Source = df_news$source$title, 
                      Title = paste0("<a href='",df_news$url,"'>",df_news$title,"</a>")
                                     )
df_news <- df_news[order(df_news$IR, decreasing = TRUE),]
#df_news <- dplyr::slice_max(df_news, n=10, order_by = Importance)

