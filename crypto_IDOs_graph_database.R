if(as.Date(file.info("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/idos.RData")$mtime)==Sys.Date() & 
   as.Date(file.info("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/platforms.RData")$mtime)==Sys.Date())
  {
  load("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/idos.RData")
  load("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/platforms.RData")
} else {
# Libraries and staff -----------------------------------------------------
#Web Scraping
library(rvest)
library(magrittr)
library(RSelenium) #paged table
library(netstat)
#Data editing and analysis
library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(plotly)
#Turn off scientific notations
options(scipen = 999)


# IDO platforms -----------------------------------------------------------
url_platforms <- "https://cryptorank.io/fundraising-platforms"

#Extract table from
platforms <- url_platforms %>%
  read_html %>%
  html_nodes(xpath = '//*[@id="__next"]/div/div[1]/div[3]/div[2]/div[1]/div[2]/div/table') %>%
  html_table()

#Table with IDO platforms
platforms <- data.table(platforms[[1]])
platforms <- platforms[, -c("Blockchain")]



# IDOs --------------------------------------------------------------------


rD <- rsDriver(browser = c("firefox"), port=netstat::free_port())
driver <- rD[["client"]]

driver$navigate("https://cryptorank.io/ico")
Sys.sleep(10)

element <-
  driver$findElement(
    using = "xpath",
    "/html/body/div[1]/div/div[1]/div[3]/div[2]/div[2]/div[1]/div[2]/div[2]/a[2]"
  )

idos <- data.frame()
project_names <- c()
#last_page <- as.numeric(driver$findElement(using = "css selector", "a.item:nth-child(6)")$getElementText()[[1]])
last_page <- 29

for (i in c(1:last_page)) {
  raw_table <-
    driver$findElements(using = "xpath", value = "/html/body/div[1]/div/div[1]/div[3]/div[2]/div[2]/div[2]/div/table")
  meta_table <-
    read_html(raw_table[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
  project_names <- meta_table %>% html_nodes("tr") %>% html_node("td") %>% 
    html_node("div") %>% html_node("a") %>% html_node("div") %>% html_node("div") %>%
    html_node("span") %>% html_text()
  meta_table <- meta_table %>% html_table()
  meta_table <- meta_table[[1]]
  
  idos <- rbind(idos, meta_table)
  
  element$clickElement()
}

#close the driver
driver$close()

#close the server
rD[["server"]]$stop()


rm(raw_table, meta_table, driver, element, i, rD, url_platforms)



# Data Editing ------------------------------------------------------------
##Edit columns of platforms-db
#1
platforms$`Current ROI` <-
  as.numeric(gsub("x", "", platforms$`Current ROI`))
#2
platforms$`ATH ROI` <- as.numeric(gsub("x", "", platforms$`ATH ROI`))
#3
platforms$Raised %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE)

platforms$Raised <- dplyr::case_when(
  stringr::str_detect(platforms$Raised, 'M') ~ readr::parse_number(platforms$Raised) * 1,
  stringr::str_detect(platforms$Raised, 'K') ~ readr::parse_number(platforms$Raised) / 100,
  TRUE ~ parse_number(platforms$Raised)
)
colnames(platforms)[which(colnames(platforms) == "Raised")] <-
  "Raised (mln USD)"
#4
platforms$`Num of Part.` <- as.numeric(platforms$`Num of Part.`)
#5
platforms$`Volume (24h)` %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE)

platforms$`Volume (24h)` <- dplyr::case_when(
  stringr::str_detect(platforms$`Volume (24h)`, 'M') ~ readr::parse_number(platforms$`Volume (24h)`) * 1,
  stringr::str_detect(platforms$`Volume (24h)`, 'K') ~ readr::parse_number(platforms$`Volume (24h)`) / 100,
  TRUE ~ parse_number(platforms$`Volume (24h)`)
)
colnames(platforms)[which(colnames(platforms) == "Volume (24h)")] <-
  "Volume 24h (mln USD)"
#6
platforms$`Market Cap` %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE)

platforms$`Market Cap` <- dplyr::case_when(
  stringr::str_detect(platforms$`Market Cap`, 'B') ~ readr::parse_number(platforms$`Market Cap`) * 1,
  stringr::str_detect(platforms$`Market Cap`, 'M') ~ readr::parse_number(platforms$`Market Cap`) / 100,
  TRUE ~ parse_number(platforms$`Market Cap`)
)
colnames(platforms)[which(colnames(platforms) == "Market Cap")] <-
  "Market Cap (bln USD)"

##Edit columns of idos-db
idos <- data.table(idos)
#1
idos$Price %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE) %>%
  as.numeric
#2
idos$`Sale Price` %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE) %>%
  as.numeric
#3
idos$`Market Cap` %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE)

idos$`Market Cap` <- dplyr::case_when(
  stringr::str_detect(idos$`Market Cap`, 'B') ~ readr::parse_number(idos$`Market Cap`) * 100,
  stringr::str_detect(idos$`Market Cap`, 'M') ~ readr::parse_number(idos$`Market Cap`) * 1,
  stringr::str_detect(idos$`Market Cap`, 'K') ~ readr::parse_number(idos$`Market Cap`) / 100,
  TRUE ~ parse_number(idos$`Market Cap`)
)
colnames(idos)[which(colnames(idos) == "Market Cap")] <-
  "Market Cap (mln USD)"
#4
idos$Raised %<>%
  gsub(" ", "", .) %>%
  gsub("$", "", ., fixed = TRUE)

idos$Raised <- dplyr::case_when(
  stringr::str_detect(idos$Raised, 'B') ~ readr::parse_number(idos$Raised) * 100,
  stringr::str_detect(idos$Raised, 'M') ~ readr::parse_number(idos$Raised) * 1,
  stringr::str_detect(idos$Raised, 'K') ~ readr::parse_number(idos$Raised) / 100,
  TRUE ~ parse_number(idos$Raised)
)
colnames(idos)[which(colnames(idos) == "Raised")] <-
  "Raised (mln USD)"
#5
idos$`ROI USD` <- as.numeric(gsub("x", "", idos$`ROI USD`))
#6
idos$`ATH ROI USD` <- as.numeric(gsub("x", "", idos$`ATH ROI USD`))
#7
idos$Ended <-
  fifelse(
    is.na(as.Date(
      idos$Ended, format = "%d %b %Y", tz = "UTC"
    )) == TRUE,
    as.Date(idos$Ended, format = "%d %b"),
    as.Date(idos$Ended, format = "%d %b %Y", tz = "UTC")
  )

colnames(idos) <- gsub( " ", "_", colnames(idos))



# Save Data ---------------------------------------------------------------

setwd("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS")
save(idos, file = "idos.RData")
save(platforms, file = "platforms.RData")
# Analysis -----------------------------------------------
##Box Plot
# #1 Barplot
# idos_filter <- idos
# idos_filter <- unique(idos[idos$Ended>as.Date("2021-01-01") & idos$Type=="IDO"])
# fig <- plot_ly(idos_filter, y = ~`ATH ROI USD`, color = ~`Category`, type = "box")
# fig
# #2 Scatterplot
# idos_filter <- idos
# idos_filter$week <- week(idos_filter$Ended)
# idos_filter <- merge(idos_filter, platforms[,c("Rank","Name")], by.x = "TGE Platform", by.y="Name", all.x = TRUE)
# idos_filter <- idos_filter[ idos_filter$Rank <= 10, median(`ATH ROI USD`), by=c("TGE Platform","week")]
# fig <- plot_ly(idos_filter, y = ~V1, color = ~`TGE Platform`, type = "scatter", mode = "lines")
# fig

}

