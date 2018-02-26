# rvest

library("easypackages")
my_packages <- c("rvest", "tidyverse", "purrr")
libraries(my_packages)


url_base <- "https://propiedades.com/tijuana/naves-industriales-renta"

pages <- 1:3

urls <- map(paste0(url_base, "?=", pages), read_html)


map(urls, html_nodes(".price"))


html_text(html_nodes(urls[[1]], ".price"))

page <- read_html(url_base)
price <- html_text(html_nodes(page, ".price"))
title <- html_text(html_nodes(page, ".row-property"))
descr <- html_text(html_nodes(page, "h4"))
act <- html_text(html_nodes(page, ".m-n"))
act <- act[2:43]
act

dim <- html_text(html_nodes(page, "li"))
ubic <- html_text(html_nodes(page, ".renta"))


df <- data.frame(cbind(title, price, ubic, descr, act)) %>%
  mutate(constru = str_extract(descr, "(\\d*[.,]?\\d+\\s?+\\W*((?i)mts(?-i)|(?i)m(?-i)))"))

df$constru

map_df(1:4, function(i){
  cat(".")
  page <- read_html(sprintf(url_base,i))
  data.frame(Price = html_text(html_nodes(page, ".price")),
             title = html_text(html_nodes(page, ".row-property")),
             descr = html_text(html_nodes(page, "h4")),
             actt = html_text(html_nodes(page, ".m-n")))
}) -> Naves




# rweb scrapping
# Loading the rvest package
install.packages("rvest")
install.packages("xml2")


#Reading the HTML code from the website

url <- "https://www.solili.mx/search/?keyword=&property=building&t=industrial"

pg <- read_html(url)

precio <- pg %>% 
  html_nodes(".strong") %>%
  html_text() %>%
  unlist()

precio

medidas <- pg %>%
  html_nodes("li")

medidas  

