#Name: Manoj Kumar
#Roll No: 220625
#Assignment-1 Solution Data Science with R (Stamatics)

library(rvest)
library(tidyverse)
library(dbplyr)
library(dtplyr)

#Q1 Solution is given below:

nifty_html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
nifty_html <- html_elements(nifty_html, "body")
company_50 <- html_element(nifty_html, ".mont-fnt.container.listCompany.margin-top")
company_50 <- html_element(company_50, ".mw-100")
company_50 <- html_element(company_50, "#stock-data")
company_50 <- html_element(company_50, "table")
#company_50 <- html_element(company_50, "tbody")
#company_50 <- html_elements(company_50, ".company-ellipses")
#company_50 <- html_element(company_50, "a")
#company_50 <- html_text(company_50)
top_50 <- company_50 %>% html_table()
Top_50 <- as.data.frame(top_50)
#Top_50 <- data.frame(Company_Name = top_50)


Top_50 <- Top_50 %>%
  select(Company.Name..M.Cap.,Var.1, DeciZen, More.Info, CMP, Price.Change, Market.Cap..Cr., X52.Week.High, X52.Week.Low, ROE, P.E, P.BV, EV.EBITDA, X5YSales.Gr..., X5YProfit.Gr...) %>%
  mutate(
    DeciZen = NULL,
    Var.1 = NULL,
    More.Info = NULL,
  )
colnames(Top_50) <- c("Company Name (M.Cap)", "CMP", "Price Change", "MArket CAp (Cr)", "52 Week High", "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%")

Top_50 <- as.data.frame(Top_50)
view(Top_50)
#NIFTY_50 <- data.frame(Company_Name = company_names)
#view(NIFTY_50)



################
#Qb:

rel_html <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/refineries/reliance-industries/company-info#faqsdiv")
rel_html <- html_element(rel_html, "body")
rel_table <- html_element(rel_html, ".margin-top.mont-fnt.py-2.company-info-pg.mt-2")
rel_table <- html_element(rel_table, ".tenyr-xray-section.py-3")
rel_table <- html_element(rel_table, ".container")
rel_table <- html_element(rel_table, ".text-center.py-4")
rel_table <- html_element(rel_table, "#tenyearxraysa")

rel_table1 <- html_element(rel_table, ".col-12")
rel_table1 <- rel_table1 %>% html_table()
rel_table1 <- as.data.frame(rel_table1)
rel_table1 <- rel_table1[-c(12,13,14)]
rel_table1 <- rel_table1[6:13,]



rel_table2 <- html_element(rel_table, ".col-12.mt-4")
rel_table2 <- rel_table2 %>% html_table
rel_table2 <- as.data.frame(rel_table2)
rel_table2 <- rel_table2[-c(12,13)]
rel_table2 <- rel_table2[2:7,]

rel_table1 <- rbind(rel_table1, rel_table2)

#r <- c(TRUE, FALSE, FALSE, FALSE, FALSE,TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,FALSE, FALSE, TRUE,TRUE,TRUE, TRUE, TRUE, TRUE)
#rel_table <- rel_table[-c(2,3,4,5,14,15,16,17,18,19,20,21)]
rel_table <-rel_table[1:11]



#################
##Qc:

tennis <- function(p){
  win_a <- 0
  win_b <- 0
  #p <- as.integer(p)
  p <- c((1-p),p)
  for (i  in 1:5) {
    a <- sample(x = 0:1, size = 1, prob = p)
    a <- as.integer(a)
    if(a){
      win_a <- win_a + 1
    }else{
      win_b <- win_b + 1
    }
    if(win_b == 3 | win_a ==3){
      break
    }
  }
  x <- win_b + win_a
  return(x)
}

matches <- c()
for(i in 1:1000){
  matches[i] <- tennis(0.70)
}
ans <- mean(matches)

print(ans)
