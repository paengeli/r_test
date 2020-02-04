################ Exercise 1 #####################

# Set working directory
#setwd("")

#install.packages("data.table")
library("data.table")

transactions <- fread('transactions.csv')

#check the types
str(myData)

#install.packages("lubridate")
library("lubridate")

#install.packages("Hmisc")
library("Hmisc")

transactions[, TransDate:=dmy(TransDate,tz="UTC")]

################ Exercise 2 #####################

max.Date <- max(transactions$TransDate) # treated as today

#frequency
transactions[, frequency := .N, by=Customer]

#monetary value
transactions[, monetary_value := mean(PurchAmount), by=Customer]

#recency
setkey(transactions,"TransDate")
transactions[, recency := max(TransDate),by=Customer]




rfm <- transactions[Customer,]

############ Solution ##########

rfm <- transactions[, list(
  recency = as.numeric(max.Date - max(TransDate)),
  frequency = .N,
  monetary = mean(PurchAmount)),
  by="Customer"]

str(rfm)

################ Exercise 3 #####################
?cut2

monetary <- rfm[, cut2(monetary, c(30,78))]
frequency <- rfm[, cut2(frequency, c(6,12))]
recency <- rfm[, cut2(recency, g=3)]

# Solution
rfm_scores <- rfm[, list(Customer,
  frequency = as.numeric(cut2(frequency, g=3)),
  recency = as.numeric(cut2(-recency, g=3)),
  monetary = as.numeric(cut2(monetary, g=3))
)]

################ Exercise 4 #####################
rfm <- rfm[, list(Customer,
                  frequency,
                  recency,
                  monetary,
                  rfm = mean((frequency,recency,monetary)),
                  rfm_w1 = (0.2*frequency + 0.6*recency + 0.2*monetary)/3,
                  rfm_w2 = (0.6*frequency + 0.2*recency + 0.2*monetary)/3
                  
                  )]
?mean

# Solution:
rfm_scores[, overall:= mean(c(recency,frequency,monetary)), by=Customer]
rfm_scores[, weighted1 := (recency*0.2+frequency*0.6+monetary*0.2), by=Customer]
rfm_scores[, weighted2 := (recency*0.6+frequency*0.2+monetary*0.2), by=Customer]


rfm_groups <- rfm_scores[,list(
  Customer,
  group_overall=round(overall),
  group_weighted1=round(weighted1),
  group_weighted2=round(weighted2))]

#Get best customers
rfm_scores[overall == max(overall),]