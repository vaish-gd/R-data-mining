library(arules)
library(arulesViz)
d1=data(Groceries)
class(d1)
summary(Groceries)
inspect(head(Groceries, 3))


library(utils)
class(transactions)
data()
try(data(package = "rpart") )
try(data(package = "arules") )
data(Groceries)

df=read.csv("proj3/df_inner_x.csv",sep = ",",header = T)

df_m=read.csv("a/loafernan.csv",sep = ",",header = T)
write.csv(df_combined,"ItemList2.csv", quote = F, row.names = FALSE)

df_combined=data.frame(df["combined"])
library(dplyr)

df_combined %>% filter(!grepl('?', df_combined$combined))

#myData = myData[myData$A > 4,]

df1<-df_combined[!grepl("?", df_combined$combined),]

df_combined=df_combined[!str_detect(df_combined$combined,"?")]

install.packages("stringr")
library(stringr)

str_detect("bananaSSSS", "banana")

df3=df_combined[!grepl("?", df_combined["combined"], ),]

#df4= df_combined[ !(c("?") %in% df_combined$combined  ), ]

write.csv(df_combined,"ItemList.csv", quote = F, row.names = FALSE)

txn = read.transactions(file="ItemList2.csv",format = "basket", rm.duplicates= TRUE,sep=",",cols=1);

basket_rules <- apriori(txn,parameter = list(supp = 0.006,conf=0.05));
#above says about min_sup only
basket_rules <- apriori(txn,parameter = list(sup = 0.00002));

plot(top10subRules,method="two-key plot")
plot(top10subRules, method="paracoord")

inspect(basket_rules)

plotly_arules(basket_rules)

top10subRules <- head(basket_rules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

inspect(head(basket_rules, n = 1, by ="lift"))

itemFrequencyPlot(txn, topN = 10)

itemFrequency(txn)


