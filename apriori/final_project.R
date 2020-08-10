
#question1
basket_rules_1 <- apriori(txn,parameter = list(supp = 0.01,conf=0.00005));
basket_rules_2 <- apriori(txn,parameter = list(supp = 0.02,conf=0.00006));
basket_rules_3 <- apriori(txn,parameter = list(supp = 0.04,conf=0.00007));

#this is standard,please dont vary that and DONT RUN IT  
basket_rules_infrequent <- apriori(txn,parameter = list(supp = 0.00001));
#this is standard,please dont vary that DONT RUN IT 

library(arulesViz)
plot(basket_rules_infrequent,method="two-key plot")
plot(basket_rules_infrequent, method="paracoord")

plot(basket_rules_1,method="two-key plot")
plot(top10subRules, method="paracoord")
plotly_arules(basket_rules)
plot(basket_rules_1, method = "graph")
plot(basket_rules_1, method = "matrix")
plot(basket_rules_1, method = "doubledecker")
plot(basket_rules_3,method="two-key plot")
plot(basket_rules_3, method="paracoord")

inspect(head(basket_rules_1,n=20,by="support"))

#question2
basket_rules_1 <- apriori(txn,parameter = list(supp = 0.01,conf=0.00005));
#plot for the above

#question3
inspect(basket_rules_1)
inspect(basket_rules_2)
inspect(basket_rules_3)
#look for transactions that have values in both lhs,rhs

min_support <- c(0.01,0.02,0.04)
min_conf <- c(0.00005,0.00006,0.00007)
no_of_rules <- c(75,32,17)

d1$no_of_rules=no_of_rules
d1$min_conf=min_conf
d1$min_support=min_support
#persp(x, y, z)
library(plotly)
p=plot_ly(df, x = min_support, y = min_conf, z = no_of_rules, colors = c('#BF382A', '#0C4B8E')) 








####question 4.
#dont run the above, look if it's already stored  DONT RUN IT 
basket_rules_infrequent <- apriori(txn,parameter = list(supp = 0.00001));
#dont run the above, look if it's already storedDONT RUN IT 
rules_lift_gt_1=subset(basket_rules_1, subset = lift > 1)
rules_lift_lt_1=subset(basket_rules_1, subset = lift < 1)
rules_lift_equalto_1=subset(basket_rules_1,subset = lift == 1)


inspect(head(rules_lift_gt_1,n=20,by="support"))
inspect(head(rules_lift_lt_1,n=20,by="support"))
inspect(head(rules_lift_equalto_1,n=20,by="support"))

####question 5.
df_2=head(basket_rules_infrequent,n=100,by="confidence")
library(arulesViz)
plot(df_2, method = "graph")
plot(df_2, method = "two-key plot")
plot(df_2, method = "paracoord")

df1=head(rules_lift_gt_1,n=20,by="support")
plot(rules_lift_gt_1, method = "graph")
plot(rules_lift_gt_1, method = "two-key plot")
plot(rules_lift_gt_1, method = "paracoord")

plot(rules_lift_lt_1, method = "graph")
plot(rules_lift_lt_1, method = "two-key plot")
plot(rules_lift_lt_1, method = "paracoord")

plot(rules_lift_equalto_1, method = "graph")
plot(rules_lift_equalto_1, method = "two-key plot")
plot(rules_lift_equalto_1, method = "paracoord")

