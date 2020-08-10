#apriori(df, parameter = list(lift = 0.3, minlen =2))
#subset(rules, subset = lift > 2)
#sort (rules, by="lift", decreasing=TRUE)
# rulesLift <- sort(subset(rules, subset = lift < 2), by="lift") 
# inspect(rulesLift)
#rules.sorted <- sort(rules, by="lift")
#sort (rules, by="lift", decreasing=TRUE)

basket_rules <- apriori(txn,parameter = list(supp = 0.00001,conf=0.0000005));

rules_lift_gt_1=subset(basket_rules, subset = lift > 1)
rules_lift_lt_1=subset(basket_rules_1, subset = lift < 1)
rules_lift_equalto_1=subset(basket_rules_1, subset = lift == 1)

#basket_rules <- apriori(txn,parameter = list(supp>0.6,conf>0.3,target="rules"));

sort(rules_lift_lt_1, by="confidence", decreasing=TRUE)

df3=head(rules_lift_lt_1,n=100,by="confidence")
inspect(df3)

library(arulesViz)
plot(df3,method="two-key plot")


rgt= head(rules_lift_gt_1, n = 5, by = "confidence")
inspect(rules_lift_gt_1)
rlt=head(rules_lift_lt_1, n = 5, by = "confidence")
reqto=head(rules_lift_equalto_1, n = 5, by = "confidence")
