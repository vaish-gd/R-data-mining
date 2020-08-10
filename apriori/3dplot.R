min_support <- c(0.01,0.02,0.04)
min_conf <- c(0.00005,0.00006,0.00007)
no_of_rules <- c(75,32,17)

d1$no_of_rules=no_of_rules
d1$min_conf=min_conf
d1$min_support=min_support
#persp(x, y, z)
library(plotly)
 p=plot_ly(df, x = min_support, y = min_conf, z = no_of_rules, colors = c('#BF382A', '#0C4B8E')) 
 chart_link = api_create(p, filename="scatter3d-basic")
 chart_link