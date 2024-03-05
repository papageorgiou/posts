library(lubridate)
months_df <- tibble(month=ymd("2019-09-01")+ months(0:47))


pizzas <- "includes: domino's, papa john's, pizza hut, blaze pizza,  hungry howie's, \n  jet's pizza, little caesars, marco's pizza, papa murphy's, round table pizza"


# top 10
searches <- c(30485000L, 35229000L, 34852000L, 36625000L, 33832000L, 35739000L, 35562000L, 37674000L, 41301000L, 37154000L, 39434000L, 39257000L, 37544000L, 38010000L, 35695000L, 40387000L, 39247000L, 36622000L, 39407000L, 37420000L, 38330000L, 35257000L, 35707000L, 35707000L, 35407000L, 35804000L, 35157000L, 40454000L, 35707000L, 35257000L, 35830000L, 35530000L, 35407000L, 34110000L, 34287000L, 33905000L, 31900000L, 34287000L, 33960000L, 39007000L, 33747000L, 32450000L, 34860000L, 32118000L, 30335000L, 32418000L, 34078000L, 30735000L)



df <- tibble(months_df, searches) %>% rename(`month year`=month)

ggplot(df, aes(x=`month year`, y = searches))  + geom_line() + geom_point(size=2, colour="blue") + #geom_smooth(linetype=3) + 
  expand_limits(y = c(0, 50000000) ) +  xlab(label = NULL) +  ylab(label = NULL) +
  #geom_smooth(method = "loess", se = F, linetype=1,  alpha=0.9, size=0.5, colour="red")   + 
  ggtitle("Monthly searches for Big Pizza 2019-23, USA \nSource:Google keyword planner") +
  labs(caption  =  pizzas) + 
  scale_y_continuous(labels = scales::label_number_si(accuracy=1)) +  theme_minimal()

ggsave("big_pizza_trends_3.png", height = 3, width= 5)
# related
searches <- c(70149020L, 81157100L, 78101460L, 81851770L, 77305770L, 81016350L, 79179400L, 89359190L, 92152340L, 83567440L, 86862570L, 85429090L, 83189760L, 86563080L, 79853780L, 89764650L, 89355690L, 83328330L, 87305840L, 82248820L, 83095610L, 78259520L, 81340550L, 80594080L, 79533830L, 82757190L, 77875040L, 89585640L, 82171520L, 77799360L, 81335590L, 78861690L, 78401290L, 75772900L, 76425370L, 73692420L, 71124400L, 75386420L, 74923700L, 86744310L, 76240240L, 73000350L, 78345240L, 71718970L, 67642020L, 70602090L, 74433930L, 69165660L)
df <- tibble(months_df, searches)

ggplot(df, aes(x=month, y = searches))  + geom_line() + geom_point() +  #geom_smooth(linetype=3) + 
 expand_limits(y = c(0, 10000000) )  

# top 3 only
searches <- c(25240000L, 28320000L, 28320000L, 30820000L, 27200000L, 28320000L, 28320000L, 29680000L, 32180000L, 28320000L, 30820000L, 30820000L, 28320000L, 28320000L, 26360000L, 30820000L, 29680000L, 28320000L, 30820000L, 28860000L, 28860000L, 26360000L, 26360000L, 26360000L, 26360000L, 26360000L, 26360000L, 30820000L, 26360000L, 26360000L, 26360000L, 26360000L, 26360000L, 25240000L, 25240000L, 25240000L, 23280000L, 25240000L, 25240000L, 28320000L, 23280000L, 23280000L, 25240000L, 23280000L, 21620000L, 23580000L, 25240000L, 21620000L)

df <- tibble(months_df, searches)

ggplot(df, aes(x=month, y = searches))  + geom_line() + geom_point() +  #geom_smooth(linetype=3) + 
  expand_limits(y = c(0, 10000000) )  

