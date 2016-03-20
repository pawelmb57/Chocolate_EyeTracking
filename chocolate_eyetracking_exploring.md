# Independent Study Project
Pawel Bogdanowicz  
March 2016  

Data collected as part of "Decisions about chocolate are
processed differently than decisions on gambles: Evidence from eye-tracking" by Betty E. Kim-Viechnicki.

* Kim-Viechnicki, B.E., Burla, Y., Feit, E., Plassmann, H.,& Kable, J.W. (2013, September). Decisions about chocolate are
processed differently than decisions on gambles: Evidence from eye-tracking. Poster session at the annual meeting of the
Society of Neuroeconomics, Lausanne, Switzerland.



# Exploring Conjoint Analysis Data

This dataset contains the results of an experiment in which participants were asked to choose one of three chocolates that were displayed on a screen.  The information that was provided was the brand, type of chocolate, and the price.  Below is an example of the choice that was presented.  


![Caption for the picture.](http://goo.gl/6lG43O)


Each trial is a set of three choices given to a participant.  Each choice consists of three attributes including brand, type, and price.  Additional variables include the number of times the respondent fixated on any one of the attributes and whether the product was chosen.




### Load Libraries and Data

We start by loading the dataset and necessary libraries.



```r
library(plotly)

data <- as.data.frame(read.csv("http://goo.gl/GC1gRs"))
```

Effects coding was used to distinguish between levels.  The following table shows the coding for five brands using four variables.  Although Hershey is not a variable in the dataset, it is accounted for by setting the four brand variables to -1.  

------------------------------------------------------------------
  Brand       B_Dove    B_Lindt     B_Godiva    B_Ghirardelli  
------------ -------- ----------   ----------  ---------------
Dove            1          0           0             0

Lindt           0          1           0             0

Godiva          0          0           1             0

Ghirardelli     0          0           0             1

Hershey         -1        -1          -1            -1
------------------------------------------------------------------


The same method is used to code the type of chocolate.

------------------------------------------------------------------
  Brand       T_MilkNuts    T_Dark    T_DarkNuts     T_White
------------ ------------ ---------- ------------  -----------
MilkNuts         1          0           0             0

Dark             0          1           0             0

DarkNuts         0          0           1             0

White            0          0           0             1

Milk            -1         -1          -1            -1
------------------------------------------------------------------



For convenience, we create two factor variables "brand" and "type" that show the individual brand and types.  By default, R will automatically code the variable levels. 



```r
data$brand <- as.factor(ifelse(data$B_Dove==1,"Dove",
                     ifelse(data$B_Lindt==1,"Lindt",
                            ifelse(data$B_Godiva==1,"Godiva",
                                   ifelse(data$B_Ghirardelli==1,"Ghirardelli","Hershey")))))
data$type <- as.factor(ifelse(data$T_MilkNuts==1,"MilkNuts",
                    ifelse(data$T_Dark==1,"Dark",
                           ifelse(data$T_DarkNuts==1,"DarkNuts",
                                  ifelse(data$T_White==1,"White","Milk")))))
```


This chunk performs basic counts on the dataset.

```r
nrow(data)                                        # 1050 observations
length(unique(data$Ind))                          # 14 Individuals (Ind) participated
aggregate(data$Trial,list(indi=data$Ind),length)  # 75 each number of trials each Ind participated in
```




# Exploring the Data

We start by exploring the dataset using the summary() function.  


```r
summary(data)
```

```
##       Ind           Trial         Alt        B_Dove       
##  Min.   :2401   Min.   : 1   Min.   :1   Min.   :-1.0000  
##  1st Qu.:2405   1st Qu.: 7   1st Qu.:1   1st Qu.: 0.0000  
##  Median :2410   Median :13   Median :2   Median : 0.0000  
##  Mean   :2409   Mean   :13   Mean   :2   Mean   : 0.0067  
##  3rd Qu.:2413   3rd Qu.:19   3rd Qu.:3   3rd Qu.: 0.0000  
##  Max.   :2417   Max.   :25   Max.   :3   Max.   : 1.0000  
##     B_Lindt          B_Godiva       B_Ghirardelli     T_MilkNuts     
##  Min.   :-1.000   Min.   :-1.0000   Min.   :-1.00   Min.   :-1.0000  
##  1st Qu.: 0.000   1st Qu.: 0.0000   1st Qu.: 0.00   1st Qu.: 0.0000  
##  Median : 0.000   Median : 0.0000   Median : 0.00   Median : 0.0000  
##  Mean   : 0.001   Mean   : 0.0267   Mean   :-0.02   Mean   : 0.0143  
##  3rd Qu.: 0.000   3rd Qu.: 0.0000   3rd Qu.: 0.00   3rd Qu.: 0.0000  
##  Max.   : 1.000   Max.   : 1.0000   Max.   : 1.00   Max.   : 1.0000  
##      T_Dark         T_DarkNuts         T_White            Price     
##  Min.   :-1.000   Min.   :-1.0000   Min.   :-1.0000   Min.   :0.50  
##  1st Qu.: 0.000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:1.30  
##  Median : 0.000   Median : 0.0000   Median : 0.0000   Median :2.20  
##  Mean   : 0.041   Mean   : 0.0219   Mean   : 0.0181   Mean   :2.21  
##  3rd Qu.: 0.000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.:3.17  
##  Max.   : 1.000   Max.   : 1.0000   Max.   : 1.0000   Max.   :4.00  
##    Brand_Fix        Type_Fix       Price_Fix         Chosen     
##  Min.   : 0.00   Min.   : 0.00   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 1.00   1st Qu.: 2.00   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 2.00   Median : 3.00   Median :1.000   Median :0.000  
##  Mean   : 2.08   Mean   : 3.27   Mean   :0.984   Mean   :0.333  
##  3rd Qu.: 3.00   3rd Qu.: 4.00   3rd Qu.:2.000   3rd Qu.:1.000  
##  Max.   :18.00   Max.   :25.00   Max.   :7.000   Max.   :1.000  
##          brand           type    
##  Dove       :214   Dark    :233  
##  Ghirardelli:186   DarkNuts:213  
##  Godiva     :235   Milk    :190  
##  Hershey    :207   MilkNuts:205  
##  Lindt      :208   White   :209  
## 
```

The summary shows that there are no missing values and variables that should be either -1,0,or 1, are indeed so.  The summary for brand and type indicate that the number of times the brands and types came up are approximately equal.


The following graphs visualize the variables and how many times each attribute was chosen.  As the code for these graphs is largely the same, chunks are hidden unless there is a significant change.



## Chocolate Attributes


```r
cBrand <- data.frame(xtabs(Chosen ~ brand , data=data))

f <- list(
  family = "Arial, sans",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Brand",
  titlefont = f
)
y <- list(
  title = "Number Chosen",
  titlefont = f
)

p <- plot_ly(
  x = cBrand$brand
  , y = cBrand$Freq
  , type = "bar"
) %>%
layout(title = "Chosen by Brand" , xaxis = x, yaxis = y)

plotly_POST(p, filename='chocolateEyeTracking/chosenByBrand.png')
```

<iframe height="600" id="igraph" scrolling="no" seamless="seamless" src="https://plot.ly/~PawelMichal/70.embed" width="800" frameBorder="0"></iframe>
<iframe width="900" height="800" frameborder="0" scrolling="no" src="https://plot.ly/~PawelMichal/58.embed"></iframe>



<div>
    <a href="https://plot.ly/~PawelMichal/68/" target="_blank" title="Chosen by Brand" style="display: block; text-align: center;"><img src="https://plot.ly/~PawelMichal/68.png" alt="Chosen by Brand" style="max-width: 100%;width: 600px;"  width="600" onerror="this.onerror=null;this.src='https://plot.ly/404.png';" /></a>
    <script data-plotly="PawelMichal:68"  src="https://plot.ly/embed.js" async></script>
</div>


#### So what?

Chosen by brand shows that Godiva gets chosen more frequently than other brands and Hershey is the least chosen.  However,it may be premature to conclude that Godiva is the best brand.  Without understanding the other attributes that were available in the trial, we may be mistaking correlation for causation.

  
<!--html_preserve--><div id="htmlwidget-4264" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-4264">{"x":{"data":[{"type":"bar","inherit":true,"x":["Dark","DarkNuts","Milk","MilkNuts","White"],"y":[99,73,84,68,26]}],"layout":{"title":"Chosen by Type","xaxis":{"title":"Type","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"margin":{"b":40,"l":60,"t":25,"r":10}},"url":null,"width":null,"height":null,"base_url":"https://plot.ly","layout.1":{"title":"Chosen by Type","xaxis":{"title":"Type","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}}},"filename":"Chosen by Type"},"evals":[]}</script><!--/html_preserve-->

#### So what?

Chosen by type reveals that people do not like white chocolate.  Out of the 350 chocolates that were chosen throughout the entire experiment, white chocolate was only picked 26 times.  The next least picked chocolate type is milk chocolate with nuts with 68 being picked.  Although we can see the stark difference in white chocolate, it seems that the other four types of chocolates are chosen at roughly the same frequency.


<!--html_preserve--><div id="htmlwidget-5058" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-5058">{"x":{"data":[{"type":"bar","inherit":true,"x":["0.5","0.6","0.7","0.8","0.9","1","1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8","1.9","2","2.1","2.2","2.3","2.4","2.5","2.6","2.7","2.8","2.9","3","3.1","3.2","3.3","3.4","3.5","3.6","3.7","3.8","3.9","4"],"y":[2.19722457733622,3.13549421592915,2.484906649788,2.63905732961526,2.63905732961526,2.484906649788,2.484906649788,2.07944154167984,2.77258872223978,2.77258872223978,1.94591014905531,1.94591014905531,2.70805020110221,2.07944154167984,1.94591014905531,2.19722457733622,1.94591014905531,2.30258509299405,1.38629436111989,1.94591014905531,2.63905732961526,2.19722457733622,1.94591014905531,2.19722457733622,2.07944154167984,2.70805020110221,0.693147180559945,1.6094379124341,2.39789527279837,2.07944154167984,2.484906649788,1.09861228866811,2.484906649788,1.79175946922805,2.19722457733622,1.09861228866811]}],"layout":{"title":"Chosen by Price","xaxis":{"title":"Price","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"},"tickvals":[0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4]},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"margin":{"b":40,"l":60,"t":25,"r":10}},"url":null,"width":null,"height":null,"base_url":"https://plot.ly","layout.1":{"title":"Chosen by Price","xaxis":{"title":"Price","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"},"tickvals":[0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4]},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}}},"filename":"Chosen by Price"},"evals":[]}</script><!--/html_preserve-->


#### So what?

Chosen by price shows that as price increases the number of times the chocolate gets chosen decreases.The prices that were chosen for the experiment range from $0.50 and $4.  Although these prices may be typical for chocolate, it should be investigated whether this range is appropriate for a conjoint analysis.  One assumption that is made for under a conjoint analysis is that the attributes levels are linear with respect to the response variable.  This assumption is further tested in the analysis section.

The following visualization shows a histogram of prices for both alternatives that were chosen and not chosen.  The difference in histogram clearly shows that less expensive chocolates (<$2) are chosen more frequently than more expensive chocolate.  FOr chocolates that are greater than $2, there is less seperation which indicates price has a different decision impact on more expensive chocolate than on less expensive chocolate.



```r
chosenPrices <- data$Price[which(data$Chosen==1)]
notChosenPrices <- data$Price[which(data$Chosen==0)]
x <- list(
  title = "Pricing Bins",
  titlefont = f
)
y <- list(
  title = "Number Chosen",
  titlefont = f
)
plot_ly(x=chosenPrices , opacity = 0.6 , type = "histogram" , name="Chosen") %>%
  add_trace(x=notChosenPrices , name="Not Chosen") %>%
  layout(barmode="overlay",title="Chosen and Not Chosen Price Histograms")
```

<!--html_preserve--><div id="htmlwidget-1300" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-1300">{"x":{"data":[{"type":"histogram","inherit":true,"x":[0.6,3.9,3.7,1,1.5,2.6,2.1,1.1,1.7,0.8,1.6,3.7,3,3.5,3.7,3.8,3.9,2.8,3.7,1.9,3,0.9,0.6,3.2,2,2.1,3.5,1.8,2.5,1.3,0.7,0.9,1.3,3.4,1.1,2.9,2.8,2.5,3.4,0.7,0.6,4,2.5,2.9,0.9,0.6,2.8,3.2,3.8,2.3,1,2.2,1.2,1.3,1,0.8,1.4,2.2,1.2,0.9,1.7,3.3,1,2.6,2.7,2.5,0.5,2.2,2.2,0.6,2.5,1.1,0.7,1.4,1.7,2.6,4,1.7,1.4,3,3.9,0.7,0.6,2.7,1.4,2.5,1.7,1.8,0.9,3.9,1.1,2.4,3.9,2.9,3.4,3.5,1.6,0.6,2.5,2,2.4,2,0.6,3,3.6,0.8,2.4,2.4,1.8,1,0.6,1.1,2,0.6,1.1,1.6,0.9,0.8,2.9,0.9,2.2,3.3,1.5,0.5,1,3.8,0.9,1.5,2.9,1.1,1.4,2.6,0.5,2.2,2.4,2.5,3,3.2,1.3,3.6,0.5,2.3,1,0.8,3,1.7,3.5,0.6,1.1,0.7,0.7,3.9,3.7,1,1.5,2.6,3.1,1.1,1.7,3.7,1.6,3.7,1.9,3.5,1.5,3.8,3.9,2.6,2.8,0.7,3,0.9,3.4,1.4,0.8,1,3.5,1.2,1.3,3.5,2.5,1.4,2.9,1.4,0.9,1.7,3.3,1,2.6,3,3,0.6,2.2,4,3.2,2.5,1.1,0.7,2.2,1.7,2.1,1.3,1.6,0.8,1.3,3.3,0.8,1.3,3.4,3.8,2.1,0.6,2.5,1.8,0.7,0.6,2.7,2.5,1.2,1.8,2.1,2.8,3.2,1.8,1.3,0.5,2.6,1.7,0.9,3.5,3.5,0.6,1.4,2.7,3.4,2.7,3,0.5,1,0.6,1.4,2.3,1.3,0.5,1,1.2,0.7,3.5,1.7,3,3.9,0.9,1.7,1.4,3,2.5,0.7,0.6,2.3,1.4,2,0.6,1.8,1.3,2.9,2.1,2.4,1.9,2.9,3.4,3.5,1.6,0.6,0.8,2.5,3.7,3.8,1.3,1.4,1.6,3.7,3.6,1.4,1.3,2.2,1.9,1.7,1.9,3.4,3,3.1,1.9,1.3,1.8,3.3,2.8,2,3.5,3.3,3,3,2,0.6,1.3,3.7,0.8,2.4,3.7,3.9,3.7,3.3,1.3,2,1.9,1.2,3.3,2.8,0.8,3.3,0.9,3.3,3.3,2.8,1.4,2.2,0.6,2.7,1.7,0.5,1.5,2.6,2.1,1.1,1.7,0.8,1.2,1.1,0.8,0.6,1.5,0.5,1.2,2.7,2.8,0.7,0.8,0.9,0.6,1.4,2],"opacity":0.6,"name":"Chosen"},{"type":"histogram","inherit":true,"x":[0.7,3.6,2.7,0.7,1.7,3,3.6,0.5,0.8,3.3,3.1,2.5,3.1,4,0.8,3.2,2.3,3.8,0.6,3.7,0.7,1.2,1.1,0.8,0.8,1.9,0.6,1.9,1.5,1.6,0.5,3.7,1.2,0.5,2.7,2.6,4,2.8,0.7,0.8,0.8,2.4,3.4,3.4,1.8,3.4,3.6,1.4,1.1,0.8,2.2,3.5,1.3,1.7,1.6,2,3.2,0.8,1.1,2.7,0.6,3.3,2.9,0.8,2,1.2,1.8,3.1,1.6,3.8,2.1,1.4,1,0.6,0.9,1.9,1.8,3.2,0.6,2.9,2.3,0.8,2.7,1.3,2.1,1.1,1.2,1.5,1.8,3.2,1.5,2.1,3.7,2.2,4,2.4,0.9,1.8,1.3,2.5,1.4,2,3.5,2.2,3.9,2.2,0.7,3.3,2.8,3.5,1.3,2.5,2.1,3.1,2.9,3,2.5,1.4,2.5,3.2,3.7,1.4,3.7,2.3,3.8,3.5,2.8,2.8,3,3,3,3.9,0.6,3.4,3.6,3.8,4,2.9,3.2,2,3.4,2.5,3,3.8,2.2,2.7,3.3,2.2,2.3,2.7,3.9,2.7,0.9,3.3,3.6,1.6,2.4,3.8,2.9,1.6,2.5,2.6,2.9,0.7,3.3,1.1,2.3,2.6,2.5,1.5,1.5,2,2.5,0.6,3.3,2.8,3.5,1.3,2.6,2.9,1.9,2.1,2.9,3.9,3.4,1.9,3.4,1.3,3.5,0.7,3,1.8,1.8,3.6,1.5,1.3,0.8,2.1,3.6,2.5,3,2.5,2.9,1.9,2.3,0.9,1.3,1.3,3.7,3.6,4,2.5,2.5,2.3,1.8,3.7,3.9,1.9,3.7,1.1,3.3,3.3,1.6,1.3,3.5,3.7,1.9,3,3,1.2,3.3,1.4,3.3,2.8,3.5,2,3.3,3.1,1.2,1.5,3.3,1.9,3.4,3.8,2.8,2.5,1.1,1.4,0.9,2.2,2,2.2,0.8,2.7,2.6,3.4,0.5,1.5,3.9,1.5,1.5,2.5,3.4,1.4,0.8,1.1,0.6,1.1,3.2,2.9,4,3.5,2,1,1.1,1,2,1.3,3.1,3.7,1,4,2.5,3.5,3.3,2.4,1.6,1.1,3.2,2.2,1.3,2.4,2.3,3.5,0.9,3,1.2,1.4,3.8,0.9,0.6,3.6,2.7,0.7,1.7,3,3.6,0.5,0.8,3.3,3.1,2.5,4,2.1,0.8,3.2,2.3,3.8,0.6,0.8,0.7,1.2,1.1,0.8,3,0.8,0.6,1.9,3.7,1.6,0.5,3.7,1.2,0.5,2.8,2.7,3.7,4,0.8,1.9,0.8,2.4,3.4,3.4,0.6,1.8,3.6,3.2,1.1,2,1.4,2,2.2,2.2,3.9,2.2,0.7,3.3,2.8,1,1.3,0.8,2.1,3.1,3,2.2,1.2,2.5,2.5,3.2,3.7,1.4,3.7,2.3,3.8,3.5,2.8,2.8,2.7,3,2.5,3.9,0.5,3.4,3.6,3.8,2.2,2.9,0.6,2,3.4,2.5,3,3.8,2.2,2.7,1.4,3.3,2.3,2.7,2.2,3.5,1.7,3.5,2,1.8,3.2,2.5,1.1,2.7,0.7,0.6,0.9,2.9,2,1.2,1.8,3.1,1.1,1.6,2.9,1.4,1,2.8,0.9,1.9,3.4,3.2,0.6,2.9,2.3,0.8,4,1.3,2.1,1.1,2.9,1.5,3.2,0.9,1.5,0.6,3.7,2.2,4,2.4,0.9,3.8,2.3,2.5,1.8,2.6,1.9,3.4,2.7,1,0.9,0.8,3.3,1.3,3.2,3.9,0.5,3.8,1.2,3.1,1.3,1.5,3.8,1.7,0.6,0.9,1.1,0.7,1.5,3.1,1,0.8,2.2,3.7,2.4,3.4,1.6,2.6,1.5,3,1,2.5,1,3.5,2.4,2.4,3.2,2.9,2.8,2.3,1.4,3.9,3.6,3.1,2.6,2.7,3.3,4,3.6,1.6,2.4,3.8,2.9,1.6,2.6,3.9,2.9,0.7,3.3,1.1,2.7,2.6,2.5,1.5,1.5,2.5,1.7,2.5,3.3,2.8,3.5,0.9,2.6,3.9,1.1,1.9,2.9,3.9,3.4,3.9,3.4,1.3,3.5,0.7,3,1.8,1.8,3.6,1.5,1.3,2.5,2.1,3.6,2,3.4,2.8,4,1.3,3,3.6,1.4,2.5,0.7,1.6,3.6,2.1,3.5,2.9,1.2,1.7,3.8,2,2.8,0.8,1.7,3.4,3.1,3.1,0.6,0.5,2.9,3.4,0.9,2,3.6,3.6,3.9,2.8,3.5,0.7,2.2,0.7,0.6,1.7,2.1,2.6,2.6,2.4,2.4,3.9,0.6,2.6,3.3,2.1,2.5,2.4,2.9,1.9,2.3,0.9,1.3,3,3.6,3.6,4,2.5,2.5,2.3,2.4,1.8,1.9,1.8,1.1,1,3.3,0.6,1.6,1.1,3.5,3.7,3,0.6,3,1.1,1.6,1.4,3.3,0.9,3.5,2,3.1,2.9,1.2,1.5,2.2,1.9,3.4,3.8,1.5,2.5,0.5,1.1,0.9,1,0.7,3.6,3.9,0.7,3.7,3,1,3.6,0.8,3.3,3.1,2.5,3.1,4,0.8,3.2,2.3,3.8,0.6,3.7,1.6,0.7,0.8,3.7,3,1.9,3.5,1.9,3.7,1.6,3.8,3.7,3.9,0.5,2.8,2.6,3.7,4,0.8,1.9,3,2.4,3.4,3.4,1.8,3.4,3.6,3.2,1.1,0.8],"opacity":0.6,"name":"Not Chosen"}],"layout":{"barmode":"overlay","title":"Chosen and Not Chosen Price Histograms","xaxis":{"title":"chosenPrices"},"margin":{"b":40,"l":60,"t":25,"r":10}},"url":null,"width":null,"height":null,"base_url":"https://plot.ly","layout.1":{"barmode":"overlay","title":"Chosen and Not Chosen Price Histograms","xaxis":{"title":"chosenPrices"}},"filename":"Chosen and Not Chosen Price Histograms"},"evals":[]}</script><!--/html_preserve-->



## Eye Tracking



An aggregated and interactive visualization of fixations over the attributes is available here: (https://pawelb.shinyapps.io/chocolate_slider/) 





```r
cBrandFix <- data.frame(table(data$Brand_Fix,data$Chosen))
cTypeFix <- data.frame(table(data$Type_Fix,data$Chosen))
cPriceFix <- data.frame(table(data$Price_Fix,data$Chosen))


x <- list(
  title = "Brand_Fix",
  titlefont = f
)
y <- list(
  title = "Number Chosen",
  titlefont = f
)

plot_ly(
  x = cBrandFix$Var1
  , y = cBrandFix$Freq[which(cBrandFix$Var2==1)]
  , opacity = 0.6
  , type = "bar"
  , name = "Brand"
  ) %>%
add_trace(x=cTypeFix$Var1
          ,y=cTypeFix$Freq[which(cTypeFix$Var2==1)]
          ,name="Type"
          ) %>%
add_trace(x=cPriceFix$Var1
          ,y=cPriceFix$Freq[which(cPriceFix$Var2==1)]
          ,name="Price"
          )  %>%
layout(barmode="stack" , title = "Chosen by Price_Fix" , xaxis = x, yaxis = y)
```

<!--html_preserve--><div id="htmlwidget-9992" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-9992">{"x":{"data":[{"type":"bar","inherit":true,"x":["0","1","2","3","4","5","6","7","8","9","10","11","12","14","18","0","1","2","3","4","5","6","7","8","9","10","11","12","14","18"],"y":[43,65,77,53,52,29,14,7,3,3,1,1,1,0,1],"opacity":0.6,"name":"Brand"},{"type":"bar","inherit":true,"x":["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","18","20","25","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","18","20","25"],"y":[24,30,51,69,58,40,24,19,11,5,6,3,5,1,2,0,1,1,0],"opacity":0.6,"name":"Type"},{"type":"bar","inherit":true,"x":["0","1","2","3","4","5","6","7","0","1","2","3","4","5","6","7"],"y":[93,122,71,41,15,4,3,1],"opacity":0.6,"name":"Price"}],"layout":{"barmode":"stack","title":"Chosen by Price_Fix","xaxis":{"title":"Brand_Fix","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"margin":{"b":40,"l":60,"t":25,"r":10}},"url":null,"width":null,"height":null,"base_url":"https://plot.ly","layout.1":{"barmode":"stack","title":"Chosen by Price_Fix","xaxis":{"title":"Brand_Fix","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}},"yaxis":{"title":"Number Chosen","titlefont":{"family":"Arial, sans","size":18,"color":"#7f7f7f"}}},"filename":"Chosen by Price_Fix"},"evals":[]}</script><!--/html_preserve-->

#### So what?

This graphic visualizes the relation between the attribute and the number of times the individual look at each.  The eye tracking data indicates how many times the individual looked at each attribute before making a decision.  The biggest difference between the attributes is the range of values on the x-axis.  Price ranges from 0 to 6 whereas brand and type are looked at more often with range between 0 and 25.  



The following visual overlays two graphs that show how many chocolates were chosen and not chosen at given prices.  



```r
cBT <- xtabs(Chosen ~ brand + type , data=data)

chosenPrices <- data$Price[which(data$Chosen==1)]
notChosenPrices <- data$Price[which(data$Chosen==0)]

plot_ly(x=chosenPrices , opacity = 0.6 , type = "histogram" , name="Chosen") %>%
  add_trace(x=notChosenPrices , name="Not Chosen") %>%
  layout(barmode="overlay")
```

<!--html_preserve--><div id="htmlwidget-1313" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-1313">{"x":{"data":[{"type":"histogram","inherit":true,"x":[0.6,3.9,3.7,1,1.5,2.6,2.1,1.1,1.7,0.8,1.6,3.7,3,3.5,3.7,3.8,3.9,2.8,3.7,1.9,3,0.9,0.6,3.2,2,2.1,3.5,1.8,2.5,1.3,0.7,0.9,1.3,3.4,1.1,2.9,2.8,2.5,3.4,0.7,0.6,4,2.5,2.9,0.9,0.6,2.8,3.2,3.8,2.3,1,2.2,1.2,1.3,1,0.8,1.4,2.2,1.2,0.9,1.7,3.3,1,2.6,2.7,2.5,0.5,2.2,2.2,0.6,2.5,1.1,0.7,1.4,1.7,2.6,4,1.7,1.4,3,3.9,0.7,0.6,2.7,1.4,2.5,1.7,1.8,0.9,3.9,1.1,2.4,3.9,2.9,3.4,3.5,1.6,0.6,2.5,2,2.4,2,0.6,3,3.6,0.8,2.4,2.4,1.8,1,0.6,1.1,2,0.6,1.1,1.6,0.9,0.8,2.9,0.9,2.2,3.3,1.5,0.5,1,3.8,0.9,1.5,2.9,1.1,1.4,2.6,0.5,2.2,2.4,2.5,3,3.2,1.3,3.6,0.5,2.3,1,0.8,3,1.7,3.5,0.6,1.1,0.7,0.7,3.9,3.7,1,1.5,2.6,3.1,1.1,1.7,3.7,1.6,3.7,1.9,3.5,1.5,3.8,3.9,2.6,2.8,0.7,3,0.9,3.4,1.4,0.8,1,3.5,1.2,1.3,3.5,2.5,1.4,2.9,1.4,0.9,1.7,3.3,1,2.6,3,3,0.6,2.2,4,3.2,2.5,1.1,0.7,2.2,1.7,2.1,1.3,1.6,0.8,1.3,3.3,0.8,1.3,3.4,3.8,2.1,0.6,2.5,1.8,0.7,0.6,2.7,2.5,1.2,1.8,2.1,2.8,3.2,1.8,1.3,0.5,2.6,1.7,0.9,3.5,3.5,0.6,1.4,2.7,3.4,2.7,3,0.5,1,0.6,1.4,2.3,1.3,0.5,1,1.2,0.7,3.5,1.7,3,3.9,0.9,1.7,1.4,3,2.5,0.7,0.6,2.3,1.4,2,0.6,1.8,1.3,2.9,2.1,2.4,1.9,2.9,3.4,3.5,1.6,0.6,0.8,2.5,3.7,3.8,1.3,1.4,1.6,3.7,3.6,1.4,1.3,2.2,1.9,1.7,1.9,3.4,3,3.1,1.9,1.3,1.8,3.3,2.8,2,3.5,3.3,3,3,2,0.6,1.3,3.7,0.8,2.4,3.7,3.9,3.7,3.3,1.3,2,1.9,1.2,3.3,2.8,0.8,3.3,0.9,3.3,3.3,2.8,1.4,2.2,0.6,2.7,1.7,0.5,1.5,2.6,2.1,1.1,1.7,0.8,1.2,1.1,0.8,0.6,1.5,0.5,1.2,2.7,2.8,0.7,0.8,0.9,0.6,1.4,2],"opacity":0.6,"name":"Chosen"},{"type":"histogram","inherit":true,"x":[0.7,3.6,2.7,0.7,1.7,3,3.6,0.5,0.8,3.3,3.1,2.5,3.1,4,0.8,3.2,2.3,3.8,0.6,3.7,0.7,1.2,1.1,0.8,0.8,1.9,0.6,1.9,1.5,1.6,0.5,3.7,1.2,0.5,2.7,2.6,4,2.8,0.7,0.8,0.8,2.4,3.4,3.4,1.8,3.4,3.6,1.4,1.1,0.8,2.2,3.5,1.3,1.7,1.6,2,3.2,0.8,1.1,2.7,0.6,3.3,2.9,0.8,2,1.2,1.8,3.1,1.6,3.8,2.1,1.4,1,0.6,0.9,1.9,1.8,3.2,0.6,2.9,2.3,0.8,2.7,1.3,2.1,1.1,1.2,1.5,1.8,3.2,1.5,2.1,3.7,2.2,4,2.4,0.9,1.8,1.3,2.5,1.4,2,3.5,2.2,3.9,2.2,0.7,3.3,2.8,3.5,1.3,2.5,2.1,3.1,2.9,3,2.5,1.4,2.5,3.2,3.7,1.4,3.7,2.3,3.8,3.5,2.8,2.8,3,3,3,3.9,0.6,3.4,3.6,3.8,4,2.9,3.2,2,3.4,2.5,3,3.8,2.2,2.7,3.3,2.2,2.3,2.7,3.9,2.7,0.9,3.3,3.6,1.6,2.4,3.8,2.9,1.6,2.5,2.6,2.9,0.7,3.3,1.1,2.3,2.6,2.5,1.5,1.5,2,2.5,0.6,3.3,2.8,3.5,1.3,2.6,2.9,1.9,2.1,2.9,3.9,3.4,1.9,3.4,1.3,3.5,0.7,3,1.8,1.8,3.6,1.5,1.3,0.8,2.1,3.6,2.5,3,2.5,2.9,1.9,2.3,0.9,1.3,1.3,3.7,3.6,4,2.5,2.5,2.3,1.8,3.7,3.9,1.9,3.7,1.1,3.3,3.3,1.6,1.3,3.5,3.7,1.9,3,3,1.2,3.3,1.4,3.3,2.8,3.5,2,3.3,3.1,1.2,1.5,3.3,1.9,3.4,3.8,2.8,2.5,1.1,1.4,0.9,2.2,2,2.2,0.8,2.7,2.6,3.4,0.5,1.5,3.9,1.5,1.5,2.5,3.4,1.4,0.8,1.1,0.6,1.1,3.2,2.9,4,3.5,2,1,1.1,1,2,1.3,3.1,3.7,1,4,2.5,3.5,3.3,2.4,1.6,1.1,3.2,2.2,1.3,2.4,2.3,3.5,0.9,3,1.2,1.4,3.8,0.9,0.6,3.6,2.7,0.7,1.7,3,3.6,0.5,0.8,3.3,3.1,2.5,4,2.1,0.8,3.2,2.3,3.8,0.6,0.8,0.7,1.2,1.1,0.8,3,0.8,0.6,1.9,3.7,1.6,0.5,3.7,1.2,0.5,2.8,2.7,3.7,4,0.8,1.9,0.8,2.4,3.4,3.4,0.6,1.8,3.6,3.2,1.1,2,1.4,2,2.2,2.2,3.9,2.2,0.7,3.3,2.8,1,1.3,0.8,2.1,3.1,3,2.2,1.2,2.5,2.5,3.2,3.7,1.4,3.7,2.3,3.8,3.5,2.8,2.8,2.7,3,2.5,3.9,0.5,3.4,3.6,3.8,2.2,2.9,0.6,2,3.4,2.5,3,3.8,2.2,2.7,1.4,3.3,2.3,2.7,2.2,3.5,1.7,3.5,2,1.8,3.2,2.5,1.1,2.7,0.7,0.6,0.9,2.9,2,1.2,1.8,3.1,1.1,1.6,2.9,1.4,1,2.8,0.9,1.9,3.4,3.2,0.6,2.9,2.3,0.8,4,1.3,2.1,1.1,2.9,1.5,3.2,0.9,1.5,0.6,3.7,2.2,4,2.4,0.9,3.8,2.3,2.5,1.8,2.6,1.9,3.4,2.7,1,0.9,0.8,3.3,1.3,3.2,3.9,0.5,3.8,1.2,3.1,1.3,1.5,3.8,1.7,0.6,0.9,1.1,0.7,1.5,3.1,1,0.8,2.2,3.7,2.4,3.4,1.6,2.6,1.5,3,1,2.5,1,3.5,2.4,2.4,3.2,2.9,2.8,2.3,1.4,3.9,3.6,3.1,2.6,2.7,3.3,4,3.6,1.6,2.4,3.8,2.9,1.6,2.6,3.9,2.9,0.7,3.3,1.1,2.7,2.6,2.5,1.5,1.5,2.5,1.7,2.5,3.3,2.8,3.5,0.9,2.6,3.9,1.1,1.9,2.9,3.9,3.4,3.9,3.4,1.3,3.5,0.7,3,1.8,1.8,3.6,1.5,1.3,2.5,2.1,3.6,2,3.4,2.8,4,1.3,3,3.6,1.4,2.5,0.7,1.6,3.6,2.1,3.5,2.9,1.2,1.7,3.8,2,2.8,0.8,1.7,3.4,3.1,3.1,0.6,0.5,2.9,3.4,0.9,2,3.6,3.6,3.9,2.8,3.5,0.7,2.2,0.7,0.6,1.7,2.1,2.6,2.6,2.4,2.4,3.9,0.6,2.6,3.3,2.1,2.5,2.4,2.9,1.9,2.3,0.9,1.3,3,3.6,3.6,4,2.5,2.5,2.3,2.4,1.8,1.9,1.8,1.1,1,3.3,0.6,1.6,1.1,3.5,3.7,3,0.6,3,1.1,1.6,1.4,3.3,0.9,3.5,2,3.1,2.9,1.2,1.5,2.2,1.9,3.4,3.8,1.5,2.5,0.5,1.1,0.9,1,0.7,3.6,3.9,0.7,3.7,3,1,3.6,0.8,3.3,3.1,2.5,3.1,4,0.8,3.2,2.3,3.8,0.6,3.7,1.6,0.7,0.8,3.7,3,1.9,3.5,1.9,3.7,1.6,3.8,3.7,3.9,0.5,2.8,2.6,3.7,4,0.8,1.9,3,2.4,3.4,3.4,1.8,3.4,3.6,3.2,1.1,0.8],"opacity":0.6,"name":"Not Chosen"}],"layout":{"barmode":"overlay","xaxis":{"title":"chosenPrices"},"margin":{"b":40,"l":60,"t":25,"r":10}},"url":null,"width":null,"height":null,"base_url":"https://plot.ly","layout.1":{"barmode":"overlay","xaxis":{"title":"chosenPrices"}},"filename":"chosenPrices"},"evals":[]}</script><!--/html_preserve-->








