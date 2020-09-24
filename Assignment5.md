PM566assignment5
================
Mingzhi Ye
9/20/2020

``` r
library(data.table)
library(leaflet)
library(tidyverse)
```

    ## -- Attaching packages ---------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.2     √ purrr   0.3.4
    ## √ tibble  3.0.3     √ dplyr   1.0.2
    ## √ tidyr   1.1.1     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.5.0

    ## -- Conflicts ------------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readr)
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

Data Wrangling
--------------

Merge
=====

``` r
ind <- data.table::fread("C:/Users/yemin/Desktop/PM566/week5/01_chs/chs_individual.csv")
reg <- data.table::fread("C:/Users/yemin/Desktop/PM566/week5/01_chs/chs_regional.csv")
chs<-merge(x=ind,y=reg,
           by.x="townname", by.y="townname",
           all.x=TRUE,all.y=FALSE)
```

impute data
===========

I'll implement the imputation to variables "gasstove" and "smoke" and "fev" and "bmi"

``` r
chs[,ibmi:=mean(bmi,na.rm=TRUE),by=.(male, hispanic)]
chs[,ifev:=mean(fev,na.rm=TRUE),by=.(male, hispanic)]
chs[,igasstove := round(mean(gasstove,na.rm=TRUE)),by=.(male,hispanic)]
chs[,ismoke := round(mean(smoke,na.rm=TRUE)),by=.(male,hispanic)]

chs[,bmi:=ifelse(is.na(bmi),ibmi,bmi)]
chs[,fev:=ifelse(is.na(fev),ifev,fev)]
chs[,gasstove:=ifelse(is.na(gasstove),igasstove,gasstove)]
chs[,smoke:=ifelse(is.na(smoke),ismoke,smoke)]
```

step1
=====

``` r
nrow(ind)==nrow(chs)
```

    ## [1] TRUE

step2
=====

``` r
chs <- chs[,obesity_level := case_when(bmi < 14 ~ "underweight",
                       bmi < 22 & bmi >= 14 ~ "normal",
                       bmi < 24 & bmi >= 22 ~ "overweight",
                       bmi >= 24 ~ "obese",
                       TRUE ~ NA_character_)]
chs[, minbmi := min(bmi),by=obesity_level][, maxbmi := max(bmi),by=obesity_level][, n := .N ,by=obesity_level][, .SD[1],by=obesity_level][,.(obesity_level,maxbmi,minbmi,n)]
```

    ##    obesity_level   maxbmi   minbmi   n
    ## 1:        normal 21.96387 14.00380 975
    ## 2:    overweight 23.99650 22.02353  87
    ## 3:         obese 41.26613 24.00647 103
    ## 4:   underweight 13.98601 11.29640  35

Step 3
======

``` r
chs <-
  chs[,smoke_gas_exposure := case_when(
  smoke == 0 & gasstove == 0 ~ "none",
                       smoke == 1 & gasstove == 0 ~ "smoke",
                       smoke == 0 & gasstove == 1 ~ "gasstove",
                       smoke == 1 & gasstove == 1 ~ "both",
                       TRUE ~ NA_character_)]
table(chs$smoke_gas_exposure)
```

    ## 
    ##     both gasstove     none    smoke 
    ##      154      791      219       36

step4
=====

``` r
chs[, average_fev := mean(fev, na.rm=TRUE), by=townname][, sd_fev := sd(fev,na.rm=TRUE), by=townname][,.(townname,average_fev,sd_fev)][,.SD[1],by=townname]
```

    ##          townname average_fev   sd_fev
    ##  1:        Alpine    2087.101 291.1768
    ##  2:    Atascadero    2075.897 324.0935
    ##  3: Lake Elsinore    2038.849 303.6956
    ##  4:  Lake Gregory    2084.700 319.9593
    ##  5:     Lancaster    2003.044 317.1298
    ##  6:        Lompoc    2034.354 351.0454
    ##  7:    Long Beach    1985.861 319.4625
    ##  8:     Mira Loma    1985.202 324.9634
    ##  9:     Riverside    1989.881 277.5065
    ## 10:     San Dimas    2026.794 318.7845
    ## 11:   Santa Maria    2025.750 312.1725
    ## 12:        Upland    2024.266 343.1637

``` r
chs[, average_fev := mean(fev, na.rm=TRUE), by=male][, sd_fev := sd(fev,na.rm=TRUE), by=male][,.(male,average_fev,sd_fev)][,.SD[1],by=male]
```

    ##    male average_fev   sd_fev
    ## 1:    0    1958.911 311.9181
    ## 2:    1    2103.787 307.5123

``` r
chs[, average_fev := mean(fev, na.rm=TRUE), by=obesity_level][, sd_fev := sd(fev,na.rm=TRUE), by=obesity_level][,.(obesity_level,average_fev,sd_fev)][,.SD[1],by=obesity_level]
```

    ##    obesity_level average_fev   sd_fev
    ## 1:        normal    1999.794 295.1964
    ## 2:    overweight    2224.322 317.4261
    ## 3:         obese    2266.154 325.4710
    ## 4:   underweight    1698.327 303.3983

``` r
chs[, average_fev := mean(fev, na.rm=TRUE), by=smoke_gas_exposure][, sd_fev := sd(fev,na.rm=TRUE), by=smoke_gas_exposure][,.(smoke_gas_exposure,average_fev,sd_fev)][,.SD[1],by=smoke_gas_exposure]
```

    ##    smoke_gas_exposure average_fev   sd_fev
    ## 1:               none    2056.693 328.7843
    ## 2:              smoke    2055.714 295.6475
    ## 3:           gasstove    2022.671 319.3449
    ## 4:               both    2024.778 300.6313

The imputation had been done before step 1

Looking at the Data
-------------------

step1
=====

``` r
chs %>%
  ggplot()+
  geom_point(aes(x=fev,y=bmi))+
  facet_wrap(~townname)+
  labs(title="BMI~FEV group by town", x="FEV",y="BMI")
```

![](Assignment5_files/figure-markdown_github/unnamed-chunk-8-1.png)

step2
=====

``` r
chs %>%
  ggplot()+
  geom_histogram(aes(x=fev,fill=obesity_level))+
  scale_fill_brewer(palette="Spectral")+
  labs(title="Distribution of FEV by bmi category", x="FEV")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Assignment5_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
chs %>%
  ggplot()+
  geom_histogram(aes(x=fev,fill=smoke_gas_exposure))+
  labs(title="Distribution of FEV by smoke&gas exposure category", x="FEV")+
  scale_fill_manual(name="Smoke&gas exposure", labels=c("both", "gasstove","none","smoke"), values=c("both"="blue", 
                                                                                                      "gasstove"="lightblue",
                                                                                                      "none"="white",
                                                                                                      "smoke"="orange"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Assignment5_files/figure-markdown_github/unnamed-chunk-9-2.png)

step3
=====

``` r
chs %>%
  ggplot()+
  geom_bar(aes(x=obesity_level,fill=smoke_gas_exposure))+
  labs(title="Distribution of BMI categories by smoke&gas exposure category", x="BMI categories")+
  scale_fill_manual(name="Smoke&gas exposure", labels=c("both", "gasstove","none","smoke"), values=c("both"="blue", 
                                                                                                      "gasstove"="lightblue",
                                                                                                      "none"="white",
                                                                                                      "smoke"="orange"))
```

![](Assignment5_files/figure-markdown_github/unnamed-chunk-10-1.png)

step4
=====

``` r
chs %>%
  ggplot(aes(x=bmi,y=fev))+
  geom_point(aes(color=smoke_gas_exposure))+
  geom_smooth(method = lm)+
  labs(title = "FEV~BMI",x="FEV",y="BMI")+
  scale_color_manual(name="Smoke&gas exposure", labels=c("both", "gasstove","none","smoke"), values=c("both"="blue", 
                                                                                                      "gasstove"="lightblue",
                                                                                                      "none"="white",
                                                                                                      "smoke"="red"))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Assignment5_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
chs %>%
  ggplot(aes(x=bmi,y=fev))+
  geom_point(aes(color=smoke_gas_exposure))+
  geom_smooth(method = lm,aes(color =smoke_gas_exposure))+
  labs(title = "FEV~BMI grouped by Smoke&gas exposure",x="FEV",y="BMI")+
  scale_color_manual(name="Smoke&gas exposure", labels=c("both", "gasstove","none","smoke"), values=c("both"="blue", 
                                                                                                      "gasstove"="lightblue",
                                                                                                      "none"="white",
                                                                                                      "smoke"="red"))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Assignment5_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
chs %>%
  ggplot(aes(x=smoke_gas_exposure,y=fev))+
  geom_boxplot()+
  labs(title = "FEV~Smoke&gas exposure",x="BMI",y="FEV")
```

![](Assignment5_files/figure-markdown_github/unnamed-chunk-11-3.png)

step5
=====

``` r
chs5 <- chs[,.(pm25_mass,lat,lon,townname)][,.SD[1],by=townname]

leaflet(chs5) %>%
  addProviderTiles('OpenStreetMap') %>% 
  addCircles(lat=~lat, lng=~lon, color=~c("red"),
             opacity=0.5, fillOpacity=0.5, radius=~pm25_mass*300) %>%
  addLegend('bottomleft',
            title='PM2.5 mass in different towns', opacity=1,colors = c("red"),labels = c("Amount of PM2.5 mass"))
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-5d0390eb51827b88033a">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[32.8350521,35.4894169,33.6680772,34.242901,34.6867846,34.6391501,33.7700504,33.9845417,33.9806005,34.1066756,34.9530337,34.09751],[-116.7664109,-120.6707255,-117.3272615,-117.275233,-118.1541632,-120.4579409,-118.1937395,-117.5159449,-117.3754942,-117.8067257,-120.4357191,-117.6483876],[2622,2244,3705,2298,2550,1788,5736,8991,6717,6156,2157,6738],null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.5},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addLegend","args":[{"colors":["red"],"labels":["Amount of PM2.5 mass"],"na_color":null,"na_label":"NA","opacity":1,"position":"bottomleft","type":"unknown","title":"PM2.5 mass in different towns","extra":null,"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[32.8350521,35.4894169],"lng":[-120.6707255,-116.7664109]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
step6
=====

According to the graphs below, we can conclude that there is significant linear relationship between PM2.5 mass and FEV. And the regression is good in linearity, normality and homoscedasticity.

``` r
library(ggfortify)
chs %>%
  ggplot(aes(x=pm25_mass,y=fev))+
  geom_point()+
  geom_smooth(method = lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Assignment5_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
model6<-lm(fev~pm25_mass,data = chs)
model6 %>%
  autoplot(which=1:3)
```

    ## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
    ## Please use `arrange()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](Assignment5_files/figure-markdown_github/unnamed-chunk-13-2.png)

``` r
summary(model6)
```

    ## 
    ## Call:
    ## lm(formula = fev ~ pm25_mass, data = chs)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1070.63  -206.57   -13.33   195.87  1277.65 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2073.452     19.309 107.383   <2e-16 ***
    ## pm25_mass     -3.016      1.184  -2.548    0.011 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 317.3 on 1198 degrees of freedom
    ## Multiple R-squared:  0.005389,   Adjusted R-squared:  0.004559 
    ## F-statistic: 6.492 on 1 and 1198 DF,  p-value: 0.01096
