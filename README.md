# DS-Math


## Data Science Math Script

## Setup  ----------------

findDup <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

library(dplyr,warn.conflicts = FALSE)
library(ggplot2,warn.conflicts = FALSE)
library(forcats,warn.conflicts = FALSE)
library(tidyr,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)
library(zoo,warn.conflicts = FALSE)
library(lubridate,warn.conflicts = FALSE)
require(stringr)
library(bigrquery,warn.conflicts = FALSE)
library(shiny,warn.conflicts = FALSE)
library(shinydashboard,warn.conflicts = FALSE)
library(googledrive,warn.conflicts = FALSE)
library(googleVis,warn.conflicts = FALSE)

setwd("C:/Users/eric.reschke/Desktop/School/DS_Math/R")

CurrentMonth <- as.Date(as.yearmon(Sys.Date()))



## Week 1
## Quadratic Equation  ----------------

a <- 1
b <- -4
c <- -5

x_Addsqrt <- -b+sqrt((b^2) - (4*a*c))
x_Addsqrt <- x_Addsqrt/(2*a)

x_Subtractsqrt <- -b-sqrt((b^2) - (4*a*c))
x_Subtractsqrt <- x_Subtractsqrt/(2*a)


## Slopes  ----------------

## x-axis continuous values
x_MIN <- -7
x_MAX <- 7

## Establish main table
x_vals <- matrix(c(x_MIN:x_MAX))
slopeTableMain <- x_vals
colnames(slopeTableMain) <- "x"
slopeTableMain <- as.data.frame(slopeTableMain)
slopeTableMain <- slopeTableMain %>%
  mutate(x = as.numeric(x))

## f(x) #1 ----------
slopeTable1 <- slopeTableMain %>%
  mutate(y = abs(x))

fx1 <- ggplot(data=slopeTable1, aes(x=x, y=y)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=y,vjust=1.5)) +
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("f(x) = abs(x)") +
  ggtitle("slopeTable1")

ggplotly(fx1)
fx1

## f(x) #2 ----------
slopeTable2 <- slopeTableMain %>%
  mutate(y = ((x+1)*(x-3))/((x-6)*(x+2)))

fx2 <- ggplot(data=slopeTable2, aes(x=x, y=y)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("f(x) = (x+1)*(x-3)/(x-6)*(x+2)") +
  ggtitle("slopeTable2")

ggplotly(fx2)
fx2

## f(x) #3 ----------
slopeTable3 <- slopeTableMain %>%
  mutate(y = ((2*(x^2)))) %>%
  mutate(m = if_else(is.na(lead(y)),0,
                     (lead(y)-y)/(lead(x)-x),0))

fx3 <-ggplot(data=slopeTable3, aes(x=x, y=y)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=y,vjust=1.5))+
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("f(x) = 2x+8 when x<=-2; x^2 when x>-2") +
  ggtitle("slopeTable3")

fx3
ggplotly(fx3)


## Question #5 ----------
questionFiveTable <- slopeTableMain %>%
  mutate(y = ((x^2) - (x+30)))

fx4 <-ggplot(data=questionFiveTable, aes(x=x, y=y)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=y,vjust=1.5)) +
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("Revenue") +
  xlab("Number of Widgets (in thousands)") +
  ggtitle("Question #5")

ggplotly(fx4)
fx4

## m == slope
questionFiveTable <- questionFiveTable %>%
  mutate(m = if_else(is.na(lead(y)),0,
                           (lead(y)-y)/(lead(x)-x),0)) #%>%
  #mutate(sameSlope = m/x)




## Question #9 ----------
questionNineTable <- slopeTableMain %>%
  mutate(y = ((x^2) - (x+30)))

fx5 <-ggplot(data=questionFiveTable, aes(x=x, y=y)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=y,vjust=1.5)) +
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("Revenue") +
  xlab("Number of Widgets (in thousands)") +
  ggtitle("Question #9")

ggplotly(fx5)
fx5

## m == slope
questionFiveTable <- questionFiveTable %>%
  mutate(m = if_else(is.na(lead(y)),0,
                     (lead(y)-y)/(lead(x)-x),0)) #%>%
#mutate(sameSlope = m/x)






## Week 2
## Derivative single function  -----------

x = -1;

# f(x) = 
(0.25*(x^4)) - 
  (5/3*(x^3)) +
  (x^2) +
  (8*x) - 
  4

# rate of change (derivative) at that point
(4*.25)*(x^3) - 
  (3*(5/3))*(x^2) + 
  (2*x) + 
  8

## Derivative table -----------

## now make a table out of this function
## looking at the table where x=0 would avoid 
## polynomial-factoring...

## x-axis continuous values
x_MIN <- -4
x_MAX <- 7

## Establish main table
x_vals <- matrix(c(x_MIN:x_MAX));
derivativeTableMain <- x_vals;
colnames(derivativeTableMain) <- "x";
derivativeTableMain <- as.data.frame(derivativeTableMain);
derivativeTableMain <- derivativeTableMain %>%
  mutate(x = as.numeric(x))
##


# f(x): x^3 + x^2 -16x then x^2 - 10x ; f'(x): 3x^2 + 2x - 16 then 2x-10
Derivative_Function_Tbl <- derivativeTableMain %>%
  mutate(y = if_else(x<=4,round((x^3)+((x^2)-(16*x)),2),round((x^2)-(10*x),2)))   %>%
  mutate(derivative = if_else(x<=4,round(3*(x^2)+(2*x)-16),round((2*x)-10,2)))

Derivative_Function_Tbl_Zero <- Derivative_Function_Tbl %>%
  subset(derivative==0)

derGraph <-ggplot(data=Derivative_Function_Tbl, aes(x=x, y=derivative)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=derivative,vjust=1.5))+
  scale_x_continuous(breaks = seq(x_MIN,x_MAX,1)) +
  ylab("f(x)") +
  ggtitle("Derivative Tbl Graph")

derGraph
ggplotly(derGraph)






##  ----------------


















