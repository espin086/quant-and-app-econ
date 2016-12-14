######################################################
#The following code performs a marketing mix model
        #1) Measures marketing effectiveness
        #2) Uses effective measures along with costs in optimization
######################################################

#Importing marketing and sales data
setwd("/Users/jjespinoza/Documents/quant-and-app-econ")
df <- read.csv("advertising.csv")

model <- lm(Sales ~ TV + Radio, data = df) #Modeling marketing's impact on sales

radio <- model[[1]][[3]] #Regression coefficients for radio's impact on sales
TV <- model[[1]][[2]]    #Regression coefficients for TV's impact on sales

######################################################
#2) Combine regression output with optimization model
library(linprog)

#Objective - maximize sales by pulling the ratio & TV levers
cvec <- c(Radio = radio, 
          TV = TV)  

## Constraints (quasi-fix factors)
bvec <- c( RadioBudget = 23.275,
           TVBudget = 147.04,
           TotalBudget = 170.315) #Add or remove 3.65 for marketing mix problem

## Needs of Production activities
Amat <- matrix(0, length(bvec), length(cvec))
rownames(Amat) <- names(bvec)
colnames(Amat) <- names(cvec)

#specifying LHS of radio budget constraint
Amat["RadioBudget", "Radio"] <- 1
Amat["RadioBudget", "TV"] <- 0

#specifying LHS of TV budget constraint
Amat["TVBudget", "Radio"] <- 0
Amat["TVBudget", "TV"] <- 1

#specifying LHS of Total budget constraint
Amat["TotalBudget", "Radio"] <- 1
Amat["TotalBudget", "TV"] <- 1

#Solving the linear program
lp <- solveLP(cvec, bvec, Amat, maximum = TRUE )
lp$opt + model[[1]][[1]]


######################################################
#Grapically representing the constraints




######################################################
#Are we spending the right amount of money?

#Q: what if we increased the budget by 10%
bvec.2 <- c(800*1.1, 400*1.1, 1000*1.1)
lp.2 <- solveLP(cvec, bvec.2, Amat, maximum = TRUE )
lp.2


######################################################
#Q Are investing in the right areas?

tv.mix.now <- (sum(df$TV)/(sum(df$TV) + sum(df$Radio)))
radio.mix.now <- sum(df$Radio)/(sum(df$TV) + sum(df$Radio))
mix.now <- data.frame(TV = tv.mix.now, Radio = radio.mix.now)

tv.mix.opt <- (lp$solution[[2]]/(lp$solution[[2]] + lp$solution[[1]]))
radio.mix.opt <- (lp$solution[[1]]/(lp$solution[[2]] + lp$solution[[1]]))
mix.opt <- data.frame(TV = tv.mix.opt, Radio = radio.mix.opt)

predict(model, mix.now)
predict(model, mix.opt)

#Rebalancing the mix yields a 4% investment
(predict(model, mix.opt) - predict(model, mix.now))/predict(model, mix.now)

######################################################
#Q What if costs of TV advertising goes up by 20%?  What are our best options and what is the net impact on our sales?

Amat.2 <- rbind( c(  10,   0), #Radio budet constraint (each ad costs $10)
               c(  0,   30*1.2), #TV budget constraint (each ad costs $30)
               c( 10,    30*1.2) ) #Total marketing budget

lp.3 <- solveLP(cvec, bvec, Amat.2, maximum = TRUE )
lp.3

#Base Optimization = 24.8m 
#This Optimization: 24.4m

#Base inputs: Radio = 130k & TV = 7k
#These inputs: Radio = 130k & TV = 0K

######################################################
#Q What if costs of radio advertising goes up by 10%?  What are our best options and what is the net impact on our sales?

Amat.3 <- rbind( c(  10*1.1,   0), #Radio budet constraint (each ad costs $10)
                 c(  0,   30), #TV budget constraint (each ad costs $30)
                 c( 10*1.1,    30) ) #Total marketing budget

lp.4 <- solveLP(cvec, bvec, Amat.3, maximum = TRUE )
lp.4

#Base Optimization = 24.8m 
#This Optimization: 22.6m

#Base inputs: Radio = 130k & TV = 7k
#These inputs: Radio = 118k & TV = O



######################################################
#Q Corporate is likely to cap our share of radio advertising to no more than 60% of the budget, what will be the impact?


bvec.3 <- c(1300, 700, 1500, 1500*.60)  # Budgets for marketing tactives
names(bvec.3) <- c("Radio Budget","TV Budget","Total Budget", "Radios Share")

Amat.4 <- rbind( c(10,0), #Radio budet constraint (each ad costs $10)
                 c(0,30), #TV budget constraint (each ad costs $30)
                 c(10,30),#Total marketing budget
                 c(10,0)) #Share of radio to total

lp.5 <- solveLP(cvec, bvec.3, Amat.4, maximum = TRUE )
lp.5


#Base Optimization = 24.8m 
#This Optimization: 17.9m

#Base inputs: Radio = 130k & TV = 7k
#These inputs: Radio = 90k & TV = 20K





