
##########################################################################################
############################ SET WORKING DIRECTORY ######################################
##########################################################################################

setwd("~/Desktop/PatchChoiceModel/NEW Manuscript Model")

##########################################################################################
######################### INSTALL PACKAGES & LIBRARIES ###################################
##########################################################################################

#install.packages("car")
library("car")

library(lattice)

#install.packages("ggplot2")
library("ggplot2")

#install.packages("dplyr")
library("dplyr")
library("reshape2")

#install.packages("truncnorm")
library(truncnorm)

library(RColorBrewer)

#install.packages("gridExtra")
library(gridExtra)
library(grid)

#install.packages("zoo")
library(zoo)

#################################################################################################################
############################## DESCRIPTION OF SAVED RESULTS IN PROJECT #########################################
#################################################################################################################

## fmax.results.matrix - optimality results -
  # for all initial parameters: patch matrix, time, i.patch, energy, previous patch depletion, this matrix returns...
  # optimal d.patch, d.patch.type, d.travelled, foraging effort, expected future fitness given these foraging decisions.

## fmax.df - optimality results in a dataframe


## MODEL PREP

#################################################################################################################
############################## SET UP FMAX MATRIX THAT HOLDS ALL OPTIMALITY RESULTS ##############################
#################################################################################################################

## Define the parameters, number of time steps, number of patches and maximum energy value

tmax = 21
patchno = 25
energymax = 100
pmmax = 100
depletions = 5

## results.matrix is a matrix with 7 dimensions. The 7th dimension has 4 dimensions
## (1)patch matrix, (2)time, (3)energy, (4)initial patch 
# (5.1)d.patch, (5.2)d.patch.type, (5.3)distance travelled to patch, (5.4)foraging effort, (5.5)fitness 

fmax.results.matrix <- array(NA, dim = c(pmmax, tmax, patchno, energymax, depletions, 5))

## generate fitness funciton associated with energy outcomes
## fitness is a function of energy
## store this relationship in fmax.results.matrix, which contains all combos of initial parameters (above) at the last time step
## we start at the last time step b/c this is where the looped model will start, it will work backward and generate new fitness functions for each time step
## this is the fitness function we use for the first loop (the last time step)


##################################################################################################
############################ SET UP FITNESS - END OF TIME ########################################
##################################################################################################

for(pm in 1:pmmax){
    for(ip in 1:patchno){
      for (e in 1: energymax){
        for(d in 1:depletions){

      fmax.results.matrix[pm, tmax, ip, e, d, 5] <- 1- exp(-(e*0.04))
        }
      }
    }
  }

## Plots energy by fitness for the last time step

x <- c(1:100)

plot((fmax.results.matrix[1, tmax,1 , x, 1, 5]) ~ x, xlab = "Energy", ylab = "Expected Future Fitness")

## column names of fmax.results.matrix
##colnames(fmax.results.matrix) <- c("time", "i.patch", "energy", "d.patch (patch destination)", 
##"f.eff (foraging effort)", "e.f.f (expected future fitness)")

#################################################################################################################
############################## SET UP DECISION MATRIX THAT HOLDS ALL SIMULATION RESULTS   ###################
#################################################################################################################

### create a decision matrix for to record the optimal decision at each time step (20 time steps)
# this is for a given initial patch and a given initial energy
# columns: (1) row  (2) pmatrix  (3) time step  (4) i.patch  (5) i.energy  
    #(6) d.patch  (7) d.patch.type.  (8) d.travelled  (9) fe  (10) f.energy  (11) fate

decision.matrix <- matrix(0,2000,14)

## fill first row of decision matrix with a row #
## this way we can be sure order of rows wasn't changed accidentally anywhere
decision.matrix[ ,1] <- c(1:2000)

colnames(decision.matrix) <- c("row", "pmatrix", "time", "i.patch", "i.p.depletion", "i.energy", "d.patch", "d.patch.type", 
                               "d.p.depletion",  "d.travelled", "fe", "f.energy", "new.p.depletion", "fate")


#################################################################################################################
############################## SET UP NORMAL DISTRIBUTION OF ENERGY STATES ######################################
#################################################################################################################

## use energy reserves from a normal distribution

## how to decide on the standard deviation for the distribution??
energy.states <- rtruncnorm(n=100, a=0, b=100, mean=50, sd=15)
energy.states <- round(energy.states, digits = 0)
histogram(energy.states)

#min(energy.states)
#max(energy.states)


## MODELS RUN

#################################################################################################################
############################## MODEL - OPTIMALITY & SIMULATION - 100 ITERATIONS #################################
#################################################################################################################

# SET UP OUTER MOST LOOP - 200 ITERATIONS

## start with first patch matrix (pmatrix = 1)
# we run through as many loops as there are patch matrices (200)
# we chose 200 b/c we have 200 initial energy values in a normal distrubution
# each energy value represents a unique animal that has a unique home are (patch matrix)

p.matrix <- 1

# this counts the # of times we're going through the forward model so that we can add to bottom of decision matrix
## this is the same as p.matrix.counter - look into just using the p.matrix.counter.
forward.counter <- 0

for (p.matrix in 1:pmmax){

#################################################################################################################
############################## SET UP PATCH MATRIX #############################################################
#################################################################################################################  
  
## generate a matrix with patch numbers - 25 patches

patch.matrix <- matrix(0,25,8)

### define levels of risk 

low.risk <- 0.00025

mod.risk <- 0.00375

high.risk <- 0.005

## Generate vectors that represent different types of habitat patches
## Each value in the vector represents the following parameter

## 1 - predation risk (0 - 1)
## 2 - probability of finding food (0 - 1)
## 3 - cost of foraging/remaining in patch (1 - 5)
## 4 - energy gain from foraging in patch (1 - 10)

home.vector <- c(0, 0, 6, 0)

safe.vector <- c(low.risk, 0.85, 6, 8)

moderate.vector <- c(mod.risk, 0.92, 6, 12)

risky.vector <- c(high.risk, 0.99, 6, 16)


## original

#safe.vector <- c(0.0025, 0.85, 6, 8)

#moderate.vector <- c(0.00375, 0.92, 6, 12)

#risky.vector <- c(0.005, 0.99, 6, 16)

## low food (safe - risky): 8, 6, 4

## high predation risk (safe - risky): 0.005, 0.00625, 0.0075

## Generate a habitat matrix of the 3 types of habitat patches
## Not including home

habitat.matrix <- rbind(safe.vector, moderate.vector, risky.vector)

## Randomly sample, with replacement, rows (habitat types) from within the habitat matrix, 25 times
## And put these 25 rows into the patches matrix to generate the habitat space

patch.matrix <- habitat.matrix [ sample ( nrow(habitat.matrix), size = 25, replace = TRUE),]

## Add a column to the left of the matrix that numbers the rows (patches)
## Now I can see configuration of habitat
patch.matrix <- cbind(c(1:25), patch.matrix)

## Assign patch 13 to be home. This is the center of the matrix
patch.matrix[13,c(2:5)] <- c(0, 0, 6, 0)

## Remove the row names of the patch matrix
rownames(patch.matrix) <- NULL

## To calculate distance traveled between any two patches...
## Add three columns to the matrix
## One for the row number and one for the column number

patch.matrix <- cbind(patch.matrix, rep(0,25), rep(0,25), rep(0,25))

## A loop to fill in the row number of each patch. 
## Row number will be stored in column 6 of patch martrix
## Start the loop counter at 1
count <- 1
marker <- 0

for (n in 1:5){
  patch.matrix[c(count:(count+4)), 6] = 1 + marker
  count <- count+5
  marker <- marker+1
}

## Assign column number of each patch. 
## Column number will be stored in column 7 of patch matrix
patch.matrix[ ,7] <- rep(c(1:5),5)


## If then statement filling patch type in column 8

x <- 0

for (n in 1:25){
  
x <- patch.matrix[n,2]

  if(x == high.risk){
  patch.type = 30

  }else if(x == mod.risk){
      patch.type = 20
  
  }else if(x == low.risk){
      patch.type = 10

  }else if(x == 0){
      patch.type = 0

  }else{
  
  patch.type = NA}
 
  
patch.matrix[n,8] <- patch.type

}

## These are the column names of the final patch matrix
## p.ff = probability of finding food
## c.f. = cost of foraging in the patch
#colnames(patch.matrix) <- c("patch", "risk", "p.ff", "c.f", "e.gain", "row", "column", patch.type)

## Calculate and store distances between patches in a matrix
## Column and row numbers represent patch numbers
## This loop pulls coordinates from patch matrix and calculates distance for every possible choice (move between 2 patches)

distance.matrix <- matrix(0,25,25)
distance <- 0

for (n in 1:25){
  for(m in 1:25){
    distance <- sqrt((patch.matrix[m,6]-patch.matrix[n,6])^2 + (patch.matrix[m,7]-patch.matrix[n,7])^2)
    distance.matrix[m,n] <- distance
  }
}

## Probability of surviving travel between patch
## Here is a matrix of the probability of mortality while traveling between any two patches

## 5.66 is maximum distance travelled across 2 patches within matrix
# so this makes maximum risk about 0.12
# risk to travel mean distance is about 0.0054
# these are similar to moderate risky patch and very risky patch (within similar continuum)

distance.risk.matrix <- matrix(0,25,25)
travel.risk <- 0

for (n in 1:25) {
    for (m in 1:25){
      travel.risk <- (distance.matrix[n,m])*0.0004
      distance.risk.matrix[n,m] <- travel.risk
    }
  }


#max(distance.risk.matrix)
#plot(distance.risk.matrix ~ distance.matrix, xlab = "Distance Travelled", ylab = "Incurred Risk")


## Energetic cost of traveling between patches
## Here is a matrix of the cost of traveling between any two patches

distance.cost.matrix <- matrix(0,25,25)
travel.cost <- 0

for (n in 1:25) {
  for (m in 1:25){
    travel.cost <- (distance.matrix[n,m])*2
    distance.cost.matrix[n,m] <- travel.cost
  }
}

#max(distance.cost.matrix)
#plot(distance.cost.matrix ~ distance.matrix, xlab = "Distance Travelled", ylab = "Energy Cost")


##################################################################################################
################ SET UP OUTCOME MATRIX TO STORE RESULTS WITHIN OPTIMALITY MODEL ##################
##################################################################################################

## generate a martrix to store all possible outcomes for one initial patch and one initial state (the inner most model loop)
## we'll see 25 possible outcomes because there are 25 possible desitnation patches
## column 1 is destination patch
## column 3 is patch type (0 = home, 1 = safe, 2 = moderate, 3 = risky)
## colunm 3 is expected future fitness

## we are omitting the outcome matrix to add patch depletion to the model
outcome.matrix <- matrix(0,25,3)
outcome.matrix[ ,1] <- patch.matrix[ ,1]

colnames(outcome.matrix) <- c("d.patch", "fe", "fitness.outcome")

##################################################################################################
######################## SET UP DISTRIUBTION OF PATCH DEPLETION #################################
##################################################################################################

## generate skewed disribution of possible depletions - 24 total, b/c home can't be depleted
# numbers represent proportion of patch depletion 0 - 1
# depletion influences probability of finding food, not the amount of that's in the patch

possible.depletions <- c(1,.75,.5,.5,.5,.5,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,0,0,0,0,0,0,0,0)
hist(possible.depletions)
m.d <- mean(possible.depletions)

##################################################################################################
############################ OPTIMALITY MODEL LOOP BEGINS ########################################
##################################################################################################

i.patch <- 0 #patch initial
energy <- 0
d.patch <- 0 #patch destination
time.step <- 0
prev.p.depletion <- 0

  ## This loops through all time steps
  for (time.step in 20:1){
  
    ## This loops through all initial patches
    for (i.patch in 1:patchno){
    
      ## This loops through all initial energy states
      for (energy in 1:energymax){
      
        ## This loops through all possible depletions of previous patch visited.
        for(prev.p.depletion in 1:depletions){
      
          ## Calculating final energy value for each foraging effort within each destination patch
          for (d.patch in 1:patchno){
        
        
          
###############################################################################################################          
#####################################  if i.patch (does not equal) d.patch ##################################################   
###############################################################################################################           
          
  if(i.patch != d.patch){
          
               
          ############################################################################################################# 
          ################################## Fitness Outcome 1, No Foraging (0%) #####################################
          #############################################################################################################
    
          ## new energy state if find food
          ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
          new.energy.food.1 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + patch.matrix[d.patch,5]*0)
          
          ## new energy state if do not find food
          ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost)  
          new.energy.nofood.1 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4])
          
          ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
          
          if(new.energy.food.1 > energymax){
            new.energy.food.1 = energymax
          }else{
            if(new.energy.food.1 < 0){
              new.energy.food.1 = 0
            }else{
              new.energy.food.1 = new.energy.food.1
            }
          }
          
          
          if(new.energy.nofood.1 > energymax){
            new.energy.nofood.1 = energymax
          }else{
            if(new.energy.nofood.1 < 0){
              new.energy.nofood.1 = 0
            }else{
              new.energy.nofood.1 = new.energy.nofood.1
            }
          }
          
        ################################## Fitness Outcome 1, No Foraging (0%) #####################################
        ############################ Fitness Outcome 1.1, Depletion = 1 (0%) ####################################
          
          ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
          
          ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
          ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
          
          ## if new energy after finding food is less than 1 then both terms in equation are 0
          ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
          ##(see above notes) and the no food term is 0
          ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
          ## terms are calculated by their formulas (see above).
          
          if(new.energy.food.1 < 1){
            find.food.term.1.1 = 0
            no.find.food.term.1.1 = 0
          }else{
            if(new.energy.nofood.1 < 1){
              find.food.term.1.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                   fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.1 <- 0                                     
            }else{
              find.food.term.1.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                   fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.1 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0))) * 
                                      fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
            }}
          
          
          ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
          ## p.survival.term = prob. surviving travel * prob. surviving foraging
          
          p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
          
          outcome.1.1 <- p.survival.term * (find.food.term.1.1 + no.find.food.term.1.1)
          
          
          
        ################################## Fitness Outcome 1, No Foraging (0%) #####################################
        ############################ Fitness Outcome 1.2, Depletion = 2 (25%) ####################################
          
          ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
          
          ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
          ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
          
          ## if new energy after finding food is less than 1 then both terms in equation are 0
          ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
          ##(see above notes) and the no food term is 0
          ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
          ## terms are calculated by their formulas (see above).
          
          if(new.energy.food.1 < 1){
            find.food.term.1.2 = 0
            no.find.food.term.1.2 = 0
          }else{
            if(new.energy.nofood.1 < 1){
              find.food.term.1.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.2 <- 0                                     
            }else{
              find.food.term.1.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.2 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.25))) * 
                                          fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
            }}
          
          
          ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
          ## p.survival.term = prob. surviving travel * prob. surviving foraging
          
          p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
          
          outcome.1.2 <- p.survival.term * (find.food.term.1.2 + no.find.food.term.1.2)
          
      
          
          ################################## Fitness Outcome 1, No Foraging (0%) #####################################
          ############################ Fitness Outcome 1.3, Depletion = 3 (50%) ####################################
          
          ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
          
          ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
          ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
          
          ## if new energy after finding food is less than 1 then both terms in equation are 0
          ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
          ##(see above notes) and the no food term is 0
          ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
          ## terms are calculated by their formulas (see above).
          
          if(new.energy.food.1 < 1){
            find.food.term.1.3 = 0
            no.find.food.term.1.3 = 0
          }else{
            if(new.energy.nofood.1 < 1){
              find.food.term.1.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.3 <- 0                                     
            }else{
              find.food.term.1.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.3 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.50))) * 
                                          fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
            }}
          
          
          ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
          ## p.survival.term = prob. surviving travel * prob. surviving foraging
          
          p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
          
          outcome.1.3 <- p.survival.term * (find.food.term.1.3 + no.find.food.term.1.3)
          
          
          ################################## Fitness Outcome 1, No Foraging (0%) #####################################
          ############################ Fitness Outcome 1.4, Depletion = 4 (75%) ####################################
          
          ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
          
          ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
          ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
          
          ## if new energy after finding food is less than 1 then both terms in equation are 0
          ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
          ##(see above notes) and the no food term is 0
          ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
          ## terms are calculated by their formulas (see above).
          
          if(new.energy.food.1 < 1){
            find.food.term.1.4 = 0
            no.find.food.term.1.4 = 0
          }else{
            if(new.energy.nofood.1 < 1){
              find.food.term.1.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.4 <- 0                                     
            }else{
              find.food.term.1.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.4 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.75))) * 
                                          fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
            }}
          
          
          ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
          ## p.survival.term = prob. surviving travel * prob. surviving foraging
          
          p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
          
          outcome.1.4 <- p.survival.term * (find.food.term.1.4 + no.find.food.term.1.4)
          
          
        ################################## Fitness Outcome 1, No Foraging (0%) #####################################
        ############################ Fitness Outcome 1.5, Depletion = 5 (100%) ####################################
          
          ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
          
          ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
          ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
          
          ## if new energy after finding food is less than 1 then both terms in equation are 0
          ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
          ##(see above notes) and the no food term is 0
          ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
          ## terms are calculated by their formulas (see above).
          
          if(new.energy.food.1 < 1){
            find.food.term.1.5 = 0
            no.find.food.term.1.5 = 0
          }else{
            if(new.energy.nofood.1 < 1){
              find.food.term.1.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.5 <- 0                                     
            }else{
              find.food.term.1.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                       fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
              no.find.food.term.1.5 <- ((1 - (patch.matrix[d.patch,3] * (1 - 1))) * 
                                          fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
            }}
          
          
          ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
          ## p.survival.term = prob. surviving travel * prob. surviving foraging
          
          p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
          
          outcome.1.5 <- p.survival.term * (find.food.term.1.5 + no.find.food.term.1.5)
          
          
        ################################## Fitness Outcome 1, No Foraging (0%) #####################################     
        ### weighted average of fitness outcomes from each possible patch depletion 
          ## each outcome is multiplied by the prob. that depletion level will occur within the patch matrix
          
        outcome1.list <- c(outcome.1.1*(8/24), outcome.1.2*(10/24), outcome.1.3*(4/24), outcome.1.4*(1/24), outcome.1.5*(1/24))

        outcome1 <- sum(outcome1.list)                   
          
          
          
          
          
          ############################################################################################################# 
          ################################## Fitness Outcome 2, Foraging (50%) #####################################
          #############################################################################################################
          
        

        ## For outcome 2, foraging at 50%         
        ## new energy state if find food
        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
        new.energy.food.2 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - (patch.matrix[d.patch,4]) + patch.matrix[d.patch,5]*0.50)
        
        ## new energy state if do not find food
        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost) 
        new.energy.nofood.2 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - (patch.matrix[d.patch,4]))
        
        ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
        
        if(new.energy.food.2 > energymax){
          new.energy.food.2 = energymax
        }else{
          if(new.energy.food.2 < 0){
            new.energy.food.2 = 0
          }else{
            new.energy.food.2 = new.energy.food.2
          }
        }
        
        
        if(new.energy.nofood.2 > energymax){
          new.energy.nofood.2 = energymax
        }else{
          if(new.energy.nofood.2 < 0){
            new.energy.nofood.2 = 0
          }else{
            new.energy.nofood.2 = new.energy.nofood.2
          }
        }
        
        ################################## Fitness Outcome 2, Foraging (50%) #####################################
        ############################ Fitness Outcome 2.1, Depletion = 1 (0%) ##################################
        
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.2 < 1){
          find.food.term.2.1 = 0
          no.find.food.term.2.1 = 0
        }else{
          if(new.energy.nofood.2 < 1){
            find.food.term.2.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                 fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.1 <- 0                                     
          }else{
            find.food.term.2.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                 fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.1 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0))) * 
                                    fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0.5))
        
        outcome.2.1 <- p.survival.term * (find.food.term.2.1 + no.find.food.term.2.1)

        
        
        ################################## Fitness Outcome 2, Foraging (50%) #####################################
        ############################ Fitness Outcome 2.2, Depletion = 2 (25%) ##################################
        
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.2 < 1){
          find.food.term.2.2 = 0
          no.find.food.term.2.2 = 0
        }else{
          if(new.energy.nofood.2 < 1){
            find.food.term.2.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.2 <- 0                                     
          }else{
            find.food.term.2.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.2 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.25))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0.5))
        
        outcome.2.2 <- p.survival.term * (find.food.term.2.2 + no.find.food.term.2.2)
        
        
      
        ################################## Fitness Outcome 2, Foraging (50%) #####################################
        ############################ Fitness Outcome 2.3, Depletion = 3 (50%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.2 < 1){
          find.food.term.2.3 = 0
          no.find.food.term.2.3 = 0
        }else{
          if(new.energy.nofood.2 < 1){
            find.food.term.2.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.3 <- 0                                     
          }else{
            find.food.term.2.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.3 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.50))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] + (patch.matrix[d.patch,2] * 0.50)))
        
        outcome.2.3 <- p.survival.term * (find.food.term.2.3 + no.find.food.term.2.3)
        
        
        
        ################################## Fitness Outcome 2, Foraging (50%) #####################################
        ############################ Fitness Outcome 2.4, Depletion = 4 (75%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.2 < 1){
          find.food.term.2.4 = 0
          no.find.food.term.2.4 = 0
        }else{
          if(new.energy.nofood.2 < 1){
            find.food.term.2.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.4 <- 0                                     
          }else{
            find.food.term.2.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.4 <- ((1 - (patch.matrix[d.patch,3] * (1 - 0.75))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0.5))
        
        outcome.2.4 <- p.survival.term * (find.food.term.2.4 + no.find.food.term.2.4)
        
        
        
        ################################## Fitness Outcome 2, Foraging (50%) #####################################
        ############################ Fitness Outcome 2.5, Depletion = 5 (100%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.2 < 1){
          find.food.term.2.5 = 0
          no.find.food.term.2.5 = 0
        }else{
          if(new.energy.nofood.2 < 1){
            find.food.term.2.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.5 <- 0                                     
          }else{
            find.food.term.2.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
            no.find.food.term.2.5 <- ((1 - (patch.matrix[d.patch,3] * (1 - 1))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0.5))
        
        outcome.2.5 <- p.survival.term * (find.food.term.2.5 + no.find.food.term.2.5)
        
        
        ################################## Fitness Outcome 2, Foraging (50%) #####################################     
        ### weighted average of fitness outcomes from each possible patch depletion 
        ## each outcome is multiplied by the prob. that depletion level will occur within the patch matrix
        
        outcome2.list <- c(outcome.2.1*(8/24), outcome.2.2*(10/24), outcome.2.3*(4/24), outcome.2.4*(1/24), outcome.2.5*(1/24))
        
        outcome2 <- sum(outcome2.list)  
        
        
        
        
        
        
        ############################################################################################################# 
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        #############################################################################################################
        
      
        ## For outcome 3, foraging at 100% 
        ## new energy state if find food
        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
        new.energy.food.3 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + patch.matrix[d.patch,5])
        
        ## new energy state if do not find food
        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost)
        new.energy.nofood.3 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4])
        
        ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
        
        if(new.energy.food.3 > energymax){
          new.energy.food.3 = energymax
        }else{
          if(new.energy.food.3 < 0){
            new.energy.food.3 = 0
          }else{
            new.energy.food.3 = new.energy.food.3
          }
        }
        
        
        if(new.energy.nofood.3 > energymax){
          new.energy.nofood.3 = energymax
        }else{
          if(new.energy.nofood.3 < 0){
            new.energy.nofood.3 = 0
          }else{
            new.energy.nofood.3 = new.energy.nofood.3
          }
        }
        
        
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        ############################ Fitness Outcome 3.1, Depletion = 1 (0%) ##################################
  
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.3 < 1){
          find.food.term.3.1 = 0
          no.find.food.term.3.1 = 0
        }else{
          if(new.energy.nofood.3 < 1){
            find.food.term.3.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                 fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.1 <- 0                                     
          }else{
            find.food.term.3.1 <- ((patch.matrix[d.patch,3] * (1 - 0)) * 
                                 fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.1 <- ((1-(patch.matrix[d.patch,3] * (1 - 0))) * 
                                    fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
        
        outcome.3.1 <- p.survival.term * (find.food.term.3.1 + no.find.food.term.3.1)
        
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        ############################ Fitness Outcome 3.2, Depletion = 2 (25%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.3 < 1){
          find.food.term.3.2 = 0
          no.find.food.term.3.2 = 0
        }else{
          if(new.energy.nofood.3 < 1){
            find.food.term.3.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.2 <- 0                                     
          }else{
            find.food.term.3.2 <- ((patch.matrix[d.patch,3] * (1 - 0.25)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.2 <- ((1-(patch.matrix[d.patch,3] * (1 - 0.25))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
        
        outcome.3.2 <- p.survival.term * (find.food.term.3.2 + no.find.food.term.3.2)
        
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        ############################ Fitness Outcome 3.3, Depletion = 3 (50%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.3 < 1){
          find.food.term.3.3 = 0
          no.find.food.term.3.3 = 0
        }else{
          if(new.energy.nofood.3 < 1){
            find.food.term.3.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.3 <- 0                                     
          }else{
            find.food.term.3.3 <- ((patch.matrix[d.patch,3] * (1 - 0.50)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.3 <- ((1-(patch.matrix[d.patch,3] * (1 - 0.50))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
        
        outcome.3.3 <- p.survival.term * (find.food.term.3.3 + no.find.food.term.3.3)
        
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        ############################ Fitness Outcome 3.4, Depletion = 4 (75%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.3 < 1){
          find.food.term.3.4 = 0
          no.find.food.term.3.4 = 0
        }else{
          if(new.energy.nofood.3 < 1){
            find.food.term.3.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.4 <- 0                                     
          }else{
            find.food.term.3.4 <- ((patch.matrix[d.patch,3] * (1 - 0.75)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.4 <- ((1-(patch.matrix[d.patch,3] * (1 - 0.75))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
        
        outcome.3.4 <- p.survival.term * (find.food.term.3.4 + no.find.food.term.3.4)
        
        
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################
        ############################ Fitness Outcome 3.5, Depletion = 5 (100%) ##################################
        
        ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
        
        ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
        ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
        
        ## if new energy after finding food is less than 1 then both terms in equation are 0
        ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
        ##(see above notes) and the no food term is 0
        ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
        ## terms are calculated by their formulas (see above).
        
        if(new.energy.food.3 < 1){
          find.food.term.3.5 = 0
          no.find.food.term.3.5 = 0
        }else{
          if(new.energy.nofood.3 < 1){
            find.food.term.3.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.5 <- 0                                     
          }else{
            find.food.term.3.5 <- ((patch.matrix[d.patch,3] * (1 - 1)) * 
                                     fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
            no.find.food.term.3.5 <- ((1-(patch.matrix[d.patch,3] * (1 - 1))) * 
                                        fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
          }}
        
        
        ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
        ## p.survival.term = prob. surviving travel * prob. surviving foraging
        
        p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
        
        outcome.3.5 <- p.survival.term * (find.food.term.3.5 + no.find.food.term.3.5)
        
        ################################## Fitness Outcome 3, Foraging (100%) #####################################     
        ### weighted average of fitness outcomes from each possible patch depletion 
        ## each outcome is multiplied by the prob. that depletion level will occur within the patch matrix
        
        outcome3.list <- c(outcome.3.1*(8/24), outcome.3.2*(10/24), outcome.3.3*(4/24), outcome.3.4*(1/24), outcome.3.5*(1/24))
        
        outcome3 <- sum(outcome3.list)  
        
        
        
        
        ############################################################################################################# 
        ################################## ID Best Foraging Effort & Associated Fitness #####################################
        #############################################################################################################
        
        
        
        ## Make list of all fitness outcomes of the 4 different foraging efforts within the patch.    
        outcome.list <- c(outcome1, outcome2, outcome3)
        
        ## An if then statment to record foraging effort of the maximum outcome
        ## If there is no max, I assume they forage at 4 - least amount of foraging.
        
        if (outcome1 == max(outcome.list)){
          f.effort <- 0
        } else {
          if(outcome2 == max(outcome.list)){
            f.effort <- 0.5
          }else {
            f.effort <- 1
          }
        }
        
        
        
        ## Name the outcome, the maximum expected fitness from the best choice of foraging effort
        fitness.outcome <- max(outcome.list)
        
        ## store each outcome in an outcome matrix
        ## columns of matrix = patch dstination, expected fitness
        outcome.matrix[d.patch,2] <- f.effort
        outcome.matrix[d.patch,3] <- fitness.outcome
        
      } # closes if statement that instructs to calcuate outcomes this way if i.patch does not equal d.patch
        
        
          
###############################################################################################################          
#####################################  if i.patch = d.patch ##################################################   
###############################################################################################################          
          
####### we need to calculate the expected fitness of staying in the same patch when the animal knows 
#the current patch's depletion state b/c it's where they just spent the previous time step.
# the difference here is that instead of multiplying the pff by mean depletion expectation,
# we multiply it by the given depletion state and we loop through the 5 possible depletion states - in our loop structure
        
if(i.patch == d.patch){
  
  ##########################
  
  ### we need to identify proportion of depletion based upon looped value for prev.p.depletion (1:5)
  # so that we can use it in the formulas for probability of finding food below.
  
  if(prev.p.depletion == 1){
    depletion = 0
  }
  
  if(prev.p.depletion == 2){
    depletion = 0.25
  }
  
  if(prev.p.depletion == 3){
    depletion = 0.5
  }
  
  if(prev.p.depletion == 4){
    depletion = 0.75
  }
  
  if(prev.p.depletion == 5){
    depletion = 1
  }
  
  
  
  
  ## For outcome 1, no foraging (0%)
  ## new energy state if find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
  new.energy.food.1 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + patch.matrix[d.patch,5]*0)
  
  ## new energy state if do not find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost)  
  new.energy.nofood.1 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4])
  
  ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
  
  if(new.energy.food.1 > energymax){
    new.energy.food.1 = energymax
  }else{
    if(new.energy.food.1 < 0){
      new.energy.food.1 = 0
    }else{
      new.energy.food.1 = new.energy.food.1
    }
  }
  
  
  if(new.energy.nofood.1 > energymax){
    new.energy.nofood.1 = energymax
  }else{
    if(new.energy.nofood.1 < 0){
      new.energy.nofood.1 = 0
    }else{
      new.energy.nofood.1 = new.energy.nofood.1
    }
  }
  
  ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
  
  ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
  ## no.find.food.term = (1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
  
  ## if new energy after finding food is less than 1 then both terms in equation are 0
  ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
  ##(see above notes) and the no food term is 0
  ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
  ## terms are calculated by their formulas (see above).
  
  if(new.energy.food.1 < 1){
    find.food.term = 0
    no.find.food.term = 0
  }else{
    if(new.energy.nofood.1 < 1){
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
      no.find.food.term <- 0                                     
    }else{
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.1, prev.p.depletion, 5])
      no.find.food.term <- ((1 - (patch.matrix[d.patch,3] * (1 - depletion))) * 
                              fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.1, prev.p.depletion, 5])
    }}
  
  
  ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
  ## p.survival.term = prob. surviving travel * prob. surviving foraging
  
  p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0))
  
  outcome1 <- p.survival.term * (find.food.term + no.find.food.term)
  
  
  
  ############################
  
  ## For outcome 2, foraging at 50%         
  ## new energy state if find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
  new.energy.food.2 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + patch.matrix[d.patch,5]*0.50)
  
  ## new energy state if do not find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic enery cost) 
  new.energy.nofood.2 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - (patch.matrix[d.patch,4]))
  
  ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
  
  if(new.energy.food.2 > energymax){
    new.energy.food.2 = energymax
  }else{
    if(new.energy.food.2 < 0){
      new.energy.food.2 = 0
    }else{
      new.energy.food.2 = new.energy.food.2
    }
  }
  
  
  if(new.energy.nofood.2 > energymax){
    new.energy.nofood.2 = energymax
  }else{
    if(new.energy.nofood.2 < 0){
      new.energy.nofood.2 = 0
    }else{
      new.energy.nofood.2 = new.energy.nofood.2
    }
  }
  
  
  ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
  
  ## find.food.term = (prob. finding food * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
  ## no.find.food.term = ( 1 - (prob. finding food * (1 - depletion))) * (energy - travel cost - foraging cost)
  
  ## if new energy after finding food is less than 1 then both terms in equation are 0
  ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
  ##(see above notes) and the no food term is 0
  ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
  ## terms are calculated by their formulas (see above).
  
  if(new.energy.food.2 < 1){
    find.food.term = 0
    no.find.food.term = 0
  }else{
    if(new.energy.nofood.2 < 1){
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
      no.find.food.term <- 0                                     
    }else{
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.2, prev.p.depletion, 5])
      no.find.food.term <- ((1 - (patch.matrix[d.patch,3] * (1 - depletion)))* 
                              fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.2, prev.p.depletion, 5])
    }}
  
  
  ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
  ## p.survival.term = prob. surviving travel * prob. surviving foraging
  
  p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1- (patch.matrix[d.patch,2] * 0.50))
  
  outcome2 <- p.survival.term * (find.food.term + no.find.food.term)
  
  
  
  ################################ 
  
  ## For outcome 3, foraging at 100% 
  ## new energy state if find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + energy gain)
  new.energy.food.3 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + patch.matrix[d.patch,5])
  
  ## new energy state if do not find food
  ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost)
  new.energy.nofood.3 <- floor(energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4])
  
  ## The following 2 if, then statements ensure that energy values remain within 0 - energymax
  
  if(new.energy.food.3 > energymax){
    new.energy.food.3 = energymax
  }else{
    if(new.energy.food.3 < 0){
      new.energy.food.3 = 0
    }else{
      new.energy.food.3 = new.energy.food.3
    }
  }
  
  
  if(new.energy.nofood.3 > energymax){
    new.energy.nofood.3 = energymax
  }else{
    if(new.energy.nofood.3 < 0){
      new.energy.nofood.3 = 0
    }else{
      new.energy.nofood.3 = new.energy.nofood.3
    }
  }
  
  ## Below is an if, then statement instructs how to calculate new fitness based upon whether food was found and the new energy value
  
  ## find.food.term = (prob. finding food in patch * (1 - depletion)) * (energy - travel cost - foraging cost + forage gain)
  ## no.find.food.term = (1 - (prob. finding food in patch * (1 - depletion))) * (energy - travel cost - foraging cost)
  
  ## if new energy after finding food is less than 1 then both terms in equation are 0
  ## if new energy after not finding food is less than 1, then the find food term is calculated by their formulas
  ##(see above notes) and the no food term is 0
  ## if new energy after finding food and new energy after not finding food are both greater than 0 then both 
  ## terms are calculated by their formulas (see above).
  
  if(new.energy.food.3 < 1){
    find.food.term = 0
    no.find.food.term = 0
  }else{
    if(new.energy.nofood.3 < 1){
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
      no.find.food.term <- 0                                     
    }else{
      find.food.term <- ((patch.matrix[d.patch,3] * (1 - depletion)) * 
                           fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.food.3, prev.p.depletion, 5])
      no.find.food.term <- ((1-(patch.matrix[d.patch,3] * (1 - depletion))) * 
                              fmax.results.matrix[p.matrix, time.step + 1, i.patch, new.energy.nofood.3, prev.p.depletion, 5])
    }}
  
  
  ## Here is how the above calculated terms are used to calcuate the new expected future fitness 
  ## p.survival.term = prob. surviving travel * prob. surviving foraging
  
  p.survival.term <- (1-distance.risk.matrix[d.patch,i.patch]) * (1 - patch.matrix[d.patch,2])
  
  outcome3 <- p.survival.term * (find.food.term + no.find.food.term)
  
  
  
  ## Make list of all fitness outcomes of the 4 different foraging efforts within the patch.    
  outcome.list <- c(outcome1, outcome2, outcome3)
  
  ## An if then statment to record foraging effort of the maximum outcome
  ## If there is no max, I assume they forage at 4 - least amount of foraging.
  
  if (outcome1 == max(outcome.list)){
    f.effort <- 0
  } else {
    if(outcome2 == max(outcome.list)){
      f.effort <- 0.5
    }else {
      f.effort <- 1
    }
  }

    
    ## Name the outcome, the maximum expected fitness from the best choice of foraging effort
    fitness.outcome <- max(outcome.list)
    
    ## store each outcome in an outcome matrix
    ## columns of matrix = patch dstination, expected fitness
    outcome.matrix[d.patch,2] <- f.effort
    outcome.matrix[d.patch,3] <- fitness.outcome
  
  } # closes if statement that instructs to calculate outcomes using actual depletion if i.patch = d.patch
        
      } ## closes d.patch loop
      
      ## identifies max fitness from all d.patch's within outcome.matrix
      max.fit <- max(outcome.matrix[ ,3])
      
      ## identifies d.patch of max fitness
      max.d.patch <- which.max(outcome.matrix[ ,3])
      
      ## identifies foraging effort at d.patch of max fitness
      max.fe <- outcome.matrix[max.d.patch,2]
      
      ## identifies patch type of patch destination that maximzes fitness
      max.patch.type <- patch.matrix[max.d.patch,8]
      
      ## identifies distance travelled from initial patch to destination patch
      dt <- distance.matrix[max.d.patch,i.patch]
      
      ## stores the above info in fmax.results.matrix so that we can use this as the fitness function 
      ##for the previous time next (next loop)
      fmax.results.matrix[p.matrix, time.step,i.patch,energy, prev.p.depletion, 1] <- max.d.patch
      fmax.results.matrix[p.matrix, time.step,i.patch,energy, prev.p.depletion, 2] <- max.patch.type 
      fmax.results.matrix[p.matrix, time.step,i.patch,energy, prev.p.depletion, 3] <- dt
      fmax.results.matrix[p.matrix, time.step,i.patch,energy, prev.p.depletion, 4] <- max.fe  
      fmax.results.matrix[p.matrix, time.step,i.patch,energy, prev.p.depletion, 5] <- max.fit
      
      ## clears the outcome matrix for next loop, relevant info is stored in fmax.results.matrix 
      outcome.matrix <- matrix(0,25,3)
      outcome.matrix[ ,1] <- c(1:25)
      
      } ## closes prev.p.depletion loop 
      
    } ## closes energy loop
    
  } ## closes i.patch loop 
  
} ## closes time loop






##############################################################################################################
##################################### OPTIMALITY MODEL LOOP END ##############################################
##############################################################################################################

## saves results matrix
#save(fmax.results.matrix, file = "FmaxMatrixOrig.RData")

##############################################################################################################
############################### INITIAL ENERGY FOR FORWARD MODEL #############################################
##############################################################################################################


## pick energy state for forward model with this patch matrix

energy.pick <- sample(energy.states, 1, replace = FALSE)





#################################################################################################################
######################################### FORWARD ITERATIONS - LOOP BEGINS ######################################
#################################################################################################################


i.energy <- energy.pick
time.step <- 1
i.patch <- 13
i.p.depletion <- 1
d.p.depletion <- 1
new.p.depletion <- 1

    
    for (time.step in 1:20){
        
        ## first we identify best d.patch and fe from fmax matrix from optimality model
      
      ## we start at home assuming no patch depletion because home has no food

        
        d.patch <- fmax.results.matrix[p.matrix, time.step, i.patch, i.energy, i.p.depletion, 1]
        d.patch.type <- fmax.results.matrix[p.matrix, time.step, i.patch, i.energy, i.p.depletion, 2]
        d.travelled <- fmax.results.matrix[p.matrix, time.step, i.patch, i.energy, i.p.depletion, 3]
        f.effort <- fmax.results.matrix[p.matrix, time.step, i.patch, i.energy, i.p.depletion, 4]
        fitness <- fmax.results.matrix[p.matrix, time.step, i.patch, i.energy, i.p.depletion, 5]
        
        
        ### if destination patch does not equal initial patch destination, then we pull from the distribution below
        ## determine patch depletion of the destination patch
        # pick from a probability distribution
        
        if(d.patch != i.patch){
          
          possible.depletions <- c(5,4,3,3,3,3,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1)
          
          d.p.depletion <- sample(possible.depletions, 1, replace = TRUE)
        
          }
        
        
        ### if destination patch is equal to initial patch destination, then 
          # d.p.depletion = new.p.depletion (the patch depletion as a consequence of foraging behavior from last time step)
        
        if(d.patch == i.patch){
          
          d.p.depletion = new.p.depletion
        
          }
        
        
        ##### if destination patch is home, depletion = 1
        
        if(d.patch == 13){
          d.p.depletion = 1
        }
        
        
        ## put this information into decision matrix to record timestep
        # first row is row # which is filled in when we create matrix before beginning largest loop
        
        decision.matrix[time.step + forward.counter,2] <- p.matrix
        
        decision.matrix[time.step + forward.counter,3] <- time.step
        
        decision.matrix[time.step + forward.counter,4] <- i.patch
        
        decision.matrix[time.step + forward.counter,5] <- i.p.depletion
        
        decision.matrix[time.step + forward.counter,6] <- i.energy
        
        decision.matrix[time.step + forward.counter,7] <- d.patch
        
        decision.matrix[time.step + forward.counter,8] <- d.patch.type
        
        decision.matrix[time.step + forward.counter,9] <- d.p.depletion
        
        decision.matrix[time.step + forward.counter,10] <- d.travelled
        
        decision.matrix[time.step + forward.counter,11] <- f.effort
        
        
        
        ## DETERMINE WHETHER ANIMAL SURVIVES TRAVELING TO PATCH
        
        ## choose a random number between 0 and 1
        
        reservoir <- runif(10000)
        
        pick <- sample(reservoir, 1, replace = TRUE)
        
        ## identify risk of traveling to patch
        
        travel.risk <- distance.risk.matrix[d.patch,i.patch]
        
        ## if pick is less than risk, the animal dies travelling to patch
        # this ensures that if risk is 0, the animal will live b/c we can't pick a number less than 0
        # if they don't survive, we enter travel risk into column 8 and break from the loop
        
        if(pick < travel.risk){
          fate <- "travel risk"
          decision.matrix[time.step + forward.counter, 14] <- fate
          break
        }
        
        
        ## if they do survive, we continue through loop
        
        ## calculate new energy if find food and if don't find foood

        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost + 
            #energy gain * foraging effort)
        
        new.energy.food <- floor(i.energy - distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4] + 
                                   (patch.matrix[d.patch,5]  * f.effort))
        
        
        ## floor rounds down (initial energy - energy cost to travel - metabolic energy cost)
        
        new.energy.nofood <- floor(i.energy-distance.cost.matrix[d.patch,i.patch] - patch.matrix[d.patch,4])
        

        
        ## DETERMINE WHETHER ANIMAL WILL FIND FOOD AND THEIR NEW ENERGY
        
        
        #### calculate percentages by which to multiply the probability of finding food
        
        if(depletion == 1){
          d.portion = 1
        }
        
        if(depletion == 2){
          d.portion = 0.75
        }
        
        if(depletion == 3){
          d.portion = 0.5
        }
        
        if(depletion == 4){
          d.portion = 0.25
        }
        
        if(depletion == 5){
          d.portion = 0
        }
        

        ## probability of finding food in destination patch pulled from patch matrix and
        # multiplied by d.portion to account for patch depletion
        pff <- patch.matrix[d.patch,3] * (d.portion)
        
        ## choose a random number between 0 and 1
        
        reservoir <- runif(10000)
       
        pick <- sample(reservoir, 1, replace = TRUE)
        
        ## if number is below or equal to the proabability of finding food, they find food
        ## if number is above the proabability of finding food, they don't find food
        
        if(pick < pff){
          f.energy = new.energy.food
        }
        
        ## this ensures that if pff is zero they will not find food
          # b/c a number below zero cannot be chosen
        if(pick == pff){
          f.energy = new.energy.nofood
        }
        
        if(pick > pff){
          f.energy = new.energy.nofood
        }
        
        
        ## ensures energy cannot be above 100
        if(f.energy > 100){
          f.energy = 100
        }
        
        ## enter new energy into column 7 of decision matrix
        
        decision.matrix[time.step + forward.counter, 12] <- f.energy
        
        if(f.energy <= 0){
          fate <- "starved"
          decision.matrix[time.step + forward.counter, 14] <- fate
          break
        }
        
       
        ## DETERMINE NEW PATCH DEPLETION AFTER FORAGING
        
        ## if the animal finds food...
          # it is the amount that the patch was already depleted when the animal arrived + the proportion that it ate
        
        if(f.energy == new.energy.food & f.effort == 1){
          
          new.p.depletion <- d.p.depletion + 2
        
        }
        
        if(f.energy == new.energy.food & f.effort == 0.5){
          
          new.p.depletion <- d.p.depletion + 1
          
        }
        
        if(f.energy == new.energy.food & f.effort == 0){
          
          new.p.depletion <- d.p.depletion
          
        }
        
        ## we have to be sure that new.d.portion cannot excede 1.
          # this is possible if an animal forages at 100% in a patch that's already depleted by 0.75
        
        if(new.p.depletion > 5){
          new.p.depletion = 5
        }
        
        
        ## if the animal did not find food....
        # it is just the amount that the patch was already depleted when the animal arrived
        
        if(f.energy == new.energy.nofood){
          
          new.p.depletion <-  d.p.depletion
          
        }
        
      
        
        ## store new patch depletion in decision matrix to we can be sure this is working correctly
        decision.matrix[time.step + forward.counter, 13] <- new.p.depletion
      
  
        ## DETERMINE WHETHER THAN ANIMAL SURVIVES FORAGING (OR NOT FORAGING) IN THE DESTINATION PATCH
        
        ## choose a random number between 0 and 1
        
        reservoir <- runif(10000)
        
        pick <- sample(reservoir, 1, replace = TRUE)
        
       ## identify patch risk - risk of being in patch with given foraging effort
        
        patch.risk <- patch.matrix[d.patch,2] + (patch.matrix[d.patch,2] * f.effort)
    
        
        ## if pick is less than risk, the animal dies travelling to patch
        # this ensures that if risk is 0, the animal will live b/c we can't pick a number less than 0
        # if they don't survive, record 'patch risk' in 8th column and break from loop
        if(pick < patch.risk){
          fate <- "patch risk"
          decision.matrix[time.step + forward.counter, 14] <- fate
          break
        }
        
        ## if they survive, record 'survive' in column 8 and continue through loop
        if(pick > patch.risk){
          fate <- "survive"
          decision.matrix[time.step + forward.counter, 14] <- fate
        }
        
       
        ### set new patch depletion as initial patch depletion for next time step
        i.p.depletion <- new.p.depletion
        
        ## set initial energy as final energy for next time step
        
        i.energy <- f.energy
        
        ## set initial patch as destination patch for next time step
        
        i.patch <- d.patch
      
    } #close time loop here 
    
    # add 20 to counter so that it starts at 21 for next loop in decision matrix  
    forward.counter <- forward.counter + 20
    
    
    
  
#################################################################################################################
######################################### FORWARD ITERATIONS - LOOP ENDS ########################################
#################################################################################################################


    ## add 1 to pmatrix - next time we run optimality model it will be with the next patch matrix
    p.matrix <- p.matrix + 1
    
    
} ## closes outer most loop - 100 patch matrices
    


## OPTIMALITY MODELS - ANALYSIS & VIS

#################################################################################################################
######################################### ANALYZING OPTIMALITY RESULTS ##########################################
################################################################################################################# 
#################################################################################################################
############################ GENERATE DATAFRAME FROM FMAX MATRIX FOR GRAPHS #####################################
#################################################################################################################  


fmax.results.matrix.spread <- matrix(0,25000000,11)

#(1)time.step, (2)i.patch, (3)energy, (4)p.depletion, (5)d.patch, (6)f.effort,
#(7)d.patch.type, (8)d.travelled, (9)fitness


pm <- 1
t <- 1
ip <- 1
e <- 1
d <- 1

# this counter is to call the row number for info we're storing
counter <- 0

# this counter is to put row number into 10th column so we have a unique identifyer in the matrix
micro.counter <- 1

for(pm in 1:pmmax){

  for(t in 1:20){
  
    for(ip in 1:patchno){
    
      for(d in 1:5){
    
        for(e in 1:energymax){
    
      d.patch <- fmax.results.matrix[pm, t, ip, e, d, 1]
      d.patch.type <- fmax.results.matrix[pm, t, ip, e, d, 2]
      d.travelled <- fmax.results.matrix[pm, t, ip, e, d, 3]
      f.effort <- fmax.results.matrix[pm, t, ip, e, d, 4] 
      fitness <- fmax.results.matrix[pm, t, ip, e, d, 5]
      
      row.no <- micro.counter
      
      fmax.results.matrix.spread[e + counter, 1] <- pm
      fmax.results.matrix.spread[e + counter, 2] <- t
      fmax.results.matrix.spread[e + counter, 3] <- ip
      fmax.results.matrix.spread[e + counter, 4] <- e
      fmax.results.matrix.spread[e + counter, 5] <- d
      fmax.results.matrix.spread[e + counter, 6] <- d.patch
      fmax.results.matrix.spread[e + counter, 7] <- d.patch.type
      fmax.results.matrix.spread[e + counter, 8] <- d.travelled
      fmax.results.matrix.spread[e + counter, 9] <- f.effort
      fmax.results.matrix.spread[e + counter, 10] <- fitness
      fmax.results.matrix.spread[d + counter, 11] <- row.no
      
      micro.counter <- micro.counter + 1
        
        } # closes energy loop
      
      counter <- counter + 100
      
      } # closes depletions loop
    } # closes i.patch loop
  } # closes time loop
} # closes patch matrix loop

#fmax.results.matrix.spread <- as.data.frame(fmax.df)
# need to use library("reshape") and the function melt to transform into a dataframe and...

colnames(fmax.results.matrix.spread) <- c("p.matrix", "time", "i.patch", "energy", "p.p.dep", "d.patch", "d.patch.type",
                                          "d.travelled", "f.effort", "fitness", "row.no")

fmax.df <- as.data.frame(fmax.results.matrix.spread)

## fmax.df is our dataframe that holds all optimal results (best d.patch and fe) from our optimality model

# we saved this to the r project
# all import as fmax.df so when we bring them back in from saved in project we have to rename them as...
#fmax.df.orig
#fmax.df.lf
#fmax.df.hr

#################################################################################################################
####################### IMPORT & SAVE (3) OPTIMALITY DATAFRAMES (ORIG, LF, HR) ##################################
################################################################################################################# 

load("FmaxDFOrig.RData")
fmax.df.orig <- fmax.df

load("FmaxDFLF.RData")
fmax.df.lf <- fmax.df

load("FmaxDFHR.RData")
fmax.df.hr <- fmax.df

###################################################################################################
############## OPTIMALITY MODEL - GRAPH OF DISTRIBUTION OF DEPLETION LEVELS #######################
###################################################################################################

possible.depletions <- c(1,.75,.5,.5,.5,.5,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,0,0,0,0,0,0,0,0)
x <- c(1:24)

dep.data <- cbind(x,possible.depletions)

dep.data <- as.data.frame(dep.data)

ggplot(dep.data, aes(x = possible.depletions))+
  geom_histogram(binwidth = 0.12)+
  theme_classic()+
  xlab("Proportion of Patch Depleted")+
  ylab("Number of Patches")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))
#################################################################################################################
################ GENERATE 3 NEW DATAFRAMES GROUPED AND SUMMARISED BY (1)TIME/ENERGY, (2)DEP/ENERGY (3)DEP/TIME ##
################################################################################################################# 

# baseline model
fmax.df.orig.time.e <- group_by(fmax.df.orig, time, energy)

fmax.df.orig.time.e <- summarise(fmax.df.orig.time.e, 
                        dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                        fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                        dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                        fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.orig.time.dep <- group_by(fmax.df.orig, time, p.p.dep)

#fmax.df.orig.time.dep <- summarise(fmax.df.orig.time.dep, 
                          #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                          #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                          #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                          #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.orig.e.dep <- group_by(fmax.df.orig, energy, p.p.dep)

#fmax.df.orig.dep.e.dep <- summarise(fmax.df.orig.e.dep, 
                              #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                              #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                              #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                              #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


# low food model
fmax.df.lf.time.e <- group_by(fmax.df.lf, time, energy)

fmax.df.lf.time.e <- summarise(fmax.df.lf.time.e, 
                                 dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                 fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                 dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                 fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.lf.time.dep <- group_by(fmax.df.lf, time, p.p.dep)

#fmax.df.lf.time.dep <- summarise(fmax.df.lf.time.dep, 
                              #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                              #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                              #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                              #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.lf.e.dep <- group_by(fmax.df.lf, energy, p.p.dep)

#fmax.df.lf.e.dep <- summarise(fmax.df.lf.e.dep, 
                               #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                               #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                               #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                               #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))

# high risk model
fmax.df.hr.time.e <- group_by(fmax.df.hr, time, energy)

fmax.df.hr.time.e <- summarise(fmax.df.hr.time.e, 
                               dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                               fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                               dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                               fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.hr.time.dep <- group_by(fmax.df.hr, time, p.p.dep)

#fmax.df.hr.time.dep <- summarise(fmax.df.hr.time.dep, 
                                 #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                 #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                 #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                 #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#fmax.df.hr.e.dep <- group_by(fmax.df.hr, energy, p.p.dep)

#fmax.df.hr.e.dep <- summarise(fmax.df.hr.e.dep, 
                              #dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                              #fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                              #dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                              #fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#################################################################################################################
################ (4) GRAPHS - BASELINE - OPTIMALITY MODEL (PATCH, FE, D.TRAVELLED, FITNESS) #####################
#################################################################################################################  

# variables should all be numeric class automatically 

## old colors: "lightcyan", "dodgerblue", "darkblue"


# D.PATCH.TYPE x ENERGY x TIME 
# Original 
dpt.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Destination Patch Type")+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16))+
  coord_cartesian(ylim=c(19,30))
 

# FORAGING EFFORT x ENERGY x TIME 
# Original
fe.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Foraging Effort")+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16))+
  coord_cartesian(ylim=c(0,1))
  

# D.TRAVELLED x ENERGY x TIME 
# Original
dt.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Distance Travelled")+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16))+
  coord_cartesian(ylim = c(0,1.1))
 

# FITNESS x ENERGY x TIME 
# Original
fit.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Expected Future Fitness")+
  theme(axis.text = element_text(size = 12), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16))+
  coord_cartesian(ylim=c(0,1))


graph.base.optimality <- grid.arrange((arrangeGrob(dpt.e.orig, fe.e.orig, dt.e.orig, fit.e.orig, ncol = 2, 
                          bottom = textGrob("Energy", gp=gpar(fontsize=16)))))

#save(graph.base.optimality, file = "Graph.base.optimality")


#################################################################################################################
############################### GRAPHS - (3) MODELS - D.PATCH TYPE X ENERGY X TIME  #############################
################################################################################################################# 

# Original 
dpt.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Destination Patch Type")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(19,30))+
  ggtitle("Baseline")


### Low Food 
dpt.e.lf <- ggplot(subset(fmax.df.lf.time.e, time == 1), aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.lf.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Destination Patch Type")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(19,30))+
  ggtitle("Low Food")

### High Risk 
dpt.e.hr <- ggplot(subset(fmax.df.hr.time.e, time == 1), aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.hr.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Destination Patch Type")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5), axis.title.x = element_text(size = 16))+
  coord_cartesian(ylim=c(19,30))+
  ggtitle("High Risk")

dpt.e.all <- grid.arrange((arrangeGrob(dpt.e.orig, dpt.e.lf, dpt.e.hr, ncol = 1, 
                          left = textGrob("Destination Patch Type", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))

#save(dpt.e.all, file = "graph.optimality.all.dpt.jpeg")



#################################################################################################################
############################### GRAPHS - (3) MODELS - FE X ENERGY X TIME  #######################################
################################################################################################################# 

## Original
fe.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Foraging Effort")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("Baseline")


## Low Food
fe.e.lf <- ggplot(subset(fmax.df.lf.time.e, time == 1), aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.lf.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Foraging Effort")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("Low Food")


## High Risk
fe.e.hr <- ggplot(subset(fmax.df.hr.time.e, time == 1), aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.hr.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Foraging Effort")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5), axis.title.x = element_text(size = 16))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("High Risk")


fe.e.all <- grid.arrange((arrangeGrob(fe.e.orig, fe.e.lf, fe.e.hr, ncol = 1, 
                          left = textGrob("Foraging Effort", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))

#save(fe.e.all, file = "graph.optimality.fe.all.jpeg")



















#################################################################################################################
############################### GRAPHS - (3) MODELS - D.TRAVELLED X ENERGY X TIME  ##############################
#################################################################################################################

# Original
dt.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Distance Travelled")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim = c(0,1.1))+
  ggtitle("Baseline")


# Low Food
dt.e.lf <- ggplot(subset(fmax.df.lf.time.e, time == 1), aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.lf.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Distance Travelled")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim = c(0,1.1))+
  ggtitle("Low Food")

# High Risk
dt.e.hr <- ggplot(subset(fmax.df.hr.time.e, time == 1), aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.hr.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Distance Travelled")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5), axis.title.x = element_text(size = 16))+
  coord_cartesian(ylim = c(0,1.1))+
  ggtitle("High Risk")

dt.e.all <- grid.arrange((arrangeGrob(dt.e.orig, dt.e.lf, dt.e.hr, ncol = 1, 
                          left = textGrob("Distance Travelled", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))

#save(dt.e.all, file = "graph.optimality.dt.all.jpeg)

#################################################################################################################
############################### GRAPHS - (3) MODELS - FITNESS X ENERGY X TIME  ##############################
#################################################################################################################

# Original
fit.e.orig <- ggplot(subset(fmax.df.orig.time.e, time == 1), aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.orig.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.orig.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Expected Future Fitness")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_blank(), plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("Baseline")

# Low Food
fit.e.lf <- ggplot(subset(fmax.df.lf.time.e, time == 1), aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.lf.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Expected Future Fitness")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 12), 
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("Low Food")

# High Risk
fit.e.hr <- ggplot(subset(fmax.df.hr.time.e, time == 1), aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.time.e, time == 10), pch = 21, fill = "grey50", size = 3)+
  geom_point(data = subset(fmax.df.hr.time.e, time == 20), pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Expected Future Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12), 
        axis.title.y = element_blank(), plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim=c(0,1))+
  ggtitle("High Risk")


fit.e.all <- grid.arrange((arrangeGrob(fit.e.orig, fit.e.lf, fit.e.hr, ncol = 1, 
                          left = textGrob("Expected Future Fitness", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))

#save(fit.e.all, file = "graph.optimality.fit.all.jpeg")
































#################################################################################################################
############################# OPTIMALITY MODEL - DISTANCE TRAVELLED - STATS #####################################
################################################################################################################# 
  
### histogram of distances travelled in baseline model
  
  ggplot(fmax.df.orig, aes(x = d.travelled))+
    geom_histogram()+
    theme_classic()+
    xlab("Distance Travelled")+
    ylab("Number of Optimal Choices")
    
### Proportions of choices that fall within ranges of travel distances
  
  ### BASELINE MODEL
  
  ### greater than or equal to 3
  x <- which(fmax.df.orig$d.travelled == 0)
  length(x)
  prop.greater.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### greater than 1 and less than 3
  x <- which(fmax.df.orig$.lfd.travelled > 1 & fmax.df.lf$d.travelled < 3)
  length(x)
  prop.1.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### equal to 1
  x <- which(fmax.df.orig$d.travelled == 1)
  length(x)
  prop.1 <- length(x) / nrow(fmax.df)
  
  ### equal to 0
  x <- which(fmax.df.orig$d.travelled == 0)
  length(x)
  prop.0 <- length(x) / nrow(fmax.df)

  
  ### LOW FOOD MODEL
  
  ### greater than or equal to 3
  x <- which(fmax.df.lf$d.travelled == 0)
  length(x)
  prop.greater.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### greater than 1 and less than 3
  x <- which(fmax.df.lf$.lfd.travelled > 1 & fmax.df.lf$d.travelled < 3)
  length(x)
  prop.1.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### equal to 1
  x <- which(fmax.df.lf$d.travelled == 1)
  length(x)
  prop.1 <- length(x) / nrow(fmax.df)
  
  ### equal to 0
  x <- which(fmax.df.lf$d.travelled == 0)
  length(x)
  prop.0 <- length(x) / nrow(fmax.df)
  
  
  ### HIGH RISK MODEL
  
  ### greater than or equal to 3
  x <- which(fmax.df.hr$d.travelled == 0)
  length(x)
  prop.greater.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### greater than 1 and less than 3
  x <- which(fmax.df.hr$.lfd.travelled > 1 & fmax.df.lf$d.travelled < 3)
  length(x)
  prop.1.3 <- length(x) / nrow(fmax.df.lf)
  
  
  ### equal to 1
  x <- which(fmax.df.hr$d.travelled == 1)
  length(x)
  prop.1 <- length(x) / nrow(fmax.df)
  
  ### equal to 0
  x <- which(fmax.df.hr$d.travelled == 0)
  length(x)
  prop.0 <- length(x) / nrow(fmax.df)
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#################################################################################################################
###################### OPTIMALITY MODEL - STATS - TABLE - PROP. PATCH TYPES USED ################################
################################################################################################################# 
  
# baseline model
# t = (1-19)
  
# prop of patch decisions to use home (0.003)
nrow(subset(fmax.df.orig, time < 20 & d.patch.type == 0)) / nrow(subset(fmax.df.orig, time < 20))

# prop of patch decisions to use safe patches (0.05)
nrow(subset(fmax.df.orig, time < 20 & d.patch.type == 10)) / nrow(subset(fmax.df.orig, time < 20))
  
# prop of patch decisions to use moderate patches (0.15)
nrow(subset(fmax.df.orig, time < 20 & d.patch.type == 20)) / nrow(subset(fmax.df.orig, time < 20))  

# prop of patch decisions to use risky patches (0.80)
nrow(subset(fmax.df.orig, time < 20 & d.patch.type == 30)) / nrow(subset(fmax.df.orig, time < 20))   


# baseline model
# t = 20

# prop of patch decisions to use home (0.001)
nrow(subset(fmax.df.orig, time == 20 & d.patch.type == 0)) / nrow(subset(fmax.df.orig, time == 20))

# prop of patch decisions to use safe patches (0.03)
nrow(subset(fmax.df.orig, time == 20 & d.patch.type == 10)) / nrow(subset(fmax.df.orig, time == 20))

# prop of patch decisions to use moderate patches (0.14)
nrow(subset(fmax.df.orig, time == 20 & d.patch.type == 20)) / nrow(subset(fmax.df.orig, time == 20))  

# prop of patch decisions to use risky patches (0.83)
nrow(subset(fmax.df.orig, time == 20 & d.patch.type == 30)) / nrow(subset(fmax.df.orig, time == 20))   

  


# low food model
# t = (1-19)

# prop of patch decisions to use home (< 0.001)
nrow(subset(fmax.df.lf, time < 20 & d.patch.type == 0)) / nrow(subset(fmax.df.lf, time < 20))

# prop of patch decisions to use safe patches (0.05)
nrow(subset(fmax.df.lf, time < 20 & d.patch.type == 10)) / nrow(subset(fmax.df.lf, time < 20))

# prop of patch decisions to use moderate patches (0.18)
nrow(subset(fmax.df.lf, time < 20 & d.patch.type == 20)) / nrow(subset(fmax.df.lf, time < 20))  

# prop of patch decisions to use risky patches (0.77)
nrow(subset(fmax.df.lf, time < 20 & d.patch.type == 30)) / nrow(subset(fmax.df.lf, time < 20))   


# low food model
# t = 20

# prop of patch decisions to use home (0.006)
nrow(subset(fmax.df.lf, time == 20 & d.patch.type == 0)) / nrow(subset(fmax.df.lf, time == 20))

# prop of patch decisions to use safe patches (0.11)
nrow(subset(fmax.df.lf, time == 20 & d.patch.type == 10)) / nrow(subset(fmax.df.lf, time == 20))

# prop of patch decisions to use moderate patches (0.21)
nrow(subset(fmax.df.lf, time == 20 & d.patch.type == 20)) / nrow(subset(fmax.df.lf, time == 20))  

# prop of patch decisions to use risky patches (0.68)
nrow(subset(fmax.df.lf, time == 20 & d.patch.type == 30)) / nrow(subset(fmax.df.lf, time == 20))   
  


# high risk model
# t = (1-19)

# prop of patch decisions to use home (0.003)
nrow(subset(fmax.df.hr, time < 20 & d.patch.type == 0)) / nrow(subset(fmax.df.hr, time < 20))

# prop of patch decisions to use safe patches (0.05)
nrow(subset(fmax.df.hr, time < 20 & d.patch.type == 10)) / nrow(subset(fmax.df.hr, time < 20))

# prop of patch decisions to use moderate patches (0.14)
nrow(subset(fmax.df.hr, time < 20 & d.patch.type == 20)) / nrow(subset(fmax.df.hr, time < 20))  

# prop of patch decisions to use risky patches (0.81)
nrow(subset(fmax.df.hr, time < 20 & d.patch.type == 30)) / nrow(subset(fmax.df.hr, time < 20))   


# high risk model
# t = 20

# prop of patch decisions to use home (0.003)
nrow(subset(fmax.df.hr, time == 20 & d.patch.type == 0)) / nrow(subset(fmax.df.hr, time == 20))

# prop of patch decisions to use safe patches (0.04)
nrow(subset(fmax.df.hr, time == 20 & d.patch.type == 10)) / nrow(subset(fmax.df.hr, time == 20))

# prop of patch decisions to use moderate patches (0.13)
nrow(subset(fmax.df.hr, time == 20 & d.patch.type == 20)) / nrow(subset(fmax.df.hr, time == 20))  

# prop of patch decisions to use risky patches (0.83)
nrow(subset(fmax.df.hr, time == 20 & d.patch.type == 30)) / nrow(subset(fmax.df.hr, time == 20))   






## SIMULATION MODELS - ANALYSIS AND VIS

#################################################################################################################
######################## SIMULATION MODELS CONVERT RESULTS TO DATAFRAME - (3) MODELS #############################
################################################################################################################# 

# load and rename all three decision matrices from the three forward models

load(file = "DecisionMatrixOrig.RData")
decision.matrix.orig <- decision.matrix

load(file = "DecisionMatrixLF.RData")
decision.matrix.lf <- decision.matrix

load(file = "DecisionMatrixHR.RData")
decision.matrix.hr <- decision.matrix


## transform each decision matrix into a database 

decision.db.orig <- as.data.frame(decision.matrix.orig)
decision.db.lf <- as.data.frame(decision.matrix.lf)
decision.db.hr <- as.data.frame(decision.matrix.hr)


## convert appropriate variables from factors to integers / numeric

## baseline model dataframe

decision.db.orig$time <- as.character(decision.db.orig$time)
decision.db.orig$time <- as.integer(decision.db.orig$time)

decision.db.orig$i.energy <- as.character(decision.db.orig$i.energy)
decision.db.orig$i.energy <- as.integer(decision.db.orig$i.energy)

decision.db.orig$f.energy <- as.character(decision.db.orig$f.energy)
decision.db.orig$f.energy <- as.integer(decision.db.orig$f.energy)

decision.db.orig$d.travelled <- as.character(decision.db.orig$d.travelled)
decision.db.orig$d.travelled <- as.numeric(decision.db.orig$d.travelled)

decision.db.orig$i.p.depletion <- as.character(decision.db.orig$i.p.depletion)
decision.db.orig$i.p.depletion <- as.numeric(decision.db.orig$i.p.depletion)

decision.db.orig$d.p.depletion <- as.character(decision.db.orig$d.p.depletion)
decision.db.orig$d.p.depletion <- as.numeric(decision.db.orig$d.p.depletion)

decision.db.orig$new.p.depletion <- as.character(decision.db.orig$new.p.depletion)
decision.db.orig$new.p.depletion <- as.numeric(decision.db.orig$new.p.depletion)


## low food dataframe

decision.db.lf$time <- as.character(decision.db.lf$time)
decision.db.lf$time <- as.integer(decision.db.lf$time)

decision.db.lf$i.energy <- as.character(decision.db.lf$i.energy)
decision.db.lf$i.energy <- as.integer(decision.db.lf$i.energy)

decision.db.lf$f.energy <- as.character(decision.db.lf$f.energy)
decision.db.lf$f.energy <- as.integer(decision.db.lf$f.energy)

decision.db.lf$d.travelled <- as.character(decision.db.lf$d.travelled)
decision.db.lf$d.travelled <- as.numeric(decision.db.lf$d.travelled)

decision.db.lf$i.p.depletion <- as.character(decision.db.lf$i.p.depletion)
decision.db.lf$i.p.depletion <- as.numeric(decision.db.lf$i.p.depletion)

decision.db.lf$d.p.depletion <- as.character(decision.db.lf$d.p.depletion)
decision.db.lf$d.p.depletion <- as.numeric(decision.db.lf$d.p.depletion)

decision.db.lf$new.p.depletion <- as.character(decision.db.lf$new.p.depletion)
decision.db.lf$new.p.depletion <- as.numeric(decision.db.lf$new.p.depletion)


## high risk dataframe

decision.db.hr$time <- as.character(decision.db.hr$time)
decision.db.hr$time <- as.integer(decision.db.hr$time)

decision.db.hr$i.energy <- as.character(decision.db.hr$i.energy)
decision.db.hr$i.energy <- as.integer(decision.db.hr$i.energy)

decision.db.hr$f.energy <- as.character(decision.db.hr$f.energy)
decision.db.hr$f.energy <- as.integer(decision.db.hr$f.energy)

decision.db.hr$d.travelled <- as.character(decision.db.hr$d.travelled)
decision.db.hr$d.travelled <- as.numeric(decision.db.hr$d.travelled)

decision.db.hr$i.p.depletion <- as.character(decision.db.hr$i.p.depletion)
decision.db.hr$i.p.depletion <- as.numeric(decision.db.hr$i.p.depletion)

decision.db.hr$d.p.depletion <- as.character(decision.db.hr$d.p.depletion)
decision.db.hr$d.p.depletion <- as.numeric(decision.db.hr$d.p.depletion)

decision.db.hr$new.p.depletion <- as.character(decision.db.hr$new.p.depletion)
decision.db.hr$new.p.depletion <- as.numeric(decision.db.hr$new.p.depletion)


## check that we have 2,000 rows in each dataframe
nrow(decision.db.orig)
nrow(decision.db.lf)
nrow(decision.db.hr)











#################################################################################################################
################## SIMULATION MODELS - SUMMARY TABLE - PROPORTION OF IND'S BY FATE ##############################
#################################################################################################################

## baseline model

##proportion of animals that survived
prop.surv.orig <- nrow(subset(decision.db.orig, time == 20 & fate == "survive")) / nrow(subset(decision.db.orig, time == 1))

## proportion of animals depredated while traveling
prop.trisk.orig <- nrow(subset(decision.db.orig, fate == "travel risk")) / nrow(subset(decision.db.orig, time == 1))

## proportion of animals depredated in patches
prop.prisk.orig <- nrow(subset(decision.db.orig, fate == "patch risk")) / nrow(subset(decision.db.orig, time == 1))

sim.summary.orig <- rbind(prop.surv.orig, prop.trisk.orig, prop.prisk.orig)
                                      
sim.summary.orig


## low food model

##proportion of animals that survived
prop.surv.lf <- nrow(subset(decision.db.lf, time == 20 & fate == "survive")) / nrow(subset(decision.db.lf, time == 1))

## proportion of animals depredated while traveling
prop.trisk.lf <- nrow(subset(decision.db.lf, fate == "travel risk")) / nrow(subset(decision.db.lf, time == 1))

## proportion of animals depredated in patches
prop.prisk.lf <- nrow(subset(decision.db.lf, fate == "patch risk")) / nrow(subset(decision.db.lf, time == 1))

sim.summary.lf <- rbind(prop.surv.lf, prop.trisk.lf, prop.prisk.lf)

sim.summary.lf


## high risk model

##proportion of animals that survived
prop.surv.hr <- nrow(subset(decision.db.hr, time == 20 & fate == "survive")) / nrow(subset(decision.db.hr, time == 1))

## proportion of animals depredated while traveling
prop.trisk.hr <- nrow(subset(decision.db.hr, fate == "travel risk")) / nrow(subset(decision.db.hr, time == 1))

## proportion of animals depredated in patches
prop.prisk.hr <- nrow(subset(decision.db.hr, fate == "patch risk")) / nrow(subset(decision.db.hr, time == 1))

sim.summary.hr <- rbind(prop.surv.hr, prop.trisk.hr, prop.prisk.hr)

sim.summary.hr



## Cobmine Summary Tables 

summary.table.all <- cbind(sim.summary.orig, sim.summary.lf[ ,1], 
                           sim.summary.hr[ ,1])

colnames(summary.table.all) <- c("Orig", "LF", "HR")

summary.table.all












#################################################################################################################
########## SIMULATION MODELS - SUMMARY TABLE - AVG PATCH TYPE & FE IN TIMESTEPS WITH GIVEN FATES  ###############
#################################################################################################################

## for these calculations we need d.patch.type and fe to be numeric

decision.db.orig$d.patch.type <- as.character(decision.db.orig$d.patch.type)
decision.db.orig$d.patch.type <- as.numeric(decision.db.orig$d.patch.type)

decision.db.orig$fe <- as.character(decision.db.orig$fe)
decision.db.orig$fe <- as.numeric(decision.db.orig$fe)

decision.db.lf$d.patch.type <- as.character(decision.db.lf$d.patch.type)
decision.db.lf$d.patch.type <- as.numeric(decision.db.lf$d.patch.type)

decision.db.lf$fe <- as.character(decision.db.lf$fe)
decision.db.lf$fe <- as.numeric(decision.db.lf$fe)

decision.db.hr$d.patch.type <- as.character(decision.db.hr$d.patch.type)
decision.db.hr$d.patch.type <- as.numeric(decision.db.hr$d.patch.type)

decision.db.hr$fe <- as.character(decision.db.hr$fe)
decision.db.hr$fe <- as.numeric(decision.db.hr$fe)


## baseline model

## avg & se of patch type used by survivors
mean.pt.surv.orig <- mean(subset(decision.db.orig, fate == "survive")$d.patch.type)

se.pt.surv.orig <- sd(subset(decision.db.orig, fate == "survive")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.orig, fate == "survive")))


## avg & se of foraging effort used by survivors
mean.fe.surv.orig <- mean(subset(decision.db.orig, fate == "survive")$fe)

se.fe.surv.orig <- sd(subset(decision.db.orig, fate == "survive")$fe) / 
  sqrt(nrow(subset(decision.db.orig, fate == "survive")))


## avg & se of patch type used by those depredated in patch
mean.pt.pr.orig <- mean(subset(decision.db.orig, fate == "patch risk")$d.patch.type)

se.pt.pr.orig <- sd(subset(decision.db.orig, fate == "patch risk")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.orig, fate == "patch risk")))


## avg & se of foraging effort used by those depredated in patch
mean.fe.pr.orig <- mean(subset(decision.db.orig, fate == "patch risk")$fe)

se.fe.pr.orig <- sd(subset(decision.db.orig, fate == "patch risk")$fe) / 
  sqrt(nrow(subset(decision.db.orig, fate == "patch risk")))


## low food model

## avg & se of patch type used by survivors
mean.pt.surv.lf <- mean(subset(decision.db.lf, fate == "survive")$d.patch.type)

se.pt.surv.lf <- sd(subset(decision.db.lf, fate == "survive")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.lf, fate == "survive")))


## avg & se of foraging effort used by survivors
mean.fe.surv.lf <- mean(subset(decision.db.lf, fate == "survive")$fe)

se.fe.surv.lf <- sd(subset(decision.db.lf, fate == "survive")$fe) / 
  sqrt(nrow(subset(decision.db.lf, fate == "survive")))


## avg & se of patch type used by those depredated in patch
mean.pt.pr.lf <- mean(subset(decision.db.lf, fate == "patch risk")$d.patch.type)

se.pt.pr.lf <- sd(subset(decision.db.lf, fate == "patch risk")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.lf, fate == "patch risk")))


## avg & se of foraging effort used by those depredated in patch
mean.fe.pr.lf <- mean(subset(decision.db.lf, fate == "patch risk")$fe)

se.fe.pr.lf <- sd(subset(decision.db.lf, fate == "patch risk")$fe) / 
  sqrt(nrow(subset(decision.db.lf, fate == "patch risk")))



## high risk model

## avg & se of patch type used by survivors
mean.pt.surv.hr <- mean(subset(decision.db.hr, fate == "survive")$d.patch.type)

se.pt.surv.hr <- sd(subset(decision.db.hr, fate == "survive")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.hr, fate == "survive")))


## avg & se of foraging effort used by survivors
mean.fe.surv.hr <- mean(subset(decision.db.hr, fate == "survive")$fe)

se.fe.surv.hr <- sd(subset(decision.db.hr, fate == "survive")$fe) / 
  sqrt(nrow(subset(decision.db.hr, fate == "survive")))


## avg & se of patch type used by those depredated in patch
mean.pt.pr.hr <- mean(subset(decision.db.hr, fate == "patch risk")$d.patch.type)

se.pt.pr.hr <- sd(subset(decision.db.hr, fate == "patch risk")$d.patch.type) / 
  sqrt(nrow(subset(decision.db.hr, fate == "patch risk")))


## avg & se of foraging effort used by those depredated in patch
mean.fe.pr.hr <- mean(subset(decision.db.hr, fate == "patch risk")$fe)

se.fe.pr.hr <- sd(subset(decision.db.hr, fate == "patch risk")$fe) / 
  sqrt(nrow(subset(decision.db.hr, fate == "patch risk")))

## combine stats into a table

# first we create each column separately

model <- c("baseline", "baseline", "low food", "low food", "high risk", "high risk")

fate <- c("survive", "patch risk", "survive", "patch risk", "survive", "patch risk")

mean.d.pt <- c(mean.pt.surv.orig, mean.pt.pr.orig, mean.pt.surv.lf, mean.pt.pr.lf, mean.pt.surv.hr, mean.pt.pr.hr)

se.d.pt <- c(se.pt.surv.orig, se.pt.pr.orig, se.pt.surv.lf, se.pt.pr.lf, se.pt.surv.hr, se.pt.pr.hr)

mean.fe <- c(mean.fe.surv.orig, mean.fe.pr.orig, mean.fe.surv.lf, mean.fe.pr.lf, mean.fe.surv.hr, mean.fe.pr.hr)

se.fe <- c(se.fe.surv.orig, se.fe.pr.orig, se.fe.surv.lf, se.fe.pr.lf, se.fe.surv.hr, se.fe.pr.hr)

# then we combine columns to make a table
decisions.by.fate.summary.stats <- cbind(model, fate, mean.d.pt, se.d.pt, mean.fe, se.fe)

decisions.by.fate.summary.stats <- as.data.frame(decisions.by.fate.summary.stats)








#################################################################################################################
######################### SIMULATION MODELS - GRAPH FORAGING DECISIONS THROUGH TIME #############################
#################################################################################################################

### we need to summarize each model by time

## baseline model

## convert time to a factor so we can group by time
decision.db.orig$time <- as.character(decision.db.orig$time)
decision.db.orig$time <- as.factor(decision.db.orig$time)

## convert these to numeric so we can calculate means
decision.db.orig$d.patch.type <- as.character(decision.db.orig$d.patch.type)
decision.db.orig$d.patch.type <- as.numeric(decision.db.orig$d.patch.type)

decision.db.orig$fe <- as.character(decision.db.orig$fe)
decision.db.orig$fe <- as.numeric(decision.db.orig$fe)

## group by and summarise by time
decision.db.orig.time <- group_by(decision.db.orig, time)

decision.db.orig.time <- summarise(decision.db.orig.time,
                                     dpt.mean = mean(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                     fe.mean = mean(fe), fe.se = (sd(fe))/sqrt(n()))

db.orig.time <- decision.db.orig.time

## convert time back to numeric so we can graph
db.orig.time$time <- as.character(db.orig.time$time)
db.orig.time$time <- as.numeric(db.orig.time$time)



## low food model

## convert time to a factor so we can group by time
decision.db.lf$time <- as.character(decision.db.lf$time)
decision.db.lf$time <- as.factor(decision.db.lf$time)

## convert these to numeric so we can calculate means
decision.db.lf$d.patch.type <- as.character(decision.db.lf$d.patch.type)
decision.db.lf$d.patch.type <- as.numeric(decision.db.lf$d.patch.type)

decision.db.lf$fe <- as.character(decision.db.lf$fe)
decision.db.lf$fe <- as.numeric(decision.db.lf$fe)

## group by and summarise by time
decision.db.lf.time <- group_by(decision.db.lf, time)

decision.db.lf.time <- summarise(decision.db.lf.time,
                                   dpt.mean = mean(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                   fe.mean = mean(fe), fe.se = (sd(fe))/sqrt(n()))

db.lf.time <- decision.db.lf.time

## convert time back to numeric so we can graph
db.lf.time$time <- as.character(db.lf.time$time)
db.lf.time$time <- as.numeric(db.lf.time$time)



## high risk model

## convert time to a factor so we can group by time
decision.db.hr$time <- as.character(decision.db.hr$time)
decision.db.hr$time <- as.factor(decision.db.hr$time)

## convert these to numeric so we can calculate means
decision.db.hr$d.patch.type <- as.character(decision.db.hr$d.patch.type)
decision.db.hr$d.patch.type <- as.numeric(decision.db.hr$d.patch.type)

decision.db.hr$fe <- as.character(decision.db.hr$fe)
decision.db.hr$fe <- as.numeric(decision.db.hr$fe)

## group by and summarise by time
decision.db.hr.time <- group_by(decision.db.hr, time)

decision.db.hr.time <- summarise(decision.db.hr.time,
                                   dpt.mean = mean(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                   fe.mean = mean(fe), fe.se = (sd(fe))/sqrt(n()))

db.hr.time <- decision.db.hr.time

## convert time back to numeric so we can graph
db.hr.time$time <- as.character(db.hr.time$time)
db.hr.time$time <- as.numeric(db.hr.time$time)


## GRAPHS OF FORAGING DECISIONS THROUGH TIME 

## plots baseline model data only
patch.type <- ggplot(aes(x = time, y = dpt.mean), data = db.orig.time)+
  geom_point(size = 3)+
  theme_classic()+
  theme(axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("Mean Patch Type")


for.effort <- ggplot(aes(x = time, y = fe.mean), data = db.orig.time)+
  geom_point(size = 3)+
  theme_classic()+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))+
  xlab("Time")+
  ylab("Mean Foraging Effort")

grid.arrange(patch.type, for.effort, ncol = 1)


## plots data for all three models on each graph
patch.type <- ggplot(aes(x = time, y = dpt.mean), data = db.orig.time)+
  geom_point(col = "grey70", size = 3)+
  geom_point(data = db.lf.time, col = "grey40", size = 3)+
  geom_point(data = db.hr.time, col = "black", size = 3)+
  theme_classic()+
  theme(axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("Mean Patch Type")


for.effort <- ggplot(aes(x = time, y = fe.mean), data = db.orig.time)+
  geom_point(col = "grey70", size = 3)+
  geom_point(data = db.lf.time, col = "grey40", size = 3)+
  geom_point(data = db.hr.time, col = "black", size = 3)+
  theme_classic()+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))+
  xlab("Time")+
  ylab("Mean Foraging Effort")

grid.arrange(patch.type, for.effort, ncol = 1)










#################################################################################################################
##################### SIMULATION MODEL - GENERATE NEW DATAFRAME WITH ENERGY CATEGORIES ##########################
#################################################################################################################


## BASELINE MODEL ##

i.e.cat <- rep(0,2000)

decision.db.orig.e <- cbind(decision.db.orig, i.e.cat)

for(i in 1:2000){
  
  if(decision.db.orig.e[i,3] == 1 & decision.db.orig.e[i,6] < 26){
    
    decision.db.orig.e[i,15] <- 1
    
  }
  
  if(decision.db.orig.e[i,3] == 1 & decision.db.orig.e[i,6] >=26 & decision.db.orig.e[i,6] <= 50){
    
    decision.db.orig.e[i,15] <- 2
    
  }
  
  if(decision.db.orig.e[i,3] == 1 & decision.db.orig.e[i,6] >=51 & decision.db.orig.e[i,6] <= 75){
    
    decision.db.orig.e[i,15] <- 3
    
    
  }
  
  if(decision.db.orig.e[i,3] == 1 & decision.db.orig.e[i,6] >= 76){
    
    decision.db.orig.e[i,15] <- 4
    
  }
  
} # closes loop


### check to see if loop categorized correctly in timestep 1
check <- subset(decision.db.orig.e, time == 1)

## we fill in the last column (initial energy category) so that the i.energy category in T = 1
# gets pulled down torigough all time steps for that animal/patch matrix
# this way we can use this category in graphs.

## all zeros in this column we switch to NA's
decision.db.orig.e$i.e.cat[(decision.db.orig.e$i.e.cat) == 0] <- "NA"

## we change this column to numeric
decision.db.orig.e$i.e.cat <- as.numeric(decision.db.orig.e$i.e.cat)

## this is the column that pulls down values until it hits another value, then pulls down again
decision.db.orig.e$i.e.cat <- na.locf(decision.db.orig.e$i.e.cat)

## convert time from a factor to numeric so we can filter
decision.db.orig.e$time <- as.character(decision.db.orig.e$time)
decision.db.orig.e$time <- as.numeric(decision.db.orig.e$time)

## get rid of rows in which time is 0
decision.db.orig.e <- filter(decision.db.orig.e, time > 0)




## LOW FOOD MODEL ##

i.e.cat <- rep(0,2000)

decision.db.lf.e <- cbind(decision.db.lf, i.e.cat)

for(i in 1:2000){
  
  if(decision.db.lf.e[i,3] == 1 & decision.db.lf.e[i,6] < 26){
    
    decision.db.lf.e[i,15] <- 1
    
  }
  
  if(decision.db.lf.e[i,3] == 1 & decision.db.lf.e[i,6] >=26 & decision.db.lf.e[i,6] <= 50){
    
    decision.db.lf.e[i,15] <- 2
    
  }
  
  if(decision.db.lf.e[i,3] == 1 & decision.db.lf.e[i,6] >=51 & decision.db.lf.e[i,6] <= 75){
    
    decision.db.lf.e[i,15] <- 3
    
    
  }
  
  if(decision.db.lf.e[i,3] == 1 & decision.db.lf.e[i,6] >= 76){
    
    decision.db.lf.e[i,15] <- 4
    
  }
  
} # closes loop


### check to see if loop categorized correctly in timestep 1
check <- subset(decision.db.lf.e, time == 1)

## we fill in the last column (initial energy category) so that the i.energy category in T = 1
# gets pulled down torigough all time steps for that animal/patch matrix
# this way we can use this category in graphs.

## all zeros in this column we switch to NA's
decision.db.lf.e$i.e.cat[(decision.db.lf.e$i.e.cat) == 0] <- "NA"

## we change this column to numeric
decision.db.lf.e$i.e.cat <- as.numeric(decision.db.lf.e$i.e.cat)

## this is the column that pulls down values until it hits another value, then pulls down again
decision.db.lf.e$i.e.cat <- na.locf(decision.db.lf.e$i.e.cat)

## convert time from a factor to numeric so we can filter
decision.db.lf.e$time <- as.character(decision.db.lf.e$time)
decision.db.lf.e$time <- as.numeric(decision.db.lf.e$time)

## get rid of rows in which time is 0
decision.db.lf.e <- filter(decision.db.lf.e, time > 0)




## HIGH RISK MODEL ##

i.e.cat <- rep(0,2000)

decision.db.hr.e <- cbind(decision.db.hr, i.e.cat)

for(i in 1:2000){
  
  if(decision.db.hr.e[i,3] == 1 & decision.db.hr.e[i,6] < 26){
    
    decision.db.hr.e[i,15] <- 1
    
  }
  
  if(decision.db.hr.e[i,3] == 1 & decision.db.hr.e[i,6] >=26 & decision.db.hr.e[i,6] <= 50){
    
    decision.db.hr.e[i,15] <- 2
    
  }
  
  if(decision.db.hr.e[i,3] == 1 & decision.db.hr.e[i,6] >=51 & decision.db.hr.e[i,6] <= 75){
    
    decision.db.hr.e[i,15] <- 3
    
    
  }
  
  if(decision.db.hr.e[i,3] == 1 & decision.db.hr.e[i,6] >= 76){
    
    decision.db.hr.e[i,15] <- 4
    
  }
  
} # closes loop


### check to see if loop categorized correctly in timestep 1
check <- subset(decision.db.hr.e, time == 1)

## we fill in the last column (initial energy category) so that the i.energy category in T = 1
# gets pulled down torigough all time steps for that animal/patch matrix
# this way we can use this category in graphs.

## all zeros in this column we switch to NA's
decision.db.hr.e$i.e.cat[(decision.db.hr.e$i.e.cat) == 0] <- "NA"

## we change this column to numeric
decision.db.hr.e$i.e.cat <- as.numeric(decision.db.hr.e$i.e.cat)

## this is the column that pulls down values until it hits another value, then pulls down again
decision.db.hr.e$i.e.cat <- na.locf(decision.db.hr.e$i.e.cat)

## convert time from a factor to numeric so we can filter
decision.db.hr.e$time <- as.character(decision.db.hr.e$time)
decision.db.hr.e$time <- as.numeric(decision.db.hr.e$time)

## get rid of rows in which time is 0
decision.db.hr.e <- filter(decision.db.hr.e, time > 0)


#################################################################################################################
##################### SIMULATION MODEL - GRAPH FORAGING DECISIONS BY ENERGY CATEGORIES ##########################
#################################################################################################################

### getting stats, means and error for patch choices of each category of initial energy states

## baseline model

decision.db.orig.e$d.patch.type <- as.character(decision.db.orig.e$d.patch.type)
decision.db.orig.e$d.patch.type <- as.numeric(decision.db.orig.e$d.patch.type)

decision.db.orig.e$fe <- as.character(decision.db.orig.e$fe)
decision.db.orig.e$fe <- as.numeric(decision.db.orig.e$fe)

decision.db.orig.e.sum <- group_by(decision.db.orig.e, i.e.cat)

decision.db.orig.e.sum <- summarise(decision.db.orig.e.sum,
                                     pt.mean = mean(d.patch.type), pt.sd = sd(d.patch.type),
                                     fe.mean = mean(fe), fe.sd = sd(fe),
                                     dt.mean = mean(d.travelled), dt.sd = sd(d.travelled))


### low food model

decision.db.lf.e$d.patch.type <- as.character(decision.db.lf.e$d.patch.type)
decision.db.lf.e$d.patch.type <- as.numeric(decision.db.lf.e$d.patch.type)

decision.db.lf.e$fe <- as.character(decision.db.lf.e$fe)
decision.db.lf.e$fe <- as.numeric(decision.db.lf.e$fe)

decision.db.lf.e.sum <- group_by(decision.db.lf.e, i.e.cat)

decision.db.lf.e.sum <- summarise(decision.db.lf.e.sum,
                                    pt.mean = mean(d.patch.type), pt.sd = sd(d.patch.type),
                                    fe.mean = mean(fe), fe.sd = sd(fe),
                                    dt.mean = mean(d.travelled), dt.sd = sd(d.travelled))



### high risk model

decision.db.hr.e$d.patch.type <- as.character(decision.db.hr.e$d.patch.type)
decision.db.hr.e$d.patch.type <- as.numeric(decision.db.hr.e$d.patch.type)

decision.db.hr.e$fe <- as.character(decision.db.hr.e$fe)
decision.db.hr.e$fe <- as.numeric(decision.db.hr.e$fe)

decision.db.hr.e.sum <- group_by(decision.db.hr.e, i.e.cat)

decision.db.hr.e.sum <- summarise(decision.db.hr.e.sum,
                                    pt.mean = mean(d.patch.type), pt.sd = sd(d.patch.type),
                                    fe.mean = mean(fe), fe.sd = sd(fe),
                                    dt.mean = mean(d.travelled), dt.sd = sd(d.travelled))



### GRAPHS OF AVERAGE FORAGING DECISIONS WITHIN INITIAL ENERGY CATEGORIES

name <- c("Baseline", "Baseline", "Baseline", "Baseline")
decision.db.orig.e.sum <- cbind(decision.db.orig.e.sum, name)

name <- c("Low Food", "Low Food", "Low Food", "Low Food")
decision.db.lf.e.sum <- cbind(decision.db.lf.e.sum, name)

name <- c("High Risk", "High Risk", "High Risk", "High Risk")
decision.db.hr.e.sum <- cbind(decision.db.hr.e.sum, name)


decisions.db.e.sum.all <- rbind(decision.db.orig.e.sum, decision.db.lf.e.sum, decision.db.hr.e.sum)

## Baseline Graphs

dpt.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = pt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = pt.mean - pt.sd, ymax = pt.mean + pt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Patch Type Choice")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())


fe.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = fe.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = fe.mean - fe.sd, ymax = fe.mean + fe.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Foraging Effort")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

energy.labs <- c("< 25", "25 - 50", "51 - 75", "> 75")

dt.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = dt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = dt.mean - dt.sd, ymax = dt.mean + dt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Distance Travelled")+
  xlab("Initial Energy State")+
  scale_x_continuous(labels = energy.labs)+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), 
        legend.position = "none")
  

## combine three baseline graphs into one panel of figures

graph.base.for.e <- grid.arrange(arrangeGrob(dpt.base, fe.base, dt.base, ncol = 1))

ggsave(graph.base.for.e, file = "Sim.For.iEnergy.Base.NEW.jpeg", height = 8, width = 5, 
       units = "in", dpi = 600)


## Graphs for all 3 models - Panel of 9 graphs

## Baesline Graphs
dpt.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = pt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = pt.mean - pt.sd, ymax = pt.mean + pt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Patch Type Choice")+
  ggtitle("Baseline")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5))+
  coord_cartesian(ylim = c(23.9, 32.1))

fe.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = fe.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = fe.mean - fe.sd, ymax = fe.mean + fe.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Foraging Effort")+
  scale_y_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2))+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  coord_cartesian(ylim = c(0,1.2))

energy.labs <- c("< 25", "25 - 50", "51 - 75", "> 75")

dt.base <- ggplot(data = subset(decisions.db.e.sum.all, name == "Baseline"), aes(x = i.e.cat, y = dt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = dt.mean - dt.sd, ymax = dt.mean + dt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Distance Travelled")+
  scale_x_continuous(labels = energy.labs)+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), 
        legend.position = "none", axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0,1.35))


## Low Food Graphs

dpt.lf <- ggplot(data = subset(decisions.db.e.sum.all, name == "Low Food"), aes(x = i.e.cat, y = pt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = pt.mean - pt.sd, ymax = pt.mean + pt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Patch Type Choice")+
  ggtitle("Low Food")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5), 
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(23.9, 32.1))

fe.lf <- ggplot(data = subset(decisions.db.e.sum.all, name == "Low Food"), aes(x = i.e.cat, y = fe.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = fe.mean - fe.sd, ymax = fe.mean + fe.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Foraging Effort")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(0,1.2))

energy.labs <- c("< 25", "25 - 50", "51 - 75", "> 75")

dt.lf <- ggplot(data = subset(decisions.db.e.sum.all, name == "Low Food"), aes(x = i.e.cat, y = dt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = dt.mean - dt.sd, ymax = dt.mean + dt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Distance Travelled")+
  scale_x_continuous(labels = energy.labs)+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), 
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(0,1.35))

## High Risk Graphs

dpt.hr <- ggplot(data = subset(decisions.db.e.sum.all, name == "High Risk"), aes(x = i.e.cat, y = pt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = pt.mean - pt.sd, ymax = pt.mean + pt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Patch Type Choice")+
  ggtitle("High Risk")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(23.9, 32.1))

fe.hr <- ggplot(data = subset(decisions.db.e.sum.all, name == "High Risk"), aes(x = i.e.cat, y = fe.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = fe.mean - fe.sd, ymax = fe.mean + fe.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Foraging Effort")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(0,1.2))

energy.labs <- c("< 25", "25 - 50", "51 - 75", "> 75")

dt.hr <- ggplot(data = subset(decisions.db.e.sum.all, name == "High Risk"), aes(x = i.e.cat, y = dt.mean))+
  geom_point(size = 5, pch = 16)+
  geom_errorbar(aes(ymin = dt.mean - dt.sd, ymax = dt.mean + dt.sd), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Distance Travelled")+
  scale_x_continuous(labels = energy.labs)+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), 
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  coord_cartesian(ylim = c(0,1.35))


graph.all.for.e <- grid.arrange(arrangeGrob(dpt.base, dpt.lf, dpt.hr,
                                            fe.base, fe.lf, fe.hr,
                                            dt.base, dt.lf, dt.hr,
                                            ncol = 3, 
                                            bottom = textGrob("Initial Energy State", gp=gpar(fontsize=16))))

ggsave(graph.all.for.e, file = "Sim.For.iEnergy.All.NEW.jpeg", height = 9, width = 8, dpi = 600)

## the baseline graphs are not as wide as low food and high risk graphs b/c there are y-axis labels
# on the baseline graphs - I think I need to use the package gtable and function width to fix this
# not sure how yet - look into this.

#################################################################################################################
################# SIMULTION MODEL - HISTOGRAMS OF ENERGY DISTRIBUTIONS THROUGH TIME #############################
#########################################################################################################

##initial energy at time = 1

## original 
energy.1.orig <- ggplot(subset(decision.db.orig, time == 1 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 1", title = "Baseline")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,10), xlim = c(0,100))
#0-9

## low food
energy.1.lf <- ggplot(subset(decision.db.lf, time == 1 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 1", title = "Low Food")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,10), xlim = c(0,100))
#0-11

## high risk
energy.1.hr <- ggplot(subset(decision.db.hr, time == 1 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 1", title = "High Risk")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,10), xlim = c(0,100))
#0-11



##initial energy at time = 10

## original 
energy.10.orig <- ggplot(subset(decision.db.orig, time == 10 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 10")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,16), xlim = c(0,100))
#0-18

## low food
energy.10.lf <- ggplot(subset(decision.db.lf, time == 10 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 10")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,16), xlim = c(0,100))
#0-13

## high risk
energy.10.hr <- ggplot(subset(decision.db.hr, time == 10 & f.energy > 0), aes(x = i.energy))+
  geom_histogram(binwidth = 2)+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 10")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,16), xlim = c(0,100))
#0-11



##initial energy at time = 20

## original 
energy.20.orig <- ggplot(subset(decision.db.orig, time == 20 & f.energy > 0), aes(x = i.energy))+
  geom_histogram(binwidth = 2)+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 20")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,30), xlim = c(0,100))
#0-30

## low food
energy.20.lf <- ggplot(subset(decision.db.lf, time == 20 & f.energy > 0), aes(x = i.energy))+
  geom_histogram()+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 20")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,30), xlim = c(0,100))
#0-9

## high risk
energy.20.hr <- ggplot(subset(decision.db.hr, time == 20 & f.energy > 0), aes(x = i.energy))+
  geom_histogram(binwidth = 2)+
  coord_cartesian(xlim = c(1,100))+
  theme_classic()+
  labs(subtitle = "Time = 20")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = 0), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  coord_cartesian(ylim = c(0,30), xlim = c(0,100))
#0-13


grid.arrange((arrangeGrob(energy.1.orig, energy.1.lf, energy.1.hr,
                          energy.10.orig, energy.10.lf, energy.10.hr,
                          energy.20.orig, energy.20.lf, energy.20.hr,
                          ncol = 3, 
                          left = textGrob("Number of Individuals", 
                                          rot = 90, vjust = 1, 
                                          gp=gpar(fontsize=16, fontface = "bold")),
                          bottom = textGrob("Initial Energy State", rot = 0, hjust = 0.5,
                                            gp=gpar(fontsize=16, fontface = "bold")))))



#################################################################################################################
################### SIMULATION MODEL - GRAPHS OF ENERGY TRAJECTORIES THROUGH TIME ###############################
#################################################################################################################

## using id energy data
## put these 3 graphs together in a combination plot

### plot of energy torigough time grouped by initial energy category
e.trajectory.orig <- ggplot(subset(decision.db.orig.e, i.e.cat == 4), aes(x = time, y = i.energy))+
  geom_smooth(method = "loess", col = "black")+
  theme_classic()+
  geom_smooth(data = subset(decision.db.orig.e, i.e.cat == 3), linetype = "longdash",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.orig.e, i.e.cat == 2), linetype = "dashed",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.orig.e, i.e.cat == 1), linetype = "dotted",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  ylab("Average Initial Energy")+
  xlab("Time Step")+
  ggtitle("Baseline")+
  theme(axis.text = element_text(size = 12), axis.title = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = -160))+
  coord_cartesian(ylim = c(0,100))


### plot of energy torigough time grouped by initial energy category
e.trajectory.lf <- ggplot(subset(decision.db.lf.e, i.e.cat == 4), aes(x = time, y = i.energy))+
  geom_smooth(method = "loess", col = "black")+
  theme_classic()+
  geom_smooth(data = subset(decision.db.lf.e, i.e.cat == 3), linetype = "longdash",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.lf.e, i.e.cat == 2), linetype = "dashed",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.lf.e, i.e.cat == 1), linetype = "dotted",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  ylab("Average Initial Energy")+
  xlab("Time Step")+
  ggtitle("Low Food")+
  theme(axis.text = element_text(size = 12), axis.title = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = -250))+
  coord_cartesian(ylim = c(0,100))


### plot of energy torigough time grouped by initial energy category
e.trajectory.hr <- ggplot(subset(decision.db.hr.e, i.e.cat == 4), aes(x = time, y = i.energy))+
  geom_smooth(method = "loess", col = "black")+
  theme_classic()+
  geom_smooth(data = subset(decision.db.hr.e, i.e.cat == 3), linetype = "longdash",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.hr.e, i.e.cat == 2), linetype = "dashed",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  geom_smooth(data = subset(decision.db.hr.e, i.e.cat == 1), linetype = "dotted",
              aes(x = time, y = i.energy), method = "loess", col = "black")+
  ylab("Average Initial Energy")+
  xlab("Time Step")+
  ggtitle("High Risk")+
  theme(axis.text = element_text(size = 12), axis.title = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = -9))+
  coord_cartesian(ylim = c(0,100))



grid.arrange((arrangeGrob(e.trajectory.orig, e.trajectory.lf, e.trajectory.hr,
                          ncol = 1, 
                          left = textGrob("Energy State", 
                                          rot = 90, vjust = 1, 
                                          gp=gpar(fontsize=16, fontface = "bold")),
                          bottom = textGrob("Time Step", rot = 0, hjust = 0.5,
                                            gp=gpar(fontsize=16, fontface = "bold")))))


#################################################################################################################
######################### SIMULATION MODEL - BOXPLOTS OF TIMES OF PATCH PREDATION ###############################
#################################################################################################################

#### we need to combine datasets from the three models that contain instances of patch risk
## so that we can generate a graph with time on the y axis and model on the x axis

### First we add a column to the end of each datset that contains the model

## subset decision dataframes to include only instances of depredation in patches
db.orig.e.pr <- subset(decision.db.orig.e, fate == "patch risk")
db.lf.e.pr <- subset(decision.db.lf.e, fate == "patch risk")
db.hr.e.pr <- subset(decision.db.hr.e, fate == "patch risk")

## add the model to the subsetted dataframes
model <- rep("Baseline", 9)
db.orig.e.pr2 <- cbind(db.orig.e.pr, model)

model <- rep("Low Food", 17)
db.lf.e.pr2 <- cbind(db.lf.e.pr, model)

model <- rep("High Risk", 29)
db.hr.e.pr2 <- cbind(db.hr.e.pr, model)

## now we can bind them together into one dataset that contains info for all patch depredations
## for all models

pr.all <- rbind(db.orig.e.pr2, db.lf.e.pr2, db.hr.e.pr2)

## Now we can graph the times of these patch depredations in each model using box plots

ggplot(data = pr.all, aes(y = time, x = model))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))+
  xlab("Model")+
  ylab("Time of Patch Depredation")




################################################################################################################
############# SIMULATION MODEL - RAW #'S & PROP OF IND'S WITH FATES WITHIN ENERGY CATEGORIES ###################
################################################################################################################

## let's get info we need to populate table of proportion of individuals that had given fates,
## within given i.energy categories
## and then we'll graph

##### BASELINE MODEL INFO

## let's look at all instances of patch depredation
p.dep.orig <- subset(decision.db.orig.e, fate == "patch risk")
## there are 9 patch deaths (1 is cat1, 6 are cat2, 2 are cat3)
## cat 1 isn't working properly in baseline model - not sure why yet
# i picked up this patch risk from cat 1, not from assigned category but by looking at data manually.
p.dep.orig

## let's look at all instances of travel depredation
t.dep.orig <- subset(decision.db.orig.e, fate == "travel risk")
## there are no travel deaths

## let's look at all instances of starvation
starve.orig <- subset(decision.db.orig.e, fate == "starved")
## there are no starvation deaths

## let's see how many animals started within each i.energy category
## first, we only care about t = 1, b/c we're looking at i.energy
t1.orig <- subset(decision.db.orig.e, time == 1)

## category 1 (3)
cat1.t1.orig <- subset(t1.orig, i.energy < 25)
nrow(cat1.t1.orig)
## category 2 (37)
cat2.t1.orig <- subset(t1.orig, i.energy > 25 & i.energy < 51)
nrow(cat2.t1.orig)
## category 3 (55)
cat3.t1.orig <- subset(t1.orig, i.energy > 50 & i.energy < 75)
nrow(cat3.t1.orig)
## category 4 (2)
cat4.t1.orig <- subset(t1.orig, i.energy > 75)
nrow(cat4.t1.orig)


#### LOW FOOD MODEL INFO

## let's look at all instances of patch depredation
p.dep.lf <- subset(decision.db.lf.e, fate == "patch risk")
## there are 17 patch deaths (3 are cat1, 4 are cat2, 2 are cat3)
## cat 1 isn't working properly in baseline model - not sure why yet
# i picked up this patch risk from cat 1, not from assigned category but by looking at data manually.
p.dep.lf

## let's look at all instances of travel depredation
t.dep.lf <- subset(decision.db.lf.e, fate == "travel risk")
## 2 of 2 are category 2

## let's look at all instances of starvation
starve.lf <- subset(decision.db.lf.e, fate == "starved")
## 2 of 2 are category 1

## let's see how many animals started within each i.energy category
## first, we only care about t = 1, b/c we're looking at i.energy
t1.lf <- subset(decision.db.lf.e, time == 1)

## category 1 (7)
cat1.t1.lf <- subset(t1.lf, i.energy < 25)
nrow(cat1.t1.lf)
## category 2 (53)
cat2.t1.lf <- subset(t1.lf, i.energy > 25 & i.energy < 51)
nrow(cat2.t1.lf)
## category 3 (36)
cat3.t1.lf <- subset(t1.lf, i.energy > 50 & i.energy < 75)
nrow(cat3.t1.lf)
## category 4 (2)
cat4.t1.lf <- subset(t1.lf, i.energy > 75)
nrow(cat4.t1.lf)

#### HIGH RISK MODEL

## let's look at all instances of patch depredation
p.dep.hr <- subset(decision.db.hr.e, fate == "patch risk")
## there are 17 patch deaths (3 are cat1, 4 are cat2, 2 are cat3)
## cat 1 isn't working properly in baseline model - not sure why yet
# i picked up this patch risk from cat 1, not from assigned category but by looking at data manually.
p.dep.hr

## let's look at all instances of travel depredation
t.dep.hr <- subset(decision.db.hr.e, fate == "travel risk")
## 2 of 2 are category 2

## let's look at all instances of starvation
starve.hr <- subset(decision.db.hr.e, fate == "starved")
## 2 of 2 are category 1

## let's see how many animals started within each i.energy category
## first, we only care about t = 1, b/c we're looking at i.energy
t1.hr <- subset(decision.db.hr.e, time == 1)

## category 1 (7)
cat1.t1.hr <- subset(t1.hr, i.energy < 25)
nrow(cat1.t1.hr)
## category 2 (53)
cat2.t1.hr <- subset(t1.hr, i.energy > 25 & i.energy < 51)
nrow(cat2.t1.hr)
## category 3 (36)
cat3.t1.hr <- subset(t1.hr, i.energy > 50 & i.energy < 75)
nrow(cat3.t1.hr)
## category 4 (2)
cat4.t1.hr <- subset(t1.hr, i.energy > 75)
nrow(cat4.t1.hr)



### Let's make a table of survivorship and type of fate for each model

r.names <- c("model", "e.cat", "total.animals", "total.deaths", "no.dep.patch", "no.dep.travel", "no.starved")

fate.energy.table <- matrix(0,12,7)

colnames(fate.energy.table) <- r.names

## fill in first 2 columns

fate.energy.table[1:4, 1] <- rep("baseline",4)
fate.energy.table[5:8, 1] <- rep("low food",4)
fate.energy.table[9:12, 1] <- rep("high risk",4)

fate.energy.table[ ,2] <- rep(1:4,3)

##baseline rows
fate.energy.table[1,3:7] <- c(3,1,1,0,0)
fate.energy.table[2,3:7] <- c(37,6,6,0,0)
fate.energy.table[3,3:7] <- c(53,2,2,0,0)
fate.energy.table[4,3:7] <- c(2,0,0,0,0)

##low food rows
fate.energy.table[5,3:7] <- c(7,5,3,0,2)
fate.energy.table[6,3:7] <- c(53,6,4,2,0)
fate.energy.table[7,3:7] <- c(36,10,10,0,0)
fate.energy.table[8,3:7] <- c(2,0,0,0,0)

##high risk rows
fate.energy.table[9,3:7] <- c(2,1,1,0,0)
fate.energy.table[10,3:7] <- c(54,18,18,0,0)
fate.energy.table[11,3:7] <- c(35,8,8,0,0)
fate.energy.table[12,3:7] <- c(7,2,2,0,0)

## convert table into a dataframe

fate.energy.table.df <- as.data.frame(fate.energy.table)

## assign variables to the appropriate class

fate.energy.table.df$total.animals <- as.character(fate.energy.table.df$total.animals)
fate.energy.table.df$total.animals <- as.numeric(fate.energy.table.df$total.animals)

fate.energy.table.df$total.deaths <- as.character(fate.energy.table.df$total.deaths)
fate.energy.table.df$total.deaths <- as.numeric(fate.energy.table.df$total.deaths)

fate.energy.table.df$no.dep.patch <- as.character(fate.energy.table.df$no.dep.patch)
fate.energy.table.df$no.dep.patch <- as.numeric(fate.energy.table.df$no.dep.patch)

fate.energy.table.df$no.dep.travel <- as.character(fate.energy.table.df$no.dep.travel)
fate.energy.table.df$no.dep.travel <- as.numeric(fate.energy.table.df$no.dep.travel)

fate.energy.table.df$no.starved <- as.character(fate.energy.table.df$no.starved)
fate.energy.table.df$no.starved <- as.numeric(fate.energy.table.df$no.starved)


### generate a new column for mortality
fate.energy.table.df <- mutate(fate.energy.table.df, mortality = total.deaths / total.animals)

fate.energy.table.df$mortality <- as.numeric(fate.energy.table.df$mortality)


### generate a new column for survivorship
fate.energy.table.df <- mutate(fate.energy.table.df, survivorship = 1 - mortality)

fate.energy.table.df$survivorship <- as.numeric(fate.energy.table.df$survivorship)


## generate a new column for prop. dep in patch
fate.energy.table.df <- mutate(fate.energy.table.df, prop.patch = no.dep.patch / total.animals)

## generate a new column for prop. dep in travel
fate.energy.table.df <- mutate(fate.energy.table.df, prop.travel = no.dep.travel / total.animals)

## generate a new column for prop. starved
fate.energy.table.df <- mutate(fate.energy.table.df, prop.starved = no.starved / total.animals)

## renaming columns so labels work out with graph in ggplot
fate.energy.table.df2 <- fate.energy.table.df

colnames(fate.energy.table.df2) <- c("model","e.cat","total.animals", "total.deaths", 
                                     "no.dep.patch","no.dep.travel", "no.starved", "All",
                                     "survivorship", "Patch.Risk", "Travel.Risk", "Starvation")

#### table is not in a useful format for graphing!

## let's separate the models to make graphing easier

fate.energy.orig <- subset(fate.energy.table.df2, model == "baseline")

fate.energy.orig <- subset(fate.energy.orig, select = c(model, e.cat, All, Patch.Risk, Travel.Risk, Starvation))

fate.energy.orig <- melt(fate.energy.orig, mort.cause = c("All", "Patch.Risk", "Travel.Risk", "Starvation"))



fate.energy.lf <- subset(fate.energy.table.df2, model == "low food")

fate.energy.lf <- subset(fate.energy.lf, select = c(model, e.cat, All, Patch.Risk, Travel.Risk, Starvation))

fate.energy.lf <- melt(fate.energy.lf, mort.cause = c("All", "Patch.Risk", "Travel.Risk", "Starvation"))



fate.energy.hr <- subset(fate.energy.table.df2, model == "high risk")

fate.energy.hr <- subset(fate.energy.hr, select = c(model, e.cat, All, Patch.Risk, Travel.Risk, Starvation))

fate.energy.hr <- melt(fate.energy.hr, mort.cause = c("All", "Patch.Risk", "Travel.Risk", "Starvation"))

### i can't get all plot titles to move down using vjust? 

## baseline model
bl <- ggplot(fate.energy.orig, aes(x = variable, y = value))+
  geom_point(aes(color = e.cat), size = 5, pch = 16)+
  scale_color_manual(values = c("grey80", "grey50", "grey20", "black"))+
  theme_classic()+
  ylab("Proportion of Individuals")+
  xlab("Cause of Mortality")+
  ggtitle("Baseline")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), 
        legend.position = "none", plot.title = element_text(size = 16, hjust = 0.5))
  coord_cartesian(ylim = c(0,0.8))

## low food model
lf <- ggplot(fate.energy.lf, aes(x = variable, y = value))+
  geom_point(aes(color = e.cat), size = 5, pch = 16)+
  scale_color_manual(values = c("grey80", "grey50", "grey20", "black"))+
  theme_classic()+
  ylab("Proportion of Individuals")+
  xlab("Source of Mortality")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        legend.position = "none", axis.title.x = element_blank(),
        plot.title = element_text(size = 16, hjust= 0.5))+
  ggtitle("Low Food")+
  coord_cartesian(ylim = c(0,0.8))

## high risk model
hr <- ggplot(fate.energy.hr, aes(x = variable, y = value))+
  geom_point(aes(color = e.cat), size = 5, pch = 16)+
  scale_color_manual(values = c("grey80", "grey50", "grey20", "black"))+
  theme_classic()+
  ylab("Proportion of Individuals")+
  xlab("Cause of Mortality")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(size = 16, hjust= 0.5))+
  ggtitle("High Risk")+
  coord_cartesian(ylim = c(0,0.8))

grid.arrange(bl, lf, hr, ncol = 1)




### EXTRA CODE

#################################################################################################################
#################### DIFFERENCE MATRIX & GRAPHS - OPTIMALITY MODE - LOW FOOD - BASELINE ########################
################################################################################################################

# GENERATE DIFFERENCE DF FOR LOW FOOD - ORIGINAL 

## rename original data as such after importing from project
fmax.df.orig <- fmax.df

##rename low food data as such after importing from project
fmax.df.lf <- fmax.df

## for each time, i.patch, energy, depletion, d.patch
## we want the difference in f.effort, d.patch.type, d.travelled, fitness

## each of these creates a vector that is the difference between the two corresponding
# columns in each df (low food - original)
f.effort.diff <- fmax.df.lf$f.effort - fmax.df.orig$f.effort
d.patch.type.diff <- fmax.df.lf$d.patch.type - fmax.df.orig$d.patch.type
d.travlled.diff <- fmax.df.lf$d.travelled - fmax.df.orig$d.travelled
fitness.diff <- fmax.df.lf$fitness - fmax.df.orig$fitness

## we create dfiference matrix by combining columns
# we want to keep the same first 5 columns (initial parameters)
# columns 6 & 7 we record d.patch for both orig and lf
# we replace the last 4 columns with the difference values
fmax.matrix.lf.effect <- cbind(fmax.df.orig$p.matrix, fmax.df.orig$time, fmax.df.orig$i.patch, 
                               fmax.df.orig$energy, fmax.df.orig$p.p.dep, 
                               fmax.df.orig$d.patch, fmax.df.lf$d.patch,
                               d.patch.type.diff, d.travlled.diff, f.effort.diff, fitness.diff, 
                               fmax.df.orig$row.no)

# we give this difference matrix the same column names as the others so that we can 
# copy and paste ggplots easily
colnames(fmax.matrix.lf.effect) <- c("p.matrix", "time", "i.patch", "energy", "p.p.dep", 
                                     "orig.d.patch", "lf.d.ppatch", 
                                     "d.patch.type", "d.travelled", "f.effort", "fitness",
                                     "row.no")

# we convert difference matrix to a difference data frame
fmax.df.lf.effect <- as.data.frame(fmax.matrix.lf.effect)

# save difference dataframe to r project
#save(fmax.df.lf.effect, file = "FmaxDFLFEffect.RData")



## LOW FOOD EFFECT ##

### summarizes by time and energy
fmax.df.lf.effect.time <- group_by(fmax.df.lf.effect, time, energy)

fmax.df.lf.effect.time <- summarise(fmax.df.lf.effect.time, 
                                    dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                    fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                    dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                    fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))

## summarizes by time and previous patch depletion
fmax.df.lf.effect.dep.time <- group_by(fmax.df.lf.effect, p.p.dep, time)

fmax.df.lf.effect.dep.time <- summarise(fmax.df.lf.effect.dep.time, 
                                        dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                        fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                        dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                        fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


## summarizes by eneryg and previous patch depletion
fmax.df.lf.effect.dep.energy <- group_by(fmax.df.lf.effect, p.p.dep, energy)

fmax.df.lf.effect.dep.energy <- summarise(fmax.df.lf.effect.dep.energy, 
                                          dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                          fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                          dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                          fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))



## LOW FOOD EFFECT - GRAPH SENSITIVITY ANALYSIS ##

#make sure our variables are in the correct class
#str(fmax.df.time)
#str(fmax.df.depletion$fit.mean)

# subset data by time

fmax.df.lf.effect.time.1 <- filter(fmax.df.lf.effect.time, time == 1)
fmax.df.lf.effect.time.10 <- filter(fmax.df.lf.effect.time, time == 10)
fmax.df.lf.effect.time.20 <- filter(fmax.df.lf.effect.time, time == 20)

# FITNESS x ENERGY x TIME 

f.e.lf.effect <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.72,0.0))+
  ggtitle("Effect of Low Food Availability")


# D.PATCH.TYPE x ENERGY x TIME 

dpt.e.time.diff <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim=c(-15,13))+
  ggtitle("Effect of Low Food Availability")


# F.EFFORT x ENERGY x TIME 

fe.e.time.diff <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim=c(-1.0,0.7))+
  ggtitle("Effect of Low Food Availability")


# F.EFFORT x D.PATCH.TYPE x TIME 

fe.dpt.time.diff <- ggplot(fmax.df.lf.effect.time.1, aes(x = dpt.mean, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Destination Patch Type")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(xlim=c(-1.5,8), ylim=c(-0.4,0.6))+
  ggtitle("Effect of Low Food Availability")


# D.TRAVELLED x ENERGY x TIME 


dt.e.time.diff <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim = c(-0.4,0.82))+
  ggtitle("Effect of Low Food Availability")



# DEPLETION X D.TRAVELLED X ENERGY  

dt.dep.energy <- ggplot(subset(fmax.df.lf.effect.dep.energy, energy == 1), aes(x = p.p.dep, y = dt.mean))+
  geom_point(size = 4, pch = 21, fill = "darkolivegreen1")+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.dep.energy, energy == 25), size = 4, pch = 21, fill = "darkolivegreen2")+
  geom_point(data = subset(fmax.df.lf.effect.dep.energy, energy == 50), size = 4, pch = 21, fill = "darkolivegreen3")+
  geom_point(data = subset(fmax.df.lf.effect.dep.energy, energy == 75), size = 4, pch = 21, fill = "darkolivegreen4")+
  geom_point(data = subset(fmax.df.lf.effect.dep.energy, energy == 100), size =4, pch = 21, fill = "darkolivegreen")+
  xlab("Patch Depletion")+
  ylab("Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim = c(-0.4,0.8))+
  ggtitle("Effect of Low Food Availability")


# DEPLETION X D.TRAVELLED X TIME  

dt.dep.time <- ggplot(subset(fmax.df.lf.effect.dep.time, time == 1), aes(x = p.p.dep, y = dt.mean))+
  geom_point(size = 4, pch = 21,fill = "grey99")+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.dep.time, time == 10), size = 4, pch = 21, fill = "grey50")+
  geom_point(data = subset(fmax.df.lf.effect.dep.time, time == 20), size = 4, pch = 21, fill = "black")+
  xlab("Patch Depletion")+
  ylab("Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim = c(-0.35,0.2))+
  ggtitle("Effect of Low Food Availability")


#################################################################################################################
#################### DIFFERENCE MATRIX & GRAPHS -OPTIMALITY MODEL - HIGH RISK - BASELINE #######################
################################################################################################################# 

## GENERATE DIFFERENCE DF FOR HIGH RISK - ORIGINAL 

## rename original data as such after importing from project
fmax.df.orig <- fmax.df

##rename low food data as such after importing from project
fmax.df.hr <- fmax.df

## for each time, i.patch, energy, depletion, d.patch
## we want the difference in f.effort, d.patch.type, d.travelled, fitness

## each of these creates a vector that is the difference between the two corresponding
# columns in each df (low food - original)
f.effort.diff <- fmax.df.hr$f.effort - fmax.df.orig$f.effort
d.patch.type.diff <- fmax.df.hr$d.patch.type - fmax.df.orig$d.patch.type
d.travlled.diff <- fmax.df.hr$d.travelled - fmax.df.orig$d.travelled
fitness.diff <- fmax.df.hr$fitness - fmax.df.orig$fitness

## we create dfiference matrix by combining columns
# we want to keep the same first 5 columns (initial parameters)
# we want to replace the last 5 columns with the difference values
fmax.matrix.hr.effect <- cbind(fmax.df.orig$p.matrix, fmax.df.orig$time, fmax.df.orig$i.patch, 
                               fmax.df.orig$energy, fmax.df.orig$p.p.dep, 
                               fmax.df.orig$d.patch, fmax.df.hr$d.patch,
                               d.patch.type.diff, d.travlled.diff, f.effort.diff, fitness.diff, 
                               fmax.df.orig$row.no)

# we give this difference matrix the same column names as the others so that we can 
# copy and paste ggplots easily
colnames(fmax.matrix.hr.effect) <- c("p.matrix", "time", "i.patch", "energy", "p.p.dep", 
                                     "orig.d.patch", "lf.d.ppatch", 
                                     "d.patch.type", "d.travelled", "f.effort", "fitness",
                                     "row.no")

# we convert difference matrix to a difference data frame
fmax.df.hr.effect <- as.data.frame(fmax.matrix.hr.effect)

# save difference dataframe to r project
#save(fmax.df.hr.effect, file = "FmaxDFHREffect.RData")




fmax.df.hr.effect.time <- group_by(fmax.df.hr.effect, time, energy)

fmax.df.hr.effect.time <- summarise(fmax.df.hr.effect.time, 
                                    dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                    fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                    dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                    fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))

## summarizes by time and previous patch depletion
fmax.df.hr.effect.dep.time <- group_by(fmax.df.hr.effect, p.p.dep, time)

fmax.df.hr.effect.dep.time <- summarise(fmax.df.hr.effect.dep.time, 
                                        dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                        fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                        dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                        fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


## summarizes by eneryg and previous patch depletion
fmax.df.hr.effect.dep.energy <- group_by(fmax.df.hr.effect, p.p.dep, energy)

fmax.df.hr.effect.dep.energy <- summarise(fmax.df.hr.effect.dep.energy, 
                                          dpt.mean = mean(d.patch.type), dpt.sd = sd(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                          fe.mean = mean(f.effort), fe.sd = sd(f.effort), fe.se = (sd(f.effort))/sqrt(n()),
                                          dt.mean = mean(d.travelled), dt.sd = sd(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()),
                                          fit.mean = mean(fitness), fit.sd = sd(fitness), fit.se = (sd(fitness))/sqrt(n()))


#make sure our variables are in the correct class
#str(fmax.df.time)
#str(fmax.df.depletion$fit.mean)

# subset data by time

fmax.df.hr.effect.time.1 <- filter(fmax.df.hr.effect.time, time == 1)
fmax.df.hr.effect.time.10 <- filter(fmax.df.hr.effect.time, time == 10)
fmax.df.hr.effect.time.20 <- filter(fmax.df.hr.effect.time, time == 20)

# FITNESS x ENERGY x TIME 

f.e.hr.effect <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.72,0.0))+
  ggtitle("Effect of High Predation Risk")


# D.PATCH.TYPE x ENERGY x TIME 

dpt.e.time.diff <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim=c(-15,13))+
  ggtitle("Effect of High Predation Risk")


# F.EFFORT x ENERGY x TIME 

fe.e.time.diff <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim=c(-1.0,0.7))+
  ggtitle("Effect of High Predation Risk")


# F.EFFORT x D.PATCH.TYPE x TIME 

fe.dpt.time.diff <- ggplot(fmax.df.hr.effect.time.1, aes(x = dpt.mean, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Destination Patch Type")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(xlim=c(-1.5,8), ylim=c(-0.4,0.6))+
  ggtitle("Effect of High Predation Risk")


# D.TRAVELLED x ENERGY x TIME 

dt.e.time.diff <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  #coord_cartesian(ylim = c(-0.4,0.82))+
  ggtitle("Effect of High Predation Risk")


# DEPLETION X D.TRAVELLED X ENERGY  

dt.dep.energy <- ggplot(subset(fmax.df.hr.effect.dep.energy, energy == 1), aes(x = p.p.dep, y = dt.mean))+
  geom_point(size = 4, pch = 21, fill = "darkolivegreen1")+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.dep.energy, energy == 25), size = 4, pch = 21, fill = "darkolivegreen2")+
  geom_point(data = subset(fmax.df.hr.effect.dep.energy, energy == 50), size = 4, pch = 21, fill = "darkolivegreen3")+
  geom_point(data = subset(fmax.df.hr.effect.dep.energy, energy == 75), size = 4, pch = 21, fill = "darkolivegreen4")+
  geom_point(data = subset(fmax.df.hr.effect.dep.energy, energy == 100), size =4, pch = 21, fill = "darkolivegreen")+
  xlab("Patch Depletion")+
  ylab("Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim = c(-0.4,0.8))+
  ggtitle("Effect of High Predation Risk")


# DEPLETION X D.TRAVELLED X TIME  

dt.dep.time <- ggplot(subset(fmax.df.hr.effect.dep.time, time == 1), aes(x = p.p.dep, y = dt.mean))+
  geom_point(size = 4, pch = 21,fill = "grey99")+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.dep.time, time == 10), size = 4, pch = 21, fill = "grey50")+
  geom_point(data = subset(fmax.df.hr.effect.dep.time, time == 20), size = 4, pch = 21, fill = "black")+
  xlab("Patch Depletion")+
  ylab("Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim = c(-0.35,0.2))+
  ggtitle("Effect of High Predation Risk")







#################################################################################################################
#################### DIFFERENCE GRAPHS PAIRED - OPTIMALITY MODEL - LOW FOOD AND HIGH RISK EFFECTS ###############
#################################################################################################################

# FITNESS x ENERGY x TIME 

f.e.lf.effect <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.72,0.0))
#ggtitle("Effect of Low Food Availability")

f.e.hr.effect <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_blank())+
  coord_cartesian(ylim=c(-0.72,0.0))
#ggtitle("Effect of High Predation Risk")

grid.arrange(f.e.lf.effect, f.e.hr.effect, ncol = 2)


# D.PATCH.TYPE x ENERGY x TIME 

dpt.e.lf.effect <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))
coord_cartesian(ylim=c(-8,8))+
  #ggtitle("Effect of Low Food Availability")
  
  
  dpt.e.hr.effect <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_blank())+
  coord_cartesian(ylim=c(-8,8))
#ggtitle("Effect of High Predation Risk")


grid.arrange(dpt.e.lf.effect, dpt.e.hr.effect, ncol = 2)


# F.EFFORT x ENERGY x TIME 

fe.e.lf.effect <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.65,0.82))
#ggtitle("Effect of Low Food Availability")


fe.e.hr.effect <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_blank())+
  coord_cartesian(ylim=c(-0.65,0.82))
#ggtitle("Effect of High Predation Risk")

grid.arrange(fe.e.lf.effect, fe.e.hr.effect, ncol = 2)


# D.TRAVELLED x ENERGY x TIME 

dt.e.lf.effect <- ggplot(fmax.df.lf.effect.time.1, aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.lf.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.lf.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim = c(-0.75,0.7))
#ggtitle("Effect of Low Food Availability")


dt.e.hr.effect <- ggplot(fmax.df.hr.effect.time.1, aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "grey99", size = 3)+
  theme_classic()+
  geom_point(data = fmax.df.hr.effect.time.10, pch = 21, fill = "grey50", size = 3)+
  geom_point(data = fmax.df.hr.effect.time.20, pch = 21, fill = "black", size = 3)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_blank())+
  coord_cartesian(ylim = c(-0.75,0.7))
#ggtitle("Effect of High Predation Risk")

grid.arrange(dt.e.lf.effect, dt.e.hr.effect, ncol = 2)



## SIMPLE GRAPHS BY TIME & ENERGY FROM OPTIMALITY MODEL - EFFECTS OF LOW FOOD AND HIGH RISK ##

# effect of low food
# FITNESS x ENERGY x TIME 

f.e.time.diff <- ggplot(subset(fmax.df.lf.effect.time.1, energy == 10 | energy == 50 | energy == 90), 
                        aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.lf.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.25,0.01))+
  ggtitle("Effect of Low Food Availability")


# effect of high predation risk
# FITNESS x ENERGY x TIME 

f.e.time.diff <- ggplot(subset(fmax.df.hr.effect.time.1, energy == 10 | energy == 50 | energy == 90), 
                        aes(x = energy, y = fit.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.hr.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Exp. Fut. Fitness")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.25,0.01))+
  ggtitle("Effect of High Predation Risk")


## effect of low food availability
# D.PATCH.TYPE x ENERGY x TIME 

dpt.e.time.diff <- ggplot(subset(fmax.df.lf.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                          aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.lf.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-12.0,10))+
  ggtitle("Effect of Low Food Availability")


## effect of high predation risk
# D.PATCH.TYPE x ENERGY x TIME 

dpt.e.time.diff <- ggplot(subset(fmax.df.hr.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                          aes(x = energy, y = dpt.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.hr.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Dest. Patch Type")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-12.0,10))+
  ggtitle("Effect of High Predation Risk")


## effect of low food
# F.EFFORT x ENERGY x TIME

fe.e.time.diff <- ggplot(subset(fmax.df.lf.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                         aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.lf.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.5,0.1))+
  ggtitle("Effect of Low Food Availability")


## effect of predation risk
# F.EFFORT x ENERGY x TIME 

fe.e.time.diff <- ggplot(subset(fmax.df.hr.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                         aes(x = energy, y = fe.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.hr.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Foraging Effort")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  coord_cartesian(ylim=c(-0.5,0.1))+
  ggtitle("Effect of High Predation Risk")


# effect of low food
#D.TRAVELLED x ENERGY x TIME 


dt.e.time.diff <- ggplot(subset(fmax.df.lf.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                         aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.lf.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.lf.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  ggtitle("Effect of Low Food Availability")+
  coord_cartesian(ylim = c(-0.2,0.2))


# effect of high predation risk
# D.TRAVELLED x ENERGY x TIME 


dt.e.time.diff <- ggplot(subset(fmax.df.hr.effect.time.1, energy == 10 | energy == 50 | energy == 90),
                         aes(x = energy, y = dt.mean))+
  geom_point(pch = 21, fill = "lightcyan", size = 5)+
  theme_classic()+
  geom_point(data = subset(fmax.df.hr.effect.time.10, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "dodgerblue", size = 5)+
  geom_point(data = subset(fmax.df.hr.effect.time.20, energy == 10 | energy == 50 | energy == 90),
             pch = 21, fill = "darkblue", size = 5)+
  xlab("Energy State")+
  ylab("Diff. in Distance Travelled")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5))+
  ggtitle("Effect of High Predation Risk")+
  coord_cartesian(ylim = c(-0.2,0.2))




















##################################################################################################
############################# OTHER SUMMARIES OF OPTIMALITY DATA #################################
##################################################################################################

## Generate a new matrix that holds average and se 
## of patch type choice for each energy level (across i.patches & patch matrices) 
## of foraging effort for each energy level (across ipatches & patch matrices)
## of fitness for each energy level (across ipatches & patch matrices)
## and all for each time step

pm <- c(1:200)
i.patch <- c(1:25)
e <- 1
t <- 1
results.means <- matrix(0,2000,10)
counter <- 0

for (t in 1:20){
  for (e in 1:100){
    pt.mean <- mean(fmax.results.matrix.risky[pm,t,i.patch,e,3])
    pt.se <- (sd(fmax.results.matrix.risky[pm,t,i.patch,e,3])) / sqrt(25)
    
    fe.mean <- mean(fmax.results.matrix.risky[pm,t,i.patch,e,1])
    fe.se <- (sd(fmax.results.matrix.risky[pm,t,i.patch,e,1])) / sqrt(25)
    
    fit.mean <- mean(fmax.results.matrix.risky[pm,t,i.patch,e,4])
    fit.se <- (sd(fmax.results.matrix.risky[pm,t,i.patch,e,4])) / sqrt(25)
    
    dt.mean <- mean(fmax.results.matrix.risky[pm,t,i.patch,e,5])
    dt.se <- (sd(fmax.results.matrix.risky[pm,t,i.patch,e,5])) / sqrt(25)
    
    results.means[e+(counter*100),1] <- e
    results.means[e+(counter*100),2] <- t
    results.means[e+(counter*100),3] <- pt.mean
    results.means[e+(counter*100),4] <- pt.se
    results.means[e+(counter*100),5] <- fe.mean
    results.means[e+(counter*100),6] <- fe.se
    results.means[e+(counter*100),7] <- fit.mean
    results.means[e+(counter*100),8] <- fit.se
    results.means[e+(counter*100),9] <- dt.mean
    results.means[e+(counter*100),10] <- dt.se
  }
  counter <- counter + 1
}

# save results.means as a databse

results.means.db <- as.data.frame(results.means)

colnames(results.means.db) <- c("energy", "time", "pt.mean", "pt.se", 
                                "fe.mean", "fe.se", "fit.mean", "fit.se", "dt.mean", "dt.se")

## This loops give us data for next graph
## It give us destination patch type and foraging effort for all energy levels, 
#initial patches and time steps

results.all <- matrix(0,52500,6)

pm <- c(1:200)
e <- 1
i.patch <- 1
time <- 1
counter <- 0


for(time in 1:20){
  
  for(i.patch in 1:25){
    
    for(e in 1:100){
      p.d <- mean(fmax.results.matrix.orig[pm,time, i.patch, e, 3]) 
      f.e <- mean(fmax.results.matrix.orig[pm,time, i.patch, e, 1])
      fit <- mean(fmax.results.matrix.orig[pm,time, i.patch, e, 4])
      
      
      results.all[e + counter, 1] <- time
      results.all[e + counter, 2] <- i.patch
      results.all[e + counter, 3] <- e
      results.all[e + counter, 4] <- p.d
      results.all[e + counter, 5] <- f.e
      results.all[e + counter, 6] <- fit
      
    }
    
    counter <- counter + 100
    
  }
}


results.all <- as.data.frame(results.all)
colnames(results.all) <- c("time", "i.patch", "e", "pt", "fe", "fit")



### Make a new matrix with average foraging effort with se per destination patch type for each time step

results.all.grouped <- group_by(results.all, time, pt) 

results.all.summary <- summarise(results.all.grouped, 
                                 fe.mean = mean(fe), fe.sd = sd(fe), fe.count = n(), fe.se = (sd(fe))/sqrt(n()))


results.all.grouped.e <- group_by(results.all, e, time)

results.all.summary.e <- summarise(results.all.grouped.e, 
                                   pt.mean = mean(pt), pt.sd = sd(pt), pt.count = n(), pt.se = (sd(pt))/sqrt(n()),
                                   fe.mean = mean(fe), fe.sd = sd(fe), fe.count = n(), fe.se = (sd(fe))/sqrt(n()),
                                   fit.mean = mean(fit), fit.sd = sd(fit), fit.count = n(), fit.se = (sd(fit))/sqrt(n()))






# GRAPH OPTIMALTY RESULTS 



## Graphs of the e.by.pt matrix comparing two parameters at a time across 
# all possible initial patches
# and the relationshps are graphed at times 5, 10, 15 and 20


par(mfrow = c(1,1))

# ENERGY VS. DESTINATION PATCH TYPE 

plot(results.means[ ,3] ~ results.means[ ,1], subset=(results.means[ ,2]==20), 
     col = "black", pch=16, cex=1.5, cex.lab=1.5, ylim = c(0,30), ylab = "Destination Patch Type", 
     xlab = "Energy", axes = FALSE)
axis(side = 1, at = c(0,20,40,60,80,100), cex.axis = 1.5)
axis(side = 2, at = c(0,10,20,30), cex.axis = 1.5)
points(results.means[ ,3] ~ results.means[ ,1], subset=(results.means[ ,2]==15), col = "grey50", pch=16, cex=1.5)
points(results.means[ ,3] ~ results.means[ ,1], subset=(results.means[ ,2]==10), col = "grey80", pch=16, cex=1.5)
points(results.means[ ,3] ~ results.means[ ,1], subset=(results.means[ ,2]==5), col = "black", cex=1.5)
#arrows(e.by.pt[ ,1], (e.by.pt[ ,3] - e.by.pt[ ,4]), e.by.pt[ ,1], (e.by.pt[ ,3] + e.by.pt[ ,4]), length = 0.05, angle = 90, code = 3)


# ENERGY VS. FORAGING EFFORT 

plot(results.means[ ,5] ~ results.means[ ,1], subset=(results.means[ ,2]==20), col = "black", pch=16,
     cex = 1.5, cex.lab=1.5, ylim = c(0,4), ylab = "Foraging Effort", xlab = "Energy", axes = FALSE)
axis(side = 1, at = c(0,20,40,60,80,100), cex.axis = 1.5)
axis(side = 2, at = c(0,1,2,3,4), cex.axis = 1.5)
points(results.means[ ,5] ~ results.means[ ,1], subset=(results.means[ ,2]==15), col = "grey50", pch=16, cex=1.5)
points(results.means[ ,5] ~ results.means[ ,1], subset=(results.means[ ,2]==10), col = "grey80", pch=16, cex=1.5)
points(results.means[ ,5] ~ results.means[ ,1], subset=(results.means[ ,2]==5), col = "black", cex=1.5)
#arrows(e.by.pt[ ,1], (e.by.pt[ ,5] - e.by.pt[ ,6]), e.by.pt[ ,1], (e.by.pt[ ,5] + e.by.pt[ ,6]), length = 0.05, angle = 90, code = 3)

# ENERGY VS. DISTANCE TRAVELLED

plot(results.means[ ,9] ~ results.means[ ,1], subset=(results.means[ ,2]==20), 
     col = "black", pch=16, cex = 1.5, cex.lab=1.5, 
     ylim = c(0,6), ylab = "Distance Travelled", xlab = "Energy", axes = FALSE)
axis(side = 1, at = c(0,20,40,60,80,100), cex.axis = 1.5)
axis(side = 2, at = c(0,1,2,3,4,5,6), cex.axis = 1.5)
points(results.means[ ,9] ~ results.means[ ,1], subset=(results.means[ ,2]==15), col = "grey50", pch=16, cex=1.5)
points(results.means[ ,9] ~ results.means[ ,1], subset=(results.means[ ,2]==10), col = "grey80", pch=16, cex=1.5)
points(results.means[ ,9] ~ results.means[ ,1], subset=(results.means[ ,2]==5), col = "black", cex=1.5)

#arrows(e.by.pt[ ,1], (e.by.pt[ ,9] - e.by.pt[ ,10]), e.by.pt[ ,1], (e.by.pt[ ,9] + e.by.pt[ ,10]), length = 0.05, angle = 90, code = 3)

### Make a graph using graph.matrix.summary new matrix of foraging efforts per patch type per time step ###

#  PATCH TYPE VS. FORAGING EFFORT 

plot(fe.mean ~ pt, subset = (time == 20), xlim = c(0,30), ylim = c(0,4), col = "black", pch = 16, 
     cex = 1.5, cex.lab = 1.5, ylab = "Mean Foraging Effort", xlab = "Patch Type",
     data = results.all.summary, axes = FALSE)
axis(side = 1, at=c(0,10,20,30), cex.axis = 1.5)
axis(side = 2, at=c(0,1,2,3,4), cex.axis = 1.5)
points(fe.mean ~ pt, subset = (time == 15), col = "grey50", pch=16, cex=1.5, data = results.all.summary)
points(fe.mean ~ pt, subset = (time == 10), col = "grey80", pch=16, cex=1.5, data = results.all.summary)
points(fe.mean ~ pt, subset = (time == 5), col = "black", cex=1.5, data = results.all.summary)
#arrows(graph.matrix.summary$pt, graph.matrix.summary$fe.mean - graph.matrix.summary$fe.se, 
#       graph.matrix.summary$pt, graph.matrix.summary$fe.mean + graph.matrix.summary$fe.se, 
#       length = 0.05, angle = 90, code = 3)

## Fitness Graphs ##

par(mfrow = c(1,2))

# ENERGY VS. EXPECTED FUTURE FITNESS 

plot(results.means[ ,7] ~ results.means[ ,1], subset=(results.means[ ,2]==20), 
     col = "black", pch=16, cex=1.5, cex.lab=1.5, ylim = c(0,1), 
     ylab = "Expected Future Fitness", xlab = "Energy", axes = FALSE)
axis(side = 1, at = c(0,20,40,60,80,100), cex.axis = 1.5)
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1.0), cex.axis = 1.5)
points(results.means[ ,7] ~ results.means[ ,1], subset=(results.means[ ,2]==15), col = "grey50", pch=16, cex=1.5)
points(results.means[ ,7] ~ results.means[ ,1], subset=(results.means[ ,2]==10), col = "grey80", pch=16, cex=1.5)
points(results.means[ ,7] ~ results.means[ ,1], subset=(results.means[ ,2]==5), col = "black", cex=1.5)
#arrows(e.by.pt[ ,1], (e.by.pt[ ,7] - e.by.pt[ ,8]), e.by.pt[ ,1], (e.by.pt[ ,7] + e.by.pt[ ,8]), length = 0.05, angle = 90, code = 3)


#  PATCH TYPE VS. EXPECTED FUTURE FITNESS
plot(results.means[ ,7] ~ results.means[ ,3], subset=(results.means[ ,2]==20), 
     col = "black", pch=16, cex.lab=1.5, ylim = c(0,1), xlim = c(0,30), cex=1.5,
     ylab= "Expected Future Fitness", xlab = "Patch Type", axes = FALSE)
axis(side = 1, at = c(0,10,20,30), cex.axis = 1.5)
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1.0), cex.axis = 1.5)
points(results.means[ ,7] ~ results.means[ ,3], subset=(results.means[ ,2]==15), col = "grey50", pch=16, cex=1.5)
points(results.means[ ,7] ~ results.means[ ,3], subset=(results.means[ ,2]==10), col = "grey80", pch=16, cex=1.5)
points(results.means[ ,7] ~ results.means[ ,3], subset=(results.means[ ,2]==5), col = "black", cex=1.5)
#arrows(e.by.pt[ ,3], (e.by.pt[ ,7] - e.by.pt[ ,8]), e.by.pt[ ,3], (e.by.pt[ ,7] + e.by.pt[ ,8]), length = 0.05, angle = 90, code = 3)





## AVERAGES & VARIANCES ACROSS PATCH MATRICES 


## FIRST ACROSS ALL INITITAL PATCHES
# So, means and standard errors are made up of data across 
# initial patches and across patch matrices

# we'll use the databse we already generated results.means
# we have means and standard errors for the following parameters
# across i.patches and patch matrices

# col. headers <- (1) energy, (2) time, (3) pt.mean (4) pt.se, (5) fe. mean, (6) fe.se
# (7) fit.mean, (8) fit.se, (9) dt.mean, (10) dt.se




# Graph Means x Std. Error (patch type and foraging effort) 

ggplot(results.means.db, aes(x = pt.mean, y = pt.se))+
  geom_point()+
  theme_classic()+
  ylab("Std. Error in Destination Patch Type")+
  xlab("Mean Destination Patch Type")

ggplot(results.means.db, aes(x = fe.mean, y = fe.se))+
  geom_point()+
  theme_classic()+
  ylab("Std. Error in Foraging Effort")+
  xlab("Mean Foraging Effort")




# Table x = energy, y = time, cell fill = destination patch type variance

pt.se.table <- xtabs(pt.se~time+energy, data=results.means.db)

#nrow(test)
#ncol(test)

pt.se.df <- as.data.frame.matrix(pt.se.table)

pt.se.df



# Table x = energy, y = time, cell fill = foraging effort variance

fe.se.table <- xtabs(fe.se~time+energy, data=results.means.db)

#nrow(test)
#ncol(test)

fe.se.df <- as.data.frame.matrix(fe.se.table)

fe.se.df

# Histograms of Varaince

ggplot(results.means.db, aes(x = fe.se))+
  geom_histogram()+
  theme_classic()

ggplot(results.means.db, aes(x = pt.se))+
  geom_histogram()+
  theme_classic()




# FOR EACH INITITAL PATCH 

# Here we get means for destination patch type and foraging effort for each
# initial patch
# so, we're only averaging across patch matrices - here the variances are only
# representative of variance across patch matrices

pm <- c(1:200)
i.patch <- 1
e <- 1
t <- 1
results.means.pm <- matrix(0,50000,11)
counter <- 0

for (i.patch in 1:25){
  for (t in 1:20){
    for (e in 1:100){
      pt.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,3])
      pt.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,3])) / sqrt(25)
      
      fe.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,1])
      fe.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,1])) / sqrt(25)
      
      fit.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,4])
      fit.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,4])) / sqrt(25)
      
      dt.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,5])
      dt.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,5])) / sqrt(25)
      
      results.means.pm[e+(counter*100),1] <- i.patch
      results.means.pm[e+(counter*100),2] <- e
      results.means.pm[e+(counter*100),3] <- t
      results.means.pm[e+(counter*100),4] <- pt.mean
      results.means.pm[e+(counter*100),5] <- pt.se
      results.means.pm[e+(counter*100),6] <- fe.mean
      results.means.pm[e+(counter*100),7] <- fe.se
      results.means.pm[e+(counter*100),8] <- fit.mean
      results.means.pm[e+(counter*100),9] <- fit.se
      results.means.pm[e+(counter*100),10] <- dt.mean
      results.means.pm[e+(counter*100),11] <- dt.se
    }
    counter <- counter + 1
  }
}

results.means.pm <- as.data.frame(results.means.pm)

colnames(results.means.pm) <- c("i.patch", "energy", "time", "pt.mean", "pt.se", 
                                "fe.mean", "fe.se", "fit.mean", "fit.se", "dt.mean", "dt.se")




## we can generate graph of means x variances (each color represents 1 initial patch)


## destination patch type
ggplot(results.means.pm, aes(x = pt.mean, y = pt.se, col = i.patch))+
  geom_point()+
  theme_classic()+
  xlab("Mean Destination Patch Type")+
  ylab("Std. Error of Destination Patch Type")

## foraging effot
ggplot(results.means.pm, aes(x = fe.mean, y = fe.se, col = i.patch))+
  geom_point()+
  theme_classic()+
  xlab("Mean Foraging Effort")+
  ylab("Std. Error of Foraging Effort")


# FOR EACH PATCH MATRIX 

## now we look at mean and standard error of choices across initial patches, 
# for each patch matrix


pm <- 1
i.patch <- c(1:25)
e <- 1
t <- 1
results.means.ip <- matrix(0,400000,11)
counter <- 0

for (pm in 1:200){
  for (t in 1:20){
    for (e in 1:100){
      pt.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,3])
      pt.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,3])) / sqrt(25)
      
      fe.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,1])
      fe.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,1])) / sqrt(25)
      
      fit.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,4])
      fit.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,4])) / sqrt(25)
      
      dt.mean <- mean(fmax.results.matrix[pm,t,i.patch,e,5])
      dt.se <- (sd(fmax.results.matrix[pm,t,i.patch,e,5])) / sqrt(25)
      
      results.means.ip[e+(counter*100),1] <- pm
      results.means.ip[e+(counter*100),2] <- e
      results.means.ip[e+(counter*100),3] <- t
      results.means.ip[e+(counter*100),4] <- pt.mean
      results.means.ip[e+(counter*100),5] <- pt.se
      results.means.ip[e+(counter*100),6] <- fe.mean
      results.means.ip[e+(counter*100),7] <- fe.se
      results.means.ip[e+(counter*100),8] <- fit.mean
      results.means.ip[e+(counter*100),9] <- fit.se
      results.means.ip[e+(counter*100),10] <- dt.mean
      results.means.ip[e+(counter*100),11] <- dt.se
    }
    counter <- counter + 1
  }
}

results.means.ip <- as.data.frame(results.means.ip)

colnames(results.means.ip) <- c("p.matrix", "energy", "time", "pt.mean", "pt.se", 
                                "fe.mean", "fe.se", "fit.mean", "fit.se", "dt.mean", "dt.se")


## destination patch type
ggplot(results.means.ip, aes(x = pt.mean, y = pt.se, col = p.matrix))+
  geom_point()+
  theme_classic()+
  xlab("Mean Destination Patch Type")+
  ylab("Std. Error of Destination Patch Type")

## foraging effot
ggplot(results.means.ip, aes(x = fe.mean, y = fe.se, col = p.matrix))+
  geom_point()+
  theme_classic()+
  xlab("Mean Foraging Effort")+
  ylab("Std. Error of Foraging Effort")













































###############################################################################################################
################################ OLD ANALYSIS OF SIMULATION MODELS ###########################################
###############################################################################################################

## generate new dataframe that summarises foraging decisions by time and energy category

### change initial energy category to a factor
decision.db.hr.e$i.e.cat <- as.character(decision.db.hr.e$i.e.cat)
decision.db.hr.e$i.e.cat <- as.factor(decision.db.hr.e$i.e.cat)

## let's summarize by i.energy range

decision.db.hr.e$d.patch.type <- as.character(decision.db.hr.e$d.patch.type)
decision.db.hr.e$d.patch.type <- as.numeric(decision.db.hr.e$d.patch.type)

decision.db.hr.e$fe <- as.character(decision.db.hr.e$fe)
decision.db.hr.e$fe <- as.numeric(decision.db.hr.e$fe)


decision.db.ie.hr.e <- group_by(decision.db.test.hr.e, i.e.cat, time)

decision.db.ie.hr.e <- summarise(decision.db.ie.hr.e,
                                 dpt.mean = mean(d.patch.type), dpt.se = (sd(d.patch.type))/sqrt(n()),
                                 fe.mean = mean(fe), fe.se = (sd(fe))/sqrt(n()),
                                 dt.mean = mean(d.travelled), dt.se = (sd(d.travelled))/sqrt(n()))


## Fates in Given Patch Types  

## Original Model  

## all animals that were alive to make choices
all.choices.orig <- filter(decision.db.orig, fate != 0)

## choices of in timesteps in which animals survived the timestep
survivors.choices.orig <- filter(decision.db.orig, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches
t.predation.choices.orig <- filter(decision.db.orig, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches
p.predation.choices.orig <- filter(decision.db.orig, fate == "patch risk")



## prop of patch types used for all animals 
# alive to make choices, regardless of the consequences of those choices


home.choices.all.orig <- filter(all.choices.orig, d.patch == 0)
safe.choices.all.orig <- filter(all.choices.orig, d.patch.type == 10)
moderate.choices.all.orig <- filter(all.choices.orig, d.patch.type == 20)
risky.choices.all.orig <- filter(all.choices.orig, d.patch.type == 30)

prop.home.all.orig <- nrow(home.choices.all.orig) / nrow(all.choices.orig)
prop.safe.all.orig <- nrow(safe.choices.all.orig) / nrow(all.choices.orig)
prop.moderate.all.orig <- nrow(moderate.choices.all.orig) / nrow(all.choices.orig)
prop.risky.all.orig <- nrow(risky.choices.all.orig) / nrow(all.choices.orig)

## check to be sure proportions add to 1
check <- prop.home.all.orig + prop.safe.all.orig + prop.moderate.all.orig + prop.risky.all.orig

## combine all animal patch choice proportions in a vector
patch.choices.all.orig <- rbind(prop.home.all.orig, prop.safe.all.orig, prop.moderate.all.orig, 
                                prop.risky.all.orig)



#### proportion of patch types used by animals that survived the given patch choice 


home.choices.surv.orig <- filter(survivors.choices.orig, d.patch == 0)
safe.choices.surv.orig <- filter(survivors.choices.orig, d.patch.type == 10)
moderate.choices.surv.orig <- filter(survivors.choices.orig, d.patch.type == 20)
risky.choices.surv.orig <- filter(survivors.choices.orig, d.patch.type == 30)

prop.home.surv.orig <- nrow(home.choices.surv.orig) / nrow(survivors.choices.orig)
prop.safe.surv.orig <- nrow(safe.choices.surv.orig) / nrow(survivors.choices.orig)
prop.moderate.surv.orig <- nrow(moderate.choices.surv.orig) / nrow(survivors.choices.orig)
prop.risky.surv.orig <- nrow(risky.choices.surv.orig) / nrow(survivors.choices.orig)          

## check to be sure all proportions add to 1
check <- prop.home.surv.orig + prop.safe.surv.orig + prop.moderate.surv.orig + prop.risky.surv.orig

## combine surviving animal patch choice proportions in a vector
patch.choices.surv.orig <- rbind(prop.home.surv.orig, prop.safe.surv.orig, prop.moderate.surv.orig, 
                                 prop.risky.surv.orig)



#### proportion of patch types chosen during time steps when animals were 
#depredated travelling to patch   


home.choices.tpred.orig <- filter(t.predation.choices.orig, d.patch == 0)
safe.choices.tpred.orig <- filter(t.predation.choices.orig, d.patch.type == 10)
moderate.choices.tpred.orig <- filter(t.predation.choices.orig, d.patch.type == 20)
risky.choices.tpred.orig <- filter(t.predation.choices.orig, d.patch.type == 30)

prop.home.tpred.orig <- nrow(home.choices.tpred.orig) / nrow(p.predation.choices.orig)
prop.safe.tpred.orig <- nrow(safe.choices.tpred.orig) / nrow(p.predation.choices.orig)
prop.moderate.tpred.orig <- nrow(moderate.choices.tpred.orig) / nrow(p.predation.choices.orig)
prop.risky.tpred.orig <- nrow(risky.choices.tpred.orig) / nrow(p.predation.choices.orig)   

## check to be sure all proportions add to 1
check <- prop.home.tpred.orig + prop.safe.tpred.orig + prop.moderate.tpred.orig + prop.risky.tpred.orig

## combine travel depredated animal patch choice proportions in a vector
patch.choices.tpred.orig <- rbind(prop.home.tpred.orig, prop.safe.tpred.orig, prop.moderate.tpred.orig, 
                                  prop.risky.tpred.orig)




#### proportion of patch types chosen during time steps when animals were 
#depredated in patch   


home.choices.ppred.orig <- filter(p.predation.choices.orig, d.patch == 0)
safe.choices.ppred.orig <- filter(p.predation.choices.orig, d.patch.type == 10)
moderate.choices.ppred.orig <- filter(p.predation.choices.orig, d.patch.type == 20)
risky.choices.ppred.orig <- filter(p.predation.choices.orig, d.patch.type == 30)

prop.home.ppred.orig <- nrow(home.choices.ppred.orig) / nrow(p.predation.choices.orig)
prop.safe.ppred.orig <- nrow(safe.choices.ppred.orig) / nrow(p.predation.choices.orig)
prop.moderate.ppred.orig <- nrow(moderate.choices.ppred.orig) / nrow(p.predation.choices.orig)
prop.risky.ppred.orig <- nrow(risky.choices.ppred.orig) / nrow(p.predation.choices.orig)                    

## check to be sure all proportions add to 1
check <- prop.home.ppred.orig + prop.safe.ppred.orig + prop.moderate.ppred.orig + prop.risky.ppred.orig

## combine patch depredated animal patch choice proportions in a vector
patch.choices.ppred.orig <- rbind(prop.home.ppred.orig, prop.safe.ppred.orig, prop.moderate.ppred.orig, 
                                  prop.risky.ppred.orig)



##column 1 is list of possible patch choices
patch.list <- rbind(0,10,20,30)

## generate matrix with all patch choice results combined
patch.choice.results.orig <- cbind(patch.list, patch.choices.all.orig, patch.choices.surv.orig, 
                                   patch.choices.tpred.orig, patch.choices.ppred.orig)

# generate column names
colnames(patch.choice.results.orig) <- c("p.type", "all", "survivors", "t.pred", "p.pred")

# remove row names
rownames(patch.choice.results.orig) <- NULL

# convert generated matrix into a dataframe
patch.choice.results.orig <- as.data.frame(patch.choice.results.orig)

patch.choice.results.orig



## Low Food Model 

## all animals that were alive to make choices
all.choices.lf <- filter(decision.db.lf, fate != 0)

## choices of in timesteps in which animals survived the timestep
survivors.choices.lf <- filter(decision.db.lf, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches
t.predation.choices.lf <- filter(decision.db.lf, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches
p.predation.choices.lf <- filter(decision.db.lf, fate == "patch risk")



#### prop of patch types used for all animals 
# alive to make choices, regardless of the consequences of those choices


home.choices.all.lf <- filter(all.choices.lf, d.patch == 0)
safe.choices.all.lf <- filter(all.choices.lf, d.patch.type == 10)
moderate.choices.all.lf <- filter(all.choices.lf, d.patch.type == 20)
risky.choices.all.lf <- filter(all.choices.lf, d.patch.type == 30)

prop.home.all.lf <- nrow(home.choices.all.lf) / nrow(all.choices.lf)
prop.safe.all.lf <- nrow(safe.choices.all.lf) / nrow(all.choices.lf)
prop.moderate.all.lf <- nrow(moderate.choices.all.lf) / nrow(all.choices.lf)
prop.risky.all.lf <- nrow(risky.choices.all.lf) / nrow(all.choices.lf)

## check to be sure proportions add to 1
check <- prop.home.all.lf + prop.safe.all.lf + prop.moderate.all.lf + prop.risky.all.lf

## combine all animal patch choice proportions in a vector
patch.choices.all.lf <- rbind(prop.home.all.lf, prop.safe.all.lf, prop.moderate.all.lf, 
                              prop.risky.all.lf)



#### proportion of patch types used by animals that survived the given patch choice 


home.choices.surv.lf <- filter(survivors.choices.lf, d.patch == 0)
safe.choices.surv.lf <- filter(survivors.choices.lf, d.patch.type == 10)
moderate.choices.surv.lf <- filter(survivors.choices.lf, d.patch.type == 20)
risky.choices.surv.lf <- filter(survivors.choices.lf, d.patch.type == 30)

prop.home.surv.lf <- nrow(home.choices.surv.lf) / nrow(survivors.choices.lf)
prop.safe.surv.lf <- nrow(safe.choices.surv.lf) / nrow(survivors.choices.lf)
prop.moderate.surv.lf <- nrow(moderate.choices.surv.lf) / nrow(survivors.choices.lf)
prop.risky.surv.lf <- nrow(risky.choices.surv.lf) / nrow(survivors.choices.lf)          

## check to be sure all proportions add to 1
check <- prop.home.surv.lf + prop.safe.surv.lf + prop.moderate.surv.lf + prop.risky.surv.lf

## combine surviving animal patch choice proportions in a vector
patch.choices.surv.lf <- rbind(prop.home.surv.lf, prop.safe.surv.lf, prop.moderate.surv.lf, 
                               prop.risky.surv.lf)



#### proportion of patch types chosen during time steps when animals were 
#depredated travelling to patch   


home.choices.tpred.lf <- filter(t.predation.choices.lf, d.patch == 0)
safe.choices.tpred.lf <- filter(t.predation.choices.lf, d.patch.type == 10)
moderate.choices.tpred.lf <- filter(t.predation.choices.lf, d.patch.type == 20)
risky.choices.tpred.lf <- filter(t.predation.choices.lf, d.patch.type == 30)

prop.home.tpred.lf <- nrow(home.choices.tpred.lf) / nrow(p.predation.choices.lf)
prop.safe.tpred.lf <- nrow(safe.choices.tpred.lf) / nrow(p.predation.choices.lf)
prop.moderate.tpred.lf <- nrow(moderate.choices.tpred.lf) / nrow(p.predation.choices.lf)
prop.risky.tpred.lf <- nrow(risky.choices.tpred.lf) / nrow(p.predation.choices.lf)   

## check to be sure all proportions add to 1
check <- prop.home.tpred.lf + prop.safe.tpred.lf + prop.moderate.tpred.lf + prop.risky.tpred.lf

## combine travel depredated animal patch choice proportions in a vector
patch.choices.tpred.lf <- rbind(prop.home.tpred.lf, prop.safe.tpred.lf, prop.moderate.tpred.lf, 
                                prop.risky.tpred.lf)




#### proportion of patch types chosen during time steps when animals were 
#depredated in patch   


home.choices.ppred.lf <- filter(p.predation.choices.lf, d.patch == 0)
safe.choices.ppred.lf <- filter(p.predation.choices.lf, d.patch.type == 10)
moderate.choices.ppred.lf <- filter(p.predation.choices.lf, d.patch.type == 20)
risky.choices.ppred.lf <- filter(p.predation.choices.lf, d.patch.type == 30)

prop.home.ppred.lf <- nrow(home.choices.ppred.lf) / nrow(p.predation.choices.lf)
prop.safe.ppred.lf <- nrow(safe.choices.ppred.lf) / nrow(p.predation.choices.lf)
prop.moderate.ppred.lf <- nrow(moderate.choices.ppred.lf) / nrow(p.predation.choices.lf)
prop.risky.ppred.lf <- nrow(risky.choices.ppred.lf) / nrow(p.predation.choices.lf)                    

## check to be sure all proportions add to 1
check <- prop.home.ppred.lf + prop.safe.ppred.lf + prop.moderate.ppred.lf + prop.risky.ppred.lf

## combine patch depredated animal patch choice proportions in a vector
patch.choices.ppred.lf <- rbind(prop.home.ppred.lf, prop.safe.ppred.lf, prop.moderate.ppred.lf, 
                                prop.risky.ppred.lf)



##column 1 is list of possible patch choices
patch.list <- rbind(0,10,20,30)

## generate matrix with all patch choice results combined
patch.choice.results.lf <- cbind(patch.list, patch.choices.all.lf, patch.choices.surv.lf, 
                                 patch.choices.tpred.lf, patch.choices.ppred.lf)

# generate column names
colnames(patch.choice.results.lf) <- c("p.type", "all", "survivors", "t.pred", "p.pred")

# remove row names
rownames(patch.choice.results.lf) <- NULL

# convert generated matrix into a dataframe
patch.choice.results.lf <- as.data.frame(patch.choice.results.lf)

patch.choice.results.lf




### High Risk Model 

## all animals that were alive to make choices
all.choices.hr <- filter(decision.db.hr, fate != 0)

## choices of in timesteps in which animals survived the timestep
survivors.choices.hr <- filter(decision.db.hr, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches
t.predation.choices.hr <- filter(decision.db.hr, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches
p.predation.choices.hr <- filter(decision.db.hr, fate == "patch risk")



#### prop of patch types used for all animals 
# alive to make choices, regardless of the consequences of those choices


home.choices.all.hr <- filter(all.choices.hr, d.patch == 0)
safe.choices.all.hr <- filter(all.choices.hr, d.patch.type == 10)
moderate.choices.all.hr <- filter(all.choices.hr, d.patch.type == 20)
risky.choices.all.hr <- filter(all.choices.hr, d.patch.type == 30)

prop.home.all.hr <- nrow(home.choices.all.hr) / nrow(all.choices.hr)
prop.safe.all.hr <- nrow(safe.choices.all.hr) / nrow(all.choices.hr)
prop.moderate.all.hr <- nrow(moderate.choices.all.hr) / nrow(all.choices.hr)
prop.risky.all.hr <- nrow(risky.choices.all.hr) / nrow(all.choices.hr)

## check to be sure proportions add to 1
check <- prop.home.all.hr + prop.safe.all.hr + prop.moderate.all.hr + prop.risky.all.hr

## combine all animal patch choice proportions in a vector
patch.choices.all.hr <- rbind(prop.home.all.hr, prop.safe.all.hr, prop.moderate.all.hr, 
                              prop.risky.all.hr)



#### proportion of patch types used by animals that survived the given patch choice 


home.choices.surv.hr <- filter(survivors.choices.hr, d.patch == 0)
safe.choices.surv.hr <- filter(survivors.choices.hr, d.patch.type == 10)
moderate.choices.surv.hr <- filter(survivors.choices.hr, d.patch.type == 20)
risky.choices.surv.hr <- filter(survivors.choices.hr, d.patch.type == 30)

prop.home.surv.hr <- nrow(home.choices.surv.hr) / nrow(survivors.choices.hr)
prop.safe.surv.hr <- nrow(safe.choices.surv.hr) / nrow(survivors.choices.hr)
prop.moderate.surv.hr <- nrow(moderate.choices.surv.hr) / nrow(survivors.choices.hr)
prop.risky.surv.hr <- nrow(risky.choices.surv.hr) / nrow(survivors.choices.hr)          

## check to be sure all proportions add to 1
check <- prop.home.surv.hr + prop.safe.surv.hr + prop.moderate.surv.hr + prop.risky.surv.hr

## combine surviving animal patch choice proportions in a vector
patch.choices.surv.hr <- rbind(prop.home.surv.hr, prop.safe.surv.hr, prop.moderate.surv.hr, 
                               prop.risky.surv.hr)



#### proportion of patch types chosen during time steps when animals were 
#depredated travelling to patch   


home.choices.tpred.hr <- filter(t.predation.choices.hr, d.patch == 0)
safe.choices.tpred.hr <- filter(t.predation.choices.hr, d.patch.type == 10)
moderate.choices.tpred.hr <- filter(t.predation.choices.hr, d.patch.type == 20)
risky.choices.tpred.hr <- filter(t.predation.choices.hr, d.patch.type == 30)

prop.home.tpred.hr <- nrow(home.choices.tpred.hr) / nrow(p.predation.choices.hr)
prop.safe.tpred.hr <- nrow(safe.choices.tpred.hr) / nrow(p.predation.choices.hr)
prop.moderate.tpred.hr <- nrow(moderate.choices.tpred.hr) / nrow(p.predation.choices.hr)
prop.risky.tpred.hr <- nrow(risky.choices.tpred.hr) / nrow(p.predation.choices.hr)   

## check to be sure all proportions add to 1
check <- prop.home.tpred.hr + prop.safe.tpred.hr + prop.moderate.tpred.hr + prop.risky.tpred.hr

## combine travel depredated animal patch choice proportions in a vector
patch.choices.tpred.hr <- rbind(prop.home.tpred.hr, prop.safe.tpred.hr, prop.moderate.tpred.hr, 
                                prop.risky.tpred.hr)




#### proportion of patch types chosen during time steps when animals were 
#depredated in patch   


home.choices.ppred.hr <- filter(p.predation.choices.hr, d.patch == 0)
safe.choices.ppred.hr <- filter(p.predation.choices.hr, d.patch.type == 10)
moderate.choices.ppred.hr <- filter(p.predation.choices.hr, d.patch.type == 20)
risky.choices.ppred.hr <- filter(p.predation.choices.hr, d.patch.type == 30)

prop.home.ppred.hr <- nrow(home.choices.ppred.hr) / nrow(p.predation.choices.hr)
prop.safe.ppred.hr <- nrow(safe.choices.ppred.hr) / nrow(p.predation.choices.hr)
prop.moderate.ppred.hr <- nrow(moderate.choices.ppred.hr) / nrow(p.predation.choices.hr)
prop.risky.ppred.hr <- nrow(risky.choices.ppred.hr) / nrow(p.predation.choices.hr)                    

## check to be sure all proportions add to 1
check <- prop.home.ppred.hr + prop.safe.ppred.hr + prop.moderate.ppred.hr + prop.risky.ppred.hr

## combine patch depredated animal patch choice proportions in a vector
patch.choices.ppred.hr <- rbind(prop.home.ppred.hr, prop.safe.ppred.hr, prop.moderate.ppred.hr, 
                                prop.risky.ppred.hr)



##column 1 is list of possible patch choices
patch.list <- rbind(0,10,20,30)

## generate matrix with all patch choice results combined
patch.choice.results.hr <- cbind(patch.list, patch.choices.all.hr, patch.choices.surv.hr, 
                                 patch.choices.tpred.hr, patch.choices.ppred.hr)

# generate column names
colnames(patch.choice.results.hr) <- c("p.type", "all", "survivors", "t.pred", "p.pred")

# remove row names
rownames(patch.choice.results.hr) <- NULL

# convert generated matrix into a dataframe
patch.choice.results.hr <- as.data.frame(patch.choice.results.hr)

patch.choice.results.hr





## Fates While Exhibiting Given Foraging Efforts  


## Original Model  


#### these are subsetted dataframes used above
## just bringing them down to reference names in this section
## all animals that were alive to make choices

#all.choices <- filter(decision.db, fate != 0)

## choices of in timesteps in which animals survived the timestep

#survivors.choices <- filter(decision.db, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches

#t.predation.choices <- filter(decision.db, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches

#p.predation.choices <- filter(decision.db, fate == "patch risk")



### proportion of foraging efforts used for all animals in all choices 
#and all consequences of those choices


fe0.choices.all.orig <- filter(all.choices.orig, fe == 0)
fe0.5.choices.all.orig <- filter(all.choices.orig, fe == 0.5)
fe1.choices.all.orig <- filter(all.choices.orig, fe == 1)


prop.fe0.all.orig <- nrow(fe0.choices.all.orig) / nrow(all.choices.orig)
prop.fe0.5.all.orig <- nrow(fe0.5.choices.all.orig) / nrow(all.choices.orig)
prop.fe1.all.orig <- nrow(fe1.choices.all.orig) / nrow(all.choices.orig)


## check to be sure all proportions add to 1
check <- prop.fe0.all.orig + prop.fe0.5.all.orig + prop.fe1.all.orig

## combine patch depredated animal patch choice proportions in a vector
fe.choices.all.orig <- rbind(prop.fe0.all.orig, prop.fe0.5.all.orig, prop.fe1.all.orig)



# proportion of foraging efforts used by all animals that survived these choices


fe0.choices.surv.orig <- filter(survivors.choices.orig, fe == 0)
fe0.5.choices.surv.orig <- filter(survivors.choices.orig, fe == 0.5)
fe1.choices.surv.orig <- filter(survivors.choices.orig, fe == 1)

prop.fe0.surv.orig <- nrow(fe0.choices.surv.orig) / nrow(survivors.choices.orig)
prop.fe0.5.surv.orig <- nrow(fe0.5.choices.surv.orig) / nrow(survivors.choices.orig)
prop.fe1.surv.orig <- nrow(fe1.choices.surv.orig) / nrow(survivors.choices.orig)


## check to be sure all proportions add to 1
check <- prop.fe0.surv.orig + prop.fe0.5.surv.orig + prop.fe1.surv.orig

## combine patch depredated animal patch choice proportions in a vector
fe.choices.surv.orig <- rbind(prop.fe0.surv.orig, prop.fe0.5.surv.orig, prop.fe1.surv.orig)



# proportion of foraging efforts used by animals that were depredated in the patch


fe0.choices.ppred.orig <- filter(p.predation.choices.orig, fe == 0)
fe0.5.choices.ppred.orig <- filter(p.predation.choices.orig, fe == 0.5)
fe1.choices.ppred.orig <- filter(p.predation.choices.orig, fe == 1)


prop.fe0.ppred.orig <- nrow(fe0.choices.ppred.orig) / nrow(p.predation.choices.orig)
prop.fe0.5.ppred.orig <- nrow(fe0.5.choices.ppred.orig) / nrow(p.predation.choices.orig)
prop.fe1.ppred.orig <- nrow(fe1.choices.ppred.orig) / nrow(p.predation.choices.orig)


## check to be sure all proportions add to 1
check <- prop.fe0.ppred.orig + prop.fe0.5.ppred.orig + prop.fe1.ppred.orig

## combine patch depredated animal patch choice proportions in a vector
fe.choices.ppred.orig <- rbind(prop.fe0.ppred.orig, prop.fe0.5.ppred.orig, prop.fe1.ppred.orig)


##column 1 is list of possible patch choices
fe.list <- rbind(0,0.5,1)

## generate matrix with all patch choice results combined
fe.choice.results.orig <- cbind(fe.list, fe.choices.all.orig, fe.choices.surv.orig, 
                                fe.choices.ppred.orig)

# generate column names
colnames(fe.choice.results.orig) <- c("fe", "all", "survivors", "p.pred")

# remove row names
rownames(fe.choice.results.orig) <- NULL

# convert generated matrix into a dataframe
fe.choice.results.orig <- as.data.frame(fe.choice.results.orig)

fe.choice.results.orig




### Low Food Model  

#### these are subsetted dataframes used above
## just bringing them down to reference names in this section
## all animals that were alive to make choices

#all.choices <- filter(decision.db, fate != 0)

## choices of in timesteps in which animals survived the timestep

#survivors.choices <- filter(decision.db, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches

#t.predation.choices <- filter(decision.db, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches

#p.predation.choices <- filter(decision.db, fate == "patch risk")



### proportion of foraging efforts used for all animals in all choices 
#and all consequences of those choices


fe0.choices.all.lf <- filter(all.choices.lf, fe == 0)
fe0.5.choices.all.lf <- filter(all.choices.lf, fe == 0.5)
fe1.choices.all.lf <- filter(all.choices.lf, fe == 1)


prop.fe0.all.lf <- nrow(fe0.choices.all.lf) / nrow(all.choices.lf)
prop.fe0.5.all.lf <- nrow(fe0.5.choices.all.lf) / nrow(all.choices.lf)
prop.fe1.all.lf <- nrow(fe1.choices.all.lf) / nrow(all.choices.lf)


## check to be sure all proportions add to 1
check <- prop.fe0.all.lf + prop.fe0.5.all.lf + prop.fe1.all.lf

## combine patch depredated animal patch choice proportions in a vector
fe.choices.all.lf <- rbind(prop.fe0.all.lf, prop.fe0.5.all.lf, prop.fe1.all.lf)



# proportion of foraging efforts used by all animals that survived these choices


fe0.choices.surv.lf <- filter(survivors.choices.lf, fe == 0)
fe0.5.choices.surv.lf <- filter(survivors.choices.lf, fe == 0.5)
fe1.choices.surv.lf <- filter(survivors.choices.lf, fe == 1)

prop.fe0.surv.lf <- nrow(fe0.choices.surv.lf) / nrow(survivors.choices.lf)
prop.fe0.5.surv.lf <- nrow(fe0.5.choices.surv.lf) / nrow(survivors.choices.lf)
prop.fe1.surv.lf <- nrow(fe1.choices.surv.lf) / nrow(survivors.choices.lf)


## check to be sure all proportions add to 1
check <- prop.fe0.surv.lf + prop.fe0.5.surv.lf + prop.fe1.surv.lf

## combine patch depredated animal patch choice proportions in a vector
fe.choices.surv.lf <- rbind(prop.fe0.surv.lf, prop.fe0.5.surv.lf, prop.fe1.surv.lf)



# proportion of foraging efforts used by animals that were depredated in the patch


fe0.choices.ppred.lf <- filter(p.predation.choices.lf, fe == 0)
fe0.5.choices.ppred.lf <- filter(p.predation.choices.lf, fe == 0.5)
fe1.choices.ppred.lf <- filter(p.predation.choices.lf, fe == 1)


prop.fe0.ppred.lf <- nrow(fe0.choices.ppred.lf) / nrow(p.predation.choices.lf)
prop.fe0.5.ppred.lf <- nrow(fe0.5.choices.ppred.lf) / nrow(p.predation.choices.lf)
prop.fe1.ppred.lf <- nrow(fe1.choices.ppred.lf) / nrow(p.predation.choices.lf)


## check to be sure all proportions add to 1
check <- prop.fe0.ppred.lf + prop.fe0.5.ppred.lf + prop.fe1.ppred.lf

## combine patch depredated animal patch choice proportions in a vector
fe.choices.ppred.lf <- rbind(prop.fe0.ppred.lf, prop.fe0.5.ppred.lf, prop.fe1.ppred.lf)


##column 1 is list of possible patch choices
fe.list <- rbind(0,0.5,1)

## generate matrix with all patch choice results combined
fe.choice.results.lf <- cbind(fe.list, fe.choices.all.lf, fe.choices.surv.lf, 
                              fe.choices.ppred.lf)

# generate column names
colnames(fe.choice.results.lf) <- c("fe", "all", "survivors", "p.pred")

# remove row names
rownames(fe.choice.results.lf) <- NULL

# convert generated matrix into a dataframe
fe.choice.results.lf <- as.data.frame(fe.choice.results.lf)

fe.choice.results.lf



#### High Risk Model  

#### these are subsetted dataframes used above
## just bringing them down to reference names in this section
## all animals that were alive to make choices

#all.choices <- filter(decision.db, fate != 0)

## choices of in timesteps in which animals survived the timestep

#survivors.choices <- filter(decision.db, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches

#t.predation.choices <- filter(decision.db, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches

#p.predation.choices <- filter(decision.db, fate == "patch risk")



### proportion of foraging efforts used for all animals in all choices 
#and all consequences of those choices


fe0.choices.all.hr <- filter(all.choices.hr, fe == 0)
fe0.5.choices.all.hr <- filter(all.choices.hr, fe == 0.5)
fe1.choices.all.hr <- filter(all.choices.hr, fe == 1)


prop.fe0.all.hr <- nrow(fe0.choices.all.hr) / nrow(all.choices.hr)
prop.fe0.5.all.hr <- nrow(fe0.5.choices.all.hr) / nrow(all.choices.hr)
prop.fe1.all.hr <- nrow(fe1.choices.all.hr) / nrow(all.choices.hr)


## check to be sure all proportions add to 1
check <- prop.fe0.all.hr + prop.fe0.5.all.hr + prop.fe1.all.hr

## combine patch depredated animal patch choice proportions in a vector
fe.choices.all.hr <- rbind(prop.fe0.all.hr, prop.fe0.5.all.hr, prop.fe1.all.hr)



# proportion of foraging efforts used by all animals that survived these choices


fe0.choices.surv.hr <- filter(survivors.choices.hr, fe == 0)
fe0.5.choices.surv.hr <- filter(survivors.choices.hr, fe == 0.5)
fe1.choices.surv.hr <- filter(survivors.choices.hr, fe == 1)

prop.fe0.surv.hr <- nrow(fe0.choices.surv.hr) / nrow(survivors.choices.hr)
prop.fe0.5.surv.hr <- nrow(fe0.5.choices.surv.hr) / nrow(survivors.choices.hr)
prop.fe1.surv.hr <- nrow(fe1.choices.surv.hr) / nrow(survivors.choices.hr)


## check to be sure all proportions add to 1
check <- prop.fe0.surv.hr + prop.fe0.5.surv.hr + prop.fe1.surv.hr

## combine patch depredated animal patch choice proportions in a vector
fe.choices.surv.hr <- rbind(prop.fe0.surv.hr, prop.fe0.5.surv.hr, prop.fe1.surv.hr)



# proportion of foraging efforts used by animals that were depredated in the patch


fe0.choices.ppred.hr <- filter(p.predation.choices.hr, fe == 0)
fe0.5.choices.ppred.hr <- filter(p.predation.choices.hr, fe == 0.5)
fe1.choices.ppred.hr <- filter(p.predation.choices.hr, fe == 1)


prop.fe0.ppred.hr <- nrow(fe0.choices.ppred.hr) / nrow(p.predation.choices.hr)
prop.fe0.5.ppred.hr <- nrow(fe0.5.choices.ppred.hr) / nrow(p.predation.choices.hr)
prop.fe1.ppred.hr <- nrow(fe1.choices.ppred.hr) / nrow(p.predation.choices.hr)


## check to be sure all proportions add to 1
check <- prop.fe0.ppred.hr + prop.fe0.5.ppred.hr + prop.fe1.ppred.hr

## combine patch depredated animal patch choice proportions in a vector
fe.choices.ppred.hr <- rbind(prop.fe0.ppred.hr, prop.fe0.5.ppred.hr, prop.fe1.ppred.hr)


##column 1 is list of possible patch choices
fe.list <- rbind(0,0.5,1)

## generate matrix with all patch choice results combined
fe.choice.results.hr <- cbind(fe.list, fe.choices.all.hr, fe.choices.surv.hr, 
                              fe.choices.ppred.hr)

# generate column names
colnames(fe.choice.results.hr) <- c("fe", "all", "survivors", "p.pred")

# remove row names
rownames(fe.choice.results.hr) <- NULL

# convert generated matrix into a dataframe
fe.choice.results.hr <- as.data.frame(fe.choice.results.hr)

fe.choice.results.hr




#### GRAPHING PATCH CHOICE RESULTS 

### Original Model 

patch.choice.orig <- ggplot(patch.choice.results.orig, aes(x = p.type, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = patch.choice.results.orig, aes(x = p.type, y = p.pred),
             size = 4, pch = 16)+
  xlab("Patch Type")+
  ylab("Poportion of Patch Choices")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Baseline")


#### Low Food Model 
patch.choice.lf <- ggplot(patch.choice.results.lf, aes(x = p.type, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = patch.choice.results.lf, aes(x = p.type, y = p.pred),
             size = 4, pch = 16)+
  xlab("Patch Type")+
  ylab("Poportion of Patch Choices")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Low Food")

### High Risk Model 
patch.choice.hr <- ggplot(patch.choice.results.hr, aes(x = p.type, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = patch.choice.results.hr, aes(x = p.type, y = p.pred),
             size = 4, pch = 16)+
  xlab("Patch Type")+
  ylab("Poportion of Patch Choices")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("High Risk")



grid.arrange((arrangeGrob(patch.choice.orig, patch.choice.lf, patch.choice.hr, ncol = 1, 
                          left = textGrob("Proportion of Patch Choices", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))




#### GRAPHING FORAGING EFFORT RESULTS

##Original Model 

fe.choice.orig <- ggplot(fe.choice.results.orig, aes(x = fe, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = fe.choice.results.orig, aes(x = fe, y = p.pred),
             size = 4, pch = 16)+
  xlab("Foraging Effort")+
  ylab("Poportion of Exhibited Foraging Efforts")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.y = element_blank())+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Baseline")


## Low Food Model 

fe.choice.lf <- ggplot(fe.choice.results.lf, aes(x = fe, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = fe.choice.results.lf, aes(x = fe, y = p.pred),
             size = 4, pch = 16)+
  xlab("Foraging Effort")+
  ylab("Poportion of Exhibited Foraging Efforts")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.y = element_blank())+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Low Food")


###High Risk Model 

fe.choice.hr <- ggplot(fe.choice.results.hr, aes(x = fe, y = survivors))+
  geom_point(size = 4, pch = 23, fill = "black")+
  theme_classic()+
  geom_point(data = fe.choice.results.hr, aes(x = fe, y = p.pred),
             size = 4, pch = 16)+
  xlab("Foraging Effort")+
  ylab("Poportion of Exhibited Foraging Efforts")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16), axis.title.y = element_blank())+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("High Risk")


grid.arrange((arrangeGrob(fe.choice.orig, fe.choice.lf, fe.choice.hr, ncol = 1, 
                          left = textGrob("Proportion of Exhibited Foraging Efforts", 
                                          rot = 90, vjust = 1, gp=gpar(fontsize=16)))))





##### GRAPHS - FORWARD ITERATIONS 

### Patch Choice Results - generate dataframe 

## all animals that were alive to make choices
all.choices <- filter(decision.db, fate != 0)

## choices of in timesteps in which animals survived the timestep
survivors.choices <- filter(decision.db, fate == "survive")

## choices in timesteps in which animals were depredated travelling between patches
t.predation.choices <- filter(decision.db, fate == "travel risk")

## choices in timesteps in which animals were depredated travelling between patches
p.predation.choices <- filter(decision.db, fate == "patch risk")





#### Patch Choice Results - generate graphs 


graph.all.pc <- ggplot(patch.choice.results, aes(x = p.type, y = all))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("All Patch Choices")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Patch Choices")+
  xlab("Destination Patch Type")


graph.survivors.pc <- ggplot(patch.choice.results, aes(x = p.type, y = survivors))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Choices - Survival")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Patch Choices")+
  xlab("Destination Patch Type")


graph.tpred.pc <- ggplot(patch.choice.results, aes(x = p.type, y = t.pred))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Choices - Travel Predation")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Patch Choices")+
  xlab("Destination Patch Type")


graph.ppred.pc <- ggplot(patch.choice.results, aes(x = p.type, y = p.pred))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Choices - Patch Predation")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Patch Choices")+
  xlab("Destination Patch Type")

grid.arrange(graph.all.pc, graph.survivors.pc, graph.tpred.pc, graph.ppred.pc)


##Foraging Effort Results - generate dataframe 


#### Foraging Effort Results - generate graphs 

graph.all.fe <- ggplot(fe.choice.results, aes(x = fe, y = all))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("All")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Foraging Effort Choices")+
  xlab("Foraging Effort")


graph.survivors.fe <- ggplot(fe.choice.results, aes(x = fe, y = survivors))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Survived")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Foraging Effort Choices")+
  xlab("Foraging Effort")

graph.ppred.fe <- ggplot(fe.choice.results, aes(x = fe, y = p.pred))+
  geom_point(size = 4)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Predated in Patches")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Proportion of Foraging Effort Choices")+
  xlab("Foraging Effort")

grid.arrange(graph.all.fe, graph.survivors.fe, graph.ppred.fe, ncol=2)


## let's compare those who died of patch risk to survivors

p.death.data <- filter(decision.db, fate == "patch risk")
## p matrices: 2, 28, 41, 43, 58, 66, 74, 84, 91

id.survivor <- rep(0,2000)

decision.db.test3 <- cbind(decision.db.test, id.survivor)

for(i in 1:2000){
  
  if(decision.db.test3[i,2] == 2 | decision.db.test3[i,2] == 28 | decision.db.test3[i,2] == 41
     | decision.db.test3[i,2] == 43 | decision.db.test3[i,2] == 58 | decision.db.test3[i,2] == 66
     | decision.db.test3[i,2] == 74 | decision.db.test3[i,2] == 84 | decision.db.test3[i,2] == 91){
    
    decision.db.test3[i,16] <- "depred"
  }else{
    decision.db.test3[i,16] <- "survivor"
  }
  
}

### remove rows in which time or pmatrix is zero
decision.db.test3 <- filter(decision.db.test3, time > 0)

### NEXT ### 
#### now we can use group by and summarize to get mean d.patch.type used, fe, i.energy at t = 1
# for survivors and depred.



### PATCH CHOICES, FORAGING EFFORTS, FOR THOSE DEPREDATED IN PATCH VS. THOSE THAT SURVIVED
## TIME OF PATCH DEPREDATIONS


## original model

decision.db.orig.e$d.patch.type <- as.character(decision.db.orig.e$d.patch.type)
decision.db.orig.e$d.patch.type <- as.numeric(decision.db.orig.e$d.patch.type)

decision.db.orig.e$fe <- as.character(decision.db.orig.e$fe)
decision.db.orig.e$fe <- as.numeric(decision.db.orig.e$fe)

db.orig.e.pr <- filter(decision.db.orig.e, fate == "patch risk")

db.orig.e.sur <- filter(decision.db.orig.e, fate == "survive")

## patch choices
mean(db.orig.e.pr$d.patch.type)
# variance is zero

mean(db.orig.e.sur$d.patch.type)
sd(db.orig.e.sur$d.patch.type)/sqrt(1885)

## foraging effort
mean(db.orig.e.pr$fe)
sd(db.orig.e.sur$fe)/sqrt(9)

mean(db.orig.e.sur$fe)
sd(db.orig.e.sur$fe)/sqrt(1885)


## low food model model

decision.db.lf.e <- decision.db.lf

decision.db.lf.e$d.patch.type <- as.character(decision.db.lf.e$d.patch.type)
decision.db.lf.e$d.patch.type <- as.numeric(decision.db.lf.e$d.patch.type)

decision.db.lf.e$fe <- as.character(decision.db.lf.e$fe)
decision.db.lf.e$fe <- as.numeric(decision.db.lf.e$fe)


db.lf.e.pr <- filter(decision.db.lf.e, fate == "patch risk")

db.lf.e.sur <- filter(decision.db.lf.e, fate == "survive")

## patch choices
mean(db.lf.e.pr$d.patch.type)
# variance is zero
sd(db.lf.e.pr$d.patch.type)/sqrt(17)

mean(db.lf.e.sur$d.patch.type)
sd(db.lf.e.sur$d.patch.type)/sqrt(1770)

## foraging effort
mean(db.lf.e.pr$fe)
sd(db.lf.e.pr$fe)/sqrt(17)

mean(db.lf.e.sur$fe)
sd(db.lf.e.sur$fe)/sqrt(1770)


## high risk model model

decision.db.hr.e <- decision.db.hr

decision.db.hr.e$d.patch.type <- as.character(decision.db.hr.e$d.patch.type)
decision.db.hr.e$d.patch.type <- as.numeric(decision.db.hr.e$d.patch.type)

decision.db.hr.e$fe <- as.character(decision.db.hr.e$fe)
decision.db.hr.e$fe <- as.numeric(decision.db.hr.e$fe)


db.hr.e.pr <- filter(decision.db.hr.e, fate == "patch risk")

db.hr.e.sur <- filter(decision.db.hr.e, fate == "survive")

## patch choices
mean(db.hr.e.pr$d.patch.type)
# variance is zero
sd(db.hr.e.pr$d.patch.type)/sqrt(29)

mean(db.hr.e.sur$d.patch.type)
sd(db.hr.e.sur$d.patch.type)/sqrt(1701)

## foraging effort
mean(db.hr.e.pr$fe)
sd(db.hr.e.pr$fe)/sqrt(29)

mean(db.hr.e.sur$fe)
sd(db.hr.e.sur$fe)/sqrt(1701)

## time
mean(db.hr.e.pr$time)
sd(db.hr.e.pr$fe)/sqrt(29)


### proportion of patch types used in each model

## baseline

x <- which(fmax.df.orig$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.orig$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.orig$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.orig$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.orig)


## low food

x <- which(fmax.df.lf$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.lf$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.lf$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.lf$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.orig)


## high risk

x <- which(fmax.df.hr$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.hr$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.hr$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.orig)

x <- which(fmax.df.hr$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.orig)

### more stats for manuscript - prop of patch use t = 21 for each model

fmax.df.orig.20 <- filter(fmax.df.orig, time == 20)
fmax.df.lf.20 <- filter(fmax.df.lf, time == 20)
fmax.df.hr.20 <- filter(fmax.df.hr, time == 20)


## baseline

x <- which(fmax.df.orig.20$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.orig.20)

x <- which(fmax.df.orig.20$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.orig.20)

x <- which(fmax.df.orig.20$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.orig.20)

x <- which(fmax.df.orig.20$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.orig.20)


## low food

x <- which(fmax.df.lf.20$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.lf.20)

x <- which(fmax.df.lf.20$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.lf.20)

x <- which(fmax.df.lf.20$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.lf.20)

x <- which(fmax.df.lf.20$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.lf.20)


## high risk

x <- which(fmax.df.hr.20$d.patch.type == 0)
prop.home <- length(x) / nrow(fmax.df.hr.20)

x <- which(fmax.df.hr.20$d.patch.type == 10)
prop.safe <- length(x) / nrow(fmax.df.hr.20)

x <- which(fmax.df.hr.20$d.patch.type == 20)
prop.mod <- length(x) / nrow(fmax.df.hr.20)

x <- which(fmax.df.hr.20$d.patch.type == 30)
prop.risky <- length(x) / nrow(fmax.df.hr.20)


mean(fmax.df.orig.20$d.patch.type)
sd(fmax.df.orig.20$d.patch.type) / sqrt(nrow(fmax.df.orig.20))

mean(fmax.df.lf.20$d.patch.type)
sd(fmax.df.lf.20$d.patch.type) / sqrt(nrow(fmax.df.lf.20))

mean(fmax.df.hr.20$d.patch.type)
sd(fmax.df.hr.20$d.patch.type) / sqrt(nrow(fmax.df.hr.20))


#### more stats for fe of low energy ind's (1-50), first half of the day (1-10)

orig.low.e.early <- filter(fmax.df.orig, time > 11 & energy < 51)
lf.low.e.early <- filter(fmax.df.lf, time > 11 & energy < 51)
hr.low.e.early <- filter(fmax.df.hr, time > 11 & energy < 51)

mean(orig.low.e.early$f.effort)
sd(orig.low.e.early$f.effort) / sqrt(nrow(orig.low.e.early))

mean(lf.low.e.early$f.effort)
sd(lf.low.e.early$f.effort) / sqrt(nrow(lf.low.e.early))

mean(hr.low.e.early$f.effort)
sd(hr.low.e.early$f.effort) / sqrt(nrow(hr.low.e.early))

## more stats for fe - high energy, last time step
orig.high.e.late <- filter(fmax.df.orig, time == 20 & energy > 79)
lf.high.e.late <- filter(fmax.df.lf, time == 20 & energy > 79)
hr.high.e.late <- filter(fmax.df.hr, time == 20 & energy > 79)

mean(orig.high.e.late$f.effort)
sd(orig.high.e.late$f.effort) / sqrt(nrow(orig.high.e.late))

mean(lf.high.e.late$f.effort)
sd(lf.high.e.late$f.effort) / sqrt(nrow(lf.high.e.late))

mean(hr.high.e.late$f.effort)
sd(hr.high.e.late$f.effort) / sqrt(nrow(hr.high.e.late))

### effect of treatments on travel distance

mean(fmax.df.lf.effect$d.travelled)
sd(fmax.df.lf.effect$d.travelled) / sqrt(nrow(fmax.df.lf.effect))

mean(fmax.df.hr.effect$d.travelled)
sd(fmax.df.hr.effect$d.travelled) / sqrt(nrow(fmax.df.hr.effect))


### forward model results - energy mean and variance at t = 10 and 20

## baseline

decision.db.orig.10 <- filter(decision.db.orig, time == 10)
decision.db.orig.20 <- filter(decision.db.orig, time == 20)

mean(decision.db.orig.10$i.energy)
sd(decision.db.orig.10$i.energy) / sqrt(nrow(decision.db.orig.10))

mean(decision.db.orig.20$i.energy)
sd(decision.db.orig.20$i.energy) / sqrt(nrow(decision.db.orig.20))


## low food

decision.db.lf.10 <- filter(decision.db.lf, time == 10)
decision.db.lf.20 <- filter(decision.db.lf, time == 20)

mean(decision.db.lf.10$i.energy)
sd(decision.db.lf.10$i.energy) / sqrt(nrow(decision.db.lf.10))

mean(decision.db.lf.20$i.energy)
sd(decision.db.lf.20$i.energy) / sqrt(nrow(decision.db.lf.20))


## high risk

decision.db.hr.10 <- filter(decision.db.hr, time == 10)
decision.db.hr.20 <- filter(decision.db.hr, time == 20)

mean(decision.db.hr.10$i.energy)
sd(decision.db.hr.10$i.energy) / sqrt(nrow(decision.db.hr.10))

mean(decision.db.hr.20$i.energy)
sd(decision.db.hr.20$i.energy) / sqrt(nrow(decision.db.hr.20))



#### let's make one more graph of fates

### baseline model
### list of i.energy's of animal's depredated in patches
ie.list.base <- c(26,22,42,39,50,57,38,57,36)


### low food model
### list of i.energy's of animal's depredated in patches
ie.list.lf <- c(42,42,53,62,52,53,56,46,46,64,58,42,25,67,64,27,58)

### high risk model
### list of i.energy's of animal's depredated in patches

ie.list.hr <- c(32,46,33,45,75,30,45,34,51,34,49,52,79,55,56,37,21,52,32,30,54,52,48,44,30,45,80,50,50)

## summary stats of animals depredated and their mean and se of initial energy states
mean.base.d <- mean(ie.list.base)
se.base.d <- sd(ie.list.base) / sqrt(9)

mean.lf.d <- mean(ie.list.lf)
se.lf.d <- sd(ie.list.lf) / sqrt(17)

mean.hr.d <- mean(ie.list.hr)
se.hr.d <- sd(ie.list.hr) / sqrt(29)


baseline <- c("Baseline", round(mean.base.d,2), round(se.base.d,2))
lowfood <- c("Low Food", round(mean.lf.d,2), round(se.lf.d,2))
highrisk <- c("High Risk", round(mean.hr.d,2), round(se.hr.d,2))

e.fate.data <- rbind(baseline,lowfood,highrisk)

colnames(e.fate.data) <- c("model", "mean", "se")

e.fate.data <- as.data.frame(e.fate.data)

e.fate.data$mean <- as.character(e.fate.data$mean)
e.fate.data$mean <- as.numeric(e.fate.data$mean)

e.fate.data$se <- as.character(e.fate.data$se)
e.fate.data$se <- as.numeric(e.fate.data$se)


### graph initial energies of animals depredated in patches of the 3 models

ggplot(e.fate.data, aes(x = model, y = mean))+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se), width = 0.2, size = 0.2)+
  theme_classic()+
  ylab("Mean Initial Energy State")+
  xlab("Model")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))









