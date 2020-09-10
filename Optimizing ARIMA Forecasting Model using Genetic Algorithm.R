library("e1071")
library("randomForest")
library(ISLR)
library("rpart")
library("caret")
library("rlang")
library(dplyr)
library(drat)
library(xgboost)


inTraining =  createDataPartition(Default$default, p = 0.8, list = FALSE)
training = Default[inTraining,]
testing = Default[-inTraining,]

# setting Cross-Validation Components. 
rec.function = function(data, lev = NULL, model = NULL) 
{
  metric = sensitivity(data$obs, data$pred, na.rm = TRUE, positive= "Yes")
  names(metric) = "Recall"
  metric
}

train.control = trainControl(method = "cv", number = 2,
                             classProbs = TRUE, summaryFunction = rec.function) 

# Fintness Function
fitness = function(prm) #nt, mt, ns, mn
{
  model = train(default ~., data = Default, method = 'rf', trControl = train.control,
                .n.trees = prm[1], .mtry = prm[2], .nodesize = prm[3], .maxnodes = prm[4], metric = "Recall")
  return(-model$results[1,'Recall'])
}

# Simulated Annealing
iteration = 0
max.iteration = 2
initial.tempreture = 1000
final.tempreture = 900
tempreture.updater = function(temp) {runif(1,0.8,0.95) * temp}

initial.solution.generator = function() 
{
  no.tree = sample(seq(100,1000,1),1)
  m = sample(seq(1:ncol(Default)),1)
  node.size = sample(seq(1:50),1)
  max.nodes = sample(seq(1:50),1)
  return(c(no.tree,m,node.size,max.nodes))
}

neighbor = function(c.solution) 
{
  variable.loc = sample(1:4,1)
  n.solution = duplicate(c.solution)
  if (variable.loc == 1) 
  {
    p.values = max(100,c.solution[variable.loc]-10):min(c.solution[variable.loc]+10, 1000)
    n.solution[variable.loc] = sample(p.values[p.values != c.solution[variable.loc]],1)
  }
  else if (variable.loc == 2)
  {
    p.values = max(1,c.solution[variable.loc]-5):min(c.solution[variable.loc]+5, ncol(Default))
    n.solution[variable.loc] = sample(p.values[p.values != c.solution[variable.loc]],1)
  }
  else if (variable.loc == 3)
  {
    p.values = max(1,c.solution[variable.loc]-5):min(c.solution[variable.loc]+5, ncol(Default))
    n.solution[variable.loc] = sample(p.values[p.values != c.solution[variable.loc]],1)
  }
  else
  {
    p.values = max(1,c.solution[variable.loc]-5):min(c.solution[variable.loc]+5, ncol(Default))
    n.solution[variable.loc] = sample(p.values[p.values != c.solution[variable.loc]],1)
  }
  return(n.solution)
}

transition = function(c.solution, n.solution, c.temp) 
{
  transit = FALSE
  if (fitness(c.solution) > fitness(n.solution)) {transit = TRUE}
  else
  {
    acceptance.rate = runif(1)
    if (exp(-(fitness(n.solution) - fitness(c.solution))/c.temp) > acceptance.rate) {transit = TRUE}
  }
  return(transit)
}


c.solution = initial.solution.generator()
#c.fitness = fitness(c.solution)
c.temp = initial.tempreture
m = 0
n.solution = neighbor(c.solution)
uphill = 0
reset.temp = c(initial.tempreture)
while (c.temp > final.tempreture) 
{
  #n.fitness = fitness(n.solution)
  while (m < max.iteration) 
  {
    if (transition(c.solution, n.solution, c.temp)) #& m <= max.iteration
    {
      uphill = 0
      m = m + 1
      c.solution =duplicate(n.solution)
      n.solution = neighbor(c.solution)
    }
    else 
    {
      n.solution = neighbor(c.solution)
      uphill = uphill + 1
      if (uphill == 10)
      {
        beta = runif(1)
        if (c.temp != reset.temp(0)) 
        {
          c.temp = beta * c.temp + (1 - beta) * reset.temp(0)
          reset.temp = append(c.temp, 0)
          uphill = 0
        }
        else 
        {
          c.temp = beta * c.temp + (1 - beta) * reset.temp[which(c.temp == sort(reset.temp)) + 1]
          reset.temp = append(c.temp, 0)
          uphill = 0
        }
      }
    }
  }
  c.temp = tempreture.updater(c.temp)
}
