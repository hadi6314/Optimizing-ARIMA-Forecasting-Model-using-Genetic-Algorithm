library('ggplot2')
library('forecast')
library('tseries')

#daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

day[,'date'] = as.Date(day$dteday)
ggplot(day, aes(date,cnt)) + geom_line() + theme_bw()

series = ts(day[,'cnt']) # to make a time series object
day[,'clean_cnt'] = tsclean(series) # to remove outliers and imput missing data
ggplot(day, aes(date,clean_cnt)) + geom_line() + theme_bw()
ggplotly()


day[,'weekly.moving'] = ma(day[,'clean_cnt'], order=7)
day[,'monthly.moving'] = ma(day[,'clean_cnt'], order=30)
ggplot() +
  geom_line(data = day, aes(x = date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = day, aes(x = date, y = weekly.moving, colour = "Weekly Moving Average")) +
  geom_line(data = day, aes(x = date, y = monthly.moving, colour = "Monthly Moving Average")) + theme_bw()


series.weekly = ts(na.omit(day[,'weekly.moving']), frequency = 30)
decom = stl(series.weekly, s.window="periodic")
deseasonal_cnt = seasadj(decom)
plot(decom)

acf(series.weekly, main = '')
pacf(series.weekly, main = '')

diff.series = diff(deseasonal_cnt,  differences = 7)
plot(diff.series)
acf(diff.series, main = '')
pacf(diff.series, main = '')


fit = auto.arima(deseasonal_cnt, max.p = 5, max.q = 10, max.P = 20,
                 max.Q = 20, max.order = 50, max.d = 20, max.D = 10, seasonal=TRUE)
tsdisplay(residuals(fit),lag.max=45, main='(2,1,0) Model Residuals')
fit2 = arima(deseasonal_cnt, order=c(1,1,7), seasonal = c(10,0,0))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
accuracy(fit)
fcast = forecast(fit2, h=1)
plot(fcast)
arimaorder(fit)

##### Google Project
#GG[,'google.ts'] = GG[,'Close.Last'] %>% ts(frequency = 30) %>% tsclean()

train.size = round(0.8*length(GG[,'Close.Last']))
train.set = GG[1:train.size,'Close.Last'] %>% ts(frequency = 30) %>% tsclean() 
test.set = GG[-(1:train.size),'Close.Last'] %>% ts(frequency = 30) %>% tsclean() 
test.set.1 = as.data.frame(GG[-(1:train.size),'Close.Last'])
colnames(test.set.1)[1] = 'test'

#plot(train.set)
#gg.decom = stl(train.set, s.window="periodic")
#plot(gg.decom)

gg.fit = auto.arima(GG[,'Close.Last'], seasonal=TRUE, allowdrift = T)
far2 <- function(x,h){forecast(auto.arima(x, seasonal=TRUE, allowdrift = T), h=h)}
ee = tsCV(ts(GG[,'Close.Last']), far2, h=1)
sqrt(mean(ee^2, na.rm=TRUE))
fitt = Arima(GG[,'Close.Last'], model=gg.fit, lambda=0)
sqrt(mean((fitt$fitted - GG[,'Close.Last'])^2))



google.ts = google[,'Close.Last'] %>% ts(frequency = 12) %>% tsclean()
#fitt = Arima(google.ts, model=gg.fit, lambda=0)

#gg.fit = auto.arima(google.ts, seasonal=TRUE, allowdrift = T)



maxs = data.frame('max.p' = 2, 'max.d' = 2, 'max.q' = 2, 'max.P' = 2, 'max.D' = 2, 'max.Q' = 2)

population.size = 20
n.keep = round(population.size*0.5)
mutation.number = round(0.2*ncol(population)*(nrow(population)-2))

rank.probaibility = (n.keep - (1:n.keep) + 1) / sum(1:n.keep)
cdf = function(probs) 
{
  cm = 0
  cmp = NULL
  for (i in 1:length(probs))
  {
    cm = cm + probs[i]
    cmp[i] = cm
  }
  return(cmp)
}

cum.probs = cdf(rank.probaibility)

# initial population
population = cbind(p = sample.int(maxs[1,'max.p'], population.size, replace = TRUE), 
                   d = sample.int(maxs[1,'max.d'], population.size, replace = TRUE),
                   q = sample.int(maxs[1,'max.q'], population.size, replace = TRUE),
                   P = sample.int(maxs[1,'max.P'], population.size, replace = TRUE),
                   D = sample.int(maxs[1,'max.D'], population.size, replace = TRUE),
                   Q = sample.int(maxs[1,'max.Q'], population.size, replace = TRUE)) %>% as.data.frame()


# fitness function
fitness = function(orders)  # p,d,q,P,D,Q
{
  #forecasting = function(x,h){forecast(arima(x, order = c(orders['p'],orders['d'],orders['q']), 
  #seasonal=list(order=c(orders['P'],orders['D'],orders['Q']))), h=h)} #, period=30
  
  fit = Arima(google.ts, order =  c(orders['p'],orders['d'],orders['q']), 
              seasonal = c(orders['P'],orders['D'],orders['Q']), hessian = FALSE)
  #cv.fun = tsCV(google.ts, forecasting, h=1)
  #cv.error = mean(cv.fun^2, na.rm = TRUE) %>% sqrt()
  return(fit$bic) #cv.error
}

# Mating based on roulette wheel rank weighting
mate = function(chroms) 
{
  rand.values = runif(population.size - n.keep)
  selected = NULL
  for (i in 1:length(rand.values))
  {
    selected[i] = length(cum.probs) - length(cum.probs[rand.values[i] < cum.probs]) + 1
  }
  for (j in seq(1, length(selected), 2))
  {
    beta = runif(1)
    alpha = runif(1)
    chroms = rbind(chroms, round(beta * chroms[selected[j],] + (1-beta) * chroms[selected[j+1],]),
                   round((1 - alpha) * chroms[selected[j],] + alpha * chroms[selected[j+1],]))
  }
  rownames(chroms) = NULL
  return(chroms)
}

# Mutation with the first two chromosomes unchanged
mutation = function(chroms) 
{
  col.indexes = sample.int(length(chroms), mutation.number, replace = TRUE)
  row.indexes = sample.int(nrow(chroms) - 2, mutation.number, replace = TRUE) + 2
  for (i in 1:mutation.number)
  {
    chroms[row.indexes[i], col.indexes[i]] = sample.int(maxs[1,col.indexes[i]], 1, replace = TRUE)
  }
  return(chroms)
}


# The GA algorithm
install.packages('compiler')
library(compiler)

cmp.fitness = cmpfun(fitness)
fits = apply(population, 1, cmp.fitness)
population = cbind(population, fit = fits)
population = arrange(population, fit)
convergence = 2^31
while (convergence > 0.01) 
{
  elits = population[1:n.keep,-length(population)]  
  elit.fitness = population[1:2,'fit']
  current.best = population[1,'fit']
  population = mate(elits)
  population = mutation(population)
  non.elit.fitness = apply(population[3:nrow(population),], 1, cmp.fitness)
  population = cbind(population, fit = cbind(elits.fitness,non.elit.fitness))
  population = arrange(population, fit)
  convergence =  abs(population[1,'fit'] - current.best)
}











