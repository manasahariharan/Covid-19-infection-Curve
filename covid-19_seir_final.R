library(lubridate)
library(plotly)
library(tidyr)
library(RColorBrewer)
library(deSolve)
cols = brewer.pal(8,"Set2")

#links for all datasets
us_link = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
confirmed_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recovered_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
confirmed = read.csv(confirmed_link)
deaths = read.csv(deaths_link)
recovered = read.csv(recovered_link)
usa = read.csv(us_link)
ny_data = usa[usa$state == "New York",]

#to reshape and prepare data
transpose_disease = function(data){
  data = data %>% unite("location",
                        c("Country.Region","Province.State"), 
                        remove = FALSE)
  drops <- c("Lat","Long","Province.State","Country.Region")
  confirmed_small = data[ , !(names(data) %in% drops)]
  confirmed_transpose = as.data.frame(t(as.matrix(confirmed_small[,2:ncol(confirmed_small)])))
  colnames(confirmed_transpose) = confirmed_small[,1:1]
  data = confirmed_transpose
  names(data)[names(data) == 'Korea, South_'] <- 'South Korea'
  #indx <- grepl(',', colnames(data))
  #data = data[!indx]
  data["worldwide"] = apply(data,1,sum)
  #indx_us <- grepl('US_', colnames(data))
  #data["US"] = apply(data[indx_us],1,sum)
  indx_china <- grepl('China_', colnames(data))
  data["China"] = apply(data[indx_china],1,sum)
  data$dates = rownames(data)
  data$dates = apply(data["dates"],2, function(x) substr(x,2,8))
  #data$dates = dates
  data$dates =mdy(data$dates)
  
  return(data)
}
confirmed_transpose = transpose_disease(confirmed)
recovered_transpose = transpose_disease(recovered)
deaths_transpose = transpose_disease(deaths)

x <- list(
  title = "Dates"
)
y <- list(
  title = "Number of Active Cases"
)
data_plot <- plot_ly(confirmed_transpose, x = ~dates, y = ~Italy_-deaths_transpose$Italy_-recovered_transpose$Italy_, 
                     name = 'Italy', type = 'scatter', mode = 'lines', line = list(color = cols[1])) 
data_plot <- data_plot %>% add_trace(y = ~`South Korea`-deaths_transpose$`South Korea`-recovered_transpose$`South Korea`, name = 'South Korea', 
                                     mode = 'lines', line = list(color = cols[2]))
data_plot <- data_plot %>% add_trace(y = ~US_-deaths_transpose$US_-recovered_transpose$US_, name = 'USA', 
                                     mode = 'lines', line = list(color = cols[3]))
data_plot <- data_plot %>% add_trace(y = ~China-deaths_transpose$China-recovered_transpose$China, name = 'China', 
                                     mode = 'lines', line = list(color = cols[4]))
data_plot <- data_plot %>% add_trace(y = ~Spain_-deaths_transpose$Spain_-recovered_transpose$Spain_, name = 'Spain', 
                                     mode = 'lines', line = list(color = cols[5]))
data_plot <- data_plot %>% add_trace(y = ~Iran_-deaths_transpose$Iran_-recovered_transpose$Iran_, name = 'Iran', 
                                     mode = 'lines', line = list(color = cols[6]))
data_plot = data_plot %>% layout(xaxis = x, yaxis=y)
#api_create(data_plot, filename = "r-active-cases")
print(data_plot)


x <- list(
  title = "Dates"
)
y <- list(
  title = "Proportion of Population",
  tickformat = ".2%"
)

#function for visualizing seir curves
plot_seir = function(parameterss, data, dates_country){
  dates = seq(as.Date("2020-01-30"), as.Date("2020-07-31"), by="days")
  times <- seq(1, length(dates), by = 1)
  solution = ode(y = init1, times = times, func = sier, parms = parameterss)
  print(nrow(solution))
  tt =  paste("Proportion on worst day = ",as.character(format(max(solution[,4])*100, digits =3)),"%", sep = "")
  print(tt)
  a <- list(
    x = dates[which.max(solution[,4])],
    y = max(solution[,4]),
    text =tt,
    width = 300,
    showarrow = FALSE ,
    xref = "x",
    yref = "y",
    yanchor = "bottom",
    xanchor = "right",
    align = "left")
  data_plot <- plot_ly(x = ~dates, y = ~solution[,2]/sum(init1), name = 'Susceptible', type = 'scatter', mode = 'lines')
  data_plot <- data_plot %>% add_trace(y = ~solution[,3]/sum(init1), name = 'Exposed', mode = 'lines')
  data_plot <- data_plot %>% add_trace(y = ~solution[,4]/sum(init1), name = 'Infected', mode = 'lines')
  data_plot <- data_plot %>% add_trace(y = ~solution[,5]/sum(init1), name = 'Dead/Recovered', mode = 'lines')
  data_plot <- data_plot %>% add_trace(y = max(solution[,4]/sum(init1)), name = 'Dead/Recovered', mode = 'lines', showlegend = FALSE,
                                       line=list(color='gray', dash='dot'))
  data_plot <- data_plot %>% add_lines(x = dates_country, y = ~data/sum(init), name = 'Actual', mode = 'lines', line = list(color = 'black'))
  data_plot = data_plot %>% layout(xaxis = x, yaxis=y, title = "Simulated SEIR model for Italy", annotations = a)
  print(data_plot)
  #api_create(data_plot, filename = "seir_italy")
  return(solution)
}

#function for visualizing effect of social distancing
plot_seir_imp = function(parameterss, data, dates_country){
  dates = seq(as.Date("2020-01-30"), as.Date("2020-12-31"), by="days")
  
  times <- seq(1, length(dates), by = 1)
  solution = ode(y = init1, times = times, func = sier, parms = parameterss)
  parameterss = c(parameterss, rho = 0.6)
  print(parameterss)
  solution_rho1 = ode(y = init1, times = times, func = sier_sd, parms = parameterss)
  
  
  parameterss[3] = 0.8
  print(parameterss)
  solution_rho2 = ode(y = init1, times = times, func = sier_sd, parms = parameterss)
  
  
  print(solution_rho2)
  print(solution_rho1)
  # Create and style traces
  fig = plot_ly(x=dates, y=solution[,3], name='Exposed with no isolation',
                line=list(color='firebrick', width=3),type = 'scatter', mode = 'lines',)
  fig = fig %>% add_trace(y=solution[,4], name = 'Infected with no isolation',
                          line=list(color='royalblue', width=3))
  fig = fig %>% add_trace(y=solution_rho1[,3], name='Exposed with more isolation',
                          line=list(color='firebrick', width=3,
                                    dash='dash'))
  fig = fig %>% add_trace(y=solution_rho1[,4], name='Infected with more isolation',
                          line = list(color='royalblue', width=3, dash='dash'))
  fig = fig %>% add_trace(y=solution_rho2[,3], name='Exposed with little isolation',
                          line = list(color='firebrick', width=3, dash='dot'))
  fig = fig %>% add_trace(y=solution_rho2[,4], name='Infected with little isolation',
                          line = list(color='royalblue', width=3, dash='dot'))
  fig = fig %>% layout(xaxis = x, yaxis=y, title = "Effect of Social Distancing for Italy")
  api_create(fig, filename = "seir_social_italy")
  print(fig)
  
  return(solution)
}

#SEIR model equations
#sier <- function(time, state, parameters) {
#  with(as.list(c(state, parameters)), {
#    #print(state)
#    #print(parameters)
#    dS <- (-beta.beta * S * I)
#    #print(dS)
#    dE <- (beta.beta*S*I)-0.1960784*E
#    #print(dE)
#    dI <- 0.1960784*E - gamma.gamma * I
#    #print(dI)
#    dR <- gamma.gamma * I
#    return(list(c(dS, dE, dI, dR)))
#  })
#}

#SEIR model equations with effect of social distancing
sier_sd <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    #print(state)
    #print(parameters)
    dS <- (-rho*beta.beta * S * I)
    #print(dS)
    dE <- (rho*beta.beta*S*I)-0.1960784*E
    #print(dE)
    dI <- 0.1960784*E - gamma.gamma * I
    #print(dI)
    dR <- gamma.gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}


#loss function for optimization
loss = function(ps, data, recovered){
  beta1 = ps[1]
  gamma1 = ps[2]
  #print(ps)
  times <- seq(1, length(data), by = 1)
  parameters <- c(beta = beta1, gamma = gamma1)
  solution = ode(y = init1, times = times, func = sier, parms = parameters)
  
  conf = (mean(((data/popn)-solution[,4])^2))^0.5
  recov = (mean(((recovered/popn)-solution[,5])^2))^0.5
  
  #conf = (mean((data-(solution[,4]*popn))^2))^0.5
  #recov = (mean(((recovered/popn)-solution[,5])^2))^0.5
  alpha = 1
  val = alpha*conf + (1-alpha)*recov 
  #print(val)
  #print(solution[,4])
  return(val)
}

#wrapper for optimization
estimate_params = function(data, recovered){
  
  optimal = optim(par = c(beta =1.3, gamma = 0.7), fn = loss, method = "L-BFGS-B", lower = c(0.1, 0.1), upper = c(2,2), control = list(trace =2,pgtol = 1e-16, maxit = 3000), data = data, recovered = recovered)
  print(optimal)
}


popn = 60000000
init1 = c(S=1-8.333333e-08-3.333333e-07,E= 3.333333e-07, I=8.333333e-08, 0.0)
estimates = estimate_params(confirmed_transpose$Italy_[10:70]-deaths_transpose$Italy_[10:70]-recovered_transpose$Italy_[10:70], deaths_transpose$Italy_[10:70])
estimates$par
estimates$par[1]/estimates$par[2]
parameters <- c(beta = estimates$par[1], gamma = estimates$par[2])
italy_dates = seq(as.Date("2020-01-31"), as.Date("2020-03-31"), by="days")
soln = plot_seir(parameters, confirmed_transpose$Italy_[10:70], italy_dates)
print(nrow(soln))
soln = plot_seir_imp(parameters, confirmed_transpose$Italy_[10:67], italy_dates)

popn = 51400000
init1 = c(S=1-1.94e-08-5.83e-08,E= 5.83e-08, I=1.94e-08, 0.0)
estimates1 = estimate_params(confirmed_transpose$`South Korea`, deaths_transpose$`South Korea`)
estimates1$par
estimates1$par[1]/estimates1$par[2]
parameters1 <- c(beta = estimates1$par[1], gamma = estimates1$par[2])
sk_dates = seq(as.Date("2020-01-22"), as.Date("2020-03-31"), by="days")
soln1 = plot_seir(parameters1, confirmed_transpose$`South Korea`, sk_dates)
print(nrow(soln1))
#soln1 = plot_seir_imp(parameters1, confirmed_transpose$`South Korea`, sk_dates)


popn = 46600000
init1 = c(S=1-2.14e-08-6.43e-08,E= 6.43e-08, I=2.14e-08, 0.0)
estimates2 = estimate_params(confirmed_transpose$Spain_[11:70], deaths_transpose$Spain_[11:70])
estimates2$par
estimates2$par[1]/estimates2$par[2]
parameters2 <- c(beta = estimates2$par[1], gamma = estimates2$par[2])
sp_dates = seq(as.Date("2020-02-01"), as.Date("2020-03-31"), by="days")
soln2 = plot_seir(parameters2, confirmed_transpose$Spain_[11:70], sp_dates)
#soln2 = plot_seir_imp(parameters2, confirmed_transpose$Spain_[11:70], sp_dates)

popn = 60000000
init1 = c(S=1-8.333333e-08-3.333333e-07,E= 3.333333e-07, I=8.333333e-08, 0.0)
estimates3 = estimate_params(confirmed_transpose$China_Hubei, deaths_transpose$China_Hubei)
estimates3$par
estimates3$par[1]/estimates3$par[2]
parameters3 <- c(beta = estimates3$par[1], gamma = estimates3$par[2])
soln3 = plot_seir(parameters3, confirmed_transpose$China_Hubei, sk_dates)


dates = seq(as.Date("2020-01-30"), as.Date("2020-7-31"), by="days")
print(dates)
fig = plot_ly(x=dates, y=soln[,4], name='Italy',
              type = 'scatter', mode = 'lines',)
fig = fig %>% add_trace(y=soln1[,4], name = 'South Korea')
fig = fig %>% add_trace(y=soln2[,4], name='Spain')
fig = fig %>% add_trace(y=soln3[,4], name='Hubei, China')
fig = fig %>% layout(xaxis = x, yaxis=y, title = "Proportion affected according to model")
api_create(fig, filename = "country-seir")
print(fig)