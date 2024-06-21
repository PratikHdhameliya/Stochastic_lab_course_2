data("faithful")
faith_data<- faithful$waiting

#First we will find mean and standard deviation of data
mean_faith <- mean(faith_data)
sd_faith<- sd(faith_data)

#for initial value guess, the nicer choice is always add and subtract half of standard deviation    
m_1 <- mean_faith - (sd_faith)/2
m_2<- mean_faith + (sd_faith)/2
p_initial <- 0.5
params_initialise<-c(p_initial, m_1, sd_faith, m_2, sd_faith)


# see data structure and change upper bound in M-Step
hist(faith_data)

#now use EM algorithm on faith data set 
optimization_results <- perform_optimization(faith_data, params_initialise,num_iterations= 10, stopping_criteria = c("iterations", "threshold"), threshold = 1e-6)



#Now for quakes data set 
data("quakes")
quakes_data<-quakes$depth

#mean and standard deviation values 
mean_quakes<-mean(quakes_data)
sd_quakes<-sd(quakes_data)

#for initial value guess, the nicer choice is always add and subtract half of standard deviation
m_1_quakes<-mean_quakes-(sd_quakes/2)
m_2_quakes<- mean_quakes+(sd_quakes/2)
params_initialise_quakes<-c(p_initial,m_1_quakes,sd_quakes,m_2_quakes,sd_quakes)

# see data structure and change upper bound in M-Step
hist(quakes_data)

optimization_results_quakes <- perform_optimization(data=quakes_data,initial_params=params_initialise_quakes,num_iterations= 10, stopping_criteria = c("iterations", "threshold"), threshold = 1e-6)


##below function take above outputs and find marginal density and plot graph

Marginal_Density_and_plot(optimization_results_quakes,quakes_data)

Marginal_Density_and_plot(optimization_results,faith_data)

