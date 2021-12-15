library(rjags)
#Read the data in. I made the data from the Stream Dataset with adjusted genres into a txt. Theres also a grouped dataset that isn't used 
data = read.table("grouped_spotify_data.txt")

#Make the streams per day variable. I changed streams to thousands of streams per day for no other reason than it was a random thing I did to try to solve/narrowing down the issues with my code. We can change it back pretty easily
data$streams_day = data$stream/data$days

#This orders by genre so the newest bugs model makes since
data = data[order(data$top.genre),]

#The bug model is weird, but essentially I coded it so each genre has its own loop over it's songs. Please check that it makes sense
#a and b needed priors in order for the model to work I think. I'm not sure I got the priors/hyperpriors assigned correctly
#I got ride of N in our model since it doesn't really make sense for this model since our data doesn't really have "trials" per se


#Assign the thousands of streams per day to Y
d <- list(Y=data[,4])

#I set random priors for a and b. I think those are the only ones that need it. Feel free to change the numbers when testing
inits <- list(list(a=.0001, b=.0001), 
              list(a=1000, b=1000), 
              list(a=.0001, b=1000))

#Run the jags model with latest bug file
m <- jags.model("hierarchical_simple.bug", d, inits, n.chains=3)

### Make a preliminary run of 1000 iterations, with monitoring
#NOTE: don't do too many iterations or it will take forever to plot the graphs/exit the code

x <- coda.samples(m, c("theta", "lambda"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in
#For some reason this never has worked for me in any assignment

summary(window(x, 400))

# Verify: Time-series SE less than 1/20 of SD
#Window seems to not work properly

plot(window(x, 400), trace=FALSE, ask=TRUE)