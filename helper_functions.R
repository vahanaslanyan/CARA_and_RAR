#load environment
library(tidyverse)
library(cmdstanr)
library(data.table)
library(stats)
library(fastDummies)
library(effectsize)
library(stringi)
library(stringr)
library(cmdstanr)
library(MASS)

#function to generate data
#N= number of observations to simulate
#K= a vector with treatment assignments
#allocation_proportion= a vector of allocation proportions to each treatment, should match with the length of K
#theta= covariate proportion
#treatment_effect= a vector of treatment effects for each treatment
#covariate_effect= the effect of covariate on the treatment 
#output= a dataframe with the simulated data
#type_interim=type of interim analysis--either "MMRM" or "regression"
simulate_data<- function(N=40, K=c(0:3),allocation_proportion=c(0.25,0.25,0.25,0.25),theta=0.5,treatment_effect,covariate_effect,type_interim="regression") {
    if(length(K)!=length(allocation_proportion)){
      stop("treatment assignment and allocation proportions should have the same length")
    }
    if(length(K)!=length(treatment_effect)){
      stop("treatment assignment and treatment effects should have the same length")
    }
    if(type_interim!="regression" & type_interim!="MMRM"){
      stop("type_interim should be either 'regression' or 'MMRM'")
    }
    #if one of the allocation proportions is lower than 1 percent, do not assign people to this treatment
    if(any(allocation_proportion<0.01)){
        allocation_proportion<-allocation_proportion[allocation_proportion>=0.01]
        K<-K[allocation_proportion>=0.01]
    }
    #generate covariate assignment
    cov<- rbinom(N, 1, theta)
    #generate treatment assignment
    treatment<-sample(K,N,replace=T,prob=allocation_proportion)
    #if there is a scenario where no one gets assigned to a specific arm, resample
        while(!length(unique(treatment))==length(K)){
            treatment<-sample(K,N,replace=T,prob=theta)
    }
    if(type_interim=="regression")
    {
        #generate outcome, normal with mean treatment effect (and covariate effect), and standard deviation 10
        y <- rnorm(N, treatment_effect[treatment]+covariate_effect*cov, 10) # outcome
        #put everything in a data.table and return 
        data_reg=data.table(y = y, treatment = treatment,covariate=cov)
        return(data_reg)
    } else{
        #longitudinal data generation
        #define the correlation matrix, assume visit 1 and 2 are mildly correlated (0.5)
        corr <- matrix(1, nrow=2, ncol=2)
        corr[upper.tri(corr) | lower.tri(corr)] <- 0.5
        #generate the data with N rows, for timepoints one and 2
        data <- mvrnorm(N, mu=c(0,0), Sigma=corr)
        #sample treatment assignments
        #get baseline and followup data without treatment
        y0 <- data[,1]
        y1 <- data[,2]
        #add in the effect of treatment and the covariate
        y1 <- y1+treatment_effect[treatment]+covariate_effect*cov+rnorm(N,0,10)
        #generate random subject ids 
        subjid=rep(stringi::stri_rand_strings(N, sample(3:8,1), pattern = "[A-Za-z0-9]"))
        #put data together in wide format (y-s separated by timepoints)
        wideData <- data.frame(subjid=subjid, covariate=cov,treatment=treatment, y0=y0,y1=y1)
        #put data together in long format (there is a variable for timepoint now)
        data_long <- gather(wideData, timepoints,y, y0:y1, factor_key=F)
        #make timepoints more descriptive
        data_long$timepoints[data_long$timepoints=="y0"]<-"bl"
        data_long$timepoints[data_long$timepoints=="y1"]<-"fl"
        #return the generated data
        return(data_long)
    }
}

#process data for STAN input
#input and outputs are dataframes
data_handling<-function(data){
    #convert treatment from numeric to a  factor variable
    data$treatment<-factor(data$treatment)
    #use dummy coding for treatment assignment
    data <- dummy_cols(data,select_columns = "treatment")
    return(data)
}

#prepare data for STAN input, using CARA
#input is a dataframe (from data_handling step), output is a list
prepare_data_for_stan_cara<-function(data,type_interim="regression"){
    if(type_interim!="regression" & type_interim!="MMRM"){
      stop("type_interim should be either 'regression' or 'MMRM'")
    }
    if(type_interim=="regression"){
    N <- nrow(data)
    K <- length(unique(data$treatment))
    # Prepare data for Stan
    stan_data_crar <- list(
        N = N,
        K = K,
        x=data$covariate,
        treatment = data[,-c(1:3)],
        y = data$y
    )
    } else{
    
        data$treatment<-as.factor(data$treatment)
        #create a simple model first, then extract the model matrix so we don't have 
        #to dummify by hand
        mod <- lm(y ~ cov * timepoints + treatment * timepoints, data)
        x <- model.matrix(mod)
        # Prepare data for Stan
        stan_data_crar <- list(
            Nobs = nrow(data),
            Npreds = ncol(x),
            Ngroups = length(unique(data$subjid)),
            y = data$y,
            x = x,
            timevar = as.numeric(as.factor(data$timepoints)),
            group = as.numeric(as.factor(data$subjid))
        )
    }
  return(stan_data_crar)
}

#prepare data for STAN input, using RAR
#input is a dataframe (from data_handling step), output is a list
prepare_data_for_stan_rar<-function(data,type_interim="regression"){
   if(type_interim!="regression" & type_interim!="MMRM"){
      stop("type_interim should be either 'regression' or 'MMRM'")
    }
    if(type_interim=="regression"){
    N <- nrow(data)
    K<-length(unique(data$treatment))
    # Prepare data for Stan
    stan_data <- list(
    N = N,
    K = K,
    treatment = data[,-c(1:3)],
    #covariate is not included in the RAR model
    y = data$y
    )
    }else{
        data$treatment<-as.factor(data$treatment)
        mod <- lm(y ~ treatment * timepoints, data)
        x <- model.matrix(mod)
        # Prepare data for Stan
        stan_data <- list(
        Nobs = nrow(data),
        Npreds = ncol(x),
        Ngroups = length(unique(data$subjid)),
        y = data$y,
        x = x,
        timevar = as.numeric(as.factor(data$timepoints)),
        group = as.numeric(as.factor(data$subjid))
        )
  
    }
  return(stan_data)
}

#function to get allocation probabilites from interim analysis
#inputs are the stan fit, and the tuning parameter c (see paper)
#output is a vector of allocation proportions
get_allocation_prob<-function(fit,c){
    #extract the betas from the stan fit
    betas<-fit$summary(variables = c("beta_t"))
    #turn the matrix into a dataframe and standardize the betas (to the first treatment)
    betas<-as_tibble(betas)
    betas$mean<-(betas$mean-betas$mean[1])/betas$sd[1]
    #calculate the probability of each treatment being the best among the options
    aux_prob<-pnorm(betas$mean)^(length(betas$mean))
    #get new allocation proportion for each treatment
    allocation_proportion<-(aux_prob^(c))/(sum(aux_prob^(c)))
    return(allocation_proportion)
}

#function to add more data after interim analysis and make it ready for STAN input
#inputs are two datasets, output is one dataframe
append_stan_data<-function(data,data_new){
    #process new block of generated data through data_handling()
    data_new<-data_handling(data_new)
    #add the new data to the old data, and if a column is missing i data_new, fill it in
    data<-rbind(data,data_new,fill=T)
    #all missing values are turned to 0 (no one gets assigned to treatment)
    data[is.na(data)]<-0
    return(data)
}

#this function gets covariate proportion imbalance in the sample
get_effect_size<-function(data){
    #convert treatment and covariate to factors, for contingency table      
    data$treatment<-as.factor(data$treatment)
    data$x<-as.factor(data$x)
    #calculate effect size, using adjusted Cramer's V
    imbalance<-cramers_v(data$treatment,data$x)$Cramers_v_adjusted
    return(imbalance)
}

