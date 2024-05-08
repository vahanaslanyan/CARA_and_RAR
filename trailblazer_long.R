BASE_PATH<-"~/Documents/CARA_and_RAR/"
OUTPUT_PATH<-paste0(BASE_PATH,"output/")
setwd(BASE_PATH)
library(pacman)
p_load(tidyverse,cmdstanr,data.table,stats,fastDummies,effectsize,stringi,stringr,cmdstanr,MASS,data.table)
options(digits=3)

## Design of Donanemab--Summary
#1736 participants, 1182 in low/medium tau, 552 in high tau (31.8%) stratums

#LSM change in low/tau group active group was -5.81 (95% CI: (-6.9 to -4.71)) (ITT N=418) SD= 11.4
#LSM change in low/tau group placebo group was -9.61 (95% CI: (-10.67 to -8.56)) (ITT N=444) SD=11.3
#LSM change in high tau group active group was -18.29 (95% CI: (-20.61 to -15.97)) (ITT N=165) SD=15.2
#LSM change in high tau group placebo group was -19.23 (95% CI: (-21.43 to -17.03)) (ITT N=208) SD=16.2

## Scenarios
#create and load STAN models from the code
# two interim analyses--at 25 and 50%
# two interim analyses--at 25 and 75%
# three interim analyses--at 25,50,75%


model_longitudinal<-cmdstan_model("./longitudinal.stan")
simulate_data<- function(N=1736, allocation_proportion=c(0.5,0.5)) {
  #generate tau status assignment
  tau_load <- rbinom(N, 1, 0.318)
  K<-c("Placebo", "Active")
  #generate treatment assignment
  treatment<-sample(K,N,replace=T,prob=allocation_proportion)
  #longitudinal data generation
  #define the correlation matrix, assume visit 1 and 2 are strongly correlated (0.7)
  corr <- matrix(1, nrow=2, ncol=2)
  corr[upper.tri(corr) | lower.tri(corr)] <- 0.7
  #generate the data with N rows, for timepoints one and 2
  data <- mvrnorm(N, mu=c(0,0), Sigma=corr)
  data_long<-data.table(tx=treatment,tau_load=tau_load,y0=data[,1],
                        y1=data[,2]
  )
  data_long$y0[data_long$tx=="Active" & data_long$tau_load==0]<-rnorm(sum(data_long$tx=="Active" & data_long$tau_load==0),mean=0, sd=11.4)
  data_long$y1[data_long$tx=="Active" & data_long$tau_load==0]<-data_long$y0[data_long$tx=="Active" & data_long$tau_load==0]+
    rnorm(sum(data_long$tx=="Active" & data_long$tau_load==0),mean=-5.81, sd=11.4)
  
  data_long$y0[data_long$tx=="Placebo" & data_long$tau_load==0]<-rnorm(sum(data_long$tx=="Placebo" & data_long$tau_load==0),mean=0, sd=11.3)
  data_long$y1[data_long$tx=="Placebo" & data_long$tau_load==0]<-data_long$y1[data_long$tx=="Placebo" & data_long$tau_load==0]+
    rnorm(sum(data_long$tx=="Placebo" & data_long$tau_load==0),mean=-9.61, sd=11.3)
  
  data_long$y0[data_long$tx=="Active" & data_long$tau_load==1]<-rnorm(sum(data_long$tx=="Active" & data_long$tau_load==1),mean=0, sd=15.2)
  data_long$y1[data_long$tx=="Active" & data_long$tau_load==1]<-data_long$y0[data_long$tx=="Active" & data_long$tau_load==1]+
    rnorm(sum(data_long$tx=="Active" & data_long$tau_load==1),mean=-18.29, sd=15.2)
  
  data_long$y0[data_long$tx=="Placebo" & data_long$tau_load==1]<-rnorm(sum(data_long$tx=="Placebo" & data_long$tau_load==1), mean=0, sd=16.2)
  data_long$y1[data_long$tx=="Placebo" & data_long$tau_load==1]<-data_long$y1[data_long$tx=="Placebo" & data_long$tau_load==1]+
    rnorm(sum(data_long$tx=="Placebo" & data_long$tau_load==1), mean=-19.23, sd=16.2)
  data_long$subjid=rep(stringi::stri_rand_strings(N, sample(3:8,1), pattern = "[A-Za-z0-9]"))
  #put data together in wide format (y-s separated by timepoints)
  data_long <- gather(data_long, timepoints,y, y0:y1, factor_key=F)
  #make timepoints more descriptive
  data_long$timepoints[data_long$timepoints=="y0"]<-"bl"
  data_long$timepoints[data_long$timepoints=="y1"]<-"fl"
  #return the generated data
  return(data_long)
}

prepare_data_for_stan<-function(data, rand_procedure="CARA"){
  if(rand_procedure!="CARA" & rand_procedure!="RAR"){
    stop("rand_procedure should be either 'CARA' or 'RAR'")
  }
  # Prepare data for Stan
  #create a simple model first, then extract the model matrix so we don't have 
  #to dummify by hand
  if(rand_procedure=="CARA"){
    mod <- lm(y ~ tau_load + tx * timepoints, data)
  }else{
    mod <- lm(y ~ tx * timepoints, data)
  }
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
  return(stan_data)
}


get_allocation_prob<-function(fit,c,version="CARA"){
  if(version!="CARA" & version!="RAR"){
    stop("version should be either 'CARA' or 'RAR'")
  }
  #extract coefficient summary from the fit model (takes time)
  betas <- fit$summary(variables = "beta")
  #depending on the version (crar or rar) there are more or less rows in betas
  if (version == "CARA") {
    betas$variable <-
      c(
        "(Intercept)",
        "tau_load",
        "txPlacebo",
        "timepointsfl",
        "txPlacebo:timepointsfl"
      )
  } else{
    betas$variable <-
      c(
        "(Intercept)",
        "txPlacebo",
        "timepointsfl",
        "txPlacebo:timepointsfl"
      )
  }
  #turn betas into a data structure
  betas <- as_tibble(betas)
  #get only slopes for treatment, remove intercepts and irrelevant data
  betas <-betas %>% filter(
    variable %in% c(
      "txPlacebo:timepointsfl"
    )         
  )
  aux_placebo <- pnorm(betas$mean,sd=betas$sd) ^ (2)
  aux_active<- pnorm(0, sd=betas$sd)^2
  aux_prob<-c(aux_active,aux_placebo)
  #get new allocation proportion for each treatment
  allocation_proportion<-(aux_prob^(c))/(sum(aux_prob^(c)))
  return(allocation_proportion)
}


#this function gets covariate proportion imbalance in the sample
get_effect_size<-function(data){
  #convert treatment and covariate to factors, for contingency table 
  data<-data[data$timepoints=="bl",]
  data$tx<-as.factor(data$tx)
  data$tau_load<-as.factor(data$tau_load)
  #calculate effect size, using adjusted Cramer's V and Cohen's D
  imbalance_tau<-cramers_v(data$tx,data$tau_load)$Cramers_v_adjusted
  return(imbalance_tau)
}


append_stan_data<-function(data,data_new){
  #add the new data to the old data, and if a column is missing i data_new, fill it in
  data%>%bind_rows(data_new)->data
  
  #all missing values are turned to 0 (no one gets assigned to treatment)
  data[is.na(data)]<-0
  
  return(data)
}

interim_steps<-function(data_cara,data_rar,tuning_parameter=nrow(data_cara)/(1736*2)){
  #preprocess data for stan
  stan_data_cara <- prepare_data_for_stan(data_cara,rand_procedure="CARA")
  stan_data_rar<-prepare_data_for_stan(data_rar,rand_procedure="RAR")
  # fit the models 
  fit_cara <- model_longitudinal$sample(data = stan_data_cara,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_longitudinal$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  #get allocation probabilities
  interim_allocation_prob_cara<-get_allocation_prob(fit=fit_cara,c=tuning_parameter,version="CARA")
  interim_allocation_prob_rar<-get_allocation_prob(fit=fit_rar,c=tuning_parameter,version="RAR")
  #get the covariate imbalance effect sizes
  interim_effect_size_cara<-get_effect_size(data_cara)
  interim_effect_size_rar<-get_effect_size(data_rar)
  #put everything in a table
  interim_df<-data.table(interim_allocation_prob_cara,interim_allocation_prob_rar,interim_effect_size_cara,interim_effect_size_rar)
  return(interim_df)
}

#N is the number of participants in the block ,K is treatment numbers,
#interim df is the output from interim_steps(), theta is the covariate proportion in the sample
interim_data_generation<-function(N=40,K=c("Placebo", "Active"),data_cara,data_rar,
                                  interim_df,tuning_parameter=nrow(data_cara)/(1736*2)){
  #generate new data
  data_new_cara<- simulate_data(N,allocation_proportion =interim_df$interim_allocation_prob_cara) 
  data_new_rar<- simulate_data(N, allocation_proportion =interim_df$interim_allocation_prob_rar)
  #append the new data to the already processed the data both for cara and rar
  data_cara<-append_stan_data(data_cara,data_new_cara)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  #create a new interim dataframe
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,tuning_parameter)
  return(list(interim_df,data_cara,data_rar))
}

# two interim analyses--at 25 and 50%
# two interim analyses--at 25 and 75%
# three interim analyses--at 25,50,75%
effect_size_cara_first_iteration_tau<-NA

effect_size_cara_second_iteration_tau<-NA
effect_size_cara_third_iteration_tau<-NA

effect_size_rar_first_iteration_tau<-NA
effect_size_rar_second_iteration_tau<-NA
effect_size_rar_third_iteration_tau<-NA


aux_effect_sizes<-data.table(effect_size_cara_first_iteration_tau,
                             effect_size_cara_second_iteration_tau,
                             effect_size_cara_third_iteration_tau,
                             effect_size_rar_first_iteration_tau,
                             effect_size_rar_second_iteration_tau,
                             effect_size_rar_third_iteration_tau)

allocation_prob_cara_second_iteration<-rep(NA,2)
allocation_prob_rar_second_iteration<-rep(NA,2)
allocation_prob_cara_third_iteration<-rep(NA,2)
allocation_prob_rar_third_iteration<-rep(NA,2)
allocation_prob_cara_fourth_iteration<-rep(NA,2)
allocation_prob_rar_fourthiteration<-rep(NA,2)
Treatment_names<-c("Active", "Placebo")

aux_allocation_probs<-data.table(Treatment_names,allocation_prob_cara_second_iteration,
                                 allocation_prob_cara_third_iteration,
                                 allocation_prob_rar_second_iteration,allocation_prob_rar_third_iteration)

effect_sizes<-data.table()
allocation_probs<-data.table()
betas_rar<-data.table()
betas_cara<-data.table()
#change number of participants from 1736/4 to 1736/2 in second allocation for scenario 2
# add another block with 1736/4 participants for the third allocation
for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  data <-simulate_data(N=434)
  data_cara<-data
  data_rar<-data
  aux_effect_sizes$effect_size_cara_first_iteration_tau<-get_effect_size(data_cara)[1]
  aux_effect_sizes$effect_size_rar_first_iteration_tau<-get_effect_size(data_rar)[1]
  interim_df1<-interim_steps(data_cara=data_cara,data_rar=data_rar,tuning_parameter=nrow(data_cara)/(1736*2))
  
  
  aux_allocation_probs$allocation_prob_cara_second_iteration<-interim_df1$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_second_iteration<-interim_df1$interim_allocation_prob_rar
  
  #second allocation and interim analysis
  interim_df<-interim_data_generation(N=1736/4,interim_df=interim_df1,data_cara = data_cara,data_rar=data_rar)
  data_cara<-interim_df[[2]]
  data_rar<-interim_df[[3]]
  aux_effect_sizes$effect_size_cara_second_iteration_tau<-get_effect_size(data_cara)[1]
  aux_effect_sizes$effect_size_rar_second_iteration_tau<-get_effect_size(data_rar)[1]
  aux_allocation_probs$allocation_prob_cara_third_iteration<-interim_df[[1]]$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_third_iteration<-interim_df[[1]]$interim_allocation_prob_rar
  
  #third allocation and interim analysis
  interim_df<-interim_data_generation(N=round(1736)/2,interim_df=interim_df[[1]],data_cara = data_cara,data_rar=data_rar)
  data_cara<-interim_df[[2]]
  data_rar<-interim_df[[3]]
  aux_effect_sizes$effect_size_cara_third_iteration_tau<-get_effect_size(data_cara)[1]
  aux_effect_sizes$effect_size_rar_third_iteration_tau<-get_effect_size(data_rar)[1]
  
  #tidy up results from the iteration of simulation
  allocation_probs<-allocation_probs%>%bind_rows(aux_allocation_probs)
  effect_sizes<-effect_sizes%>%bind_rows(aux_effect_sizes)
  
  data_cara$tx<-factor(data_cara$tx,levels=c("Placebo","Active"))
  data_cara$tau_load<-as.factor(data_cara$tau_load)
  data_rar$tx<-factor(data_rar$tx,levels=c("Placebo","Active"))
  
  cara_data_final <- prepare_data_for_stan(data_cara,rand_procedure="CARA")
  
  fit_cara <- model_longitudinal$sample(data = cara_data_final,chains=5,parallel_chains = 10,refresh = 0)
  betas_cara_aux <- fit_cara$summary(variables = "beta")
  betas_cara_aux$variable <-
    c(
      "(Intercept)",
      "tau_load",
      "txActive",
      "timepointsfl",
      "txActive:timepointsfl"
    )
  betas_cara_aux<-betas_cara_aux%>% filter(
    variable %in% c(
      "txActive:timepointsfl"
    ))         
  betas_cara<-betas_cara%>%bind_rows(betas_cara_aux)
  
  rar_data_final <- prepare_data_for_stan(data_rar,rand_procedure="RAR")
  
  fit_rar <- model_longitudinal$sample(data = rar_data_final,chains=5,parallel_chains = 10,refresh = 0)
  betas_rar_aux <- fit_rar$summary(variables = "beta")
  betas_rar_aux$variable <-
    c(
      "(Intercept)",
      "txActive",
      "timepointsfl",
      "txActive:timepointsfl"
    )
  betas_rar_aux<-betas_rar_aux%>% filter(
    variable %in% c(
      "txActive:timepointsfl"
    ))         
  betas_rar<-betas_rar%>%bind_rows(betas_rar_aux)
  
}

write_csv(effect_sizes,paste0(OUTPUT_PATH,"effect_sizes_trailblazer_25_50_long_",i,".csv"))
write_csv(allocation_probs,paste0(OUTPUT_PATH,"allocation_probabilities_trailblazer_25_50_long_",i,".csv"))
write_csv(betas_cara,paste0(OUTPUT_PATH,"beta_cara_trailblazer_25_50_long_",i,".csv"))
write_csv(betas_rar,paste0(OUTPUT_PATH,"beta_rar_trailblazer_25_50_long_",i,".csv"))
