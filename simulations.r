setwd("/Users/vahanaslanyan/Documents/CARA_and_RAR/")
#source helper functions
source("helper_functions.R")
#create and load STAN models from the code
model_cara <- cmdstan_model("./stan_cara.stan")
model_rar<-cmdstan_model("./stan_rar.stan")
longitudinal_model <- cmdstan_model("./longitudinal.stan")
#treatments--fixed for this project
K=c(1:4)
#initialize effect size vectors and the data.table
effect_size_cara_first_iteration<-NA
effect_size_rar_first_iteration<-NA
effect_size_cara_second_iteration<-NA
effect_size_rar_second_iteration<-NA
effect_size_cara_third_iteration<-NA
effect_size_rar_third_iteration<-NA
effect_size_cara_fourth_iteration<-NA
effect_size_rar_fourth_iteration<-NA
effect_size_cara_fifth_iteration<-NA
effect_size_rar_fifth_iteration<-NA
aux_effect_sizes<-data.table(effect_size_cara_first_iteration,effect_size_cara_second_iteration,
                         effect_size_cara_third_iteration,effect_size_cara_fourth_iteration,
                         effect_size_cara_fifth_iteration,effect_size_rar_first_iteration,
                         effect_size_rar_second_iteration,effect_size_rar_third_iteration,
                         effect_size_rar_fourth_iteration,effect_size_rar_fifth_iteration)

effect_sizes<-data.table()
#initialize allocation probability vectors and the data table.
allocation_prob_cara_second_iteration<-rep(NA,length(K))
allocation_prob_rar_second_iteration<-rep(NA,length(K))
allocation_prob_cara_third_iteration<-rep(NA,length(K))
allocation_prob_rar_third_iteration<-rep(NA,length(K))
allocation_prob_cara_fourth_iteration<-rep(NA,length(K))
allocation_prob_rar_fourth_iteration<-rep(NA,length(K))
allocation_prob_cara_fifth_iteration<-rep(NA,length(K))
allocation_prob_rar_fifth_iteration<-rep(NA,length(K))
allocation_prob_cara_final<-rep(NA,length(K))
allocation_prob_rar_final<-rep(NA,length(K))
Treatment_names<-paste0("Treatment",K)
aux_allocation_probs<-data.table(Treatment_names,allocation_prob_cara_second_iteration,
                         allocation_prob_cara_third_iteration,allocation_prob_cara_fourth_iteration,
                         allocation_prob_cara_fifth_iteration,allocation_prob_cara_final,
                         allocation_prob_rar_second_iteration,allocation_prob_rar_third_iteration,
                         allocation_prob_rar_fourth_iteration,allocation_prob_rar_fifth_iteration,allocation_prob_rar_final)

allocation_probs<-data.table()

#interim steps, input data generated with cara randomization, rar randomization, 
#type interim--either regression with change score or longitudinal--mixed models for repeated measures
#tuning parameter (n/2N for main project, 0.4, 0.6 for sensitivity analyses)
interim_steps<-function(data_cara,data_rar,type_interim="regression", tuning_parameter){
    if(type_interim!="regression" & type_interim!="MMRM"){
    stop("type_interim should be either 'MMRM' or 'MMRM'")
  }
  if(type_interim=="regression"){
    #preprocess data for stan
    stan_data_cara <- prepare_data_for_stan(data_cara,type_interim="regression", rand_procedure="CARA")
    stan_data_rar<-prepare_data_for_stan(data_rar,type_interim="regression", rand_procedure="RAR")
    # fit te models 
    fit_cara <- model_cara$sample(data = stan_data_cara,chains=5,parallel_chains = 10,refresh = 0)
    fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
    #get allocation probabilities
    interim_allocation_prob_cara<-get_allocation_prob(fit=fit_cara,c=tuning_parameter,type_interim="regression",version="CARA")
    interim_allocation_prob_rar<-get_allocation_prob(fit=fit_rar,c=tuning_parameter,type_interim="regression",version="RAR")
    } else{
        #prepare the data for Stan sampling 
        stan_data_cara <- prepare_data_for_stan(data_cara,type_interim="MMRM", rand_procedure="CARA")
        stan_data_rar<-prepare_data_for_stan(data_rar,type_interim="MMRM", rand_procedure="RAR")
        #sample crar and rar models
        fit_cara <-longitudinal_model$sample(data = stan_data_cara,chains = 5,parallel_chains = 5,refresh = 0)
        fit_rar <-longitudinal_model$sample(data = stan_data_rar,chains = 5,parallel_chains = 5,refresh = 0)
        interim_allocation_prob_cara<-get_allocation_prob(fit=fit_cara,c=tuning_parameter,type_interim="MMRM",version="CARA")
        interim_allocation_prob_rar<-get_allocation_prob(fit=fit_rar,c=tuning_parameter,type_interim="MMRM",version="RAR")
    }
    #get the covariate imbalance effect sizes
    interim_effect_size_cara<-get_effect_size(data_cara)
    interim_effect_size_rar<-get_effect_size(data_rar)
    #put everything in a table
    #note that since the allocation probability is a vector of length 4, the effect sizes will be replicated 4 times.
    interim_df<-data.table(interim_allocation_prob_cara,interim_allocation_prob_rar,interim_effect_size_cara,interim_effect_size_rar)
    return(interim_df)
}

#N is the number of participants in the block ,K is treatment numbers,
#interim df is the output from interim_steps(), theta is the covariate proportion in the sample
interim_data_generation<-function(N=40,K=c(1:4),interim_df,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="regression",tuning_parameter=nrow(data_cara)/400){
  if(type_interim!="regression" & type_interim!="MMRM"){
    stop("type_interim should be either 'MMRM' or 'MMRM'")
  }
  #generate new data
  data_new_cara<- simulate_data(N, K,allocation_proportion =interim_df$interim_allocation_prob_cara,theta,treatment_effect,covariate_effect,type_interim)
  data_new_rar<- simulate_data(N, K,allocation_proportion =interim_df$interim_allocation_prob_rar,theta, treatment_effect,covariate_effect, type_interim)
  #append the new data to the already processed the data both for cara and rar
  data_cara<-append_stan_data(data_cara,data_new_cara)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  #create a new interim dataframe
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim,tuning_parameter)
  return(interim_df)
}
  
#for loop for Scenario 4, theta=0.5
for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  data <- simulate_data(N=40, K=c(1:4),allocation_proportion=c(0.25,0.25,0.25,0.25),theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="MMRM")
  #in a case when no participant has a covariate value=1, get_effect_size errors out, so avoid that case.
  if(length(unique(data$covariate))==1){
    next
  }
  data_cara<-data
  data_rar<-data
  aux_effect_sizes$effect_size_cara_first_iteration<-get_effect_size(data_cara)
  aux_effect_sizes$effect_size_rar_first_iteration<-get_effect_size(data_rar)
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="MMRM",tuning_parameter=nrow(data_cara)/400)
  aux_allocation_probs$allocation_prob_cara_second_iteration<-interim_df$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_second_iteration<-interim_df$interim_allocation_prob_rar
  
  #second allocation and interim analysis
  interim_df<-interim_data_generation(N=40,K=c(1:4),interim_df,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="MMRM",tuning_parameter=nrow(data_cara)/400)
  aux_effect_sizes$effect_size_cara_second_iteration<-interim_df$interim_effect_size_cara[1]
  aux_effect_sizes$effect_size_rar_second_iteration<-interim_df$interim_effect_size_rar[1]
  aux_allocation_probs$allocation_prob_cara_third_iteration<-interim_df$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_third_iteration<-interim_df$interim_allocation_prob_rar
  
  #third allocation and interim analysis
  interim_df<-interim_data_generation(N=40,K=c(1:4),interim_df,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="MMRM",tuning_parameter=nrow(data_cara)/400)
  aux_effect_sizes$effect_size_cara_third_iteration<-interim_df$interim_effect_size_cara[1]
  aux_effect_sizes$effect_size_rar_third_iteration<-interim_df$interim_effect_size_rar[1]
  aux_allocation_probs$allocation_prob_cara_fourth_iteration<-interim_df$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_fourth_iteration<-interim_df$interim_allocation_prob_rar
  
  #fourth allocation and interim analysis
  interim_df<-interim_data_generation(N=40,K=c(1:4),interim_df,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="MMRM",tuning_parameter=nrow(data_cara)/400)
  aux_effect_sizes$effect_size_cara_fourth_iteration<-interim_df$interim_effect_size_cara[1]
  aux_effect_sizes$effect_size_rar_fourth_iteration<-interim_df$interim_effect_size_rar[1]
  aux_allocation_probs$allocation_prob_cara_fifth_iteration<-interim_df$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_fifth_iteration<-interim_df$interim_allocation_prob_rar
  

  #fifth allocation   
  interim_df<-interim_data_generation(N=40,K=c(1:4),interim_df,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="MMRM",tuning_parameter=nrow(data_cara)/400)
  aux_effect_sizes$effect_size_cara_fifth_iteration<-interim_df$interim_effect_size_cara[1]
  aux_effect_sizes$effect_size_rar_fifth_iteration<-interim_df$interim_effect_size_rar[1]
  aux_allocation_probs$allocation_prob_cara_final<-interim_df$interim_allocation_prob_cara
  aux_allocation_probs$allocation_prob_rar_final<-interim_df$interim_allocation_prob_rar
  
  #tidy up results from the iteration of simulation
  allocation_probs<-allocation_probs%>%bind_rows(aux_allocation_probs)
  effect_sizes<-effect_sizes%>%bind_rows(aux_effect_sizes)
  
  #save interim results every 500 iterations
  if(i%%500==0){
  write_csv(effect_sizes,paste0("effect_sizes_sim_",i,".csv"))
  write_csv(allocation_probs,paste0("allocation_probabilities_sim_",i,".csv"))
  }
}

