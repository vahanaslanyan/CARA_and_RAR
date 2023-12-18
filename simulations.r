model_cara <- cmdstan_model("./stan_cara.stan")
model_rar<-cmdstan_model("./stan_rar.stan")
longitudinal_model <- cmdstan_model("./longitudinal.stan")

effect_size_cara_first_iteration<-c()
effect_size_rar_first_iteration<-c()
effect_size_cara_second_iteration<-c()
effect_size_rar_second_iteration<-c()
effect_size_cara_third_iteration<-c()
effect_size_rar_third_iteration<-c()
effect_size_cara_fourth_iteration<-c()
effect_size_rar_fourth_iteration<-c()
effect_size_cara<-c()
effect_size_rar<-c()

final_assignment_cara<-c()
final_assignment_rar<-c()

theta_cara_second_iteration<-c()
theta_rar_second_iteration<-c()

theta_cara_third_iteration<-c()
theta_rar_third_iteration<-c()


theta_cara_fourth_iteration<-c()
theta_rar_fourth_iteration<-c()


theta_cara_fifth_iteration<-c()
theta_rar_fifth_iteration<-c()



interim_steps<-function(data_cara,data_rar,type_interim="regression", tuning_parameter){
    if(type_interim!="regression" & type_interim!="MMRM"){
    stop("type_interim should be either 'regression' or 'MMRM'")
  }
  if(rand_procedure!="CARA" & rand_procedure!="RAR"){
    stop("rand_procedure should be either 'CARA' or 'RAR'")
  }
  if(type_interim=="regression"){
    stan_data_cara <- prepare_data_for_stan(data_cara,type_interim="regression", rand_procedure="CARA")
    stan_data_rar<-prepare_data_for_stan(data_rar,type_interim="regression", rand_procedure="RAR")
    fit_cara <- model_cara$sample(data = stan_data_cara,chains=5,parallel_chains = 10,refresh = 0)
    fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
    interim_allocation_prob_cara<-get_allocation_prob(fit=fit_cara,c=tuning_parameter,type_interim="regression",version="CARA")
    interim_allocation_prob_rar<-get_allocation_prob(fit=fit_rar,c=tuning_parameter,type_interim="regression",version="RAR")
    } else{
        #prepare the data for Stan sampling 
        stan_data_cara <- prepare_data_for_stan(data_cara,type_interim="MMRM", rand_procedure="CARA")
        stan_data_rar<-prepare_data_for_stan(data_rar,type_interim="regression", rand_procedure="RAR")
        #sample crar and rar models
        fit_cara <-longitudinal_model$sample(data = stan_data_crar,chains = 5,parallel_chains = 5,refresh = 0)
        fit_rar <-longitudinal_model$sample(data = stan_data_rar,chains = 5,parallel_chains = 5,refresh = 0)
        interim_allocation_prob_cara<-get_allocation_prob(fit=fit_cara,c=tuning_parameter,type_interim="MMRM",version="CARA")
        interim_allocation_prob_rar<-get_allocation_prob(fit=fit_rar,c=tuning_parameter,type_interim="MMRM",version="RAR")
    }
    interim_effect_size_cara<-get_effect_size(data_crar)
    interim_effect_size_rar<-get_effect_size(data_rar)
    interim_df<-data.table(interim_allocation_prob_cara,interim_allocation_prob_rar,interim_effect_size_cara,interim_effect_size_rar)
    return(interim_df)
}




for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),allocation_proportion=c(0.25,0.25,0.25,0.25),theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="regression")
  if(length(unique(data$x))==1){
    next
    }
  data_cara<-data
  data_rar<-data
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="regression",tuning_parameter=nrow(data_cara)/400)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,interim_df$interim_effect_size_cara)
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,interim_df$interim_effect_size_rar)
  allocation_prob_cara_second_iteration<-c(allocation_prob_cara_second_iteration,interim_df$interim_allocation_prob_cara)
  allocation_prob_rar_second_iteration<-c(allocation_prob_rar_second_iteration,interim_df$interim_allocation_prob_rar)
  #second allocation and interim analysis
  data_new_cara<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_cara,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="regression")
  data_new_rar<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_rar,theta=0.5, treatment_effect = c(0,1,2,6),covariate_effect=-4, type_interim="regression")
  data_cara<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)

  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="regression",tuning_parameter=nrow(data_cara)/400)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,interim_df$interim_effect_size_cara)
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,interim_df$interim_effect_size_rar)
  allocation_prob_cara_third_iteration<-c(allocation_prob_cara_third_iteration,interim_df$interim_allocation_prob_cara)
  allocation_prob_rar_third_iteration<-c(allocation_prob_rar_third_iteration,interim_df$interim_allocation_prob_rar)
  
  #third allocation and interim analysis
  data_new_cara<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_cara,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="regression")
  data_new_rar<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_rar,theta=0.5, treatment_effect = c(0,1,2,6),covariate_effect=-4, type_interim="regression")
  data_cara<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)

  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="regression",tuning_parameter=nrow(data_cara)/400)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,interim_df$interim_effect_size_cara)
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,interim_df$interim_effect_size_rar)
  allocation_prob_cara_fourth_iteration<-c(allocation_prob_cara_fourth_iteration,interim_df$interim_allocation_prob_cara)
  allocation_prob_rar_fourth_iteration<-c(allocation_prob_rar_fourth_iteration,interim_df$interim_allocation_prob_rar)
  
  #fourth allocation and interim analysis
  data_new_cara<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_cara,theta=0.5,treatment_effect = c(0,1,2,6),covariate_effect=-4,type_interim="regression")
  data_new_rar<- simulate_data(N=40, K=c(1:4),allocation_proportion =interim_df$interim_allocation_prob_rar,theta=0.5, treatment_effect = c(0,1,2,6),covariate_effect=-4, type_interim="regression")
  data_cara<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="regression",tuning_parameter=nrow(data_cara)/400)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,interim_df$interim_effect_size_cara)
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,interim_df$interim_effect_size_rar)
  allocation_prob_cara_fifth_iteration<-c(allocation_prob_cara_fifth_iteration,interim_df$interim_allocation_prob_cara)
  allocation_prob_rar_fifth_iteration<-c(allocation_prob_rar_fifth_iteration,interim_df$interim_allocation_prob_rar)
  

  #fifth allocation   
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  interim_df<-interim_steps(data_cara=data_cara,data_rar=data_rar,type_interim="regression",tuning_parameter=nrow(data_cara)/400)
  effect_size_cara_fifth_iteration<-c(effect_size_cara_fifth_iteration,interim_df$interim_effect_size_cara)
  effect_size_rar_fifth_iteration<-c(effect_size_rar_fifth_iteration,interim_df$interim_effect_size_rar)
  allocation_prob_cara_final<-c(allocation_prob_cara_final,interim_df$interim_allocation_prob_cara)
  allocation_prob_rar_final<-c(allocation_prob_rar_final,interim_df$interim_allocation_prob_rar)
  #save interim results every 500 iterations
  if(i%%500==0){
  effect_sizes<-as_tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara_fifth_iteration,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar_fifth_iteration))
  write_csv(effect_sizes,paste0("effect_sizes_sim_",i,".csv"))
  allocation_probabilities<-as_tibble(cbind(allocation_prob_cara_second_iteration,allocation_prob_cara_third_iteration,
                                    allocation_prob_cara_fourth_iteration,allocation_prob_cara_fifth_iteration,
                                    allocation_prob_cara_final,allocation_prob_rar_second_iteration,
                                    allocation_prob_rar_third_iteration,allocation_prob_rar_fourth_iteration,
                                    allocation_prob_rar_fifth_iteration,allocation_prob_rar_final  ))  
  write_csv(allocation_probabilities,paste0("allocation_probabilities_sim_",i,".csv"))

  }
  
  }

