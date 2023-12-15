model_crar <- cmdstan_model("./stan_cara.stan")
model_rar<-cmdstan_model("./stan_rar.stan")

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



  for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),theta =rep(1/length(K),length(K)), treatment_effect = c(0,1,2,6),covariate_effect=-4)
  if(length(unique(data$x))==1){
    next}
  data<-data_handling(data)
  data_crar<-data
  data_rar<-data
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,get_effect_size(data_crar))
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,get_effect_size(data_rar))

  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_second_iteration<-c(theta_cara_second_iteration,theta_crar)
  theta_rar_second_iteration<-c(theta_rar_second_iteration,theta_rar)
  #second allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,get_effect_size(data_crar))
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 5,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_third_iteration<-c(theta_cara_third_iteration,theta_crar)
  theta_rar_third_iteration<-c(theta_rar_third_iteration,theta_rar)
  #third allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,get_effect_size(data_crar))
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  #fourth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,get_effect_size(data_crar))
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_fourth_iteration<-c(theta_cara_fourth_iteration,theta_crar)
  theta_rar_fourth_iteration<-c(theta_rar_fourth_iteration,theta_rar)
  #fifth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  final_assignment_cara<-c(final_assignment_cara,theta_crar)
  final_assignment_rar<-c(final_assignment_rar,theta_rar)
  effect_size_cara<-c(effect_size_cara,get_effect_size(data_crar))
  effect_size_rar<-c(effect_size_rar,get_effect_size(data_rar))
  print(i)
  if(i%%500==0){
  effect_sizes<-as.tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar))
  write_csv(effect_sizes,paste0("effect_sizes_covariate",i,".csv"))
  test1<-matrix(final_assignment_rar,nrow=length(final_assignment_rar)/4,ncol=4,byrow = T)
  test1<-as_tibble(test1)
  write_csv(test1,paste0("theta_rar_",i,".csv"))
  test2<-matrix(final_assignment_cara,nrow=length(final_assignment_cara)/4,ncol=4,byrow = T)
  test2<-as_tibble(test2)
  write_csv(test2,paste0("theta_cara_",i,".csv"))
  }
  
  }


setwd("/Users/vahanaslanyan/Desktop/Stan_plots/new_effect_sizes_inv_gamma/stan40cov03_cn2N")
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
simulate_data <- function(N=40, K=c(1:4), theta=0.5,treatment_effect,covariate_effect) {
  x <- rbinom(N, 1, 0.3) # covariate assignment
  if(any(theta<0.01)){
    K=K[theta>=0.01]
    theta<-theta[theta>=0.01]
    
  }
  treatment<-sample(K,N,replace=T,prob=theta)
  while(!length(unique(treatment))==length(K)){
    treatment<-sample(K,N,replace=T,prob=theta)
  }
  y <- rnorm(N, treatment_effect[treatment]+covariate_effect*x, 10) # outcome
  data.table(y = y, treatment = treatment,x=x)
}

for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),theta =rep(1/length(K),length(K)), treatment_effect = c(0,1,2,6),covariate_effect=-4)
  if(length(unique(data$x))==1){
    next}
  data<-data_handling(data)
  data_crar<-data
  data_rar<-data
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,get_effect_size(data_crar))
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_second_iteration<-c(theta_cara_second_iteration,theta_crar)
  theta_rar_second_iteration<-c(theta_rar_second_iteration,theta_rar)
  #second allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,get_effect_size(data_crar))
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 5,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_third_iteration<-c(theta_cara_third_iteration,theta_crar)
  theta_rar_third_iteration<-c(theta_rar_third_iteration,theta_rar)
  #third allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,get_effect_size(data_crar))
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  #fourth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,get_effect_size(data_crar))
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_fourth_iteration<-c(theta_cara_fourth_iteration,theta_crar)
  theta_rar_fourth_iteration<-c(theta_rar_fourth_iteration,theta_rar)
  #fifth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  final_assignment_cara<-c(final_assignment_cara,theta_crar)
  final_assignment_rar<-c(final_assignment_rar,theta_rar)
  effect_size_cara<-c(effect_size_cara,get_effect_size(data_crar))
  effect_size_rar<-c(effect_size_rar,get_effect_size(data_rar))
  print(i)
  if(i%%500==0){
    effect_sizes<-as.tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar))
    write_csv(effect_sizes,paste0("effect_sizes_covariate",i,".csv"))
    test1<-matrix(final_assignment_rar,nrow=length(final_assignment_rar)/4,ncol=4,byrow = T)
    test1<-as_tibble(test1)
    write_csv(test1,paste0("theta_rar_",i,".csv"))
    test2<-matrix(final_assignment_cara,nrow=length(final_assignment_cara)/4,ncol=4,byrow = T)
    test2<-as_tibble(test2)
    write_csv(test2,paste0("theta_cara_",i,".csv"))
  }
  
}



setwd("/Users/vahanaslanyan/Desktop/Stan_plots/new_effect_sizes_inv_gamma/stan40cov05_cn2N")
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

simulate_data <- function(N=40, K=c(1:4), theta=0.5,treatment_effect,covariate_effect) {
  x <- rbinom(N, 1, 0.5) # covariate assignment
  if(any(theta<0.01)){
    K=K[theta>=0.01]
    theta<-theta[theta>=0.01]
    
  }
  treatment<-sample(K,N,replace=T,prob=theta)
  while(!length(unique(treatment))==length(K)){
    treatment<-sample(K,N,replace=T,prob=theta)
  }
  y <- rnorm(N, treatment_effect[treatment]+covariate_effect*x, 10) # outcome
  data.table(y = y, treatment = treatment,x=x)
}

theta_cara_second_iteration<-c()
theta_rar_second_iteration<-c()

theta_cara_third_iteration<-c()
theta_rar_third_iteration<-c()


theta_cara_fourth_iteration<-c()
theta_rar_fourth_iteration<-c()


theta_cara_fifth_iteration<-c()
theta_rar_fifth_iteration<-c()


for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),theta =rep(1/length(K),length(K)), treatment_effect = c(0,1,2,6),covariate_effect=-4)
  if(length(unique(data$x))==1){
    next}
  data<-data_handling(data)
  data_crar<-data
  data_rar<-data
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,get_effect_size(data_crar))
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_second_iteration<-c(theta_cara_second_iteration,theta_crar)
  theta_rar_second_iteration<-c(theta_rar_second_iteration,theta_rar)
  #second allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,get_effect_size(data_crar))
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 5,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_third_iteration<-c(theta_cara_third_iteration,theta_crar)
  theta_rar_third_iteration<-c(theta_rar_third_iteration,theta_rar)
  #third allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,get_effect_size(data_crar))
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  #fourth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,get_effect_size(data_crar))
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_fourth_iteration<-c(theta_cara_fourth_iteration,theta_crar)
  theta_rar_fourth_iteration<-c(theta_rar_fourth_iteration,theta_rar)
  #fifth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  final_assignment_cara<-c(final_assignment_cara,theta_crar)
  final_assignment_rar<-c(final_assignment_rar,theta_rar)
  effect_size_cara<-c(effect_size_cara,get_effect_size(data_crar))
  effect_size_rar<-c(effect_size_rar,get_effect_size(data_rar))
  print(i)
  if(i%%500==0){
    effect_sizes<-as.tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar))
    write_csv(effect_sizes,paste0("effect_sizes_covariate",i,".csv"))
    test1<-matrix(final_assignment_rar,nrow=length(final_assignment_rar)/4,ncol=4,byrow = T)
    test1<-as_tibble(test1)
    write_csv(test1,paste0("theta_rar_",i,".csv"))
    test2<-matrix(final_assignment_cara,nrow=length(final_assignment_cara)/4,ncol=4,byrow = T)
    test2<-as_tibble(test2)
    write_csv(test2,paste0("theta_cara_",i,".csv"))
  }
  
}




setwd("/Users/vahanaslanyan/Desktop/Stan_plots/new_effect_sizes_inv_gamma/stan40cov07_cn2N")
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

simulate_data <- function(N=40, K=c(1:4), theta=0.5,treatment_effect,covariate_effect) {
  x <- rbinom(N, 1, 0.7) # covariate assignment
  if(any(theta<0.01)){
    K=K[theta>=0.01]
    theta<-theta[theta>=0.01]
    
  }
  treatment<-sample(K,N,replace=T,prob=theta)
  while(!length(unique(treatment))==length(K)){
    treatment<-sample(K,N,replace=T,prob=theta)
  }
  y <- rnorm(N, treatment_effect[treatment]+covariate_effect*x, 10) # outcome
  data.table(y = y, treatment = treatment,x=x)
}

theta_cara_second_iteration<-c()
theta_rar_second_iteration<-c()

theta_cara_third_iteration<-c()
theta_rar_third_iteration<-c()


theta_cara_fourth_iteration<-c()
theta_rar_fourth_iteration<-c()


theta_cara_fifth_iteration<-c()
theta_rar_fifth_iteration<-c()


for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),theta =rep(1/length(K),length(K)), treatment_effect = c(0,1,2,6),covariate_effect=-4)
  if(length(unique(data$x))==1){
    next}
  data<-data_handling(data)
  data_crar<-data
  data_rar<-data
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,get_effect_size(data_crar))
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_second_iteration<-c(theta_cara_second_iteration,theta_crar)
  theta_rar_second_iteration<-c(theta_rar_second_iteration,theta_rar)
  #second allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,get_effect_size(data_crar))
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 5,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_third_iteration<-c(theta_cara_third_iteration,theta_crar)
  theta_rar_third_iteration<-c(theta_rar_third_iteration,theta_rar)
  #third allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,get_effect_size(data_crar))
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  #fourth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,get_effect_size(data_crar))
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_fourth_iteration<-c(theta_cara_fourth_iteration,theta_crar)
  theta_rar_fourth_iteration<-c(theta_rar_fourth_iteration,theta_rar)
  #fifth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  final_assignment_cara<-c(final_assignment_cara,theta_crar)
  final_assignment_rar<-c(final_assignment_rar,theta_rar)
  effect_size_cara<-c(effect_size_cara,get_effect_size(data_crar))
  effect_size_rar<-c(effect_size_rar,get_effect_size(data_rar))
  print(i)
  if(i%%500==0){
    effect_sizes<-as.tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar))
    write_csv(effect_sizes,paste0("effect_sizes_covariate",i,".csv"))
    test1<-matrix(final_assignment_rar,nrow=length(final_assignment_rar)/4,ncol=4,byrow = T)
    test1<-as_tibble(test1)
    write_csv(test1,paste0("theta_rar_",i,".csv"))
    test2<-matrix(final_assignment_cara,nrow=length(final_assignment_cara)/4,ncol=4,byrow = T)
    test2<-as_tibble(test2)
    write_csv(test2,paste0("theta_cara_",i,".csv"))
  }
  
}


setwd("/Users/vahanaslanyan/Desktop/Stan_plots/new_effect_sizes/stan40cov01_cn2N")
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

simulate_data <- function(N=40, K=c(1:4), theta=0.5,treatment_effect,covariate_effect) {
  x <- rbinom(N, 1, 0.1) # covariate assignment
  if(any(theta<0.01)){
    K=K[theta>=0.01]
    theta<-theta[theta>=0.01]
    
  }
  treatment<-sample(K,N,replace=T,prob=theta)
  while(!length(unique(treatment))==length(K)){
    treatment<-sample(K,N,replace=T,prob=theta)
  }
  y <- rnorm(N, treatment_effect[treatment]+covariate_effect*x, 10) # outcome
  data.table(y = y, treatment = treatment,x=x)
}

theta_cara_second_iteration<-c()
theta_rar_second_iteration<-c()

theta_cara_third_iteration<-c()
theta_rar_third_iteration<-c()


theta_cara_fourth_iteration<-c()
theta_rar_fourth_iteration<-c()


theta_cara_fifth_iteration<-c()
theta_rar_fifth_iteration<-c()


for(i in 1:5000){
  set.seed(i)
  # First allocation and interim analysis
  K=c(1:4)
  data <- simulate_data(N=40, K=c(1:4),theta =rep(1/length(K),length(K)), treatment_effect = c(0,1,2,6),covariate_effect=-4)
  if(length(unique(data$x))==1){
    next}
  data<-data_handling(data)
  data_crar<-data
  data_rar<-data
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_first_iteration<-c(effect_size_cara_first_iteration,get_effect_size(data_crar))
  effect_size_rar_first_iteration<-c(effect_size_rar_first_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_second_iteration<-c(theta_cara_second_iteration,theta_crar)
  theta_rar_second_iteration<-c(theta_rar_second_iteration,theta_rar)
  #second allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_second_iteration<-c(effect_size_cara_second_iteration,get_effect_size(data_crar))
  effect_size_rar_second_iteration<-c(effect_size_rar_second_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 5,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_third_iteration<-c(theta_cara_third_iteration,theta_crar)
  theta_rar_third_iteration<-c(theta_rar_third_iteration,theta_rar)
  #third allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_third_iteration<-c(effect_size_cara_third_iteration,get_effect_size(data_crar))
  effect_size_rar_third_iteration<-c(effect_size_rar_third_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  #fourth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  effect_size_cara_fourth_iteration<-c(effect_size_cara_fourth_iteration,get_effect_size(data_crar))
  effect_size_rar_fourth_iteration<-c(effect_size_rar_fourth_iteration,get_effect_size(data_rar))
  
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  
  
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  theta_cara_fourth_iteration<-c(theta_cara_fourth_iteration,theta_crar)
  theta_rar_fourth_iteration<-c(theta_rar_fourth_iteration,theta_rar)
  #fifth allocation and interim analysis
  data_new_crar<- simulate_data(N=40, K=c(1:4),theta =theta_crar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_new_rar<- simulate_data(N=40, K=c(1:4),theta =theta_rar, treatment_effect = c(0,1,2,6),covariate_effect=-4)
  data_crar<-append_stan_data(data_crar,data_new_crar)
  data_rar<-append_stan_data(data_rar,data_new_rar)
  stan_data_crar <- prepare_data_for_stan_cara(data_crar)
  stan_data_rar<-prepare_data_for_stan_rar(data_rar)
  fit_crar <- model_crar$sample(data = stan_data_crar,chains=5,parallel_chains = 10,refresh = 0)
  fit_rar<-model_rar$sample(data=stan_data_rar,chains=5,parallel_chains = 10,refresh = 0)
  theta_crar<-get_allocation_prob(fit_crar,nrow(data_crar)/400)
  theta_rar<-get_allocation_prob(fit_rar,nrow(data_crar)/400)
  final_assignment_cara<-c(final_assignment_cara,theta_crar)
  final_assignment_rar<-c(final_assignment_rar,theta_rar)
  effect_size_cara<-c(effect_size_cara,get_effect_size(data_crar))
  effect_size_rar<-c(effect_size_rar,get_effect_size(data_rar))
  print(i)
  if(i%%500==0){
    effect_sizes<-as.tibble(cbind(effect_size_cara_first_iteration,effect_size_cara_second_iteration,effect_size_cara_third_iteration,
                                  effect_size_cara_fourth_iteration,effect_size_cara,effect_size_rar_first_iteration,effect_size_rar_second_iteration,
                                  effect_size_rar_third_iteration,effect_size_rar_fourth_iteration,effect_size_rar))
    write_csv(effect_sizes,paste0("effect_sizes_covariate",i,".csv"))
    test1<-matrix(final_assignment_rar,nrow=length(final_assignment_rar)/4,ncol=4,byrow = T)
    test1<-as_tibble(test1)
    write_csv(test1,paste0("theta_rar_",i,".csv"))
    test2<-matrix(final_assignment_cara,nrow=length(final_assignment_cara)/4,ncol=4,byrow = T)
    test2<-as_tibble(test2)
    write_csv(test2,paste0("theta_cara_",i,".csv"))
  }
  
}
