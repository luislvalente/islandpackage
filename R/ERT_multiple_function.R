ERTs_multiple<-function(mad_future,
                        new_bat_species,
                        new_nonvol_species,
                        new_extinct_bats,
                        new_extinct_nonvol){
  
  store_erts<-c()
  
  ### SETTINGS
  ## high impact or low impact
  human_impact<-'high'
  M_all<-1000
  
  ## Time settings for the future diversity simulation
  timestep<-0.5   #decrease for a "higher-resolution" simulation
  ages<-seq(0,60,timestep)
  
  for (i in 1:nrow(mad_future)) {
    
    the_dataset<-as.numeric(mad_future[i,1])
    pars<-as.numeric(mad_future[i,11:20])
    
    ## separate the ML pars
    terrestrial_pars<-pars[1:5]
    bat_pars<-pars[6:10]
    
    ## NON_VOLANT
    
    Mad_pars<-terrestrial_pars
    nmax=400
    ## Based on the proportion of total bats vs terrestrial
    M_terrestrial=M_all-(M_all*0.22)
    if(human_impact=='high'){
      prehuman_diversity<-203+new_nonvol_species}
    
    
    current_diversity_total<-175+new_nonvol_species-new_extinct_nonvol
    current_diversity_endemic<-175+new_nonvol_species-new_extinct_nonvol
    current_diversity_non_endemic<-0
    future_2010_total<-122
    future_2010_endemic<-122
    future_2010_non_endemic<-0
    future_2015_total<-69
    future_2015_endemic<-69
    future_2015_non_endemic<-0
    future_2021_total<-52
    future_2021_endemic<-52
    future_2021_non_endemic<-0 
    
    
    ## TERRESTRIAL
    start_diversity<-c(current_diversity_endemic,current_diversity_non_endemic)
    d1<-DAISIE_ExpEIN(t = ages, pars = Mad_pars, M = M_terrestrial, initEI= start_diversity)
    a_terrestrial<-return_time_intercept(prehuman_diversity,d1[[3]],timestep)
    a_terrestrial_95<-return_time_intercept(prehuman_diversity*0.95,d1[[3]],timestep)
    
    start_diversity<-c(future_2021_endemic,future_2021_non_endemic)
    d4<-DAISIE_ExpEIN(t = ages, pars = Mad_pars, M = M_terrestrial, initEI= start_diversity)
    d_terrestrial<-return_time_intercept(current_diversity_total,d4[[3]],timestep)
    d_terrestrial_95<-return_time_intercept(current_diversity_total*0.95,d4[[3]],timestep)
    
    
    
    ### BATS
    
    Mad_pars<-bat_pars
    nmax=200
    ## Based on the proportion of total bats vs terrestrial
    M_bats=M_all*0.22
    if(human_impact=='high'){
      prehuman_diversity<-46+new_bat_species}
    
    
    current_diversity_total<-44+new_bat_species-new_extinct_bats
    current_diversity_endemic<-35+new_bat_species-new_extinct_bats
    current_diversity_non_endemic<-9
    future_2010_total<-41
    future_2010_endemic<-32
    future_2010_non_endemic<-9
    future_2015_total<-40
    future_2015_endemic<-31
    future_2015_non_endemic<-9
    future_2021_total<-39
    future_2021_endemic<-30
    future_2021_non_endemic<-9
    
    
    
    
    
    
    ### BATS
    start_diversity<-c(current_diversity_endemic,current_diversity_non_endemic)
    d1<-DAISIE_ExpEIN(t = ages, pars = Mad_pars, M = M_bats, initEI= start_diversity)
    a_bats<-return_time_intercept(prehuman_diversity,d1[[3]],timestep)
    if(length(a_bats)==0){a_bats<-0}
    a_bats_95<-return_time_intercept(prehuman_diversity*0.95,d1[[3]],timestep)
    if(length(a_bats_95)==0){a_bats_95<-0}
    
    start_diversity<-c(future_2021_endemic,future_2021_non_endemic)
    d4<-DAISIE_ExpEIN(t = ages, pars = Mad_pars, M = M_bats, initEI= start_diversity)
    d_bats<-return_time_intercept(current_diversity_total,d4[[3]],timestep)
    d_bats_95<-return_time_intercept(current_diversity_total*0.95,d4[[3]],timestep)
    if(length(d_bats_95)==0){d_bats_95<-0}
    
    
    
    ## PRINT RETURN TIMES 
    erts_1_tree<-c(the_dataset,round(a_terrestrial,2),round(a_bats,2),
                   round(d_terrestrial,2),round(d_bats,2))
    
    store_erts<-rbind(store_erts,erts_1_tree)
    
  }
  
  
  colnames(store_erts)<-c("dataset","pre_nonvol","pre_bat","2021_nonvol","2021_bat")
  
  return(store_erts)
  
}