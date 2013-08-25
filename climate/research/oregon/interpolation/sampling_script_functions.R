sampling_training_testing<-function(list_param_sampling){
  
  #This function creates testing and training list for input sation data based on a list of dates. 
  #This function works for montly time scale if dates are provided as mid-months or other forms of for monthly records.
  #It requires 6 inputs:                                           
  # 1) seed_number: allow comparison across runs, if seed zero then no seed number is used
  # 2) nb_sample: number of time random sampling must be repeated for every hold out proportion 
  # 3) step : step for proportion range 
  # 4) constant: if value 1 then use the same samples as date one for the all set of dates
  # 5) prop_minmax: if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
  # 6) dates: list of dates for prediction and subsetting                              
  # 7) ghcn: station data as data.frame -daily input                                                                                
  # 11) out_prefix: output suffix added to output names--it is the same in the interpolation script
  #
  #The output is a list of four shapefile names produced by the function:
  # 1) sampling_dat: sampling information for every run by date and sampling combintation
  # 2) sampling_index: list of indexes for training and testing for every dates
  # 3) sampling_stat_id: list of station ID for training and testing for every dates
  # 4) ghcn_data: ghcn subsets by date, can be monthly or daily with mulitple sampling
  
  #AUTHOR: Benoit Parmentier                                                                       
  #DATE: 08/25/2013                                                                                 
  #PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363, TASK#558--     
  #Comments and TODO
  #
  ##################################################################################################
  
  #Parsing input arguments
  
  seed_number <-list_param_sampling$seed_number
  nb_sample <- list_param_sampling$nb_sample
  step<-list_param_sampling$step #if seed zero then no seed?     
  constant <- list_param_sampling$constant
  prop_minmax<-list_param_sampling$prop_minmax
  dates<-list_param_sampling$dates
  #ghcn_name<-list_param_sampling$ghcn_name
  ghcn<-list_param_sampling$ghcn #can be daily or monthly!!
  #ghcn<-get(ghcn_name) 
  
  ### BEGIN FUNCTION ####
  
  if (seed_number>0) {
    set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
  }
  
  nel<-length(dates)
  dates_list<-vector("list",nel) #list of one row data.frame
  prop_min<-prop_minmax[1]
  prop_max<-prop_minmax[2]
      
  prop_range<-(seq(from=prop_min,to=prop_max,by=step))*100     #range of proportion to run
  sn<-length(dates)*nb_sample*length(prop_range)               #Number of samples to run
  
  for(i in 1:length(dates)){
    d_tmp<-rep(dates[i],nb_sample*length(prop_range)) #repeating same date
    s_nb<-rep(1:nb_sample,length(prop_range))         #number of random sample per proportion
    prop_tmp<-sort(rep(prop_range, nb_sample))
    tab_run_tmp<-cbind(d_tmp,s_nb,prop_tmp)
    dates_list[[i]]<-tab_run_tmp
  }
  
  sampling_dat<-as.data.frame(do.call(rbind,dates_list))
  names(sampling_dat)<-c("date","run_samp","prop")
  
  for(i in 2:3){            # start of the for loop #1
    sampling_dat[,i]<-as.numeric(as.character(sampling_dat[,i]))  
  }
  
  sampling_dat$date<- as.character(sampling_dat[,1])
  #ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates
  ghcn.subsets <-lapply(as.character(sampling_dat$date), function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates
  
  #Make this a function??
  ## adding choice of constant sample 
  
  if (seed_number>0) {
    set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
  }
  
  sampling<-vector("list",length(ghcn.subsets))
  sampling_station_id<-vector("list",length(ghcn.subsets))
  for(i in 1:length(ghcn.subsets)){
    n<-nrow(ghcn.subsets[[i]])
    prop<-(sampling_dat$prop[i])/100
    ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
    nv<-n-ns              #create a sample for validation with prop of the rows
    ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
    ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
    #Find the corresponding 
    data_sampled<-ghcn.subsets[[i]][ind.training,] #selected the randomly sampled stations
    station_id.training<-data_sampled$station     #selected id for the randomly sampled stations (115)
    #Save the information
    sampling[[i]]<-ind.training #index of training sample from data.frame
    sampling_station_id[[i]]<- station_id.training #station ID for traning samples
  }
  ## Use same samples across the year...
  if (constant==1){
    sampled<-sampling[[1]]
    data_sampled<-ghcn.subsets[[1]][sampled,] #selected the randomly sampled stations
    station_sampled<-data_sampled$station     #selected id for the randomly sampled stations (115)
    list_const_sampling<-vector("list",sn)
    list_const_sampling_station_id<-vector("list",sn)
    for(i in 1:sn){
      station_id.training<-intersect(station_sampled,ghcn.subsets[[i]]$station)
      ind.training<-match(station_id.training,ghcn.subsets[[i]]$station)
      list_const_sampling[[i]]<-ind.training
      list_const_sampling_station_id[[i]]<-station_id.training
    }
    sampling<-list_const_sampling 
    sampling_station_id<-list_const_sampling_station_id
  }
  
  sampling_obj<-list(sampling_dat,sampling,sampling_station_id,ghcn.subsets)
  names(sampling_obj)<- c("sampling_dat","sampling_index","sampling_stat_id","ghcn_data")
  
  return(sampling_obj)
  
}
