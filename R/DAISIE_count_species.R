### function to count the number of species and colonisation events in
## DAISIE_data lists (can be single empirical datasets or simulated datasets)

DAISIE_count_species<- function(islands,sort_clade_sizes=T){

if(length(grep("not_present",islands))==1) {islands<-list(islands)}

replicates<-length(islands)
time<-islands[[1]][[1]]$island_age


###### Calculate overall species richness and
## colonization statistics across all islands
number_colonists<-c()
number_species<-c()
size_largest_clade<-c()
mean_clade_size<-c()
clade_sizes<-c()

for(i in 1:replicates){
  the_island<-islands[[i]]
  ncols<-length(the_island)-1
  number_colonists<-append(number_colonists,ncols)

  if(ncols==0){
    number_species<-append(number_species,0)
    size_largest_clade<-append(size_largest_clade,0)
    mean_clade_size<-append(mean_clade_size,0)
    clade_sizes<-append(clade_sizes,0)
     }

  if(ncols>0){
    btimes<-c()
    miss_specs<-c()

    for(o in 2:length(the_island)){
      btimes<-append(btimes,length(the_island[[o]]$branching_times)-1)
      miss_specs<-append(miss_specs,the_island[[o]]$missing_species)
    }
    number_species<-append(number_species,sum(btimes,miss_specs))}
    clade_sizes_dist<-btimes+miss_specs
    size_largest_clade<-append(size_largest_clade,max(clade_sizes_dist))
    mean_clade_size<-append(mean_clade_size,round(mean(clade_sizes_dist),2))
    if(sort_clade_sizes==T) {clade_sizes[i]<-list(sort(clade_sizes_dist))} else{
    clade_sizes[i]<-list(clade_sizes_dist)}

}

overall_results<-list(clade_sizes_sorted=clade_sizes,
                      size_largest_clade=size_largest_clade,
                      mean_clade_size=mean_clade_size,
                      number_colonisations=number_colonists,
                      total_number_species=number_species
                      )

if(sort_clade_sizes==F) {names(overall_results)[1]<-"clade_sizes"}

return(overall_results)
}
