#' Count number of species in DAISIE datalist or simulated data .
#'
#' Calculates various island diversity metrics.
#'
#' @param islands Island datalist or simulated data in DAISIE datalist format.
#' Can be a single island (empirical data) generated with DAISIE_dataprep or
#' DAISIEprep. Can also be simulated data generated with DAISIE_sim function.
#' @param sort_clade_sizes Default sort_clade_sizes=T outputs clade sizes
#' sorted in ascending order of number of species. sort_clade_sizes=F outputs
#' clade sizes in the same order as they appear in the input datalist.
#' @return List
#' @author Luis Valente
#' @seealso \code{\link{DAISIE_dataprep}},
#' \code{\link{DAISIE_plot_island}}
#' @keywords models
#' @return The output is a list containing one island, or, in the case of simulations
#' with multiple replicates, it is a list composed of individual lists for each island
#' which can be called using [[i]]. The output for each island contains:
#' \item{clade_sizes_sorted}{ The total number of species in each island clade
#'  (including missing species). If option sort_clade_sizes = T, the vector will
#'  be sorted by increasing number of species. If option sort_clade_sizes = F
#'  the vector will be given in the same order as in the datalist.
#' \item{size_largest_clade}{ The total number of species in the largest island clade.}
#' \item{mean_clade_size}{ Mean clade size (average of all island clades)}
#' \item{number_colonisations}{ The total number of colonisations (clades) on the island.}
#' \item{total_number_species}{ The total number of species on the island. These are the extant
#'  species at present,including missing species; in case of simulations the number
#'  of species present on the island at the end of the simulation. }
#'  @examples
#'  data("NewZealand_birds_datalist")
#'  DAISIE_count_species(NewZealand_birds_datalist)
#' @export DAISIE_count_species
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
