#' Analysis U.S. domestic crop trade network, in terms of weight, and vritual water 
#' content.
#' 
#' \enumerate{
#'  \item Data preprocessing.
#'  \item Virtual water content calculation.
#'  \item Network analysis.
#' }
#' 
#' @docType package
#' @name uscropnetwork
NULL

#' Read csv file
csv2table <- function(csv = "stateLevel.csv") {
  dat <- read.csv(csv, header = T, sep = ",", stringsAsFactors = F)
  #   str(dat)
  
  # 2 sort the nodes##
  # 2.1 states
  states <- dat$originState
  sorted.states <- sort(unique(states))
  
  # 2.2 destinations
  destinations <- dat$USStateCFS
  sorted.destinations <- sort(unique(destinations))
  
  
  # 2.3 origins
  origins <- dat$stateCFS
  sorted.origins <- sort(unique(origins))
  
  # differences between origins and destinations
  setdiff(sorted.origins, sorted.destinations) #  more origins than distinaitons
  setdiff(sorted.destinations, sorted.origins)  
  
  # 2.4 Commodity description
  # commodity.description <-  dat$commodity
  # sorted.commodity <- sort(unique(commodity.description))
  
  # 3 index based on destinations, or the one with longer list
  obs.number <- length(dat[, 1])
  destination.indexes <- match(destinations, sorted.origins)
  origin.indexes <- match(origins, sorted.origins) #  notice that you always use the longer one
  
  # 4 weight
  weight.list <- dat$tonsThousands
  weight.str <- as.character(weight.list) #unlist the data
  weight <- as.numeric(weight.str) #convert the string into numeric
  # weight[is.na(weight)] <- 0 ##convert NA into 0
  
  value.list <- dat$valueMillion
  value.str <- as.character(value.list) #unlist the data
  value <- as.numeric(value.str) #convert the string into numeric
  
  # 5 matrix table: origin, importer, commodity code, tons. cereal grain->2, other ag prod->3, animal feed and animal origin->4, meat->5
  weight.mat <- matrix(nrow = obs.number, ncol = 4)
  weight.mat <- cbind(origin.indexes, destination.indexes, SCTG = dat$SCTG, weight)
  value.mat <- matrix(nrow = obs.number, ncol = 4)
  value.mat <- cbind(origin.indexes, destination.indexes, SCTG = dat$SCTG, value)
  # 5.1 matrix table
  wMat <- weight.mat[weight.mat[,1]!=weight.mat[,2],]  # delete the ones export to itself
  wMat.no.na <- wMat[complete.cases(wMat * 0), , drop=FALSE]  # remove records with NA
  vMat <- value.mat[value.mat[,1]!=value.mat[,2],]  # delete the ones export to itself
  vMat.no.na <- vMat[complete.cases(vMat * 0), , drop=FALSE]  # remove records with NA
  return(list(wMat.no.na, vMat.no.na, sorted.origins))
}