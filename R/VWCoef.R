
#' class definition of VWCoef
#' 
#' @export VWCoef
#' @exportClass VWCoef
#' @field filename filename of input file
VWCoef <- setRefClass(
  Class = "VWCoef", 
  fields = list(
    filename = "character",
    content = "data.frame",
    cereal = "numeric",
    milled = "numeric"
  )
)
#' constructor
#' 
#' @name new
#' @docType methods
#' @param dir path to VWCCrop.csv file
#' @return NULL
#' @export
VWCoef$methods(initialize = function(dir) {
    filename <<- dir
    content <<- read.csv(dir, header = T, stringsAsFactors = F)
})

VWCoef$methods(findCerealGrains = function() {
  collector <- c()
  cleaner <- as.data.frame(content)
  len <- length(cleaner$FAOSTAT)
  for (i in 1:len) {
    if (cleaner$ProductDescriptionFAOSTAT[i] > 0) {
      collector <- rbind(collector, cleaner[c(i, i+1), ])
    }
  }
  return(collector)
})

VWCoef$methods(findSelectedGrains = function(collector, criteria, selects) {
  selected <- c()
  collector <- as.data.frame(collector)
  len <- length(collector[, 1])
  for (i in 1:len) {
    if (criteria[i] %in% selects) {
      selected <- rbind(selected, collector[c(i, i+1), ])
    }
  }
  return(selected)
})

VWCoef$methods(calculateCerealGrainVWC = function(selected) {
  data <- selected[, -9:0]
  n.col <- dim(data)[2]
  output = vector("numeric", length = n.col)
  for (i in 1:n.col) {
    output[i] <- mean(data[, i], na.rm = T) * 2
  }
  names(output) <- colnames(selected[, -9:0])
  return(output)
})

#' generate cereal field
#' 
#' @name getCereal
#' @docType methods
VWCoef$methods(getCereal = function() {
  cleaned.crop <- findCerealGrains()
  selected.crop <- findSelectedGrains(cleaned.crop, cleaned.crop$ProductDescriptionFAOSTAT, c("Wheat", "Maize", "Rye", "Barley", "Oats", "Sorghum", "Rice paddy"))
  # 52 nodes, ending with UsGreatLakes, LakeOntarioUS, LakeErieUS
  cereal.vwc <- calculateCerealGrainVWC(selected.crop) 
  land.cereal.vwc <- cereal.vwc[1:49]
  cereal <<- land.cereal.vwc
  return(cereal)
})

VWCoef$methods(findMilledGrains = function(cleaner, grains, select) {
  selected = c()
  len <- length(cleaner[, 1])
  for (i in 1:len) {
    if (cleaner$ProductDescriptionHS[i] > 0) {
      tmp <- cleaner$ProductDescriptionHS[i]
    } else {
      cleaner$ProductDescriptionHS[i] <- tmp
    }
  }
  
  for (i in 1:len) {
    if (cleaner$ProductDescriptionFAOSTAT[i] > 0) {
      tmp <- cleaner$ProductDescriptionFAOSTAT[i]
    } else {
      cleaner$ProductDescriptionFAOSTAT[i] <- tmp
    }
  }
  
  for (i in 1:len) {
    if (cleaner$ProductDescriptionFAOSTAT[i] %in% grains) {
      for (sel in select) {
        if (grepl(sel, cleaner$ProductDescriptionHS[i]) && cleaner$WF.type[i] != "Grey") {
          selected <- rbind(selected, cleaner[i, ])
          break
        }
      }
    }
  }
  return(selected)
})

#' generate milled grain field
#' 
#' @name getMilled
#' @docType methods
VWCoef$methods(getMilled = function() {
  grains <- c("Wheat", "Maize", "Rye", "Barley", "Oats", "Sorghum", "Rice paddy")
  select <- c("milled", "starches", "pasta", "bake", "cereal", "malt",
              "flour", "groat", "meal", "inulin", "gluten", "breakfast",
              "batter", "pellet", "husked", "flake", "hull", "chill" 
  )
  milled.crop <- findMilledGrains(content, grains, select)
  # 52 nodes, ending with UsGreatLakes, LakeOntarioUS, LakeErieUS
  milled.vwc <- calculateCerealGrainVWC(milled.crop)
  land.milled.vwc <- milled.vwc[1:49]
  milled <<- land.milled.vwc
})



