#' class definition of TradeMatrix
#' 
#' @export TradeMatrix
#' @exportClass TradeMatrix
#' @field weightMatrix a trade graph represented by matrix, enhanced by value matrix
#' @field valueMatrix a trade graph represented by matrix, enhanced by weight matrix
TradeMatrix <- setRefClass(
  Class = "TradeMatrix",
  fields = list(
    weightMatrix = "matrix",
    valueMatrix = "matrix",
    virtualMatrix = "matrix",
    SCTGs = "numeric",
    labels = "character"
  )
)

#' given weight or value option, SCTG, return corresponding matrix
#' 
#' @name toMatrix
#' @docType methods
#' @param tab weigh table or value table
#' @param SCTG SCTG number
#' @return trade matrix
TradeMatrix$methods(toMatrix = function(tab, SCTG, origins){
  crop <- tab[tab[,"SCTG"] == SCTG, ]
  # matrix crosstable
  if(!"package:Matrix" %in% search()) require(Matrix)
  crop.mat <- sparseMatrix(crop[,"origin.indexes"], crop[,"destination.indexes"], x = crop[,4])
  n <- max(tab[,"origin.indexes"], tab[,"origin.indexes"], na.rm = TRUE)
  if(sum(dim(crop.mat)) < 2*n){crop <- rbind(crop, c(n, n, SCTG, 0))}
  crop.mat <- sparseMatrix(crop[,"origin.indexes"], crop[,"destination.indexes"], x = crop[,4])
  crop.mat <- as.matrix(crop.mat)
  colnames(crop.mat) <- origins
  rownames(crop.mat) <- origins
  return(crop.mat)
})

#' get a list of w.mat (enhenced by v.mat) and v.mat (enhenced by w.mat). 
#' 
#' @name calculateWVMat
#' @docType methods
#' @param weighTable weight table
#' @param valueTable value table
#' @param SCTG SCTG number
#' @param origins labels
#' @return a list of weight matrix and value matrix
TradeMatrix$methods(calculateWVMat = function(weightTable, valueTable, SCTG, origins){
  # SCTG must be a single number
  comm.wMat <- toMatrix(weightTable, SCTG, origins)
  comm.wMat.ad <- comm.wMat
  comm.wMat.ad[comm.wMat.ad!=0] <- 1
  #   all.equal(comm.wMat.ad*comm.wMat, comm.wMat)
  
  comm.vMat <- toMatrix(valueTable, SCTG, origins)
  comm.vMat.ad <- comm.vMat
  comm.vMat.ad[comm.vMat.ad!=0] <- 1
  #   all.equal(comm.wMat.ad*comm.wMat, comm.wMat)
  
  vdw.ad <- comm.vMat.ad - comm.wMat.ad
  vdw.ad[vdw.ad==-1] <- 0
  wdv.ad <- comm.wMat.ad - comm.vMat.ad
  wdv.ad[wdv.ad==-1] <- 0
  
  pcomm <- comm.vMat/comm.wMat #  price of comm
  pcomm.na.no <- pcomm[is.finite(pcomm)][pcomm[is.finite(pcomm)] >  0] # exclude na, inf and 0
  calculated.weight <- comm.vMat/mean(pcomm.na.no)
  calculated.value <- comm.wMat * mean(pcomm.na.no)
  cal.wMat <- calculated.weight * vdw.ad
  cal.vMat <- calculated.value * wdv.ad
  
  comm.wMat <- comm.wMat + cal.wMat
  comm.vMat <- comm.vMat + cal.vMat
  
  result <- list(comm.wMat, comm.vMat)
  return(result)
})

#' get a list of w.mat (enhenced by v.mat) and v.mat (enhenced by w.mat), 
#' both aggregated for multiple SCTGs. 
#' 
#' @name aggregateWVMat
#' @docType methods
#' @param weighTable weight table
#' @param valueTable value table
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @return a list of weight matrix and value matrix
TradeMatrix$methods(aggregateWVMat = function(weightTable, valueTable, SCTGs_, origins) {
  d <- length(origins)
  w.ret <- matrix(0, nrow = d, ncol = d)
  v.ret <- matrix(0, nrow = d, ncol = d)
  for (i in 1:length(SCTGs_)){
    wv <- calculateWVMat(weightTable, valueTable, SCTGs_[i], origins)
    w.ret <- w.ret + wv[[1]] #  weight
    v.ret <- v.ret + wv[[2]] #  value
  }
  agg.ret <- list(w.ret, v.ret)
  return(agg.ret)
})

#' calculate virtual water content matrix
#' 
#' @name getVirtualMatrix
#' @docType methods
#' @param weighTable weight table
#' @param valueTable value table
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @param VWCTable virtual water content table. It must have 51 row, from Alabama to Wyoming. 
#'        Columns have to be corresponding to SCTGs_.  
#' @return matrix
TradeMatrix$methods(getVirtualMatrix = function(weightTable, valueTable, SCTGs_, origins, VWCTable) {
  w.tab <- weightTable
  v.tab <- valueTable
  VWCs <- VWCTable
  n.row <- dim(calculateWVMat(w.tab, v.tab, SCTGs_[1], origins)[[1]])[1]
  mat.vwc <- matrix(0, nrow = n.row, ncol = n.row)
  #   vwc.lst <- list()
  len <- length(SCTGs_)
  for (i in 1:len) {
    estimate.NA <- mean(VWCs[, i], na.rm=T)
    VWCs[, i][which(is.na(VWCs[, i]))] <- estimate.NA
    w.mat <- calculateWVMat(w.tab, v.tab, SCTGs_[i], origins)[[1]] 
    vw.mat <- matrix(0, nrow = n.row, ncol = n.row)
    # the trade states/locations entries in the vwc entries
    w.ins <- which(colnames(w.mat) %in% names(VWCs[, i])) #  all 51 in our case
    # the trade states/locations entries not in the vwc entries
    w.outs <- which(!colnames(w.mat) %in% names(VWCs[, i])) #  all 0 in our case
    # the vwc entries in the trade states/locations entries
    v.ins <- which(names(VWCs[, i]) %in% colnames(w.mat))
    vw.mat[w.ins, w.ins] <- w.mat[w.ins, w.ins] * VWCs[, i][v.ins]
    vw.mat[w.outs, w.outs] <- w.mat[w.outs, w.outs] * mean(VWCs[, i][v.ins])
    # remove NAs from the 
    #     vwc.lst[[i]] <- w.mat
    mat.vwc <- mat.vwc + vw.mat
  }
  return(mat.vwc)
})

#' constructor
#' 
#' @name new
#' @docType methods
#' @param weighTable weight table
#' @param valueTable value table
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @param VWCTable matrix
#' @return NULL
TradeMatrix$methods(initialize = function(weightTable, valueTable, SCTGs_, origins, VWCTable) {
  if (!is.matrix(VWCTable)) VWCTable <- as.matrix(VWCTable)
  wv <- aggregateWVMat(weightTable, valueTable, SCTGs_, origins)
  weightMatrix <<- wv[[1]]
  valuematrix <<- wv[[2]]
  SCTGs <<- SCTGs_
  virtualMatrix <<- getVirtualMatrix(weightTable, valueTable, SCTGs_, origins, VWCTable)
  labels <<- origins
})

#' analysis matrix
#' 
#' @name analysis
#' @docType methods
#' @param mat matrix
#' @param sorted.origins labels
#' @param result.name path to make the retults
#' @param index.name path to make index file
#' @param matrix.name path to output the matrix
#' @return NULL
TradeMatrix$methods(analysis = function(mat, sorted.origins, result.name="data\\cropResult.csv", index.name="data\\indexes.csv", matrix.name="data\\cropMatrix.csv"){
  "string doc is working"  
  crop <- mat
  
  if(!"package:igraph" %in% search()) library(igraph)
  crop.matrix <- as.matrix(crop)
  
  # c is commodity weight, u is undirected, a is unweighted
  if(!"package:igraph" %in% search()) require(igraph)
  crop.cu <- (crop.matrix + t(crop.matrix))
  crop.au <- crop.cu
  crop.au[crop.au > 0] <- 1
  d.crop.au <- colSums(crop.au)  # degree
  s.crop.cu <- colSums(crop.cu)  # strength
  g.crop.au <- graph.adjacency(crop.au, weighted=NULL, mode = "undirected", diag = FALSE)
  b.crop.au <- betweenness(graph = g.crop.au, v = V(g.crop.au), directed = FALSE, weights = NULL,
                           nobigint = TRUE, normalized = TRUE)
  
  
  k.crop.au <- graph.knn(g.crop.au)$knn
  g.crop.cu <- graph.adjacency(crop.cu, weighted=TRUE, mode = "undirected", diag = FALSE)
  k.crop.cu <- graph.knn(g.crop.cu)$knn
  
  
  
  t.crop.au <- transitivity(g.crop.au, type = c("local"), vids = V(g.crop.au),
                            weights = NULL)
  t.crop.cu <- transitivity(g.crop.cu, type = c("weighted"), vids = V(g.crop.cu),
                            weights = NULL)
  
  
  # ad: unweighted, directed
  crop.ad <- crop.matrix
  crop.ad[crop.ad > 0] <- 1
  d.out.crop <- rowSums(crop.ad)
  d.in.crop <- colSums(crop.ad)
  s.out.crop <- rowSums(crop.matrix)
  s.in.crop <- colSums(crop.matrix)
  
  
  # wd: weighted directed
  # nearest neighbor
  knn.ii <- AveKnn(crop.matrix, d.in.crop, d.in.crop)
  knn.oo <- AveKnn(crop.matrix, d.out.crop, d.out.crop, mode="out")
  knn.io <- AveKnn(crop.matrix, d.in.crop, d.out.crop)
  knn.oi <- AveKnn(crop.matrix, d.out.crop, d.in.crop, mode="out")
  
  knn.w.ii <- AveKnn(crop.matrix, s.in.crop, d.in.crop, weighted=TRUE)
  knn.w.oo <- AveKnn(crop.matrix, s.out.crop, d.out.crop, weighted=TRUE, mode="out")
  knn.w.io <- AveKnn(crop.matrix, s.in.crop, d.out.crop, weighted=TRUE)
  knn.w.oi <- AveKnn(crop.matrix, s.out.crop, d.in.crop, weighted=TRUE, mode="out")
  # clustering coef
  
  cout.a <- ClusteringCoefficiencyUnw(crop.matrix, d.out.crop, mode="out")
  cin.a <- ClusteringCoefficiencyUnw(crop.matrix, d.in.crop, mode="in")
  cout.w <- ClusteringCoefficiencyW(crop.matrix, d.out.crop, mode="out", s.out.crop)
  cin.w <- ClusteringCoefficiencyW(crop.matrix, d.in.crop, mode="in", s.in.crop)
  
  ccyc.a <- ClusteringCoefficiencyUnwCyc(crop.matrix, d.in.crop, d.out.crop, mode="cyc")
  cmid.a <- ClusteringCoefficiencyUnwCyc(crop.matrix, d.in.crop, d.out.crop, mode="mid")
  ccyc.w <- ClusteringCoefficiencyWCyc(crop.matrix, d.in.crop, d.out.crop, mode="cyc", stot=s.in.crop+s.out.crop)
  cmid.w <- ClusteringCoefficiencyWCyc(crop.matrix, d.in.crop, d.out.crop, mode="mid", stot=s.in.crop+s.out.crop)
  
  g.crop.ad <- graph.adjacency(crop.ad, weighted=NULL, mode = "directed", 
                               diag = FALSE)
  # node betweenness sum( g_ivj / g_ij, i!=j,i!=v,j!=v); normalization
  
  b.crop.ad <- betweenness(graph = g.crop.ad, v = V(g.crop.ad), directed = TRUE, 
                           weights = NULL, normalized = TRUE)  # Bnorm=2*B/(n*n-3*n+2)
  # output
  nearest.neighbor.crop <- cbind(knn.ii, knn.oo, knn.io, knn.oi, knn.w.ii, 
                                 knn.w.oo, knn.w.io, knn.w.oi);
  clustering.coef <- cbind(cout.a, cin.a, cout.w, cin.w,
                           ccyc.a, cmid.a, ccyc.w, cmid.w)
  betweenness.centrality <- cbind(b.crop.au, b.crop.ad)
  
  
  write.table(cbind(d.in.crop, d.out.crop, s.in.crop, s.out.crop, 
                    nearest.neighbor.crop, clustering.coef, betweenness.centrality), file = result.name, 
              append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = "escape")
  write.table(sorted.origins, file = index.name, 
              append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = "escape")
  rownames(crop.matrix) = as.character(sorted.origins)
  colnames(crop.matrix) = as.character(sorted.origins)
  write.csv(crop.matrix, file = matrix.name)
})

#' generate circos input file
#' 
#' @name generateCircos
#' @docType methods
#' @param mat matrix
#' @param sorted.origins labels
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @param VWCTable matrix
#' @return NULL
TradeMatrix$methods(generateCircos = function(state.matrix, filename = "data/circosRainbow.txt", short.state.names=labels, scale = 0.1^4) {
  require(reshape2)
  state.matrix <- round(state.matrix * scale)
  
  state.tab.crop <- melt(state.matrix)
  crop.order <- order(state.tab.crop[, 3], decreasing = TRUE)
  sorted.state.tab.crop <- state.tab.crop[crop.order, ]
  # write.csv(sorted.state.tab.crop, 
  #           file = "newmaps\\stateTab.csv", 
  #           row.names = FALSE)
  state.strength <- colSums(state.matrix) + rowSums(state.matrix)
  o <- order(state.strength,decreasing = TRUE)
  # FindTopN(state.tab, 4, 3, short.state.names)
  c1 <- cbind(short.state.names[o], state.matrix[o, o])
  # View(c1)
  c2 <- rbind(c("data", short.state.names[o]), c1)
  # View(c2)
  # colorful <- rainbow(51)
  colorful <- t(col2rgb(rainbow(51)))
  # colorful <- cbind(colorful, seq(from=200, to=100, by=-2))
  nRgb <- vector()
  for (i in 1:length(short.state.names)){
    nRgb[i] <- paste(colorful[i,], collapse = ',')
  }
  nRgb[1:3]
  c3 <- cbind(c("data", nRgb), c2)
  c4 <- rbind(c("data","data",nRgb), c3)
  #   View(c4)
  write.table(c4, 
              file = filename, 
              row.names = FALSE, col.names = FALSE)
})

# correlation
# if(!\"package:Hmisc\" %in% search()) require(Hmisc)
# pearson <- rcorr(weight.matrix, type=\"pearson\")
# pearson.v <- rcorr(value.matrix, type=\"pearson\")
#' get network pearson coefficient
#' 
#' @name getNetworkPearson
#' @docType methods
#' @param mat matrix
#' @param sorted.origins labels
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @param VWCTable matrix
TradeMatrix$methods(getNetworkPearson = function(weight.matrix = mat.vwc) {
  if(!"package:reshape2" %in% search()) require(reshape2)
  weight.table <- melt(weight.matrix)
  links <- weight.table[weight.table[, 3]>0, ]
  link.length <- length(links[,1])
  # in degree of origin
  in.degree <- colSums(weight.matrix>0)
  out.degree <- rowSums(weight.matrix>0)
  in.out.pair <- matrix(nrow=link.length, ncol=2)
  for(i in 1:link.length){
    in.out.pair[i, 1] <- in.degree[links[i, 1]]
    in.out.pair[i, 2] <- out.degree[links[i, 2]]
  }
  pearson <- cor(in.out.pair[,1], in.out.pair[, 2], method="pearson")
  return(pearson)
})

#' getNetworkLorenz
#' 
#' @name getNetworkLorenz
#' @docType methods
#' @param mat matrix
#' @param sorted.origins labels
#' @param SCTGs_ SCTG numbers
#' @param origins labels
#' @param VWCTable matrix
TradeMatrix$methods(getNetworkLorenz = function(weight.matrix = mat.vwc, filename="VWCGini.pdf") {
  # lorenz curve and gini index
  pearson <- getNetworkPearson(weight.matrix)
  in.strength.weight <- colSums(weight.matrix)
  if(!"package:ineq" %in% search()) require(ineq)
  state.external.gini <- ineq(in.strength.weight, type="Gini")
  lorenz.curve.asym <- Lasym(in.strength.weight)
  hoover <- max(Lc(in.strength.weight)$p - Lc(in.strength.weight)$L)
  pdf(filename)
  plot(Lc(in.strength.weight),col="darkred",lwd=2, 
       xlab="culmulative proportion of population", 
       ylab="culmulative proportion of external water used",
       main="Lorenz curve for substate external water weight footprint")
  text(0.3, 0.8, paste("Lorenz curve asymmetry", round(lorenz.curve.asym, 4)),
       cex = .8)
  text(0.18, 0.9, paste("Gini", round(state.external.gini, 4)),
       cex = .8)
  text(0.27, 0.7, paste("Pearson correlation", round(pearson, 4)),
       cex = .8)
  text(0.2, 0.6, paste("Hoover", round(hoover, 4)),
       cex = .8)
  dev.off()
})

#' make heat meaps valued on values provided for each state
#' 
#' @name heatMap
#' @docType methods
#' @param values a vector indicate the value in each state
#' @param origins labels for the map
#' @param color choose among g, b, y, v. o
#' @param filename a path to output pdf
#' @return NULL
TradeMatrix$methods(heatMap = function(values, origins, color, filename) {
  if(!"package:grDevices" %in% search()) require(grDevices)
  mi <- min(values, na.rm=T)
  ma <- max(values, na.rm=T)
  fac <- (values - mi) / (ma - mi)
  
  # get color for all states
  if(!"package:maps" %in% search()) require(maps)
  state.to.map <- match.map("state", origins)
  mycol <- vector("numeric", length=length(fac))
  mycol[is.na(fac)] <- "#545454FF"
  if (color == "r") {
    mycol[!is.na(fac)] <- rgb(1, 0, 0, fac[!is.na(fac)])
  }else if (color == "g") {
    mycol[!is.na(fac)] <- rgb(0, 1, 0, fac[!is.na(fac)])
  }else if (color == "b") {
    mycol[!is.na(fac)] <- rgb(0, 0, 1, fac[!is.na(fac)])
  }else if (color == "y") {
    mycol[!is.na(fac)] <- rgb(1, 1, 0, fac[!is.na(fac)])
  }else if (color == "v") {
    mycol[!is.na(fac)] <- rgb(1, 0, 1, fac[!is.na(fac)])
  }else if (color == "o") {
    mycol[!is.na(fac)] <- rgb(0, 1, 1, fac[!is.na(fac)])
  }else{
    simpleError("wrong color option! Pick among r, g, b, y!")
  }
  pdf(filename)
  layout(matrix(c(1,1,1,2,3,4), nrow=3, ncol=2), widths=c(8, 1), heights=c(1,3,1))
  par(mar=c(1,1,1,1))
  map("state", col = mycol[state.to.map], border = "grey", fill=TRUE, add = FALSE)
  par(mar=c(1,1,1,1))
  plot(1,1, type="n", xaxt="n", yaxt="n",bty='n')
  par(mar=c(1,1,1,3))
  myfil <- sort(unique(mycol[!is.na(fac)]))
  image.scale(values, col=myfil, horiz=F)
  dev.off()
})

AveKnn <- function(matrix, k1, k2, weighted=FALSE, mode="in"){
  # args: matrix, crosstable matrix
  # args: k1, k2, can be node degree of importers or outporters. 
  #       k1 can be the strength of the node
  wd <- matrix
  ad <- wd
  ad[ad > 0] <- 1
  
  if(weighted==FALSE){
    wd <- ad
  }
  
  knn <- vector()
  
  ind <- 1:dim(matrix)[1]
  
  for(i in ind){
    V <- vector()
    #  get the Vin/Vout
    if(mode=="in"){
      V <- ind[ad[,i] > 0]
    }else{ 
      V <- ind[ad[i,] > 0]
      if(i==1) wd <- t(wd)
    }
    
    knn[i] <- 0
    
    for(j in V){
      knn[i] <- knn[i] + 1 / k1[i] * wd[j, i] * k2[j]
    }
    
    
  }
  knn[knn==Inf] <- NA
  return(knn)
}

ClusteringCoefficiencyUnw <- function(matrix, k, mode="out"){
  ad <- matrix
  au <- ad
  au <- (au + t(au)) / 2
  au[au > 0] <- 1
  
  ad [ad > 0] <- 1
  
  vert <- list()
  ind <- 1:dim(matrix)[1]
  c <- vector()
  for(i in ind){
    V <- vector()
    if(mode=="out"){ 
      V <- ind[ad[i,] > 0]  
    }else{     
      V <- ind[ad[,i] > 0]
    }
    
    c[i] <- 0
    for(j in V){
      for(h in V){
        if(mode=="in"){
          c[i] <- c[i] + ((ad[j,i] + ad[h,i]) * au[j, h])/2/k[i]/(k[i]-1)
        }else{
          c[i] <- c[i] + ((ad[i,j] + ad[i,h]) * au[j, h])/2/k[i]/(k[i]-1)
        }        
      }
    }
    
  }
  
  return(c)
  
}


ClusteringCoefficiencyW <- function(matrix, k, mode="out", strength){
  wd <- matrix
  au <- (wd + t(wd)) / 2
  au[au>0] <- 1
  
  vert <- list()
  ind <- 1:dim(wd)[1]
  c <- vector()
  for(i in ind){
    V <- vector()
    if(mode=="out"){ 
      V <- ind[wd[i,] > 0]  
      c[i] <- 0
      for(j in V){
        for(h in V){
          c[i] <- c[i] + ((wd[i,j] + wd[i,h]) * au[j, h])/2/strength[i]/(k[i]-1)
        }
      }
    }else{     
      V <- ind[wd[,i] > 0]
      c[i] <- 0
      for(j in V){
        for(h in V){
          c[i] <- c[i] + ((wd[j,i] + wd[h,i]) * au[j, h])/2/strength[i]/(k[i]-1)
        }
      }
    }
    
    
  }
  return(c)
  
}


ClusteringCoefficiencyUnwCyc <- function(matrix, kin, kout, mode="cyc"){
  ad <- matrix
  ad[ad > 0] <- 1
  vert <- list()
  ind <- 1:dim(matrix)[1]
  c <- vector()
  if(mode=="cyc"){
    for(i in 1:dim(matrix)[1]){
      Vout <- vector()
      Vin <- vector()
      Vout <- ind[ad[i,] > 0]
      Vin <- ind[ad[,i] > 0]
      ktot <- kin + kout
      vert[i] <- list(V)
      c[i] <- 0
      for(j in Vout){
        for(h in Vin){
          c[i] <- c[i] + ((ad[i,j] + ad[h,i]) * ad[j, h])/ktot[i]/(ktot[i]-1)
        }
      }
      
    }
  }else{
    # mode is mid
    for(i in 1:dim(matrix)[1]){
      Vout <- vector()
      Vin <- vector()
      Vout <- ind[ad[i,] > 0]
      Vin <- ind[ad[,i] > 0]
      ktot <- kin + kout
      vert[i] <- list(V)
      c[i] <- 0
      for(j in Vin){
        for(h in Vout){
          c[i] <- c[i] + ((ad[i,h] + ad[j,i]) * ad[j, h])/ktot[i]/(ktot[i]-1)
        }
      }
      
    }
  }
  return(c)
  
}

ClusteringCoefficiencyWCyc <- function(matrix, kin, kout, mode="cyc", stot){
  ad <- matrix
  ad[ad > 0] <- 1
  vert <- list()
  ind <- 1:dim(matrix)[1]
  c <- vector()
  if(mode=="cyc"){
    for(i in 1:dim(matrix)[1]){
      Vout <- vector()
      Vin <- vector()
      Vout <- ind[ad[i,] > 0]
      Vin <- ind[ad[,i] > 0]
      ktot <- kin + kout
      vert[i] <- list(V)
      c[i] <- 0
      for(j in Vout){
        for(h in Vin){
          c[i] <- c[i] + ((matrix[i,j] + matrix[h,i]) * ad[j, h])/stot[i]/(ktot[i]-1)
        }
      }
      
    }
  }else{
    # mode is mid
    for(i in 1:dim(matrix)[1]){
      Vout <- vector()
      Vin <- vector()
      Vout <- ind[ad[i,] > 0]
      Vin <- ind[ad[,i] > 0]
      ktot <- kin + kout
      vert[i] <- list(V)
      c[i] <- 0
      for(j in Vin){
        for(h in Vout){
          c[i] <- c[i] + ((matrix[i,h] + matrix[j,i]) * ad[j, h])/stot[i]/(ktot[i]-1)
        }
      }
      
    }
  }
  return(c)
  
}

image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}