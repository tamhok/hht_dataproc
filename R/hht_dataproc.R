library(hhtutils)

#' Plate to List Converter
#' 
#' Turns a data structure that is in "plate format" to a data frame with row, column, etc.
#' @param data Plate data formet
#' @return a list consisting of a column positions e.g "A1" and data
plate2list=function(data) {
  newdata=matrix(as.matrix(data), prod(dim(data)),1)
  pos = outer(rownames(data), colnames(data), FUN=function(x,y) paste(x, y, sep = ""))
  pos = matrix(as.matrix(pos), prod(dim(pos)),1)
  newdata = data.frame(pos, newdata, stringsAsFactors = FALSE)
  newdata = newdata[which(newdata[,2] != ""),]
  colnames(newdata)=c("pos", "data")
  return(newdata)
}

#' Plan loader
#'
#' Given an excel sheet, find the plate plan and the data format
#' Finds strings in col 1 with "FORMAT" and "PLAN", interprets 
#' the plan string in each cell and converts it to list format
#' @param xlsx.sheet excel sheet object called from xlsx package
#' @param sep separator for plan format objects, default :
#' @return a matrix with columns pos, containing the posiion, and columns named in the format string with separated out plan elements
#' @examples Formats: "CELLS:SAMPLE:CONC", Cells: "HeLa:ctx3:1uM"
load.plate.plan <- function (xlsx.sheet, sep = ":"){
  cols = xlsx::readColumns(xlsx.sheet, 1,1,1, header = FALSE)
  
  #Read format string, will be colnames of the plan
  form.row = which(cols == "FORMAT")
  formatstr= xlsx::readRows(xlsx.sheet, form.row, form.row, 2,2)
  
  #Read and parse the plan
  plan.start = which(cols == "PLAN")
  plan.end = which(cols == "ENDPLAN")
  plan.d = xlsx::readRows(xlsx.sheet, plan.start, plan.end - 1, 1)
  plan = plate2list(remove.headers.footers(plan.d))
  
  #Expand plan to create new variables and give them correct colnames
  planvals = Reduce(rbind, strsplit(as.character(plan$data), sep))
  colnames(planvals) = strsplit(formatstr, sep)[[1]]
  newplan = data.frame(pos = plan$pos, planvals, stringsAsFactors = FALSE)
}

#' Plate format checker
#'
#' Used for tecan data, this checks whether or not the format is in 96
#' well plate format or actually already in list format.
#' @param plan plate data
#' @return whether there are numbers in the first col (yes list, no plate)
isPlate  <- function(plan) {
  plan.dim  <- dim(plan)
  if(length(grep("[0-9]", plan[plan.dim[1],1])) == 0) {
	 return(TRUE)
  } else {
	 return(FALSE)
  } 
}

#' Aggregate replicates
#'
#' @param data matrix with positions, data, and the plan
agg.data <- function(plan, data) {
	by.list = lapply(2:ncol(plan), function(x) plan[,x])
	adata = aggregate(data[,-1], by.list, function(x) c(mean(x), sd(x)))
  colnames(adata) = c(colnames(plan)[-1], "data")
  return(adata)
}

#' Divide by probes
#' 
#' Function to process data by normalizing genes by control probes. Used for bDNA or qPCR studies.
#' @param adata aggregated data
#' @param smpl name of sample probe
#' @param ctrl name of control probe
#' @param field name of probe field
div.probes <- function(adata, smpl = "AHA1", ctrl = "ACTB", field = "PROBE") {
  samples= adata[which(adata[,field]==smpl),-which(colnames(adata)==field)]
  controls = adata[which(adata[,field]==ctrl),-which(colnames(adata)==field)]
  merge_fields = colnames(adata)[1:(ncol(adata)-1)]
  merge_fields = merge_fields[-which(merge_fields==field)]
  combd = merge(samples, controls, by = merge_fields)
  return(cbind(combd[,merge_fields], combd$data.x / combd$data.y))
}

#' Normalize data
#' 
#' Normalize data by the positive or negative controls.
#' @param aggdata data passed through the aggregate function
#' @param pos.control Text with which to look for negative controls
#' @param neg.control Text with which to look for positive controls
#' @param weights value with which to weight differences in the different fields. Default is 2^(1:number of fields)
#' @return normalized data
norm.data <- function(data, pos.control = NA, neg.control = NA, weights = 2^(1:(ncol(data)-2))) {
  fields = data[,1:(ncol(data)-2)]
  combf = apply(fields,1, function(x) paste0(x, collapse = ":"))
    
  #Finds the control which is closest
  find.closest = function(match, controls) {
    return(which.max(apply(controls, 1, function(x) sum((match == x)*weights))))
  }
  
  #Returns the amount to scale
  make.scale = function(scale.str) {
    scale.id = grep(scale.str, combf)
    scale = fields[scale.id,]
    scale.data = data[scale.id,ncol(data)-1]
    return(scale.data[apply(fields, 1, function(x) find.closest(x, scale))])
  }
  
  if(!is.na(neg.control)) {
    neg.vals = make.scale(neg.control)
  } else {
    neg.vals = rep(0, nrow(data))
  }
  if(!is.na(pos.control)) {
    pos.vals = make.scale(pos.control)
  } else {
    pos.vals = neg.vals + 1
  }
  
  means = (data[,ncol(data)-1] - neg.vals) / (pos.vals -neg.vals)
  sds = data[,ncol(data)] / (pos.vals - neg.vals)
  return(cbind(fields, means, sds))
}

#' 

#' Tecan data loader
#' 
#' Loads tecan data from an excel sheet. Returns 0 if no tecan data found
#' Otherwise, returns lists of the matrices in the form of lists with the 
#' field plate or plate and aux, if it is a timeseries data set. 
#' @param xlsx.sheet Excel sheet with data
#' @return a list of lists, each element containing either a plate matrix or a plate matrix as well as auxiliary information if it is timeseries data. Usually includes things like time and temperature.
load.tecan.data <- function(xlsx.sheet) {
  #Get the first column
  cols = xlsx::readColumns(xlsx.sheet, 1,1,1, header = FALSE)
  
  #Check for markers of the first corner of a data table. If none, return 0
  plate.start = which(cols == "<>" | cols == "Cycle Nr.")
  if(length(plate.start) == 0) {
	  return(0)
  }
  #Check for end of plate.
  plate.end = c(which(cols == ""), length(cols))
  plate.end = sapply(plate.start, function(x) plate.end[which(plate.end > x)][1])
  
  pindices = rbind(plate.start, plate.end) 

  plate.interpret  <- function(p.index) {
     plate.d  <- xlsx::readRows(xlsx.sheet, p.index[1], p.index[2] - 1, 1)
     if(isPlate(plate.d)) {
	  plate = df.nf(plate2list(remove.headers.footers(plate.d))) 
     	  return(list(plate = convert.df(plate, s=2)))
     }	else {
	  datarows = grep("[A-Z][0-9]+", plate.d[,1]) 
	  plate = df.nf(plate.d[datarows,])
	  rownames(plate) = plate.d[datarows, 1]
	  colnames(plate) = c("pos", plate.d[1, -1])
	  aux.data = df.nf(plate.d[c(-1,-datarows),])
	  rownames(aux.data) = plate.d[c(-1, -datarows),1]
	  colnames(aux.data) = c("aux", plate.d[1, -1])
	  return(list(plate = convert.df(plate, s=2), 
		      aux = convert.df(aux.data, s=2)))
     }     
  }
  return(apply(pindices, 2, plate.interpret))
}


bdna.test <- function() {
	wb <- xlsx::loadWorkbook("../samples/tecan_bdna.xlsx")
	shts <- xlsx::getSheets(wb)
  
  ad = Reduce(rbind, lapply(shts, function(x) load.tecan.data(x)[[1]]$plate))
  ap = Reduce(rbind, lapply(shts, load.plate.plan))
	td= load.tecan.data(shts[[1]])
	tp = load.plate.plan(shts[[1]])
	at = agg.data(tp, td[[1]]$plate)
  nt = div.probes(at)
}

#' Kind of testing function
#' Don't use it.
dp.test  <- function() {
	wb <- xlsx::loadWorkbook("../samples/tecan_kin_test.xlsx")
	shts  <- xlsx::getSheets(wb)
	return(load.tecan.data(shts[[9]]))
}
