library('xlsx')

#' Turns a data structure that is in "plate format" to a data frame with row, column, etc.
plate2list=function(data) {
  newdata=matrix(as.matrix(data), prod(dim(data)),1)
  pos = outer(rownames(data), colnames(data), FUN=function(x,y) paste(x, y, sep = ""))
  pos = matrix(as.matrix(pos), prod(dim(pos)),1)
  newdata = data.frame(pos, newdata, stringsAsFactors = FALSE)
  newdata = newdata[which(newdata[,2] != ""),]
  colnames(newdata)=c("pos", "data")
  return(newdata)
}

#Trim row and column names out of data
remove.headers.footers <- function(data) {
  data.n = data[-1,-1]
  rownames(data.n) = data[-1,1]
  colnames(data.n) = data[1,-1] 
  return(data.n)
}

#Given an excel sheet, find the plate plan and the data format
load.plate.plan <- function (xlsx.sheet, sep = ":"){
  cols = readColumns(xlsx.sheet, 1,1,1, header = FALSE)
  
  #Read format string, will be colnames of the plan
  form.row = which(cols == "FORMAT")
  formatstr= readRows(xlsx.sheet, form.row, form.row, 2,2)
  
  #Read and parse the plan
  plan.start = which(cols == "PLAN")
  plan.end = which(cols == "ENDPLAN")
  plan.d = readRows(xlsx.sheet, plan.start, plan.end - 1, 1)
  plan = plate2list(remove.headers.footers(plan.d))
  
  #Expand plan to create new variables and give them correct colnames
  planvals = Reduce(rbind, strsplit(as.character(plan$data), sep))
  colnames(planvals) = strsplit(formatstr, sep)[[1]]
  newplan = data.frame(pos = plan$pos, planvals, stringsAsFactors = FALSE)
}

#Checks and sees if it is time series (kinetic) data or normal plate data
isPlate  <- function(plan) {
  plan.dim  <- dim(plan)
  if(length(grep("[0-9]", plan[plan.dim[2],1])) > 0) {
	 return TRUE
  } else {
	 return FALSE
  } 
}

load.tecan.data <- function(xlsx.sheet) {
  cols = readColumns(xlsx.sheet, 1,1,1, header = FALSE)
  
  plate.start = which(cols == "<>")
  plate.end = sapply(ps2, function(x) plate.end[which(plate.end > x)][1])
  
  pindices = rbind(plate.start, plate.end) 

  plate.interpret  <- function(p.index) {
     plate.d  <- readRows(xlsx.sheet, p.index[1], p.index[2] - 1, 1)
     if(isPlate(plate.d)) {
	  plate = plate2list(remove.headers.footers(plate.d))
     }	else {
	  datarows = grep("[A-Z][0-9]+", plate.d[,1]) 
	  plate = plate.d[datarows, -1]
	  rownames(plate) = plate.d[datarows, 1]
	  colnames(plate) = plate.d[1, -1]
	  aux.data = plate.d[c(-1,-datarows), -1]
	  rownames(aux.data) = plate.d[c(-1, -datarows),1]
	  colnames(plate) = plate.d[1, -1]
     }     
  }
  return(apply(pindices, 2, plate.interpret))
}
