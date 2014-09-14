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
remove_headers_footers = function(data) {
  data_n = data[-1,-1]
  rownames(data_n) = data[-1,1]
  colnames(data_n) = data[1,-1] 
  return(data_n)
}

#Given an excel sheet, find the plate plan and the data format
load_plate_plan = function (xlsx_sheet, sep = ":"){
  cols = readColumns(xlsx_sheet, 1,1,1, header = FALSE)
  
  #Read format string, will be colnames of the plan
  form_row = which(cols == "FORMAT")
  formatstr= readRows(xlsx_sheet, form_row, form_row, 2,2)
  
  #Read and parse the plan
  plan_start = which(cols == "PLAN")
  plan_end = which(cols == "ENDPLAN")
  plan_d = readRows(xlsx_sheet, plan_start, plan_end - 1, 1)
  plan = plate2list(remove_headers_footers(plan_d))
  
  #Expand plan to create new variables and give them correct colnames
  planvals = Reduce(rbind, strsplit(as.character(plan$data), sep))
  colnames(planvals) = strsplit(formatstr, sep)[[1]]
  newplan = data.frame(pos = plan$pos, planvals, stringsAsFactors = FALSE)
}

load_tecan_data = function(xlsx_sheet) {
  cols = readColumns(xlsx_sheet, 1,1,1, header = FALSE)
  
  plate_start = which(cols == "<>")
  plate_end = sapply(ps2, function(x) plate_end[which(plate_end > x)][1])
  
}