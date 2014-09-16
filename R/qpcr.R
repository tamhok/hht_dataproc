# For processing LightCycler files

#' LightCycler text reader
#'
#' Reads text from exported lightcycler data files
#' @param filename data file name
#' @return list with a list of attributes (experimentname and filer) and CpValues with position information
lightcycler_read = function(filename) {
  data = read.table(filename, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
  tags = strsplit(data[1,1], " ")[[1]]
  pdata = data.frame(pos = data[c(-1:-2), 3], cp = as.numeric(data[c(-1:-2),5]), stringsAsFactors = FALSE)  
  colnames(pdata)[2] = tags[6]
  list(attrib = list(expname = tags[2], filter = tags[6]), data = pdata)
}

#' Processes lightcycler data
#' 
#' Takes two lightcycler data sets and a plan and returns the delta Cp values
#' @param l1 first lightcycler file (either color)
#' @param l2 second lightcycler file (other color)
#' @param plan contains the plan, (format loaded from load.plan)
#' @return data combined along with plan and means and stds for replicates
lightcycler_proc = function(l1, l2, plan) {
  cdata = merge(merge(l1$data, l2$data, by="pos"), plan, by ="pos")
  cdata$sub  = cdata$FAM - cdata$HEX
  return(tdata = aggregate(cdata$sub, by = list(cdata$SAMPLE, cdata$CELLS, cdata$PROBEMGB), 
        FUN = function(x) c(mn = mean(x), std=sd(x))))
}
