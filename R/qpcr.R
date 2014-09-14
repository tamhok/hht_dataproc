
#Read LightCycler text files
#Returns list of the form 
lightcycler_read = function(filename) {
  data = read.table(filename, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
  tags = strsplit(data[1,1], " ")[[1]]
  pdata = data.frame(pos = data[c(-1:-2), 3], cp = as.numeric(data[c(-1:-2),5]), stringsAsFactors = FALSE)  
  colnames(pdata)[2] = tags[6]
  list(attrib = list(expname = tags[2], filter = tags[6]), data = pdata)
}

#processes lightcycler data
lightcycler_proc = function(l1, l2, plan) {
  cdata = merge(merge(l1$data, l2$data, by="pos"), plan, by ="pos")
  cdata$sub  = cdata$FAM - cdata$HEX
  return(tdata = aggregate(cdata$sub, by = list(cdata$SAMPLE, cdata$CELLS, cdata$PROBEMGB), 
        FUN = function(x) c(mn = mean(x), std=sd(x)))
}