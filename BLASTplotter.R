setwd("~/Downloads")
library(ggplot2)
#In group = C. hydrogenoformns and C.ljungdahlii
#Out group = C. acetobutylicum
#I'm trying to plot each gene in terms of the highest score
#(last column, column M)
#with the in group score on the x axis and the 
#out group score on the y axis. 
#Since the in group has two organisms,
#choose the highest score for the group. H 
#"Moorella vs. Carboxydothermus 
#"Moorella vs. Clostridium ljungdahlii "
#"Moorella vs. clostridium acetobutylicum"
#first column => genes of morella
#second column => genes of organism X
#last column => score
ingroup1 = read.csv("ljungdahlii.csv", header = FALSE, stringsAsFactors = FALSE)
ingroup2 = read.csv("carboxy.csv", header = FALSE, stringsAsFactors = FALSE)
outgroup = read.csv("aceto.csv", header = FALSE, stringsAsFactors = FALSE)
c(length(unique(paste(outgroup$V1, outgroup$V2, sep = ""))), nrow(outgroup)) 
c(length(unique(paste(ingroup1$V1, ingroup1$V2, sep = ""))), nrow(ingroup1))
c(length(unique(paste(ingroup2$V1, ingroup2$V2, sep = ""))), nrow(ingroup2))
hist(ingroup2$V13)
hist(ingroup1$V13)
hist(outgroup$V13)
length(unique(c(ingroup1[,1], ingroup2[,1], outgroup[,1])))
i1 = ingroup1
i2 = ingroup2
og = outgroup
i1$orig = 1:nrow(i1)
i2$orig = 1:nrow(i2)
og$orig = 1:nrow(og)
#i1$status = TRUE
#i2$status = TRUE
#og$status = TRUE
#uniqlo = function(versus){
#  bagra = gsub("\\|","",paste(versus[,1], versus[,2], sep = ""))
#  for(i in 1:length(bagra)){
#    if(bagra[i] %in% bagra[-i]){
#      catch = paste("^",bagra[i],"$", sep = "")
#      targets = grep(catch, bagra)
#      locks = versus$orig[targets]
#      blaze = (which(versus[targets,13]==max(versus[targets,13])))[1] #picks the first max if there are multiple maximums
#      stay = locks[blaze] 
#      versus$status[targets] = FALSE 
#      versus$status[stay] = TRUE
#   }
#  }
#  return(versus)
#}
#ui1 = uniqlo(i1)
#ui2 = uniqlo(i2)
#uog = uniqlo(og)
rectify = function(genes){
  genes = gsub("\\|","", genes)
  return(genes)
}
ci1 = i1[,c(1, 2, 13, 14)]
ci2 = i2[,c(1, 2, 13, 14)]
cog = og[,c(1, 2, 13, 14)]
ci1[,1] = rectify(ci1[,1])
ci2[,1] = rectify(ci2[,1])
cog[,1] = rectify(cog[,1])
xfactor = unique(c(cog[,1], ci1[,1], ci2[,1]))
xfactor = rectify(xfactor)
xfactor = paste("^", xfactor, "$", sep = "")
oaxis = numeric()
iaxis = numeric()
oindex = numeric()
iindex = numeric()
types = numeric()
for(i in 1:length(xfactor)){
  orange = cog[grep(xfactor[i], cog[,1]),]
  blaze = which(orange[,3]==max(orange[,3]))[1]
  blazer = orange$orig[blaze]
  oaxis = c(oaxis, cog[blazer,3])
  oindex = c(oindex, cog[blazer, 4])
  iciness = ci1[grep(xfactor[i], ci1[,1]),]
  #feisty = ci2[grep(xfactor[i], ci2[,1]),]
  raze = which(iciness[,3]==max(iciness[,3]))[1]
  razer = iciness$orig[raze]
  iaxis = c(iaxis, ci1[razer, 3])
  iindex = c(iindex, ci1[razer, 4])
  #daze = which(feisty[,3]==max(feisty[,3]))[1]
  #dazer = feisty$orig[daze]
  #if(ci1[razer, 3]>=ci2[dazer, 3]){
  #  iaxis = c(iaxis, ci1[razer, 3])
  #  types = c(types, 1)
  #  iindex = c(iindex, ci1[razer, 4])
  #}
  #if(ci1[razer, 3]<ci2[dazer, 3]){
    #iaxis = c(iaxis, ci2[dazer, 3])
    #types = c(types, 2)
    #iindex = c(iindex, ci2[dazer, 4])
  #}
}
finished = data.frame(oaxis, iaxis, oindex, iindex, types)
ggplot(data = finished, aes(x = iaxis)) + 
  geom_point(aes(y = oaxis))

oaxis = numeric()
oindex = numeric()
for(i in 1:length(xfactor)){
  orange = cog[grep(xfactor[i], cog[,1]),]
  blaze = which(orange[,3]==max(orange[,3]))[1]
  blazer = orange$orig[blaze]
  oaxis = c(oaxis, cog[blazer,3])
  oindex = c(oindex, cog[blazer, 4])
}
iaxis = numeric()
iindex = numeric()
for(i in 1:length(xfactor)){
  iciness = ci1[grep(xfactor[i], ci1[,1]),] #ljungdahlii
  raze = which(iciness[,3]==max(iciness[,3]))[1]
  razer = iciness$orig[raze]
  iaxis = c(iaxis, ci1[razer, 3])
  iindex = c(iindex, ci1[razer, 4])
}

eaxis = numeric()
eindex = numeric()
for(i in 1:length(xfactor)){
  feisty = ci2[grep(xfactor[i], ci2[,1]),] #carboxy
  daze = which(feisty[,3]==max(feisty[,3]))[1]
  dazer = feisty$orig[daze]
  eaxis = c(eaxis, ci2[dazer, 3])
  eindex = c(eindex, ci2[dazer, 4])
}
woahthere = data.frame(oaxis, iaxis, eaxis, oindex, iindex, eindex)


ggplot(data = woahthere, aes(y = oaxis)) + 
  geom_point(aes(x = eaxis), color = "red") +
  geom_point(aes(x = iaxis), color = "blue") +
  ggtitle("Red corresponds to carboxy, blue corresponds to ljungdahlii") +
  ylab("outgroup score") + 
  xlab("ingroup score")

