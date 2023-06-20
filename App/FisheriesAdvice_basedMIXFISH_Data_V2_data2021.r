
rm(list=ls())
load("/mnt/merdouille/F/D/Expertise/WGMIXFISH/2019/git/2019_NorthSea_MixedFisheriesAdvice/data/NS_Fleet database 2019-10-28/01a_ExportAllCatches_to_2018_beforeAnyWorkinUp_NS_Fleet database 2019-10-28.Rdata")# load('F:\\D\\Expertise\\WGNSSK\\2016\\FisheriesAdvice\\01_catch_effort_data_NS Fleet database_CPUEstocks_v2.Rdata')
# outputWd <- 'F:\\D\\Expertise\\WGNSSK\\2016\\FisheriesAdvice\\'

library(fields)
library(data.table)
library(gplots)
library(pander);library(captioner)
library(lattice)



listSpp <- unique(Ca$Species)[order(unique(Ca$Species))]
listSpp <- listSpp[-which(listSpp%in% c("OTH","OTHER"))]


resLan = array(NA,dim = c(length(listSpp),
                          length(listSpp),
                          length(c("4","3AN", "7D" )),
                          length(c(2009:2021)),
                          length(c(seq(5,50, by=10)))))

dimnames(resLan) <- list(listSpp, listSpp,c("4","3AN", "7D" ),c(2009:2021),c(seq(5,50, by=10)))

resVal = array(NA,dim = c(length(listSpp),
                          length(listSpp),
                          length(c("4","3AN", "7D" )),
                          length(c(2009:2021)),
                          length(c(seq(5,50, by=10)))))
dimnames(resVal) <- list(listSpp, listSpp,c("4","3AN", "7D" ),c(2009:2021),c(seq(5,50, by=10)))

#   vector("list", 3) 
# names(resLan) <- c("4", "3A", "7D")
# 
# resVal = vector("list", 3) 
# names(resVal) <- c("4", "3A", "7D")
Y <- 0
T <- 0
A <- 0

for(thresh in seq(5,50, by=10)){
  T <- T+1
  for (Years in 2009:2021){
  Y <- Y+1
  print(Years)
       for(Areas in c("4","3AN", "7D" )){
         A<- A+1
       print(Areas)   
      
          sp1 <- data.table(Ca)
          sp1 <- sp1[!Species%in%c("OTH", "OTHER"),]
          sp1[is.na(Discards), Discards:=0]
          sp1[, Year:=as.numeric(as.character(Year))]
          
          
          sp <- sp1[Year%in%Years,]
          sp[, id:=paste(Country,Year,Quarter,Vessel_length,Metier,Area, sep='')]
          unique(sp$Species)
          unique(sp$Area)
          
          ####!!!!! NEED TO SELECT ALL 27.4.... TO BE DONE!!
          sp <- sp[Area%in%Areas,]
          unique(sp$Species)
          # sp <- sp[substr(stock,1,3)=="NEP",stock:="NEP"]
          # unique(sp$stock)
          
          sp <- sp[,list(land=sum(Landings), catch=sum(Landings+Discards, na.rm=T), value=sum(Value, na.rm=T)), by=list(id,Species, Metier, Area,Quarter)]
          
         # sp[, propLanStock:=land/sum(land)*100, by=list(Species, Area)]
          sp[, propLanGear:=land/sum(land)*100, by=list(id, Metier, Area,Quarter)]
          
         # sp[, propCatchStock:=catch/sum(catch, na.rm=T)*100, by=list(Species, Area)]
          sp[, propCatchGear:=catch/sum(catch, na.rm=T)*100, by=list(id, Metier, Area,Quarter)]
          
          
         # sp[, propValStock:=value/sum(value, na.rm=T)*100, by=list(Species, Area)]
          sp[, propValchGear:=value/sum(value, na.rm=T)*100, by=list(id, Metier, Area,Quarter)]
          
          
          
          			### main species by gear
          			# MaisSpp <- sp[,list(land=sum(land, na.rm=T), catch=sum(catch,na.rm=T), value=sum(value, na.rm=T)), by=c('Metier','Species')]
          			# MaisSpp <- MaisSpp[order(Metier,land, decreasing=T),]
          			# 
          #### define treshold for not only accidental bycatch
          ThreshByCatch <- thresh
          
          ## Option 1: threshold based on the landings of one species compared to the total landings of that species
          #sp[propLanStock>ThreshByCatch, landSpp:=1]
          #sp[propLanStock<=ThreshByCatch, landSpp:=0]
          
          ## Option 2: threshold based on the landings of one species compared to the total landings of that métier
          sp[propLanGear>ThreshByCatch, landSpp:=1]
          sp[propLanGear<=ThreshByCatch, landSpp:=0]
          
          sp[propCatchGear>ThreshByCatch, landSpp:=1]
          sp[propCatchGear<=ThreshByCatch, landSpp:=0]
          
          
          sp[propValchGear>ThreshByCatch, valSpp:=1]
          sp[propValchGear<=ThreshByCatch, valSpp:=0]
          
          
          #reshape(sp[,c('gear','stock','landSpp'),with=F], timevar='gear',idvar='stock' , direction='wide')
          
          #### compute the proportion of landings of one spp that are caught together with other species based on the landings
          	recap <- data.frame(matrix(rep(NA,length(unique(sp$Species))*length(unique(sp$Species)) ), ncol=length(unique(sp$Species))))
          	colnames(recap) <- rownames(recap) <- unique(sp$Species)
          	
          	### main gears
          	recapGears <- data.frame(matrix(rep(NA,length(unique(sp$Species))*length(unique(sp$Species)) ), ncol=length(unique(sp$Species))))
          	colnames(recapGears) <- rownames(recapGears) <- unique(sp$Species)
          	
          	thresholdMainGear <- 0.8 ### gears contributing to co catches
          	thresholdCoCatches <- 50 ### when do we decide that there is co cathes
          	
        if(dim(sp)[1]>0)  {	
          	for(i in unique(sp$Species)){
          		for(j in unique(sp$Species)){
          	    ## get the "targetting"
          			thres <- sp[Species==j,c('landSpp','id'), with=F]
          			prop <- sp[Species==i,]
          			prop <- merge(prop, thres, by='id', all=T)
          			landCombined <- prop[, sum(land*landSpp.y, na.rm=T)/sum(land, na.rm=T)*100]
          			recap[colnames(recap)==i, rownames(recap)==j] <- landCombined
          	
          			prop <- prop[,coocLand:=land*landSpp.y]
          			prop <- prop[order(coocLand, decreasing=TRUE),]
          			prop2 <- prop[,list(sumGear=sum(coocLand, na.rm=TRUE)),by=c('Metier')]
          			prop2 <- prop2[,Cumsum:=cumsum(sumGear)]
          			prop2 <- prop2[,propCatch:=Cumsum/max(Cumsum)]
          			if(landCombined>thresholdCoCatches){
          			listGear <- max(1, max(which(prop2$propCatch<thresholdMainGear)))
          			mainGears <- paste(prop2[1:listGear,Metier], collapse='_')
          			recapGears[colnames(recapGears)==i, rownames(recapGears)==j] <- mainGears
          			}else{
          			recapGears[colnames(recapGears)==i, rownames(recapGears)==j] <- ""
          			
          			}
          		}
          	}
          
          	
          	# resLan[[Areas]][[Y]] <- list(recap)
          	# names(resLan[[Areas]][[Y]]) <- Years	
          	if(dim(recap)[1]>0){
          #	resLan[1:dim(recap)[1],1:dim(recap)[2],A,Y,T] <- (recap["BLL","BLL"])
            recapSpp <- colnames(recap)	  
          	resLan[rownames(resLan)%in%recapSpp,colnames(resLan)%in%recapSpp,A,Y,T] <- as.matrix(recap)
          	
          	}
        }
          # png(paste0(outputWd,"CoocurenceLandings.png"))	
          # 	 par(mar=c(5, 4, 4, 5), las=2)
          # 	 image(t(recap)[,nrow(recap):1], col = rev(heat.colors(5)), axes=F)
          # 	 mtext(text=rev(row.names((recap))), side=2, line=0.3, at=c( (0:(dim(recap)[1]-1))/(dim(recap)[1]-1) ) , las=1, cex=0.8)
          # 	 mtext(text=(row.names((recap))), side=3, line=0.3, at=c( (0:(dim(recap)[1]-1))/(dim(recap)[1]-1) ) , las=2, cex=0.8)
          # 	 image.plot(as.matrix(recap), legend.only=T, col=rev(heat.colors(5)))
          # dev.off()
          # write.csv(recapGears, file=paste0(outputWd, "MainGearsLandings.csv"), row.names=T)
          # 
          # 
          # 
          # png(paste0(outputWd,"CoocurenceLandingsWithGear.png"))	
          # 	 par(mar=c(5, 4, 4, 5), las=2)
          # 	 image(t(recap)[,nrow(recap):1], col = rev(heat.colors(5)), axes=F)
          # 	 mtext(text=rev(row.names((recap))), side=2, line=0.3, at=c( (0:(dim(recap)[1]-1))/(dim(recap)[1]-1) ) , las=1, cex=0.8)
          # 	 mtext(text=(row.names((recap))), side=3, line=0.3, at=c( (0:(dim(recap)[1]-1))/(dim(recap)[1]-1) ) , las=2, cex=0.8)
          # 	for(i in c(1:dim(recap)[1])){
          # 		for(j in c(1:dim(recap)[1])){
          # 			text( (j-1)/(dim(recap)[1]-1) , 1-(i-1)/((dim(recap)[1]-1)) , as.matrix(recapGears)[i,j])
          # 		}
          # 	}
          # 	 image.plot(as.matrix(recap), legend.only=T, col=rev(heat.colors(5)))
          # dev.off()
          # 
          # 
          
          #### compute the proportion of landings of one spp that are caught together with other species based on the catches
          	recapCatch <- data.frame(matrix(rep(NA,length(unique(sp$Species))*length(unique(sp$Species)) ), ncol=length(unique(sp$Species))))
          	colnames(recapCatch) <- rownames(recapCatch) <- unique(sp$Species)
          	
          	### main gears
          	recapGearsCatch <- data.frame(matrix(rep(NA,length(unique(sp$Species))*length(unique(sp$Species)) ), ncol=length(unique(sp$Species))))
          	colnames(recapGearsCatch) <- rownames(recapGearsCatch) <- unique(sp$Species)
          	
          	thresholdMainGear <- 0.8 ### gears contributing to co catches
          	thresholdCoCatches <- 50 ### when do we decide that there is co cathes
          	
          
          	#sp[is.na(propValStock), propValStock:=0]
          	sp[is.na(propValchGear), propValchGear:=0]
       if(dim(recapCatch)[1]>0) {  	
          	for(i in unique(sp$Species)){
          		for(j in unique(sp$Species)){
          		  ## get the "targetting"
          			thres <- sp[Species==j,c('valSpp','id'), with=F]
          			prop <- sp[Species==i,]
          			prop <- merge(prop, thres, by='id', all=T)
          			landCombined <- prop[, sum(value*valSpp.y, na.rm=T)/sum(value, na.rm=T)*100]
          			if(is.na(landCombined)){
          			  landCombined<-0
          			}
          			recapCatch[colnames(recapCatch)==i, rownames(recapCatch)==j] <- landCombined
          	
          			prop <- prop[,coocLand:=land*valSpp.y]
          			prop <- prop[order(coocLand, decreasing=TRUE),]
          			prop2 <- prop[,list(sumGear=sum(coocLand, na.rm=TRUE)),by=c('Metier')]
          			prop2 <- prop2[,Cumsum:=cumsum(sumGear)]
          			prop2 <- prop2[,propCatch:=Cumsum/max(Cumsum)]
          			if(landCombined>thresholdCoCatches){
          			listGear <- max(1, max(which(prop2$propCatch<thresholdMainGear)))
          			mainGears <- paste(prop2[1:listGear,Metier], collapse='_')
          			recapGearsCatch[colnames(recapGearsCatch)==i, rownames(recapGearsCatch)==j] <- mainGears
          			}else{
          			recapGearsCatch[colnames(recapGearsCatch)==i, rownames(recapGearsCatch)==j] <- ""
          			
          			}
          		}
          	}
          	
          # 	resVal[[Areas]][[Y]] <- list(recapCatch)
          #   names(resVal[[Areas]][[Y]]) <- Years		
          	if(dim(recapCatch)[1]>0){
          	  
          	  recapSpp <- colnames(recapCatch)	  
          	  resVal[rownames(resVal)%in%recapSpp,colnames(resVal)%in%recapSpp,A,Y,T] <- as.matrix(recapCatch)
          	  
          	}
         
       }
          # 	resVal[,,A,Y,T] <- as.matrix(recapCatch)
          # # png(paste0(outputWd,"CoocurenceCatches.png"))		
          # 	 par(mar=c(5, 4, 4, 5), las=2)
          # 	 image(t(recapCatch)[,nrow(recapCatch):1], col = rev(heat.colors(5)), axes=F)
          # 	 mtext(text=rev(row.names((recapCatch))), side=2, line=0.3, at=c( (0:(dim(recapCatch)[1]-1))/(dim(recapCatch)[1]-1) ) , las=1, cex=0.8)
          # 	 mtext(text=(row.names((recapCatch))), side=3, line=0.3, at=c( (0:(dim(recapCatch)[1]-1))/(dim(recapCatch)[1]-1) ) , las=2, cex=0.8)
          # 	 image.plot(as.matrix(recapCatch), legend.only=T, col=rev(heat.colors(5)))
          # dev.off()
          # 
          # png(paste0(outputWd,"CoocurenceCatchesWithGear.png"))	
          # 	 par(mar=c(5, 4, 4, 5), las=2)
          # 	 image(t(recapCatch)[,nrow(recapCatch):1], col = rev(heat.colors(5)), axes=F)
          # 	 mtext(text=rev(row.names((recapCatch))), side=2, line=0.3, at=c( (0:(dim(recapCatch)[1]-1))/(dim(recapCatch)[1]-1) ) , las=1, cex=0.8)
          # 	 mtext(text=(row.names((recapCatch))), side=3, line=0.3, at=c( (0:(dim(recapCatch)[1]-1))/(dim(recapCatch)[1]-1) ) , las=2, cex=0.8)
          # 	for(i in c(1:dim(recapCatch)[1])){
          # 		for(j in c(1:dim(recapCatch)[1])){
          # 			text( (j-1)/(dim(recapCatch)[1]-1) , 1-(i-1)/((dim(recapCatch)[1]-1)) , as.matrix(recapGears)[i,j])
          # 		}
          # 	}
          # 	 image.plot(as.matrix(recapCatch), legend.only=T, col=rev(heat.colors(5)))
          # dev.off()

       } 
  A <- 0
  }
  Y<-0
}



save(resLan, file="/mnt/merdouille/F/D/Expertise/TACMAN_2022/res/resLan2021.rdata")
save(resVal, file="/mnt/merdouille/F/D/Expertise/TACMAN_2022/res/resVal2021.rdata")






## results 

load(file="/mnt/merdouille/F/D/Expertise/TACMAN_2022/res/resLan.rdata")
load(file="/mnt/merdouille/F/D/Expertise/TACMAN_2022/res/resVal.rdata")



# resL <- resLan["BLL","BLL",,,]
# resV <- resVal["BLL","BLL",,,]
# 
# 
# resL <- resLan["TUR","TUR",,,]
# resV <- resVal["TUR","TUR",,,]
# 
# resL <- resLan["LEM","LEM",,,]
# resV <- resVal["LEM","LEM",,,]





library(ggplot2)
library(tidyverse)

spp <- "BLL"


funPlot <- function(data = resLan, spp) {
resL4 <- data.frame(data[spp,spp,"4",,])
resL4$Year <- as.numeric(rownames(resL4))
resL4$Area <- "4"


resL3A <- data.frame(data[spp,spp,"3AN",,])
resL3A$Year <- as.numeric(rownames(resL3A))
resL3A$Area <- "3AN"

resL7D <- data.frame(data[spp,spp,"7D",,])
resL7D$Year <- as.numeric(rownames(resL7D))
resL7D$Area <- "7D"

resL <- bind_rows(resL4, resL3A)
resL <- bind_rows(resL, resL7D)
colnames(resL) <- c("Threshold_5", "Threshold_15", "Threshold_25", "Threshold_35", "Threshold_45", "Year", "Area")

resL <- resL %>% pivot_longer(
  cols = starts_with("Threshold"),
  names_to = "Threshold", 
  values_to = "Perc"
)


resL <- resL %>% 
  mutate(Threshold=factor(Threshold)) %>% 
  mutate(Threshold=fct_relevel(Threshold,c("Threshold_5", "Threshold_15", "Threshold_25", "Threshold_35", "Threshold_45"))) %>%
  arrange(Threshold)


print(ggplot() + geom_line(data = resL, aes(x = Year, y = Perc, group = Threshold, color = Threshold)) +
  facet_grid(.~Area) +
  ggtitle(paste0("Species : ", spp)))

}



funPlot(data = resLan, spp = "LEM")


## LEM
resL4 <- data.frame(resLan["LEM","LEM","4",,])
resL4$Year <- as.numeric(rownames(resL4))
resL4$Area <- "4"


resL3A <- data.frame(resLan["LEM","LEM","3AN",,])
resL3A$Year <- as.numeric(rownames(resL3A))
resL3A$Area <- "3AN"

resL7D <- data.frame(resLan["LEM","LEM","7D",,])
resL7D$Year <- as.numeric(rownames(resL7D))
resL7D$Area <- "7D"

resL <- bind_rows(resL4, resL3A)
resL <- bind_rows(resL, resL7D)
colnames(resL) <- c("Threshold_5", "Threshold_15", "Threshold_25", "Threshold_35", "Threshold_45", "Year", "Area")

resL <- resL %>% pivot_longer(
  cols = starts_with("Threshold"),
  names_to = "Threshold"
)

colnames(resL3A) <- colnames(resL7D) <- colnames(resL4) <- c("Threshold_5", "Threshold_15", "Threshold_25", "Threshold_35", "Threshold_45", "Year")

library(tidyr)

plot_data <- gather(resL4, variable, value, -Year)
ggplot(plot_data, aes(x = Year, y = value, color = variable)) +
  geom_line()


toto <- (rainbow(5))
ggplot()+
  geom_line(data=resL4, aes(Year, Threshold_5),col=toto[1])+
  geom_line(data=resL4, aes(Year, Threshold_15), col=toto[2])+
  geom_line(data=resL4, aes(Year, Threshold_25), col=toto[3])+
  geom_line(data=resL4, aes(Year, Threshold_35), col=toto[4])+
  geom_line(data=resL4, aes(Year, Threshold_45), col=toto[5])+
  scale_color_discrete(name = "Y series", labels = c("5%", "15%","25%","35%","45%"))

ggplot(resL3A, aes(Year, Threshold_5),col=toto[1])+geom_line()+geom_line( aes(Year, Threshold_15), col=toto[2])+
  geom_line( aes(Year, Threshold_25), col=toto[3])+
  geom_line( aes(Year, Threshold_35), col=toto[4])+
  geom_line( aes(Year, Threshold_45), col=toto[5])

ggplot(resL7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])


resVal4 <- data.frame(resVal["LEM","LEM","4",,])
resVal4$Year <- as.numeric(rownames(resVal4))
resVal3A <- data.frame(resVal["LEM","LEM","3AN",,])
resVal3A$Year <- as.numeric(rownames(resVal3A))
resVal7D <- data.frame(resVal["LEM","LEM","7D",,])
resVal7D$Year <- as.numeric(rownames(resVal7D))

ggplot(resVal4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])






## WITCH
resL4 <- data.frame(resLan["WIT","WIT","4",,])
resL4$Year <- as.numeric(rownames(resL4))
resL3A <- data.frame(resLan["WIT","WIT","3A",,])
resL3A$Year <- as.numeric(rownames(resL3A))
resL7D <- data.frame(resLan["WIT","WIT","7D",,])
resL7D$Year <- as.numeric(rownames(resL7D))

toto <- (rainbow(5))
ggplot(resL4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])


resVal4 <- data.frame(resVal["WIT","WIT","4",,])
resVal4$Year <- as.numeric(rownames(resVal4))
resVal3A <- data.frame(resVal["WIT","WIT","3AN",,])
resVal3A$Year <- as.numeric(rownames(resVal3A))
resVal7D <- data.frame(resVal["WIT","WIT","7D",,])
resVal7D$Year <- as.numeric(rownames(resVal7D))

ggplot(resVal4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])









## Turbot
resL4 <- data.frame(resLan["TUR","TUR","4",,])
resL4$Year <- as.numeric(rownames(resL4))
resL3A <- data.frame(resLan["TUR","TUR","3AN",,])
resL3A$Year <- as.numeric(rownames(resL3A))
resL7D <- data.frame(resLan["TUR","TUR","7D",,])
resL7D$Year <- as.numeric(rownames(resL7D))

toto <- (rainbow(5))
ggplot(resL4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])


resVal4 <- data.frame(resVal["TUR","TUR","4",,])
resVal4$Year <- as.numeric(rownames(resVal4))
resVal3A <- data.frame(resVal["TUR","TUR","3AN",,])
resVal3A$Year <- as.numeric(rownames(resVal3A))
resVal7D <- data.frame(resVal["TUR","TUR","7D",,])
resVal7D$Year <- as.numeric(rownames(resVal7D))

ggplot(resVal4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])





## Brill
resL4 <- data.frame(resLan["BLL","BLL","4",,])
resL4$Year <- as.numeric(rownames(resL4))
resL3A <- data.frame(resLan["BLL","BLL","3AN",,])
resL3A$Year <- as.numeric(rownames(resL3A))
resL7D <- data.frame(resLan["BLL","BLL","7D",,])
resL7D$Year <- as.numeric(rownames(resL7D))

toto <- (rainbow(5))
ggplot(resL4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resL7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])


resVal4 <- data.frame(resVal["BLL","BLL","4",,])
resVal4$Year <- as.numeric(rownames(resVal4))
resVal3A <- data.frame(resVal["BLL","BLL","3AN",,])
resVal3A$Year <- as.numeric(rownames(resVal3A))
resVal7D <- data.frame(resVal["BLL","BLL","7D",,])
resVal7D$Year <- as.numeric(rownames(resVal7D))

ggplot(resVal4, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal3A, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])

ggplot(resVal7D, aes(Year, X5),col=toto[1])+geom_line()+geom_line( aes(Year, X15), col=toto[2])+
  geom_line( aes(Year, X25), col=toto[3])+
  geom_line( aes(Year, X35), col=toto[4])+
  geom_line( aes(Year, X45), col=toto[5])









## NS
    NSLan <- data.frame(cbind(c(2009:2017),
                resLan[,,1,,1],
                resLan[,,1,,2],
                resLan[,,1,,3],
                resLan[,,1,,4],
                resLan[,,1,,5]))
    colnames(NSLan) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    toto <- (rainbow(5))
    ggplot(NSLan, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])
    
    
    
    NSVal <- data.frame(cbind(c(2009:2017),
                              resVal[,,1,,1],
                              resVal[,,1,,2],
                              resVal[,,1,,3],
                              resVal[,,1,,4],
                              resVal[,,1,,5]))
    colnames(NSVal) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    
    ggplot(NSVal, aes(Year, P25))+geom_ribbon(aes(ymin=P45, ymax=P5), fill="steelblue2", color="steelblue2")+
      geom_line(color="steelblue4", lwd=1)
    
    ggplot(NSVal, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])


tmp <- data.table(Ca)    
tmp <- tmp[, list(meanPrice=weighted.mean(Value/(Landings*1000), Landings, na.rm=T)), by=list( Species, Area)]    

boxplot(tmp$meanPrice~tmp$Species)

## IIIa NOTHING!!
    NSLan <- data.frame(cbind(c(2009:2017),
                              resLan[,,2,,1],
                              resLan[,,2,,2],
                              resLan[,,2,,3],
                              resLan[,,2,,4],
                              resLan[,,2,,5]))
    colnames(NSLan) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    toto <- (rainbow(5))
    ggplot(NSLan, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])
    
    
    
    NSVal <- data.frame(cbind(c(2009:2017),
                              resVal[,,2,,1],
                              resVal[,,2,,2],
                              resVal[,,2,,3],
                              resVal[,,2,,4],
                              resVal[,,2,,5]))
    colnames(NSVal) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    
    ggplot(NSVal, aes(Year, P25))+geom_ribbon(aes(ymin=P45, ymax=P5), fill="steelblue2", color="steelblue2")+
      geom_line(color="steelblue4", lwd=1)
    
    ggplot(NSVal, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])
    
    
## VIId!!
    NSLan <- data.frame(cbind(c(2009:2017),
                              resLan[,,3,,1],
                              resLan[,,3,,2],
                              resLan[,,3,,3],
                              resLan[,,3,,4],
                              resLan[,,3,,5]))
    colnames(NSLan) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    toto <- (rainbow(5))
    ggplot(NSLan, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])
    
    
    
    NSVal <- data.frame(cbind(c(2009:2017),
                              resVal[,,3,,1],
                              resVal[,,3,,2],
                              resVal[,,3,,3],
                              resVal[,,3,,4],
                              resVal[,,3,,5]))
    colnames(NSVal) <- c("Year", "P5", "P15", "P25", "P35", "P45" )
    
    
    ggplot(NSVal, aes(Year, P25))+geom_ribbon(aes(ymin=P45, ymax=P5), fill="steelblue2", color="steelblue2")+
      geom_line(color="steelblue4", lwd=1)
    
    ggplot(NSVal, aes(Year, P5),col=toto[1])+geom_line()+geom_line( aes(Year, P15), col=toto[2])+
      geom_line( aes(Year, P25), col=toto[3])+
      geom_line( aes(Year, P35), col=toto[4])+
      geom_line( aes(Year, P45), col=toto[5])
    
    


resVal[[1]][[1]]$`2009`

spp1 <- "BLL"
spp2 <- "BLL"

resL <- c(resLan[[1]][[1]]$`2009`[[spp1,spp2]],
         resLan[[1]][[2]]$`2010`[[spp1,spp2]],
         resLan[[1]][[3]]$`2011`[[spp1,spp2]],
         resLan[[1]][[4]]$`2012`[[spp1,spp2]],
         resLan[[1]][[5]]$`2013`[[spp1,spp2]],
         resLan[[1]][[6]]$`2014`[[spp1,spp2]],
         resLan[[1]][[7]]$`2015`[[spp1,spp2]],
         resLan[[1]][[8]]$`2016`[[spp1,spp2]],
         resLan[[1]][[9]]$`2017`[[spp1,spp2]])


resV <- c(resVal[[1]][[1]]$`2009`[[spp1,spp2]],
          resVal[[1]][[2]]$`2010`[[spp1,spp2]],
          resVal[[1]][[3]]$`2011`[[spp1,spp2]],
          resVal[[1]][[4]]$`2012`[[spp1,spp2]],
          resVal[[1]][[5]]$`2013`[[spp1,spp2]],
          resVal[[1]][[6]]$`2014`[[spp1,spp2]],
          resVal[[1]][[7]]$`2015`[[spp1,spp2]],
          resVal[[1]][[8]]$`2016`[[spp1,spp2]],
          resVal[[1]][[9]]$`2017`[[spp1,spp2]])


resV <- c(resVal[[3]][[1]]$`2009`[[spp1,spp2]],
          resVal[[3]][[2]]$`2010`[[spp1,spp2]],
          resVal[[3]][[3]]$`2011`[[spp1,spp2]],
          resVal[[3]][[4]]$`2012`[[spp1,spp2]],
          resVal[[3]][[5]]$`2013`[[spp1,spp2]],
          resVal[[3]][[6]]$`2014`[[spp1,spp2]],
          resVal[[3]][[7]]$`2015`[[spp1,spp2]],
          resVal[[3]][[8]]$`2016`[[spp1,spp2]],
          resVal[[3]][[9]]$`2017`[[spp1,spp2]])


save(resLan, file="F:\\D\\Expertise\\WGNSSK\\TACMAN\\res\\resLan.rdata")
save(resVal, file="F:\\D\\Expertise\\WGNSSK\\TACMAN\\res\\resVal.rdata")

# write.csv(recapGearsCatch, file=paste0(outputWd, "MainGearsCatchs.csv"), row.names=T)

