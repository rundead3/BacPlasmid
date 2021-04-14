library('plot.matrix')


#OPTIMIZATION , logging of bacteria into vectors, growth vector


#pop coordinates into list DONE
AddBac <- function(xcoord,ycoord,truthp,truths,bacchain) {
  x<- append(bacchain[[1]],xcoord)
  y<- append(bacchain[[2]],ycoord)
  P<- append(bacchain[[3]],truthp)
  S<- append(bacchain[[4]],truths)
 
  return(list(x,y,P,S))

}
# Masterlist <- AddBac()

#removing bacteria from the list
RemoveBac <- function(metalist,TruthVector){

  metalist[[1]] <- metalist[[1]][!TruthVector]
  metalist[[2]] <- metalist[[2]][!TruthVector]
  metalist[[3]] <- metalist[[3]][!TruthVector]
  metalist[[4]] <- metalist[[4]][!TruthVector]

  return(metalist)
}
# Masterlist <- RemoveBac(Masterlist,TruthVec(posicaox,posicaoy))
#Sampling
Randombac  <- function() {
  return(sample(1:length(Masterlist[[1]]),1,replace=TRUE))

  }
# randomindex <- Randombac()
# #Finding coord
# coordinatex <- xvalues[randomindex]
# 
# coordinatey <- yvalues[randomindex]

#Create Truth vector
TruthVec <- function(index){
  truthx<-c()
  if (index!=1){
    for(i in 1:(index-1)){
      truthx[i]<-FALSE
    }
  }
  truthx[index]<-TRUE
  if (index!=length(Masterlist[[1]])){
    for(i in (index+1):length(Masterlist[[1]])){
      truthx[i]<-FALSE
    }
     
  }
  return(truthx)
}
# ultimatetruth <- TruthVec()



#Variables
xvalues <- c()
yvalues <- c()

Propagatable <- c()
Shareable <- c()

Masterlist<-list(xvalues,yvalues,Propagatable,Shareable)

timex<- c()
plasmidy<- c()

Transconjugant<- 0
Donor <- 0
Receptor <- 0
Zeroes <- 0

#gridsize
gz <- 150

#theta constants
thetaCustom <- 0.8
thetaRandomAmount <- 2  #MAX 5 because then we run out of colors!
thetaRandomRange <- 0.5:2
thetas <- c(thetaCustom)
thetas <- append(thetas, runif(thetaRandomAmount, min=thetaRandomRange[1], max=thetaRandomRange[2]))
thetas

#conjugation constants
conjc1<- 0.2
conjc2<- 0.3

# Spatial Bacteria Matrix , 1 to 3 corresponds to Receptor,Donor and Transconjugant Respectively
field <- matrix(c(0:0), nrow=gz,ncol=gz)

gridvalues <- c(1:gz)

fillage <- 0.005
#Initial number of bacterias Seeded
Seed <- max(round(gz^2*fillage/2)*2,2)

#Quantum Bacterioteleportation function
donut <- function(coordinate) {
  ((coordinate-1)%%gz)+1
}

#Crescer Bacterias
#tipo 1-Receptoras, tipo 2+ - Doadoras
_________________________for

  bac <- 0
  while (bac<Seed/2){
    
    # bb is bacteria born
    bb <- sample(gridvalues,1,replace = TRUE)
    posicaox<- c(bb)
    bb <- sample(gridvalues,1,replace = TRUE)
    posicaoy<- c(bb)
    
    
    if (field[posicaox,posicaoy]==0) {
      
      
      
      field[posicaox,posicaoy]<-(1)
      
      
        Masterlist<-AddBac(posicaox,posicaoy,TRUE,FALSE,Masterlist)  
        
      

      bac <- bac +1
    
  }
}

while (bac<Seed){
  
  # bb is bacteria born
  bb <- sample(gridvalues,1,replace = TRUE)
  posicaox<- c(bb)
  bb <- sample(gridvalues,1,replace = TRUE)
  posicaoy<- c(bb)
  
  
  if (field[posicaox,posicaoy]==0){
    
    field[posicaox,posicaoy]<-(2)
   
    Masterlist<-AddBac(posicaox,posicaoy,TRUE,TRUE,Masterlist) 
    
    

    
    bac <- bac +1
  }
}



realtime  <- 0
BacNumber <-Seed
maxFill <- 0.94 
while (BacNumber<(maxFill*(gz^2))) {
  

  for (randomindex in length(Masterlist[[2]]):1){
  #Soace 42 the meaning of life
  Spacec <- c(42)
  Trash <-0
  
  # Picking a bacteria 
  posicaox <- Masterlist[[1]][randomindex]
  posicaoy <- Masterlist[[2]][randomindex]
  
  if (Masterlist[[3]][randomindex]==TRUE){
    
   
    #Check for blockage
    size <- 1
    foodc <- c()
    Spacec <- c()
    XPropGrid <- donut(c((posicaox-size):(posicaox+size)))
    YPropGrid <- donut(c((posicaoy-size):(posicaoy+size)))
    
    for (x in XPropGrid){
      for (y in YPropGrid){
        if (field[x,y]==0){
          Spacec <- cbind(Spacec,c(x,y))
          
        }
      }
    }
    #Removing blocked cells from masterlist
    if(length(Spacec)==0){
      if(Masterlist[[4]][randomindex]==FALSE){
        Masterlist <- RemoveBac(Masterlist,TruthVec(randomindex))
        Trash<-Trash+1
      } else {
        Masterlist[[3]][randomindex]<-FALSE
      }
    }else {
      
      #Checking food 
      size <- 3
      
      xgrid <- donut(c((posicaox-size):(posicaox+size)))
      ygrid <- donut(c((posicaoy-size):(posicaoy+size)))
      
      
      space <- 0
      
      for (x in xgrid){
        for (y in ygrid){
          if (field[x,y]==0){
            space <- space+1
            
          }
        }
      }
      foodc <- ((space)/49)
      
    
      #Bacterial Growth calculation
      if (foodc>=thetaCustom){
        growth <- 1
        
      } else {
        growth <- (foodc/thetaCustom)
        
      }
      
      if (field[posicaox,posicaoy]!=1){
        growth <- 0.95*growth
      }
      #GROWTH SPAWNAGE
      if (runif(1)<=growth){
        tipo <- field[Masterlist[[1]][randomindex],Masterlist[[2]][randomindex]]
        
        NewHome <- sample(c(1:length(Spacec)),1,replace=TRUE)
        
        if (NewHome%%2==0) {
          newx <- Spacec[NewHome-1]
          newy <- Spacec[NewHome] 
          
        }else{
          newx <- Spacec[NewHome]
          newy <- Spacec[NewHome+1] 
        }
        
        field[newx,newy]<- tipo
        
        Masterlist <- AddBac(newx,newy,TRUE,tipo!=1,Masterlist)
        
      


        Spaces <- length(Spacec)-2
        BacNumber <- BacNumber + 1
            
            # for (x in gridvalues){
            #   for (y in gridvalues){
            #     if (field[x,y]==1){
            #       Receptor <- Receptor+1
            #     }else if(field[x,y]==2){
            #       Donor <- Donor+1
            #     } else if(field[x,y]==3){
            #       Transconjugant<- Transconjugant+1
            #     }else {
            #       Zeroes <- Zeroes +1
            #     }
            #   }
            # }

            # cat("BacNumber is" ,BacNumber ,"\n" )
            # cat("Receptor is" ,Receptor ,"\n" )
            # cat(Receptor/((gz)^2),"\n")
            # cat("Transconjugant is" , Transconjugant,"\n")
            # cat(Transconjugant/((gz)^2),"\n")
            # cat("Donor is" , Donor,"\n" )
            # cat(Donor/((gz)^2),"\n")
            # cat("free space is", Zeroes,"\n" )
            # cat(Zeroes/((gz)^2),"\n")
            
            
      }
          
      
        #Growth else ends
    }
  }
  
  #plasmid transfer
  if (Trash!=1){
    
   if(Masterlist[[4]][randomindex]==TRUE){
    #Check for blockage
    NumberofSpaces <- 0
    size <- 1
    Receptorsc <- c()
    XPropGrid <- donut(c((posicaox-size):(posicaox+size)))
    YPropGrid <- donut(c((posicaoy-size):(posicaoy+size)))
    
    #If no growth counting free spaces and finding receptors
    if(length(Spacec)==1){
      
      for (x in XPropGrid){
        for (y in YPropGrid){
          if (field[x,y]==1){
            Receptorsc <- cbind(Receptorsc,c(x,y))
          }else if(field[x,y]==0){
            NumberofSpaces <- NumberofSpaces+1
            
          }
        }
      }#Given growth only find receptors, use spaces arelady calculated
     }else {
       
       for (x in XPropGrid){
         for (y in YPropGrid){
           if (field[x,y]==1){
             Receptorsc <- cbind(Receptorsc,c(x,y))
           }
         }
       }
      NumberofSpaces <- (Spaces/2)
     }
    #Rremoving Bacteria that cannot grow nor transconjugate
    if(length(Receptorsc)==0 && NumberofSpaces==0){
      Masterlist <- RemoveBac(Masterlist,TruthVec(randomindex))
      
    }else {
    
    if(length(foodc)==0){
      
  
      
    size <- 3
    
    xgrid <- donut(c((posicaox-size):(posicaox+size)))
    ygrid <- donut(c((posicaoy-size):(posicaoy+size)))
    
    
    space <- 0
    
    for (x in xgrid){
      for (y in ygrid){
        if (field[x,y]==0){
          space <- space+1
          
        }
      }
    }
    
    foodc <- ((space)/49)
    }
      
    #Equations of conjugation parameters based on 7x7
    if (foodc>=conjc2){
      plasmid <- 1
    }else if(foodc<conjc1){
      plasmid <- 0
      Masterlist[[4]][randomindex]<-FALSE
    } else{
      plasmid <- (foodc-conjc1)/(conjc2-conjc1)
    }
    
  
  
    
    #check if transfer fail or pass
    
    if (runif(1)<=plasmid){
      
      neighbours <- 0
      size <- 1
      receptorc <- c()
      xgrid <- donut(c((posicaox-size):(posicaox+size)))
      ygrid <- donut(c((posicaoy-size):(posicaoy+size)))
      
      receptorc <- c()
      for (x in xgrid){
        for (y in ygrid){
          if (field[x,y]==1){
            receptorc <- cbind(receptorc,c(x,y))
            
            neighbours <- neighbours+1
            
          }
        }
      }
    
      
      if (neighbours>0){
        
        chosen <- sample(c(1:length(receptorc)),1,replace=TRUE)
        if (chosen%%2==0) {
          newx <- receptorc[chosen-1]
          newy <- receptorc[chosen] 
          
        }else{
          newx <- receptorc[chosen]
          newy <- receptorc[chosen+1] 
        }
        
        field[newx,newy]<- 3
        Masterlist[[4]][randomindex]<-TRUE
        
      }
    }
    }
  }  
  } 
}
  
  realtime  <- realtime+1
  cat("T:",realtime,"\n" )
  cat("B:",BacNumber,"\n" )
  
  # 1. Open jpeg file
  jpeg(paste(realtime,"rplot.jpg"), width = 3000, height = 3000)
  # 2. Create the plot

  # 3. Close the file
  plot(field, col=c('white', 'green','blue','red', sample(c('black','yellow','magenta','cyan','gray'), thetaRandomAmount, replace = FALSE)), breaks=c(c(0, 1, 2,3,4), 5:(5+thetaRandomAmount)),border=NA)   
  
  dev.off()
  
}

  # timex<-append(timex,realtime)
  # plasmidy<-append(plasmidy,(Transconjugant+Donor)/(Transconjugant+Donor+Receptor))
