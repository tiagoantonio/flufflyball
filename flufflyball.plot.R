###############################################################################
# May,12 2016 
# flufflyball.plot
#
# Plot beautiful sun graphs in R like a fluffly ball.
#
# Help: flufflyball.plot.Rd
#
# Author: Tiago A. de Souza
# tiagoantonio@gmail.com
# github.com/tiagoantonio/
#
################################################################################


flufflyball.plot=function(id,group,coord,values, filter=0, cex.circle=6, plot.legend=TRUE, cex.legend=0.5, exclude.na=TRUE){
  
  # main parameter checking
  if(length(id)!=length(group)||length(group)!=length(values)){stop("vectors must be the same lengths")} # main length checking
  
  id=as.character(id) #receiving vector id and coerces into character.
  
  if(class(group)!="integer" & class(group)!="numeric" ){ # checks if group is integer or numeric
    stop("groups is not a vector of integers")}  # stop with warning msg
  if(class(coord)!="integer" & class(group)!="numeric"){ #checks if coord is interger or numeric
    stop("coord is not a vector of integers")} # stop with warning msg
  if(class(values)!="numeric" & class(values)!="integer" ){ # checks if values is numeric
    stop("values is not a vector of numbers")} # stop with warning msg
  if(class(filter)!="numeric"){ # checks if filter is numeric
    stop("values is not numeric")} # stop with warning msg
  if(filter<0){ #checks if filter is >0
    stop("filter for values must be >0")} # stop with warning msg
  if(cex.circle<0) { #checkis if cex.circle is >0
    stop("cex.circle must be >0")} # stop with warning msg
  
  # parameter exclude .na for NAs in values
  if (exclude.na==TRUE){ # if exclude.na=TRUE 
    group=group[!is.na(values)] #remove elements in group if there is NA is values
    coord=coord[!is.na(values)] #remove elements in coords if there is NA is values
    values.checked=values[!is.na(values)] # object values.checked without NAs (passed)
  }
  if (exclude.na==FALSE){ #if exclude.na=FALSE
    if(any(is.na(group))){stop("There are NAs in group. Please review data or use exclude.na=TRUE")}  # stop with warning msg
    if(any(is.na(coord))){stop("There are NAs in coord. Please review data or use exclude.na=TRUE")} # stop with warning msg
    if(any(is.na(values))){stop("There are NAs in values. Please review data or use exclude.na=TRUE")} # stop with warning msg
    values.checked=values # object values.checked without NAs (passed) 
  }
  
  # different groups
  ngroup=factor(group) # coerces group to factor
  ngroup=length(levels(ngroup)) # total number of levels in object ngroup
  
  # grid for graphics based on total number of different groups
  if (ngroup==0){stop("group must be different from 0")} # stop with warning msg if ngroup==0
  if (ngroup==1){par(mfrow=c(1,1))} #divides 1x1 if ngroup=1
  if (ngroup==2){par(mfrow=c(2,1))} #divides 2x1 if ngroup=2
  if (ngroup>2 & ngroup<=4 ){par(mfrow=c(2,2))} #divides 2x2 if ngroup=2..4
  if (ngroup>4 & ngroup<9 ){par(mfrow=c(3,3))} #divides 3x3 if ngroup=4..9
  if (ngroup>=10 & ngroup<=16 ){par(mfrow=c(4,4))} #divides 4x4 if ngroup=10..16
  if (ngroup>17 & ngroup<=25 ){par(mfrow=c(5,5))} # divides 5x5 if ngroup=17..25
  if (ngroup>26){par(mfrow=c(8,8))} # divides 8x8 if ngroup=25..64
  if (ngroup>64){stop("group number must be less than 64")} # stop with warning msg if ngroup>64
  
  filtered.names=c() # creates an empty object to save object 'id' levels filtered by parameter filter 
  
  # loop for fluffy ball plot 
  for (i in min(group):max(group)){ # loop to build a plot for each group
    
    subset.group=group[group==i] # subset of group data
    
    #checking if there is a empty i group between min(group) & max(group)
    if (length(subset.group)==0){
      plot(0,xlim=c(-1,1), # empty plot.
           ylim=c(-1,1), # lims are defined by max and min values.
           axes=F, type="n", main=i) # without axes centered on 0,0. group is the main title. 
      points(0,0,pch=21,cex=cex.circle, bg="White", col=1)# draw a circle in front of lines using cex.circle user parameter
      if (plot.legend==TRUE){
          legend("bottom", c("4th quartile", "3rd quartile", "2nd quartile", "1st quartile"), # legend of 4 quartiles
                fill=c("#E41A1C", "#377EB8","#4DAF4A","#984EA3"),  # ColorBrewer colors 
                cex=cex.legend, horiz=TRUE,inset=.02, bg="White", box.lty=0, # horizontal legend without border
                title=paste(deparse(substitute(values)))) # legend title with vector group name
      }
    }
    
    else{
      max.coord=max(coord[group==i]) # max coord value 
      norm.angles=(2*pi*coord[group==i])/max.coord # normalized angles for each coord based on max coord value 
      max.value=max(values.checked) # max value for 'values'
    
      par(mar=c(0,0,2,0)) # setting margins for each plot
    
      plot(0,xlim=c(-max.value,max.value), # empty plot.
          ylim=c(-max.value,max.value), # lims are defined by max and min values.
          axes=F, type="n", main=i) # without axes centered on 0,0. group is the main title. 
    
      for (ii in 1:length(norm.angles)){ # inside loop to build each spine in the plot
      
        radius=values.checked[group==i][ii] # pick a single value "ii" for the group "i"
      
      #picking colors according to quartiles
        if(values.checked[group==i][ii]>=quantile(values.checked[values.checked!=0])[1] & # if value "ii" is bigger than 1st quartile
          values.checked[group==i][ii]<quantile(values.checked[values.checked!=0])[2]){color.line="#984EA3"} # AND value "ii" is smaller than 2nd quartile
        if(values.checked[group==i][ii]>=quantile(values.checked[values.checked!=0])[2] & # if value "ii" is bigger than 2nd quartile
          values.checked[group==i][ii]<quantile(values.checked[values.checked!=0])[3]){color.line="#4DAF4A" } # AND value "ii" is smaller than 3rd quartile
        if(values.checked[group==i][ii]>=quantile(values.checked[values.checked!=0])[3] & # if value "ii" is bigger than 3rd quartile
          values.checked[group==i][ii]<quantile(values.checked[values.checked!=0])[4]){color.line="#377EB8"} # AND value "ii" is smaller than 4th quartile
        if(values.checked[group==i][ii]>=quantile(values.checked[values.checked!=0])[4] & # if value "ii" is bigger than 4th quartile
          values.checked[group==i][ii]<=quantile(values.checked[values.checked!=0])[5]){color.line="#E41A1C"} # AND value "ii" is smaller than 5th quartile
      
        #applying filter parameter and drawing lines
        if (radius>filter){ #if value>filter value(radius)
          lines(c(0,sin(norm.angles[ii])*radius),c(0,cos(norm.angles[ii])*radius), col=color.line) #draw the line using picked color and normalized angles
          filtered.names=append(filtered.names, id[group==i][ii],after=length(filtered.names)) # adding id to filtered values in object "filtered names"
        }
      }
    
      points(0,0,pch=21,cex=cex.circle, bg="White", col=1) # draw a circle in front of lines using cex.circle user parameter
    
      if (plot.legend==TRUE){
        legend("bottom", c("4th quartile", "3rd quartile", "2nd quartile", "1st quartile"), # legend of 4 quartiles
              fill=c("#E41A1C", "#377EB8","#4DAF4A","#984EA3"),  # ColorBrewer colors 
              cex=cex.legend, horiz=TRUE,inset=.02, bg="White", box.lty=0, # horizontal legend without border
              title=paste(deparse(substitute(values)))) # legend title with vector group name
      }
    }
  }
  
  filtered.names=as.factor(filtered.names) # coercing filtered names vector as factor
  filtered.names=levels(filtered.names) # unique "id" names
  
  par(mfrow=c(1,1)) # resets par(mfrow)
  
  return(filtered.names) # return unique filtered "id" 
}