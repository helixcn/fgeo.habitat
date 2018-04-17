### Code to do Torus Translation Test (TT test) to determine habitat associations of tree species

### Sabrina E. Russo, 26 February 2017, srusso2@unl.edu
### Based on code written by Kyle Harms

### This file contains:
###	- a function to do the TT test for one species (torusonesp.all)
###	- R code to use that function to loop through all species at once and quantify habitat asspciations for each species in a plot for one census
###	- step by step guide for how to use this function and R code

### This example code uses the first census of Pasoh as an example, 
###	You must change the name of the plot and associated census data files and R Objects accordingly to match your plot & census



#####################################################################################################################################################################################################################

# Step 1: load CTFS R package & date package (make sure date package is installed)
#	NOTE that you should fill in XX:\\"your path" with your own path information
#	The path is where (folder or directory) the  plot data file can be found on the computer

load("XX:\\"your path"\CTFSRPackage.rdata")
require(date)



#####################################################################################################################################################################################################################

# Step 2: load data for the plot and census that you wish to use to do the TT test
#	NOTE that you should fill in XX:\\"your path" with your own path information
#	The path is where (folder or directory) the  plot data file can be found on the computer

load("XX:\\"your path"\\pasoh.full1.rdata")



#####################################################################################################################################################################################################################

# Step 3: split data or load already splitted data
#	This uses the split.data function of CTFS R package - see that for more description of this function
# *Warning - this may take a while, depending on your computer!

pasoh.full1.split = split.data(censdata=pasoh.full1, splitcol='sp') 


#####################################################################################################################################################################################################################

# Step 4: get the abundances in each 20x20 m quadrat for ALL alive stems of all species, including unidentified stems
#	NOTEs:
#	- abundanceperquad is from the CTFS R package - see that for more description of this function
#	- abundanceperquad gives a list of two elements, and we want the first one , which is named "abund", (hence the $abund at the end of the function call below)
#	- if your habitats are defined on a different scale than 20 x 20 m, then you need to change gridsize to the desired square grid dimension
#	- the habitat file (see below) must match the grid size (ie, abundance and habitats have to be defined on the same spatial scale)
#	- this assumes you wish to use all of the stems to identify habitat associations
#	- if you wish to use a particular size class, then you should subset the full data for that size class prior to splitting it (ie, go back to Step 3)
# *Warning - this may take a while, depending on your computer!

allabund20 = abundanceperquad(pasoh.full1, plotdim=c(1000,500), gridsize=20, type='abund')$abund



#####################################################################################################################################################################################################################

# Step 5: Load habitat data
#	NOTES:
#	- to do the TT test, you need an R Object that indicates the habitat type for each 20 x 20 m grid in the plot (quad index number)
#	- you can also use a grid of another size, but usually the habitats are defined on 20 x 20 m scale
#	- whatever habitat gridsize you use, it must match the gridsize on which the abundances were calculated in Step 4
# 	- For example, here is the first 10 rows of habitat file for Pasoh:
#	- the x and y are the actual grid points of the 20x20 m quadrat, and the index20 is the index of that quadrat
#	- the habitats column gives the habitat category of that 20 x20 m quadrat
#	- the column names in the habitat R object MUST MATCH EXACTLY what is shown below or else the function will not work - so, you may need to change the column names!
#> head(hab.index20)
#   x   y habitats index20
#1  0   0        2       1
#5  0  20        2       2
#9  0  40        2       3
#13 0  60        2       4
#17 0  80        2       5
#21 0 100        2       6

load("XX:\\"your path"\\hab.index20.rdata")



#####################################################################################################################################################################################################################

# Step 6: Load the TT test function for ONE species
#	NOTEs:
#	- you can copy and paste the function below into your R console in order to load the function  (make sure you copy completely from beginning to end)
#	- there are defaults listed for each argument - you will need to change these according to what you want to do
#	- if you are doing ONE species only, then type the code of that species in quotes for the species argument and 
#	- note that you should only try to determine the habitat association for sufficiently abundant species - in a 50-ha plot, a minimum abundance of 50 trees/species has been used
#	- if you are doing ALL species, then see below in Step XXX for what to type for the species argument
#	- hab.index20 is an R Object file giving the habitat designation for each 20x20 quad - see Step 5
#	- the argment for allabund20 is the Robject you made in Step 4
#	- change plotdim to match your plot, and make sure the gridsize entered here matches the gridsize on which the habitats are defined and the abundances were calculated


torusonesp.all=function(species="GIROPA", hab.index20=hab.index20, allabund20=allabund20, plotdim=c(1000,500), gridsize=20)
 {
  plotdimqx = plotdim[1]/gridsize  		# Calculates no. of x-axis quadrats of plot. (x is the long axis of plot in the case of Pasoh)
  plotdimqy = plotdim[2]/gridsize  		# Calculates no. of y-axis quadrats of plot.
  num.habs = length(unique(hab.index20$habitats))  	# Determines tot. no. of habitat types.

  GrLsEq = matrix(0,1,num.habs*6)    		# Creates empty matrix for output.
  rownames(GrLsEq) = species       		# Names single row of output matrix.


  for(i in 1:num.habs)           		# Creates names for columns of output matrix.
   {
    if(i==1)
      cols=c(paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))
    if(i>1)
      cols=c(cols, paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))  
   }
  colnames(GrLsEq)=cols    			# Names columns of output matrix.


 # CALCULATIONS FOR OBSERVED RELATIVE DENSITIES ON THE TRUE HABITAT MAP

    allabund20.sp = allabund20[which(rownames(allabund20)==species),]  				# pulls out the abundance by quad data for the focal species
    spmat = matrix(as.numeric(allabund20.sp), nrow=plotdimqy, plotdimqx, byrow=F)  		# Fills a matrix, with no. rows = plotdimqy (dim 2) and no. columns = plotdimqx (dim 1), with the indiv. counts per quadrat of one species.
    totmat = matrix(apply(allabund20, MARGIN=2, FUN="sum"), plotdimqy, plotdimqx, byrow=F)     	# calculates total number of stems in each quad for all species and puts in matrix

    habmat = matrix(hab.index20$habitats, nrow=plotdimqy, ncol=plotdimqx, byrow=F)		# fills matrix with habitat types, oriented in the same way as the species and total matrices above	
	
    spstcnthab = numeric()   			# Creates empty vector for stem counts per sp. per habitat.
    totstcnthab = numeric()  			# Creates empty vector for tot. stem counts per habitat.
    
    for(i in 1:num.habs)  
     {
      totstcnthab[i] = sum(totmat[habmat==i])	# Determines tot. no. stems per habitat of the true map.
      spstcnthab[i] = sum(spmat[habmat==i]) 	# Determines tot. no. stems for focal sp. per habitat of the true map. 	
     }

    spprophab=spstcnthab/totstcnthab         	# Calculates observed relative stem density of focal sp. per habitat of the true map.

 # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS
    habmat.template=habmat
    
    for (j in 1:4)    {
    # apply rotations and mirrors
    # if j==1 do nothing

      if (j==2) habmat = apply(habmat.template, 2, rev)
      if (j==3) habmat = t(apply(habmat.template, 1, rev))
      if (j==4) habmat = t(apply(apply(habmat.template, 2, rev),1,rev))


 # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS

    for(x in 0:(plotdimqx-1))    # Opens "for loop" through all 20-m translations along x-axis.
      { 
      for(y in 0:(plotdimqy-1))  # Opens "for loop" through all 20-m translations along y-axis.
       { 
        newhab=matrix(0,plotdimqy,plotdimqx)  # Creates empty matrix of quadrats' habitat designations.


        # The following "if" statements create the x,y torus-translation of the habitat map.

        if(y==0 & x==0)          
           newhab=habmat

        if(y==0 & x>0)
           newhab=habmat[c(1:plotdimqy), c((plotdimqx-x+1):plotdimqx, 1:(plotdimqx-x))] 
  
        if(x==0 & y>0)
           newhab=habmat[c((plotdimqy-y+1):plotdimqy, 1:(plotdimqy-y)), c(1:plotdimqx)]

        if(x>0 & y>0)
            newhab=habmat[c((plotdimqy-y+1):plotdimqy, 1:(plotdimqy-y)), c((plotdimqx-x+1):plotdimqx, 1:(plotdimqx-x))] 


        Torspstcnthab = numeric()   # Creates empty vector for stem counts per sp. per habitat in torus-based maps.
        Tortotstcnthab = numeric()  # Creates empty vector for tot. stem counts per habitat in torus-based maps.

        for(i in 1:num.habs)
         {
          Tortotstcnthab[i] = sum(totmat[newhab==i])  # Determines tot. no. stems per habitat of the focal torus-based map.
          Torspstcnthab[i] = sum(spmat[newhab==i])    # Determines tot. no. stems for focal sp. per habitat of the focal torus-based map.
         }

        Torspprophab = Torspstcnthab/Tortotstcnthab   # Calculates relative stem density of focal sp. per habitat of the focal torus-based map.  


        for(i in 1:num.habs)
         {

          if(spprophab[i] > Torspprophab[i])          # If rel. dens. of focal sp. in focal habitat of true map is greater than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "greater than" count. 		
            GrLsEq[1,(6*i)-4] = GrLsEq[1,(6*i)-4]+1  

          if(spprophab[i] < Torspprophab[i])          # If rel. dens. of focal sp. in focal habitat of true map is less than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "less than" count. 
            GrLsEq[1,(6*i)-3] = GrLsEq[1,(6*i)-3]+1 
          	            
          if(spprophab[i] == Torspprophab[i])         # If rel. dens. of focal sp. in focal habitat of true map is equal to rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "equal to" count.
            GrLsEq[1,(6*i)-2] = GrLsEq[1,(6*i)-2]+1 
  
         }
       }    # Closes "for loop" through all 20-m translations along x-axis.
     }      # Closes "for loop" through all 20-m translations along y-axis.
   }	    # Closes for loop through mirrors and rotations (j)
    

    for(i in 1:num.habs)
     {
      GrLsEq[1,(6*i)-5] = spstcnthab[i]								# add counts of No. stems in each habitat

      if (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) <= 0.025) GrLsEq[1,(6*i)-1] = -1	# if rel.dens. of sp in true map is greater than rel. dens. in torus map less than 2.5% of the time, then repelled
      if (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) >= 0.975) GrLsEq[1,(6*i)-1] = 1	# if rel.dens. of sp in true map is greater than rel. dens. in torus map more than 97.5% of the time, then aggregated
      if (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) < 0.975 & GrLsEq[1,(6*i)-4]/(plotdimqx*plotdimqy) > 0.025) GrLsEq[1,(6*i)-1] = 0		# otherwise it's neutral (not different from random dist)

      GrLsEq[1,(6*i)] = GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy))				# quantile in the TT distribtution of relative densities of the true relative density 
     } 
       
  return(GrLsEq)
 } 



#####################################################################################################################################################################################################################

# Step 7: Do the TT test for one species
# Notes:
#	- this example is for GIROPA, a fairly abundant species in the Pasoh plot
#	- you should change the arguments to match your desired species, etc.

GIROPA.TT = torusonesp.all(species="GIROPA", hab.index20=hab.index20, allabund20=allabund20, plotdim=c(1000,500), gridsize=20)



#####################################################################################################################################################################################################################

# STEP 8: Interpret the output of the TT test for one species

##### Do not RUN THIS SECTION AS R Code! -  These are only NOTES on interpretation of the results output by torusonesp.all()

# The output below resulted from this R code (from Step 7): 

# GIROPA.TT = torusonesp.all(species="GIROPA", hab.index20=hab.index20, allabund20=allabund20, plotdim=c(1000,500), gridsize=20)

# Output:
#> GIROPA.TT
#       N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1 N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2 N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3
#GIROPA      78        0     1249        1             -1              0    1572       91     1158        1              0         0.0728    1085     1242        7        1              1
#       Obs.Quantile.3 N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#GIROPA         0.9936    1224     1248        1        1              1         0.9984


# Interpretation of OUTPUT: 

# Note that the output wraps onto two lines

# N.Hab.1 -> there are 78 stems in total of GIROPA (the focal species) on Hab 1
# Gr.Hab.1 -> in 0 instances the observed relative density of the focal species on habitat 1 was greater than the relative density based on the TT habitat map
# Ls.Hab.1 -> in 1249 instances the observed relative density of the focal species on habitat 1 was less than the relative density based on the TT habitat map
# Eq.Hab.1 -> in 1 instance the observed relative density of the focal species on habitat 1 was equal to the relative density based on the TT habitat map
# This information is repeated for each habitat sequentially across the columns
# The sum of the Gr.Hab.x, Ls.Hab.x, and Eq.Hab.x columns for one habitat equals the number of 20 x20 quads in the plot.

# The Rep.Agg.Neut columns for each habitat indicate whether the species is significantly repelled (-1), aggregated (1), or neutrally distributed (0) on the habitat in question
# In this case, GIROPA is 
#	significantly repelled from habitat 1, 
#	neutrally distributed on habitat 2 (neither repelled nor aggregated), 
#	significantly aggregated on habitat 3, and 
#	significantly aggregated on habitat 4

# The probabilities associated with the test for whether these patterns are statistically significant are in the Obs.Quantile columns for each habitat:
# 	for habitat 1, the probability is 0 (so p << 0.0001)
# 	for habitat 2, the probability is 0.0728
# 	for habitat 3, the probability is 1-0.9936 = 0.0064 
# 	for habitat 4, the probability is 1-0.9984 =  0.0016
# Note that to calculate the probability for repelled, it is the value given, but to calculate the probability for aggregated, it is 1- the value given
# In other words, for habitat 2, even though the observed relative density was less than the torus relative density 1,158 times, this is not enough times to be statistically significant 



#####################################################################################################################################################################################################################

# STEP 9: map the species with habitat to make sure this makes sense! 

# load the elevation data for your plot
#	NOTE that you should fill in XX:\\"your path" with your own path information
#	The path is where (folder or directory) the  plot data file can be found on the computer

load("XX:\\"your path"\\CTFSElev_pasoh.rdata")


# map of stems and contours
map(splitdatafile=pasoh.full1.split,species='GIROPA', plotdim=c(1000,500), cutoff=c(1,10,30,300), 
	elevdata = readelevdata(CTFSElev_pasoh$col), labsize=0.6, topoclr = "gray30", clrs="gray60")


# make 4 transparent colors; alpha at low values means more transparent
# 1st color in transp.col is green, 2nd is red, 3rd is violet, 4th is aqua
# we want 4 colors because we have 4 habitats

transp.col4 = c(rgb(red=0, green=1, blue=0, alpha=0.2), rgb(red=1, green=0, blue=0, alpha=0.2), rgb(red=0, green=0, blue=1, alpha=0.2), rgb(red=0, green=1, blue=1, alpha=0.2))


# arrange habitat quads according to what image() wants - long axis of the plot is across columns

plotdim=c(1000,500)
gridsize=20
plotdimqx = plotdim[1]/gridsize  
plotdimqy = plotdim[2]/gridsize  

habmat = matrix(hab.index20$habitats, nrow=plotdimqy, ncol=plotdimqx, byrow=F)		# fills matrix with habitat types, orientied in the same way as the species and all matrices above	


# add habitats onto stem map
# in this map, habitat 1 = green, habitat 2 = red, habitat 3 = violet, habitat 4 = aqua
# so the order of the colors in the color vector matches the numbering of the habitats
# in other words, the habitats are colored by matching their number with the position in the color vector

image(x=seq(0,1000,20), y=seq(0,500,20), z=t(habmat), col = transp.col4, xlab="Meters", ylab="Meters", add=T)	# need to transpose



#####################################################################################################################################################################################################################

# Step 10: Do TT test for  ALL species in a plot with sufficient sample size

# loop to run torusonesp for all species at once
#	this will save the result for all species in dat.all.sp in which
#	rows are species and columns are the torus translation result for each habitat


# First - Find the species with minimum sample size of N>=50 stems across all 20 x 20 quads
# NOTES:
#	- you can change the minimum sample size by changing the value of min.N in the code below to another number
# 	- this uses the R Objects made in the steps above 
# 	- that is, here I used the same names as I used in the steps above - if you used different names in the steps above, then you should use those names here
#	- no.species is the number of species in the plot with N>=50 across the entire plot
# 	- species.N are the names of the species with N>=50 across the entire plot
#	- copy and paste this chunk of R code into your R console

min.N=50
sab = apply(allabund20, MARGIN=1, FUN="sum")
sp.w.min.N = which(sab >= min.N)
no.species = length(sp.w.min.N)
species.N = names(sp.w.min.N)


# Second - Run torusonesp.all for ALL species
#	NOTES:
#	- do not change the value SP for the argument species in the torusonesp.all function
#	- the other arguments n the torusonesp.all function should match your plot, as above when you ran the TT test for one species
#	- otherwise, the code should be run exactly
#	- copy and paste this chunk of R code into your console


# make an empty matrix to store the results in

dat.all.sp = matrix(NA, nrow=no.species, ncol=6*length(unique(hab.index20$habitats)))


# loop to do the TT test for each species with minimum sample size

for (kk in 1:no.species) {

print(paste(kk,"of", no.species))

	SP = species.N[kk]

	dat.all.sp[kk,] = torusonesp.all(species=SP, hab.index20=hab.index20, allabund20=allabund20, plotdim=c(1000,500), gridsize=20)

}
rownames(dat.all.sp) = species.N					# adds the species' rownames to the output file 

  for(i in 1:length(unique(hab.index20$habitats)) )           		# Creates names for columns of output matrix.
   {
    if(i==1)
      cols=c(paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))
    if(i>1)
      cols=c(cols, paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))  
   }

  colnames(dat.all.sp)=cols    			# adds the Names to the columns of output matrix.



