# Source code/work flows in Flenniken et al. 2026. Current Gaps and Future Threats to Say’s Spiketail Habitat in the Southeastern USA. Ecology and Evolution.

###########################################################

# ArcGIS Pro 3.3.2: Produced detailed streams from DEM using Spatial Analyst extension
#Hydrology tools (default parameters): Fill DEM --> Flow Direction --> Flow Accumulation (FA) --> 
#Reclassify tool: Bifurcate FA to 2 classes [used threshold of <300 for lower class] and reclassify lower class to 0 and upper class to 1 -->
#Hydrology tool: Stream Order - Strahler to identify 1st and 2nd order streams
# Convert raster to polyline

# ArcGIS Pro 3.3.2: Reduced noise (artefacts from detailed stream process)
#Remove straight lines by computing sinuosity in stream polyline with Python script
#Add field named sinuosity in polyline attribute table --> Calculate Field --> Set to Python 3 -->
#Copy/paste code below into code block 
def getSinuosity(shape):
    if shape.length == 0:
        return 0
    # Calculate straight-line distance between first and last points
    d = math.sqrt((shape.firstPoint.X - shape.lastPoint.X)**2 + (shape.firstPoint.Y - shape.lastPoint.Y)**2)
    # Sinuosity = Total Length / Straight Line Distance
    return shape.length / d 
# Copy/paste code below into expression box
getSinuosity(!Shape!)
#Remove sinuosity values <=1.01 (straight lines)

###########################################################

# R 4.3.3: Weight of Evidence (WOE) and Importance Value (IV) for selecting optimal variables to use in SDM
library(Information)
#include presence and background locations (as 1,0) and covariate data in the same file
mydata <- read.csv ("locdata.csv", header = T)

#Compute IV, default bins=10
IV <- create_infotables(data=mydata, y="presence")

#IV summary of variables
IV$Summary

#WOE and IV tables of all variables
IV

###########################################################

# Correlation tests 
mydata <- read.csv ("allvars.csv", header = T)

#continuous variables - Pearson correlation matrix
cor(mydata)

#continuous vs categorical - ANOVA correlation ratio; test each continuous (cont) variable with the categorical (cat) variable
model <- lm(cont1 ~ cat1,data=mydata)
anova_res <- anova(model)
eta_sq <- anova_res$`Sum Sq`[1] / sum(anova_res$`Sum Sq`); eta_sq

###########################################################

# Create background points with sample bias included (i.e., generate similar distribution to presence locations)
#Limit distribution to landforms 6-10 (potential stream valleys) -->
#Proportionately distribute nearer to roads; example below is distribution of presence locations to roads, then computed a similar proportion of background locations
dis m	pres	prop	bkgd
100	36	0.27	2,727
400	54	0.41	4,091
>400	42	0.32	3,182

# ArcGIS Pro 3.3.2:
#Buffered NAVTEQ roads by the 3 distance classes -->
#Intersected buffered road polygon with landforms 6-10 polygon -->
#Dissolved the polygon by road class distances (buffer) field and populated with the 'bkgd' values above
#Used Create Random Points tool to generate 10,000 random points constrained to the intersected polygon using the field with bkgd values to stratify randomization

###########################################################

# R 4.3.3: ENMeval for optimal parameter selection in SDM (Maxent)
library(ENMeval)

#Run ENMeval without rasters; must include same covariate names in occurrence and background data files
mydata <- read.csv("occurrences.csv",header = T); occ= as.data.frame(mydata)
mydata2 <- read.csv("background_points.csv",header = T) ; bgd= as.data.frame(mydata2)

#use set.seed for reproducibility; must identify all categorical variables
set.seed(1557);eswd <- ENMevaluate(occ, bg=bgd, algorithm = "maxnet", tune.args = list(fc = c("L", "Q", "H", "P", "LQ", "LH", "LP", "QH", "QP", "PH, "LQH", "LQP", "LHP", "PQH, "LQHP"), 
	rm = 0.5:3.5),categoricals = c("cat1", "cat2"), method="randomkfold",clamp = TRUE)
eswd@results

#Based on lowest AIC, the optimal regularization multiplier (rm) and feature class (L,Q,H,P; or a combination) was input into the Maxent 3.4.1 software package; used 10-fold cross validation (all other parameters were default).

###########################################################

# R 4.3.3: Data fit (threshold dependent) to binary SDM output
#Create a confusion matrix (observed vs. predicted) based on binary model output from Maxent and occurrence (pres) and background (abs) locations
         		  observed
#			pres	abs
#    predicted 	pres	a	b
# 		abs	c	d

#Define a-d; example data
a=123; b=541; c=9; d=9459
#Sensitivity 
s=a/(a+c);s
#Specificity
sp=1-(b/(b+d));sp
#TSS
tss=s+sp-1;tss
#SEDI
F=b/(b+d)
sedi=(log(F)-log(s)-log(sp)+log(1-s))/(log(F)+log(s)+log(sp)+log(1-s));sedi
#Accuracy
acc=(a+d)/(a+b+c+d);acc
