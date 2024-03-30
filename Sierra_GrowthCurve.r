################################################################################
####                   Reproducibility code for:                             ###
##        Total carbon accumulation in a tropical forest landscape            ##
# Authors: Carlos A. Sierra, Jorge I. del Valle, Hector I. Restrepo            #
##############################################################     08/2012
# Submitted to Carbon Balance and Management
################################################################################
# Modified by MRU 04/07/2024 to keep only AGB predictions

# Import reference dataset:
TC = read.csv("./PorcedB2012.csv") #set to location where accompanying data is stored

## Estimate AGB from all the pools:
TAGB = 0.45*(TC$AGB+TC$Palm+TC$HV)
Age = TC$Age

## Plot the reference data (AGB as a function of Age):
plot(TC$Age,TC$AGB, ylab="Tree aboveground biomass", xlab="Age (years)",pch=19)

# Fit a Von Lamberty function to the data (find the best parameters):
TAGB.m = nls(TAGB~(248*0.45)*(1-exp(-b1*Age))^b2, start=list(b1=0.05,b2=1)) #The fitted model for total aboveground biomass

# Implement models as function of time
TAGBf <- function(t){(248*0.45)*(1-exp(-coef(TAGB.m)[1]*t))^coef(TAGB.m)[2]}

# Run the function to get predicted AGB:
x = c(0:100)  # A vector with time
TAGB.fit = TAGBf(x) # Predicted AGB

# Plot the modeled AGB:
plot(TC$Age, TAGB,
     xlim=c(0,100),
     ylim=c(0,200),
     xlab="Successional age (years)",
     ylab=expression(paste("TAGB (Mg C ",ha^-1,")")))
lines(x,TAGB.fit)

# Organize your modeled C in AGB and BGB, total C and total CO2e:
treeC_df <- data.frame(agbC = TAGB.fit[1:41],
                       bgbC = TAGB.fit[1:41] * 0.42) %>%
  mutate(treeC = agbC + bgbC) %>%
  mutate(treeCO2e = treeC * (44/12))
treeC_df
## Export it:
write_csv(treeC_df, 
          file = "/Users/rosariouribe_climatefocus/Library/CloudStorage/OneDrive-ClimateFocus/Projects/2024/Mirova_ARR_DD/Data/1_processeddata/predict_ctree_naturere_SierraFit.csv")

