#---
# title: "R Notebook 6 - Climate policy scenarios (SO2 injection)"

# Tom Pugh
# 11.12.19

# Peter Hopcroft
# 09.20
#---

# The aim of this practical is to allow you to explore the climate mitigation policy that you have chosen to investigate in this module. This exploration is fundamental to everything that follows in the coming weeks and to your main coursework - if you are to understand the implications of a policy, you must first understand what that policy involves.

# This practical relys on the skills you developed during R Notebooks 2 and 3.


###--- Exploring the effects of SO2 injection  ---

# SO2 injection directly affects the climate, so to understand the impacts of the policy on the Earth system you should directly assess the impact on different climate variables. In the "Policy_scenarios/SO2_injection" folder you will find results from the GeoMIP G3 simulation (see associated reading material for more information about this experiment). The climate model results here are calculated and presented comparably to those you looked at in Notebook 4. As the G3 scenario is an RCP4.5 scenario with added SO2 injection, you should compare the climate variables against those for RCP4.5 in "ESM_simulations/rcp45". This builds directly on the work you did looking at climate variables in Notebook 4.

# Begin with reading in the temperature for 2040-2069 (if you're wondering why this time period, check the protocol for the G3 experiment) for both the standard RCP 4.5 and the G3 experiment. You can mostly copy and paste the code developed in Notebook 4.

# Load HadGEM2-ES simulation of RCP85 (the 'business-as-usual' scenario)

# uncomment these two lines if these packages are not working:
# install.packages("rworldmap")
# install.packages("RColorBrewer")
library(rworldmap)
data(coastsCoarse)
library("ncdf4")
library("fields")
library("RColorBrewer")

setwd("~/shared/data/ESM_simulations/rcp85/HadGEM2-ES/annual_means") 

file_tas_rcp85_2040_2069 <- nc_open('tas_HadGEM2-ES_rcp85_r1i1p1_ext2040-2069_ann_mean_1deg.nc')
lat_hadgem=ncvar_get(nc=file_tas_rcp85_2040_2069, varid='lat')
lon_hadgem=ncvar_get(nc=file_tas_rcp85_2040_2069,varid='lon')
tas_rcp85_2040_2069 = ncvar_get(nc=file_tas_rcp85_2040_2069, varid='tas')

# Now get the same years from the SO2 injection scenario:
setwd("~/shared/data/Policy_scenarios/SO2_injection/G3/HadGEM2-ES/annual_means") 
file_tas_g3_2040_2069 <- nc_open('tas_Amon_HadGEM2-ES_G3_r1i1p1_201912_208912_annual_ext2040-2069_mean_1deg.nc')
tas_g3_2040_2069 = ncvar_get(nc=file_tas_g3_2040_2069, varid='tas')

# Make a difference map of temperature between the two experiments

tas_anom=tas_g3_2040_2069 - tas_rcp85_2040_2069

tas_anom_flip=tas_anom[,180:1]
lat_hadgem_flip=lat_hadgem[180:1]
image.plot(lon_hadgem, lat_hadgem_flip, tas_anom_flip, col = rev(brewer.pal(9,"RdBu")), xlab="", ylab="", main="Mean surface temperature, HagGEM2-ES, RCP 8.5, 2070-2099", legend.lab="K", legend.line=4, legend.mar=7)
plot(coastsCoarse,add=T)

# There are still blues for warming, so let's change the extent of the scale bar by adding zlim=c(-5,5):

image.plot(lon_hadgem, lat_hadgem_flip, tas_anom_flip, col = rev(brewer.pal(9,"RdBu")), xlab="", ylab="", main="Mean surface temperature, HagGEM2-ES, RCP 8.5, 2070-2099", legend.lab="K", legend.line=4, legend.mar=7,zlim=c(-5,5))
plot(coastsCoarse,add=T)


# What is the difference in global mean temperature as a result of the SO2 forcing? To get a very rough estimate you could simply find the mean of the difference array that you have created above, using the statistics functions from Notebook 3.
global_temp_diff_approx = mean(tas_anom_flip) 

# Print this to the R terminal:
print(global_temp_diff_approx)

#Change the name of the array as appropriate
# However a catch is that this is a mean over grid cells and grid cell size changes with latitude - so the mean above over emphasises the effect of temperature changes at high latitudes on the global mean. Therefore, you need to weight by the grid cell area. We have provided a file with pre-calculated grid cell areas.

setwd("~/shared/data/")
library("ncdf4")
areamaskfile <- nc_open("gridcell_areas_1deg.nc")
print(areamaskfile) 
gridcell_area = ncvar_get(areamaskfile, 'area') 
image(gridcell_area)

# You can see that as you go to the poles, the gridcell area gets much larger.

# If you first multiply your difference array by area, then find the mean, and then divide by mean grid-cell area then you will get a mean weighted by grid-cell area, e.g.
global_temp_diff = mean(tas_anom_flip * gridcell_area) / mean(gridcell_area)
print(global_temp_diff)

# In your difference map you will see that there is a lot of spatial variation in the temperature difference caused by SO2 injection. A histogram would be a useful way to look at the global variation of this (using the hist() function, see Notebook 3).

# e.g.:
hist(tas_anom_flip, col='black', main='', xlab='temperature difference (C)')

# Going back to the difference map:
image.plot(lon_hadgem, lat_hadgem_flip, col = rev(brewer.pal(9,"RdBu")), tas_anom_flip,zlim=c(-5,5),main="Temperature anomaly")
plot(coastsCoarse,add=T)


# You may also want to subset some regions of interest with particularly high or low temperature anomalies using the subsetting procedures from Notebook 3 and look at stats over these regions. You may end up choosing to use these as focus regions for your coursework.

# e.g. 

australia_tas_anom = tas_anom_flip[290:335 , 45:80]
# Make a quick plot of the data.
image.plot(asp=1,lon_hadgem[290:335], lat_hadgem_flip[45:80], col = rev(brewer.pal(9,"RdBu")), australia_tas_anom,zlim=c(-5,5),ylim=c(-90,0),main="Temperature anomaly")
plot(coastsCoarse,add=T)

# Once you have explored temperature differences then look at other variables. There are a wide range of climate-relevant variables provided for the GCM simulations. Precipitation is an obvious one to look at, but other variables (e.g. radiation-related) may also be interesting. Let your reading and curiosity guide you! Remember that the key for the variable names for the GCM simulations is in the data catalogue pdf. As always, write your code in this notebook so that you always have a copy of what you have calculated.

# pr - total precipitation (kg per metre squared per second)  (convert to mm/day by multiplying by 86400.0)
# clt - total cloud cover %
# huss - surface specific humidity (kg water per kg air)
# rsds - dowards short-wave (solar) radiation at the land surface (W per metre squared)
# rsut - upwards short-wave (solar) radiation at the top of the atmosphere (W per metre squared)
# rsus - upwards short-wave (solar) radiation at the top of the land surface (W per metre squared)

# See the end of practical 4:RCP Scenarios for example of reading in the precipitation fields. You'll need to substitute e.g. RCP85 with the G3 geoengineering scenario above.

# Every .nc file in the data catalogue has an associated plot with the same file name in .pdf format. You can look at these to ascertain the typical scale you might need.


# Now we'll briefly look at carbon cycle changes in response to drivers similar to these:
 
# the code below repeats some material from The GLobal Carbon Cycle practical. This will serve you as a basis for exploring the potential impacts of the sulphate geoengineering on the carbon cycle.

library("ncdf4")
library("fields")
library("RColorBrewer")
setwd("~/shared/data/DGVM_simulations/Natural_vegetation_everywhere/baseline") 
file_baseline <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
lat_lpjg=ncvar_get(nc=file_baseline, varid='latitude')
lon_lpjg=ncvar_get(nc=file_baseline,varid='longitude')
cveg_baseline_pnv = ncvar_get(nc=file_baseline, varid='CVeg')

setwd("~/shared/data/DGVM_simulations/Natural_vegetation_everywhere/CO2plus150") 
file_co2plus150 <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
cveg_co2plus150_pnv = ncvar_get(nc=file_co2plus150, varid='CVeg')

del_cveg_co2plus150_pnv<-cveg_co2plus150_pnv - cveg_baseline_pnv
image.plot(lon_lpjg, lat_lpjg, del_cveg_co2plus150_pnv,main="Veg C",sub="CO2plus150-baseline", col = rev(brewer.pal(9, "RdBu")))


#Now repeat for Pasture for agricultural areas:
setwd("~/shared/data/DGVM_simulations/Pasture_everywhere/baseline") 
file_baseline <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
cveg_baseline_pasture = ncvar_get(nc=file_baseline, varid='CVeg')

setwd("~/shared/data/DGVM_simulations/Pasture_everywhere/CO2plus150") 
file_co2plus150 <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
cveg_co2plus150_pasture = ncvar_get(nc=file_co2plus150, varid='CVeg')

del_cveg_co2plus150_pasture<-cveg_co2plus150_pasture - cveg_baseline_pasture
image.plot(lon_lpjg, lat_lpjg, del_cveg_co2plus150_pasture,main="Veg C",sub="CO2plus150-baseline", col = rev(brewer.pal(9, "RdBu")),asp=1)



# WHAT ABOUT SOIL? You can also estimate changes in soil carbon stocks in the same way. 
# Hint: soil carbon is stored in similar files with varid ='Csoil' and the vilename is 
# lpj-guess_CSoil_annual_1970_1999_mean.nc4


# WHAT ABOUT RADIATION? Another interesting question would be whether the change in solar radiation has a substantial effect on the carbon cycle. Again you have LPJ-GUESS sensitivity simulations to address this. To assess the magnitude of the changes, you can compare the rsds outputs from the ESMs in the same way as you did for temperature and precipitation in Practical 6.

# You can look at how the radiation impacts vegetation and soil carbon by examining the simulations:
# radminus2 - minus 2% from incoming radiation
# radminus5 - minus 5% from incoming radiation


# You may ask yourself, how good is the model in the first place? If you want to answer that question, consider comparing the baseline model outputs against the observation-based estimates (note that these also have large inherent errors).

# This can be done by comparing the baseline simulation against e.g.
# GPP or NPP or Soil Carbon  .nc files in the Historical_global_fields folder



#--- Regional winners and losers ---

# Although global mean radiative forcing is kept constant in this policy scenario, leading to a stabilising global temperature, as you will have seen in Practical 6, regional climates may still change substantially. To what extent might a change in regional climate affect the carbon sink calculations you have just made based on CO2? Pick one or more regions of the globe where you found substantial changes in climate and suspect that there might be a relevant impact on the carbon cycle. Then perform calculations analogous to those above, but for changes in temperature and/or precipitation.


#--- Taking this forward and finishing up ---

# The above is not prescriptive. Don't feel limited by it. If you have another idea for something related to this topic you want to calculate the effect of, give it a try - if you can't find the data you need, ask (we will not always be able to provide additional datasets, but sometimes it may be possible).

# Once you have finished, summarise what you have found out - this feeds directly into your main report. What have you learnt about uncertainties and effects of climate and CO2 relevant to your particular policy scenario? What are the caveats and assumptions that apply to your calculations? How does this affect how you interpret them (if at all)?



