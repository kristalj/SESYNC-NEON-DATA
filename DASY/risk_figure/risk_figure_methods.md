# Methods for case study: how does the dasymetric method affect our inference about environmental hazards?

For the case study, we explored how the method used to dasymetrically estimate population distribution might affect policy-relevant inference. We investigated two key environmental hazards: wildfire and coastal flooding. Raster data products are available for both wildfire and coastal flooding. The U.S. Forest Service produced a map of wildfire hazard potential (WHP) for the contiguous United States at 270-meter pixel resolution, with five risk categories. The U.S. Federal Emergency Management Agency (FEMA) has released flood risk data products for many U.S. counties, including the water surface elevation (WSE) for a 1% flood event (expected to occur once every 100 years). The WSE product is provided at 10-meter pixel resolution.

For each of the two hazard categories, we compared estimation methods from four different sources: the present study, the U.S. EPA (cite), Huang et al. (cite), and Facebook (cite). In addition, we compared all the methods to a fifth baseline method: assuming that individuals are evenly distributed across the entire geographical area of a Census block group (cite the methods comparison paper).

## Selection of counties for case study

For the wildfire case study, we took a random sample, stratified by population, of 15 counties in the eleven western states of the contiguous U.S. (WA, OR, CA, ID, NV, MT, WY, CO, UT, AZ, NM), sampling three counties in each population quintile. We chose five counties to display in the final visualization that have sufficient spatial variation in wildfire risk to differentiate between the population methods. For the flood case study, we took a population-stratified random sample from all counties bordering a coastline in the contiguous U.S. However, FEMA does not provide flood risk data products for all of these counties, so we continued sampling until we had ten counties for which flood risk data products were available. Again, we chose five to display that maximize differentiation between the population methods.

## Data sources

We obtained the WHP raster product for the entire contiguous United States, and the 1% flood event WSE product for each of the counties in the case study. We obtained the gridded population estimates for the three comparison methods as raster layers covering the contiguous United States. Finally, we obtained the population estimates for 2016 from the American Community Survey for each of the counties chosen for the case study, as well as the boundaries of each block group as a polygon layer. Locations where we downloaded each data product are in (supplemental table x).

## Initial raster processing

We clipped the WHP raster layer and the population raster layers (U.S. EPA, Huang et al., and Facebook) to the extent of each county in the case study. The water surface elevation rasters were already provided at the single county level. For simplicity, we converted both environmental raster layers to binary form (i.e., at risk and not at risk). For the wildfire layer, we treated all pixels in the medium, high, and very high risk categories as being at risk, and the remainder as not at risk. For the flooding layer, we treated all pixels with water surface elevation > 0 as being at risk. Next, we converted the wildfire and flooding rasters to polygons by merging all adjacent pixels with the same value into a polygon. Finally, we transformed these polygon layers into the coordinate reference system of each of the population rasters.

## Finding population totals in each risk category

We overlaid the polygonized wildfire and flood layers onto the population raster layers for each dasymetric estimation method. For each wildfire and flood polygon, we summed the counts of individuals in all pixels of the population raster contained within that polygon, then calculated the grand totals for each risk category in each county.

For the block group population polygons, we calculated the areas of intersection of each block group polygon with each wildfire or flood polygon. We multiplied the population total of the block group polygon by the proportional area of overlap of each environmental risk polygon to yield the population total at risk within each block group, then calculated the grand totals for each county.

# Additional notes

- get the data: other population dasymetric estimates (Huang, FaceBook, and EPA)
- get the data: environmental rasters (for whole USA WHP, for individual counties FEMA WSE)
- take a stratified random sample of the counties of interest, stratified by population.
- clip population rasters and environmental rasters to study area boundaries (we're using a few selected counties). 
- convert the environmental rasters to polygons, merging all adjacent pixels with the same class
- for each polygon, sum the dasymetric population pixels in that polygon to get numbers of people in that risk category
- sum across risk classes to get a histogram for each county
- compare the histograms for each county side-by-side to see if there is a difference in inference between population estimates

For the first example I am using the Forest Service's wildfire hazard map that classifies the contiguous USA into 5 fire risk categories, at 270-meter pixel size. For the second example I am using the flood risk maps provided by FEMA, which include the flood water surface elevation (WSE) for a 100-year flood event at a 10-meter pixel resolution.

I took the SRS of counties in a separate script. It isn't too important which counties are used because this is just illustrative. For the wildfire risk, I took all counties in the eleven Western states (WA, OR, CA, ID, NV, MT, WY, CO, UT, AZ, NM) and divided them up by population quintile (5 groups). I selected three random counties from each quintile. I did the same for flood risk except I used only coastal states in the lower 48 (both Atlantic and Pacific coasts). I had to sample quite a few counties to get to 10 counties that actually had flood risk rasters available from FEMA, so they are not perfectly divided among the quintiles. But there is a decent spread of population.

The wildfire risk map is available for the entire contiguous USA but the FEMA flood risk maps are only available for some of the counties. I went to the FEMA site and searched each county manually to get the data. Many coastal counties do not have the water surface elevation for 100-year flood event data product available in GeoTIFF format --- it's also not a random sample because there are a good number of coastal states that do not have the product for any county. After going through a lot of counties, I got data for ten counties which should be plenty for our case study.
