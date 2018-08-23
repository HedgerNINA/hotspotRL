# hotspotRL
PROJECT DETAILS:

Project title: Geografisk utbredelse av trua arter i norge

Project leader: Siri Lee Olsen

Project owner: NINA

Project lifespan: June-December 2018

Project funding: Miljødirektoratet




OVERVIEW:
This document provides a description of the data collection and preparation. The method is detailed in the additional scripts (written in R). All data is projected as EPSG 25833 (ETRS 89 UTM zone 33N).




INDEX:
1. Basic variables

        1.1 GBIF red list species
  
        1.2 SSB grids
  
        1.3 Protected areas
  
        1.4 Water
  

2. Anthropogenic datasets

        2.1 Distance to road
  
        2.2 Distance to railway
  
        2.3 Distance to port
  
        2.4 Distance to river
  
        2.5 Distance to settlement
  
        2.6 Distance to herbarium
  
        2.7 Population density
  
        2.8 Income
  
        2.9 Land use
  

3. Biophysical datasets

       3.1 Rainfall
  
       3.2 Temperature
  
       3.3 Frost/snow
  
       3.4 Date of spring arrival
  
       3.5 Height above sea level
  
       3.6 Lime content in soil
  
       3.7 Solar radiation
  
       3.8 NDVI
  
  
  
  
  SECTION 1:
  1.1 GBIF red list species
  
  - Data source: GBIF data was downloaded from Artsdatabanken (https://artsdatabanken.no/Pages/233748) on 18 July 2018 as shapefiles.
  - Data preparation: As we were only interested in red listed species, only the CR (critically endangered), EN (endangered) and DD     (data deficient) datasets were included (no data was available for VU and NT categories). Data were filtered to only include plants (kingdom = plantae) that had been collected in the last 10 years (identification year >= 2008). A total of 2079 observations fit these criteria.
  
  
  1.2 SSB grids
  
  - Data source: The 1km and 5km grids were downloaded from Statistics Norway (https://www.ssb.no/natur-og-miljo/geodata#SSBs_kartportal_kartssbno) on 18 July 2018 as shapefiles. 
  - Data preparation: No preparation was necessary
  
  
  1.3 Protected areas
 
 - Data source: The boundary shapefiles of protected areas (Naturverneområde) and proposed protected areas (Foreslåtte naturvernområder) were downloaded from Miljødirektoratet (https://kartkatalog.miljodirektoratet.no/Dataset/) on 21 August 2018.
 - Data preparation: No preparation was necessary
 
 
 1.4 Water
 
 - Data source: Three additional files were supplied to mask water bodies and the sea. These were lakes and rivers (original data can be downloaded from NVE https://nedlasting.nve.no/gis/) and the administrative boundaries of Norway.
 - Data preparation: No preparation was necessary
 
 
 
 
 SECTION 2:
  2.1 Distance to road
  
  - Data source: Norway's road network was sourced from internal NINA data (R:\GeoSpatialData\TransportNetworks\Norway_Roads\Processed\Elveg_Norge.shp). This dataset is created by Kartverket and available to download from https://kartkatalog.geonorge.no/metadata/kartverket/elveg/ed1e6798-b3cf-48be-aee1-c0d3531da01a
  - Data preparation: The road shapefile was converted to raster format before the distance function was run in R studio. As this was such a heavy dataset and we were not able to achieve results after the script had been running for a week, the "Euclidean distance" tool in the ArcGIS spatial analyst toolbox was used. It took around 3 minutes to produce the distance raster with 100m spatial resolution. 
    
    
  2.2 Distance to railway
  
  - Data source: The railway network is derived from the FKB (Felles kartdatabase) dataset produced by the Norwegian mapping authority. While the data was already available on the NINA server (GISDATA/Topography/Norway_FKB_bane_line) , the original data can be downloaded from https://kartkatalog.geonorge.no/metadata/geovekst/fkb-bane/3165138f-1461-44fe-8b10-eac44e08a10a
  - Data preparation: The railway shapefile was converted to raster format and the distance function run in R studio.
  
  
  2.3 Distance to port
  
  - Data source: A shapefile containing the location of all ports in Norway was downloaded from https://kystinfo.no/ on 7 August 2018.
  - Data preparation: The port shapefile was converted to raster format and the distance function run in R studio.
  
  
  2.4 Distance to river
  
  - Data source: A shapefile of the rivers in Norway was downloaded from Norges vassdrags- og energidirektorat (https://nedlasting.nve.no/gis/) on 5 August 2018.
  - Data preparation: The river shapefile was converted to raster format before the distance function was run in R studio. As this was such a heavy dataset and we were not able to achieve results after the script had been running for several days, the "Euclidean distance" tool in the ArcGIS spatial analyst toolbox was used. It took around 3 minutes to produce the distance raster with 100m spatial resolution. 
  
  
  2.5 Distance to settlement
  
  - Data source: A shapefile containing the boundaries of settlements (all towns, villages and cities) in Norway was downloaded from the SSB website (https://www.ssb.no/natur-og-miljo/geodata#Nedlasting_av_rutenettsstatistikk) on 5 August 2018.
  - Data preparation: The settlements shapefile was converted to raster format and the distance function run in R studio.
  
  
  2.6 Distance to herbarium
  
  - Data source: The exact location of herbariums in Norway is not available as spatial data so the cities where universities with the known herbariums are were used as a proxy. These were Oslo, Bergen, Trondheim, Tromsø and Kristiansand. The location of the cities was derived from the settlements dataset available from SSB.
  - Data preparation: The herbaria shapefile was converted to raster format and the distance function run in R studio.
  
  
  2.7 Population density
  
  - Data source: Population density data is available in shapefile format from Statistics Norway (https://www.ssb.no/natur-og-miljo/geodata#Nedlasting_av_rutenettsstatistikk). Data was downloaded at the 1km and 5km resolution on 18 July 2018.
  - Data preparation: The shapefiles were converted to raster format.
  
  
  2.8 Income
  
  - Data source: Income data is not readily accessible and was therefore omitted from this project.
  
  
  2.9 Land use
  
  - Data source: The FKB AR5 land use map was already available on the NINA server, but can be downloaded from https://kartkatalog.geonorge.no/metadata/uuid/166382b4-82d6-4ea9-a68e-6fd0c87bf788. 
  - Data preparation: The AR5 land use map had already been extracted to the SSB 1km grid in a previous project (IAS hotspots). The 'extract' function (with fun=majority)in R (raster library) was used to do the same for the 5km grid. Both datasets were rasterized.
  
  
  
  
  
  SECTION 3:
  3.1 Rainfall
  
  - Data source: Rainfall data were already available from a previous NINA project (IAS hotspots). We thank Jens Åström for sharing this data. The average total annual rainfall over a 30 year period was calculated based on weather data downloaded from https://www.met.no/. 
  - Data preparation: A detailed description of how the data were prepared is available here: https://github.com/NINAnor/hotspotIAS/blob/master/data/data_prep.pdf. Data were already available for the 1km SSB grid and were extracted to the 5km SSB grid in R studio.
  
  
  3.2 Temperature
  
  - Data source: Temperature data were already available from a previous NINA project (IAS hotspots). We thank Jens Åström for sharing this data. The average yearly mean temperature over a 30 year period was calculated based on weather data downloaded from https://www.met.no/. 
  - Data preparation: A detailed description of how the data were prepared is available here: https://github.com/NINAnor/hotspotIAS/blob/master/data/data_prep.pdf. Data were already available for the 1km SSB grid and were extracted to the 5km SSB grid in R studio.
  
  
  3.3 Frost/snow
  - Data source: Global snowPack data (Dietz et al. 2015, Remote Sensing Letters 6:844-853) had been obtained by Jane Jepsen (NINA) from the DLR with a NINA license agreement (R:\Prosjekter\15801000_geografisk_utbredelse_av_trua_arter_i_norge\GIS\Biophysical_variables\Inputs). The dataset included the mean snow coverage duration (days) which was used in this project.
  - Data preparation: The mean snow coverage duration (days) was extracted for the 1km and 5km SSB grids in R studio.
  
  
  3.4 Date of spring arrival
  
 - Data source: The onset of spring was calculated by Torkild Tveraa and Jens Åström and was already available on the NINA server. The dataset is based on MODIS NDVI data. 
 - Data preparation: A detailed description of how the data were prepared is available here: https://github.com/NINAnor/hotspotIAS/blob/master/data/data_prep.pdf. Data were already available for the 1km SSB grid and were extracted to the 5km SSB grid in R studio.
  
  
  3.5 Height above sea level
  
  - Data source: A 100m digital elevation model (DEM) was already available on the NINA server. This data was derived from the 10m DTM mapped by kartverket and available for download from: https://kartkatalog.geonorge.no/metadata/kartverket/dtm-10-terrengmodell-utm33/dddbb667-1303-4ac5-8640-7ec04c0e3918
  - Data preparation: The mean altitude was extracted for the 1km and 5km SSB grids in R studio.
  
  
  3.6 Lime content in soil
  
  - Data source: The lime content of soil was already available on the NINA server and consists of 3 classes representing calciferous content. The data is derived from the 'berggrunnskart' from NGU.
  - Data preparation: A detailed description of how the data were prepared is available here: https://github.com/NINAnor/hotspotIAS/blob/master/data/data_prep.pdf. Data were already available for the 1km SSB grid and were extracted to the 5km SSB grid in R studio.
  
  
  3.7 Solar radiation
  
  - Data source: Solar radiation data was downloaded from WorldClim (http://worldclim.org/version2) at 30 seconds resolution (~1km2). The data are monthly averages from 1970-2000.
  - Data preparation: The monthly solar radiation was extracted for the 1km SSB grid in R studio. The annual mean was calculated and extracted to the 5km grid as well.
  
  
  3.8 NDVI
  
  - Data source: MODIS NDVI data was downloaded from LP DAAC (https://lpdaac.usgs.gov/tools/data_access/daac2disk_web) for Norway (tiles h18 v1-3 and h19 v1-2) for 2017. MOD13A3 data were selected which are monthly composite vegetation indices at 1km resolution. 
  - Data preparation: An annual average NDVI value was calculated and extracted to the 1km and 5km SSB grids in R studio. 
  
