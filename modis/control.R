### 'MOD16A2'

## add 'MOD16A2' to list of featured products
MODIS:::addProduct(product = "MOD16A2", sensor = "MODIS", platform = "Combined", 
                   pf1 = "MOD16", pf2 = "MOD", res = "1000m", temp_res = "8 Day", 
                   topic = "Global Terrestrial Evapotranspiration", 
                   server = "NTSG", overwrite = TRUE)

getProduct()
getProduct("MOD16A2")

product <- getProduct("MOD16A2", quiet = TRUE)
product

## add 'MOD16A2' server to list of remote servers
MODIS:::addServer(name = "NTSG", sensor = "MODIS",
                  basepath = "ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/",
                  varpath = "PF1/PRODUCT.CCC/YYYY/DDD/", overwrite = TRUE)

# print information about 'ntsg' server to console
MODIS:::MODIS_FTPinfo[[7]]

## add available 'MOD16A2' product collections
MODIS:::addCollection(product = "MOD16A2", collection = 105, overwrite = TRUE)

getCollection("MOD16A2")
getCollection("MOD16A2", forceCheck = TRUE)

## data download
MODISoptions("/media/fdetsch/FREECOM_HDD/MODIS_ARC/", 
             "/media/fdetsch/FREECOM_HDD/MODIS_ARC/PROCESSED/")

orgStruc(move = TRUE)

getHdf("MOD16A2", tileH = 21, tileV = 9, end = "2013-12-31")


### 'MOD16A3'

## add 'MOD16A3' to list of featured products
MODIS:::addProduct(product = "MOD16A3", sensor = "MODIS", platform = "Combined", 
                   pf1 = "MOD16", pf2 = "MOD", res = "1000m", temp_res = "Yearly", 
                   topic = "Global Terrestrial Evapotranspiration", 
                   server = "NTSG", overwrite = TRUE)

# build & reload, then
product <- getProduct("MOD16A3", quiet = FALSE)
product

## add available 'MOD16A3' product collections
MODIS:::addCollection(product = "MOD16A3", collection = 105, overwrite = TRUE)

getCollection("MOD16A3")
getCollection("MOD16A3", forceCheck = TRUE)

## data download
getHdf("MOD16A3", tileH = 21, tileV = 9, end = "2014-12-31", forceDownload = TRUE, 
       wait = 60)

runGdal("MOD16A3", tileH = 21, tileV = 9, end = "2014-12-31", 
        job = "modis_et_annual", SDSstring = "10001", 
        outProj = "+init=epsg:21037")
        )