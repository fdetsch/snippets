# List available state_1km layers
tmp.fls <- list.files("MODIS_ARC/PROCESSED/", recursive = TRUE, 
                      pattern = "MYD09GA.*state_1km", full.names = TRUE)[1:64]

# Crop raster data
tmp.rst <- foreach(j = tmp.fls, .packages = lib) %dopar% {
  crop(raster(j), template.rst.utm, 
       filename = paste0("myd09ga/processed/CRP_", basename(j)),  
       overwrite = TRUE)
}

# Disaggregate raster data with coarser resolution
tmp.rst <- foreach(j = tmp.rst, .packages = lib) %dopar% 
  disaggregate(j, fact = 4, 
               filename = paste0("myd09ga/processed/DAG_", names(j)), 
               format = "GTiff", overwrite = TRUE)
  
# Stack and return cropped RasterLayers
state_1km <- stack(tmp.rst)

# Aggregate to 8 days
indices <- rep(1:(nlayers(state_1km)/8), each = 8)

state_1km_agg <- stackApply(state_1km, indices, fun = function(x, ...) {
  index <- sapply(as.numeric(x), function(i) {
    if (!is.na(i)) {
      bit <- number2binary(i, 16)
      
      state <- paste(bit[c(15, 16)], collapse = "") %in% c("00", "11", "10")
      # shadow <- bit[14] == 0
      cirrus <- paste(bit[c(7, 8)], collapse = "") %in% c("00", "01")
      intcl <- bit[6] == 0
      snow <- bit[4] == 0
      adjcl <- bit[3] == 0
      
    } else {
      state <- cirrus <- intcl <- snow <- adjcl <- FALSE
    }
    
    return(all(state, cirrus, intcl, snow, adjcl))
  })
  
  if (any(index, ...)) return(1) else return(0)
}, filename = "myd09ga/processed/AGG", bylayer = TRUE, 
suffix = names(state_1km)[seq(1, nlayers(state_1km), 8)], format = "GTiff", 
overwrite = TRUE)