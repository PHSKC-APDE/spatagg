# A script to assign BRFSS respondents to a HRA and KC Council District
library('sf')
library('data.table')
library('spatagg')
library('kcparcelpop') #this might need updating in the future

set.seed(1)

# Load brfss ----
kc0021 <- readRDS("//dphcifs/APDE-CDIP/BRFSS/prog_all/kc0021.rds")
kc0021[, id := .I]
brfss = kc0021[, .(id, zipcode)]

# Load shapefiles ----
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/HRA_2010.shp")
zip = st_read("//dphcifs/APDE-CDIP/Shapefiles_protected/ZIP/adci_wa_zip_confidential.shp")
zip = st_transform(zip, st_crs(hra))
ptpop = st_transform(kcparcelpop::parcel_pop, st_crs(hra))

# assign each parcel to a ZIP code
ptpop = st_join(ptpop, zip)
ptpop = subset(ptpop)
# randomly assign a parcel based on ZIP
# ideally this would also be year based
# but we'll leave that for later
assign_xy = function(x, zip, pts){
  pts = pts[pts$POSTCODE %in% zip,]
  pts = subset(pts, !is.na(pop) & !is.na(POSTCODE))
  
  if(nrow(pts)==0){
    coords = matrix(NA_real_, nrow = length(x), ncol = 2)
    colnames(coords)  <- c('X', "Y")         
  }else{
    link = sample(seq_len(nrow(pts)),size = length(x),replace = TRUE, prob = pts$pop/sum(pts$pop))
    
    coords = st_coordinates(pts)[link,] 
  }
  

  
  return(coords)
  
}
brfss =split(brfss, by = 'zipcode')

brfss = lapply(names(brfss), function(b){
  # print(b)
  cbind(brfss[[as.character(b)]], assign_xy(seq_len(brfss[[as.character(b)]][, .N]), b, ptpop))
  
})

brfss = rbindlist(brfss)
