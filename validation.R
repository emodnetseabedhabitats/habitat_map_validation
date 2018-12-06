## Create dataframe of column names, types, tests and whether they're (a) essential or (b) present in the DEF -----
colval <- data.frame(colnm = c('GUI','POLYGON','ORIG_HAB','ORIG_CLASS','COMP','COMP_TYPE','HAB_TYPE','VERSION',
                               'DET_MTHD','DET_NAME','DET_DATE','TRAN_COM','T_RELATE','VAL_COMM','ANNEXI',
                               'SUBTYPE','CONFIDENCE','UUID','AVAILABLE','SUM_CONF'),
                     esntl = c('OH,TH,HD,SA','OH,TH,HD','OH TH','','OH,TH','OH,TH','TH','TH','','TH','TH','',
                               'TH','','HD','','HD','SA','SA','SA'),
                     prest = c('OH,TH,HD,SA','OH,TH,HD','OH,TH','OH,TH','OH,TH','OH,TH','TH','TH','TH','TH',
                               'TH','TH','TH','TH','HD','HD','HD','SA','SA','SA'),
                     class = c('factor','integer','factor','factor','factor','factor','factor','factor',
                               'factor','factor','Date','factor','factor','factor','integer','factor',
                               'factor','factor','factor','integer'),
                     value = c("^[A-Z]{2}[0-9]{6}$", #has two alpha chars then 6 numbers - must be the same for all features
                               "", #na
                               "\\S", #is not whitespace, can repeat
                               "", #na
                               "primary|secondary|unknown|^(([0](\\.([0-9])+)?)?)$|^[1]$", #either primary, secondary, unknown, or proportion
                               "^((single habitat)|(heterogeneous)|(transition)|(data inconclusive)|(no information))$", #one of the 5 options
                               "^([A-HX]([1-9](\\.([1-9]|[A-Z])+)?)?)$", #is a letter a-h or x, followed by a number, point, and number or letter
                               "eunis|EUNIS|^NA$", #either contains 'eunis' or is 'NA'
                               "", #na
                               "\\w", #is a word [A-z0-9_]
                               "", #na
                               "", #na
                               "^(=|~|>|<|#|S)$", #one of the 6 options
                               "", #na
                               "^[A-z0-9]{4}$", #4 character alphanumeric
                               "", #na
                               "^((High)|(Potential))$", #one of the 2 options
                               "^([A-z0-9]{8}\\-[A-z0-9]{4}\\-[A-z0-9]{4}\\-[A-z0-9]{4}\\-[A-z0-9]{12})$", ### escape neccessary? ###
                               "^((View/Download)|(View only)|(Not available))$",
                               "[0-9]" #is integer
                     ),
                     procs = c('grepl','calc','grepl','na','grepl','grepl','grepl','grepl','na','grepl','na','na','grepl',
                               'na','grepl','na','grepl','grepl','grepl','grepl'),
                     stringsAsFactors = F)

## ID the DEF used based on column names -----
id.def <- function(hb_data){
  cols <- colnames(hb_data)
  if('HAB_TYPE' %in% cols){
    def <- 'TH'
  } else if('ANNEXI' %in% cols){
    def <- 'HD'
  } else if('UUID' %in% cols){
    def <- 'SA'
  } else {
    def <- 'OH'
  }
  return(def)
}

## Test that CRS is correct -----

crs.check <- function(hb_data){
  d1 <- identical(sf::st_crs(hb_data)$proj4string, "+proj=longlat +datum=WGS84 +no_defs")
  d2 <- identical(sf::st_crs(hb_data)$epsg, as.integer(4326))
  
  if(d1){
    d3 <- paste0('PROJ.4 string correct, ')
  } else if(!d1) {
    d3 <- paste0("PROJ.4 string incorrect (not '+proj=longlat +datum=WGS84 +no_defs'), ")
  }
  if(d2){
    d4 <- paste0('EPSG correct.')
  } else if(!d2){
    d4 <- paste0('EPSG incorrect (not 4326).')
  }
  
  d5 <- paste0(d3, d4)
  return(d5)
}

## Test shapefile validity -----

geom.test <- function(hb_data, def){
  if(def != 'SA'){
    stvalid <- sf::st_is_valid(hb_data, reason = T)
    validdf <- tibble('Polygon ID' = hb_data[which(stvalid != 'Valid Geometry'),][['POLYGON']],
                      'Invalid Reason' = stvalid[which(stvalid != 'Valid Geometry')]) 
    validdf<- validdf[order(validdf$`Polygon ID`),]
  } else {
    stvalid <- sf::st_is_valid(hb_data, reason = T)
    if(stvalid != 'Valid Geometry'){
     validdf <- tibble('Result' = paste0('Geometry invalid: ', stvalid)) 
    } else {
      validdf <- tibble('Result' = 'No geometry validity errors detected.')
    }
  }
  
  if(nrow(validdf)<1){
    validdf <- tibble('Result' = 'No geometry validity errors detected.')
  }
  
  return(validdf)
}

## Test overlaps are exact and have the same ID -----

overlap.test <- function(hb_data){
  
  sf_mat <- sf::st_equals(hb_data, hb_data, sparse = F)
  
  diag(sf_mat) <- F
  iden_mat <- which(sf_mat, arr.ind = T)
  #iden_mat <- which(`dim<-`(grepl(sf_mat, pattern = '[0-2]{1}[A-z0-9]{1}[F]{1}[A-z0-9]{2}[F]{3}[A-z0-9]{1}'), dim(sf_mat)), arr.ind = T)
  if(nrow(iden_mat)!=0){
    iden_poly <- bind_rows(apply(iden_mat, 1, function (x,y) return(
      data.frame('p1' = y['POLYGON'][x[1],][[1]], 'p2' = y['POLYGON'][x[2],][[1]], 'Identical' = y['POLYGON'][x[1],][[1]]==y['POLYGON'][x[2],][[1]])
    ), hb_data))
    names(iden_poly) <- c('Polygon 1 ID','Polygon 2 ID','Error')
    iden_poly <- iden_poly[!iden_poly$Error,]
    iden_poly[,'Error'] <- "Polygons are identical but have different 'POLYGON' field values."
    iden.sort <- t(apply(iden_poly, 1, sort))
    iden_poly<-iden_poly[!duplicated(iden.sort),]
    iden.id <- 1
  } else {
    iden_poly <- data.frame(paste0('No exact overlap errors detected.'))
    names(iden_poly) <- 'Result'
    iden.id <- 2
  }
  
  
  share_ids <- unique(hb_data[duplicated(hb_data[['POLYGON']]),][['POLYGON']])
  if(iden.id==2){
    nshared <- share_ids
  } else {
    nshared <- share_ids[!(share_ids %in% c(iden_poly[,2],iden_poly[,1]))]  
  }
  
  if(length(nshared)!=0){
    if(!('Result' %in% names(iden_poly))){
      temp_df <- data.frame(poly1 = share_ids[!(share_ids %in% c(iden_poly[,2],iden_poly[,1]))], poly2 = NA, Error = "Polygons share 'POLYGON' field values but have are not identical.")
      names(temp_df) <- c('Polygon 1 ID','Polygon 2 ID','Error')
      iden_poly <- bind_rows(iden_poly, temp_df)
    } else {
      temp_df <- data.frame(poly1 = share_ids, poly2 = NA, Error = "Polygons share 'POLYGON' field values but are not identical.")
      names(temp_df) <- c('Polygon 1 ID','Polygon 2 ID','Error')
      iden_poly <- temp_df
    }
  }
  
  if(nrow(iden_poly)>1){
    iden_poly <- iden_poly[order(iden_poly$`Polygon 1 ID`),]
  }
  
  return(iden_poly)
}

## Test for intersects -----

intersect.test <- function(hb_data, def) {
  
  if(def == 'SA'){
    if(nrow(hb_data) > 1){
      new_df <- data.frame(paste0('Shapefile contains more than 1 feature. Study area DEF required only one polygon.'))
      names(new_df) <- 'Result'
      return(new_df)
    } else {
      new_df <- data.frame(paste0('No additional unnecessary polygons detected.'))
      names(new_df) <- 'Result'
      return(new_df)
    }
  } else {
    t1 <- sf::st_overlaps(hb_data, sparse = F)
    
    t2 <- data.frame(which(t1, arr.ind = T))
    t2[,1] <- hb_data[['POLYGON']][t2[,1]]
    t2[,2] <- hb_data[['POLYGON']][t2[,2]]
    t3 <- as.numeric(names(sort(-table(c(t2[,1], t2[,2])))))
    
    if(length(t3)!=0){
      new_df <- NULL
      count<-0
      for(i in 1:length(t3)){
        #i<-1
        x1 <- filter(t2, row==t3[i])
        if(length(x1[,2])!=0){
          count <- count+1
          x2 <- data.frame('x'=paste0(t3[i]), 'y'=paste0(x1[,2], collapse = ', '))
          names(x2)<-c('POLYGON', 'Overlaps POLYGONS')
          t2 <- t2[!(t2[,'row']==t3[i]|t2[,'col']==t3[i]),]
          new_df[[count]] <- x2
        }
      }
      return(suppressWarnings(bind_rows(new_df)))
    } else {
      new_df <- data.frame(paste0('No partial overlap errors detected.'))
      names(new_df) <- 'Result'
      return(new_df)
    }
  }
}


## Test that GUI is unique -----

GUI.unique <- function(hb_data){
  sf::st_geometry(hb_data) <- NULL
  return(list(GUI = unique(hb_data[,'GUI']), unique = length(unique(hb_data[,'GUI']))==1))
}


## Test that all essential DEF columns are present, report extra columns, test that columns are the right class and the data is formatted properly -----
field.checks <- function(hb_data, def){
  sf::st_geometry(hb_data) <- NULL
  col_lst <- colval[grepl(colval$prest, pattern = def),]
  
  colcheck <- function(col_lst, hb_data){
    is.present   <- col_lst['colnm'][[1]] %in% colnames(hb_data)
    is.essential <- grepl(col_lst['esntl'][[1]], pattern = def)
    if(is.present){
      is.unique    <- length(unique(hb_data[,col_lst['colnm'][[1]]]))==1
      is.corclass  <- (class(hb_data[,col_lst['colnm'][[1]]]) == col_lst['class'][[1]])
      is.complete  <- !any(is.na(hb_data[,col_lst['colnm'][[1]]]) | grepl(hb_data[,col_lst['colnm'][[1]]], pattern = "^\\s*$"))
      is.complete_ <- paste0(which(is.na(hb_data[,col_lst['colnm'][[1]]]) | grepl(hb_data[,col_lst['colnm'][[1]]], pattern = "^\\s*$")), collapse = ',')
      if(col_lst['procs'][[1]] == 'grepl'){
        is.cordtype  <- any(grepl(hb_data[,col_lst['colnm'][[1]]], pattern = col_lst['value'][[1]]))
      } else if(col_lst['procs'][[1]] == 'calc'){
        is.cordtype  <- all(hb_data[,col_lst['colnm'][[1]]] > 0)
      } else {
        is.cordtype  <- NA
      }
    } else {
      is.corclass  <- NA
      is.complete  <- NA
      is.complete_ <- NA
      is.cordtype  <- NA
      is.unique    <- NA
    }
    
    r_df <- data.frame(column.name  = col_lst['colnm'][[1]],
                       is.present   = is.present,
                       is.unique    = is.unique,
                       is.essential = is.essential,
                       is.corclass  = is.corclass,
                       is.complete  = is.complete,
                       is.complete_ = is.complete_,
                       is.cordtype  = is.cordtype)
    
    if(!r_df$is.present & r_df$is.essential){
      d_pass <- F
      d_reas <- 'Field is mandatory but not present in dataset.'
    } else if(r_df$is.present){
      if(r_df$column.name=='GUI' & !r_df$is.unique){
        d_pass <- F
        d_reas <- 'GUI field does not contain a single, unique value.'
      } else if(!r_df$is.corclass){
        d_pass <- F
        d_reas <- paste0("Field is class '", class(hb_data[,col_lst['colnm']]), "', should be class '", col_lst['class'], "'.")
      } else if(!r_df$is.complete & r_df$is.essential){
        d_pass <- F
        d_reas <- paste0("Field has missing values but is mandatory.")
      } else if(!is.na(r_df$is.cordtype) && !r_df$is.cordtype){
        d_pass <- F
        d_reas <- paste0("Unexpected data format or presentation.")
      } else {
        d_pass <- T
        d_reas <- paste0("")
      }} else {
        d_pass <- NA
        d_reas <- paste0("Field absent.")
      }
    
    
    r_df2 <- data.frame('Field' = r_df$column.name, 'Passed' = d_pass, 'Reason' = d_reas)
    
    return(r_df2)
    
  }
  
  t_df <- reshape2::melt(apply(col_lst, 1, colcheck, hb_data))[,-(4)]
  is.extrac <- colnames(hb_data)[!(colnames(hb_data) %in% col_lst[,'colnm'])]
  
  if(length(is.extrac) > 0){
    x_df <- data.frame('Field' = is.extrac, 'Passed' = NA, 'Reason' = 'This field is not present in the DEF and may be discarded upon upload.')
    t_df <- rbind(t_df, x_df)
  }
  
  return(t_df)
  
}


### End -----
