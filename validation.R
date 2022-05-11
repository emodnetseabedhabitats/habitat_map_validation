## Create dataframe of column names, types, tests and whether they're (a) essential or (b) present in the Data Exchange Format ####
colval <- data.frame(column = c('GUI','POLYGON','ORIG_HAB','ORIG_CLASS','COMP','COMP_TYPE','HAB_TYPE','VERSION',
                                'DET_MTHD','DET_NAME','DET_DATE','TRAN_COM','T_RELATE','VAL_COMM','ANNEXI',
                                'SUBTYPE','CONFIDENCE','UUID','AVAILABLE','SUM_CONF'),
                     essential = c('OH,TH,HD,SA','OH,TH,HD','OH, TH','','OH,TH','OH,TH','TH','TH','','TH','TH','',
                                   'TH','','HD','','HD','SA','SA',''),
                     present = c('OH,TH,HD,SA','OH,TH,HD','OH,TH','OH,TH','OH,TH','OH,TH','TH','TH','TH','TH',
                                 'TH','TH','TH','TH','HD','HD','HD','SA','SA','OH,TH'),
                     class = c('factor','integer','factor','factor','factor','factor','factor','factor',
                               'factor','factor','Date','factor','factor','factor','integer','factor',
                               'factor','factor','factor','integer'),
                     value = c("^[A-Z]{2}[0-9]{6}$", 
                               # has two alpha chars then 6 numbers - must be the same for all features
                               "", 
                               # no set value
                               "\\S", 
                               # is not whitespace, can repeat
                               "", 
                               # no set value
                               "primary|secondary|unknown|1.0|^(([0](\\.([0-9])+)?)?)$|^[1]$", 
                               # either 'primary', 'secondary', 'unknown', a proportion (0.x), or 1
                               "^((single habitat)|(heterogeneous)|(transition)|(data inconclusive)|(no information))$", 
                               # one of the 5 options
                               "^([A-HX]([1-9](\\.([1-9]|[A-Z])+)?)?)$", 
                               # is a letter a-h or x, followed by a single number, a point, and then a number or letter
                               "eunis|EUNIS|^NA$", 
                               # either contains 'eunis' or is 'NA'
                               "", 
                               # no set value
                               "\\w", 
                               # is a word [A-z0-9_]
                               "", 
                               # no set value
                               "", 
                               # no set value
                               "^(=|~|>|<|#|S)$", 
                               # one of the 6 options
                               "", 
                               # no set value
                               "^[A-z0-9]{4}$", 
                               # 4 characters long, alphanumeric
                               "", 
                               # no set value
                               "^((High)|(Potential))$", 
                               # one of the 2 options
                               "^([A-z0-9]{8}\\-[A-z0-9]{4}\\-[A-z0-9]{4}\\-[A-z0-9]{4}\\-[A-z0-9]{12})$", 
                               # A string of alphanumeric characters, separated by hyphens with the lengths pre-set (8,4,4,4,12)
                               "^((View/Download)|(View only)|(Not available))$",
                               # one of the three options
                               "[0-9]" 
                               # is an integer
                     ),
                     process = c('grepl','calc','grepl','NA','grepl','grepl','grepl','grepl','NA','grepl','NA','NA','grepl',
                                 'NA','grepl','NA','grepl','grepl','grepl','grepl'),
                     stringsAsFactors = F)

## Identify the Data Exchange Format used based on column names - id.def(hb_data) ####
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

## Test that CRS is correct - crs.check(hb_data) ####
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

## Test shapefile validity - geom.test(hb_data,def) ####
geom.test <- function(hb_data, def){
  if((as.numeric(object.size(hb_data))/(1024^2))>=30){  
    return("File size too large, checking of shapefile geometry validity skipped.")
  } else {
    if(def != 'SA'){
      stvalid <- sf::st_is_valid(hb_data, reason = T)
      validdf <- data.frame('Polygon_ID' = hb_data[which(stvalid != 'Valid Geometry'),][['POLYGON']],
                            'Invalid_Reason' = stvalid[which(stvalid != 'Valid Geometry')]) 
      validdf<- validdf[order(validdf$`Polygon_ID`),]
    } else {
      stvalid <- sf::st_is_valid(hb_data, reason = T)
      if(stvalid != 'Valid Geometry'){
        validdf <- data.frame('Result' = paste0('Geometry invalid: ', stvalid)) 
      } else {
        validdf <- data.frame('Result' = 'No geometry validity errors detected.')
      }
    }
    
    if(nrow(validdf)<1){
      validdf <- data.frame('Result' = 'No geometry validity errors detected.')
    }
    
    return(validdf)
  }
}

## Test overlaps are exact and have the same ID - overlap.test(hb_data) ####
overlap.test <- function(hb_data){
  if((as.numeric(object.size(hb_data))/(1024^2))>=30){   
    return("File size too large, checking of overlap validity skipped.")
  } else { 
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
}

## Test for intersects - intersect.test(hb_data,def) ####
intersect.test <- function(hb_data, def) {
  if((as.numeric(object.size(hb_data))/(1024^2))>=30){  
    return("File size too large, checking for intersects skipped.")
  } else {
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
}

## Test that GUI is unique - GUI.unique(hb_data) ####
GUI.unique <- function(hb_data){
  sf::st_geometry(hb_data) <- NULL
  return(list(GUI = unique(hb_data[,'GUI']), unique = length(unique(hb_data[,'GUI']))==1))
}

## Test that all essential DEF columns are present, report extra columns, test that columns are the right class and the data is formatted properly - field.checks(hb_data,def) ####
field.checks <- function(hb_data, def){
  sf::st_geometry(hb_data) <- NULL
  col_lst <- colval[grepl(colval$present, pattern = def),]
  
  if(any(!grepl("^[[:upper:]]+$",colnames(hb_data)))){
    FieldNames<-"UPDATED COLUMN NAMES"
    OriginalData<-hb_data
    for(i in 1:ncol(hb_data)){
      colnames(hb_data)[i]<-paste(toupper(colnames(hb_data)[i]))
    } 
  } else {
    FieldNames<-"COLUMN NAMES OK"
  }
  
  colcheck <- function(col_lst, hb_data){
    is.present <- col_lst['column'][[1]] %in% colnames(hb_data)
    is.essential <- grepl(col_lst['essential'][[1]], pattern = def)
    
    if(is.present){
      is.corclass <- (class(hb_data[,col_lst['column'][[1]]]) == col_lst['class'][[1]])
      is.complete  <- !any(is.na(hb_data[,col_lst['column'][[1]]]) | grepl(hb_data[,col_lst['column'][[1]]], pattern = "^\\s*$"))
      is.complete_ <- paste0(which(is.na(hb_data[,col_lst['column'][[1]]]) | grepl(hb_data[,col_lst['column'][[1]]], pattern = "^\\s*$")), collapse = ',')
      if(col_lst['process'][[1]] == 'grepl'){
        is.cordtype  <- any(grepl(hb_data[,col_lst['column'][[1]]], pattern = col_lst['value'][[1]]))
      } else if(col_lst['process'][[1]] == 'calc'){
        is.cordtype  <- all(hb_data[,col_lst['column'][[1]]] > 0)
      } else {
        is.cordtype  <- NA
      }
      if(FieldNames=="COLUMN NAMES OK"){
        is.corname<-T
      } else {
        if(col_lst['column'][[1]] %in% colnames(OriginalData)){
          is.corname<-T
        } else {
          is.corname<-F
        }
      }
    } else {
      is.corclass  <- NA
      is.complete  <- NA
      is.complete_ <- NA
      is.cordtype  <- NA
      is.corname   <- NA
    }
    
    r_df <- data.frame(column.name  = col_lst['column'][[1]],
                       is.present   = is.present,
                       is.essential = is.essential,
                       is.corclass  = is.corclass,
                       is.complete  = is.complete,
                       is.complete_ = is.complete_,
                       is.cordtype  = is.cordtype,
                       is.corname   = is.corname)
    
    r_df_messages<-as.data.frame(r_df[1,])
    r_df_messages[1,]<-NA
    
    if(!r_df$is.present & r_df$is.essential){
      r_df_messages$is.essential <- 'Field is mandatory but not present in dataset.'
    } else if(r_df$is.present){
      if(!r_df$is.corclass){
        r_df_messages$is.corclass <- paste0("Field is class '", class(hb_data[,col_lst[['column']]]), "', should be class '", col_lst[['class']], "'.")
      } else if(!r_df$is.complete & r_df$is.essential){
        r_df_messages$is.complete <- paste0("Field has missing values but is mandatory. See row ",r_df$is.complete_,".")
      } else if(!is.na(r_df$is.cordtype) && !r_df$is.cordtype){
        r_df_messages$is.cordtype <- paste0("Unexpected data format or presentation.")
      } else if (!is.corname) {
        r_df_messages$is.corname <- paste0("Unexpected field name format.")
      } else {
        r_df_messages$is.corname <- NA
      }} else {
        d_pass <- NA
        r_df_messages$is.present <- paste0("Field absent, but not mandatory.")
      }
    
    message<-list()
    for(j in 1:ncol(r_df_messages)){
      if(!is.na(r_df_messages[,j])){
        message<-append(message,paste(r_df_messages[,j]))
      }
      if(length(message)==0){
        d_reas<-"NA"
        d_pass<-T
      } else {
        d_reas<-paste0(unlist(message), sep=" ", collapse="")
        d_pass<-F
      }
    }
    
    r_df2 <- data.frame('Field' = r_df$column.name, 'Passed' = d_pass, 'Reason' = d_reas)
  }
  
  t_df <- reshape2::melt(apply(col_lst, 1, colcheck, hb_data))[,-(4)]
  
  is.extrac <- colnames(hb_data)[!(colnames(hb_data) %in% col_lst[,'column'])]
  
  if(length(is.extrac) > 0){
    x_df <- data.frame('Field' = is.extrac, 'Passed' = NA, 'Reason' = 'This field is not present in the DEF and may be discarded upon upload.')
    t_df <- rbind(t_df, x_df)
  }
  return(t_df)
}
### End -----