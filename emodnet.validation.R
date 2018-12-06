## packages
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(shinythemes))
suppressPackageStartupMessages(require(shinyjs))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(sf))
suppressPackageStartupMessages(require(grDevices))
## suppressPackageStartupMessages(require(shinycssloaders))


## functions
source('./validation.R')
options(shiny.maxRequestSize=30*1024^2)


## UI
ui <- fluidPage(titlePanel('EMODnet Data Validation Tool'),
                theme = shinytheme('spacelab'),
                useShinyjs(),
                sidebarLayout(
                  sidebarPanel(
                    tags$head(tags$style(HTML('.progress-bar{background-color:darkorange}'))),
                    fileInput('hb_file', h4('1. Select a .zip file'),
                              accept = c('application/zip'), multiple = F),
                    helpText(HTML('<strong>Warning:</strong> Large files may take considerable time to process and plot, especially if they have a large number of features.<br>For reference, a 13MB .zip file (17MB .shp file) with ~3,000 features will take ~20 minutes.')),
                    tags$hr(),
                    disabled(radioButtons('def_type', h4('2. Specify DEF'),
                                          choices = list('Original Habitat DEF' = 'OH',
                                                         'Translated EUNIS Habitat DEF' = 'TH',
                                                         'Habitats Directive DEF' = 'HD',
                                                         'Study Area DEF' = 'SA'), selected = 'OH')),
                    helpText(HTML('Further details on the different Data Exchange Formats can be found <a href=https://www.emodnet-seabedhabitats.eu/contribute-data/data-exchange-format/ target=_blank>here</a>.')),
                    tags$hr(),
                    disabled(radioButtons('plot_type', h4('3. Choose plotting type'),
                                          choices = list('Basic' = 'fst',
                                                         'Interactive' = 'int'), selected = 'fst')),
                    helpText(HTML('<strong>Basic</strong> for a static plot that is faster to render, or<br><strong>Interactive</strong> for a plot which is slower to render, but allows you to zoom and toggle layer visibility.<br><strong>Warning:</strong> Plotting may take time for large files. In the case of especially large files, <strong>basic</strong> is the suggested option.')),
                    tags$hr(),
                    h4('4. Validate'),
                    disabled(actionButton('validate', HTML('<strong>Validate</strong>'), icon = icon('check-square'), width = '100%')),
                    width = 3),
                  
                  mainPanel(
                    tags$head(tags$style(HTML(".shiny-notification {height: 100px;
                                              width: 400px;position:fixed;
                                              top: calc(50% - 150px);;
                                              left: calc(57.5% - 200px);;}"))),
                    conditionalPanel(condition = "output.change",
                                     h3(textOutput("t2")),
                                     tabsetPanel(id = 'tabs',
                                                 tabPanel("Shapefile mapping",
                                                          useShinyjs(),
                                                          htmlOutput('spec_tbl'),
                                                          tags$br(),
                                                          htmlOutput('hlptxt'),
                                                          tags$br(),
                                                          plotOutput('hbmap_fst', height = '550px', width = '100%'),
                                                          plotlyOutput('hbmap_int', height = '550px', width = '100%')),
                                                 tabPanel("Spatial validation",
                                                          tags$br(),
                                                          h4("CRS"),
                                                          textOutput('crs_txt'),
                                                          tags$br(),
                                                          h4(HTML("Geometry errors")),
                                                          tableOutput('geoertbl')
                                                 ),
                                                 tabPanel("Overlap validation",
                                                          tags$br(),
                                                          h4("Exact overlap errors"),
                                                          tableOutput('spaer_tbl'),
                                                          tags$br(),
                                                          h4("Partial overlap errors"),
                                                          tableOutput('spaer2_tbl')
                                                 ),
                                                 tabPanel("Dataset validation ",
                                                          tags$br(),
                                                          h4("Data errors"),
                                                          tableOutput('dater_tbl'))
                                     )
                    )
                  )
                )  
)


## server
server <- function(input, output, session){
  
  observeEvent(input$hb_file,{
    enable('def_type')
    enable('plot_type')
    enable('validate')
  })
  
  observeEvent(input$def_type,{
    enable('validate')
  })
  
  observeEvent(input$plot_type,{
    enable('validate')
  })
  
  dispData <- eventReactive(input$validate, {
    hb_file <- input$hb_file
    
    if(is.null(hb_file)){
      return(NULL)
      
    } else {
      disable('validate')
      path <- hb_file['datapath']
      path <- gsub(path, pattern = '.{3}$', replacement = tolower(tools::file_ext(path)))
      npath <- gsub(path, pattern = '.{4}$', replacement = '')
      
      withProgress(message = 'Analysing', value=0,{
        hb_sdata <- unzip(path, exdir = npath)
        fname <- list.files(npath)[grepl(list.files(npath), pattern = '.shp$')]
        
        incProgress(1/9, detail='Reading file...')
        hb_data <- sf::read_sf(npath, gsub(fname, pattern = '.{4}$', replacement = ''),
                               as_tibble = F, stringsAsFactors = T)
        c.map <- input$def_type
        
        c.plt <- ifelse(input$plot_type == 'int', '<font color=\"#a6a6a6\">Single click a legend entry to hide/show that layer.<br>Double click a legend entry for a <strong>displayed</strong> layer to show <strong>only</strong> that layer.<br>Double click a legend entry for a <strong>hidden</strong> layer to display <strong>all</strong> layers.<br>Double click the plot area to <strong>reset</strong> plot zoom and extent.<br>Scroll to zoom.</font>', '')
        c.plt_ <- input$plot_type
        
        incProgress(1/9, detail='Checking fields...')
        c.def <- id.def(hb_data)
        c.gui <- GUI.unique(hb_data)[[1]]
        c.gui_u <- GUI.unique(hb_data)[[2]]
        c.field <- field.checks(hb_data, c.def)
        
        incProgress(2/9, detail='Checking geometry...')
        c.crs  <- crs.check(hb_data)
        c.geom <- as.data.frame(geom.test(hb_data, c.def))
        
        incProgress(2/9, detail='Checking for exact overlaps...')
        c.olap <- overlap.test(hb_data)
        
        incProgress(2/9, detail='Checking for partial overlaps...')
        c.olap2 <- intersect.test(hb_data, c.def)
        
        incProgress(1/9, detail='Writing results...')
        return(list(c.def   = c.def,
                    c.data  = hb_data,
                    c.gui   = c.gui,
                    c.gui_u = c.gui_u,
                    c.field = c.field,
                    c.crs   = c.crs,
                    c.geom  = c.geom,
                    c.olap  = c.olap,
                    c.olap2 = c.olap2,
                    c.map   = c.map,
                    c.plt   = c.plt,
                    c.plt_  = c.plt_))
      })
    }
  })
  
  output$change <- reactive({
    return(!is.null(dispData()))
  })
  
  outputOptions(output, 'change', suspendWhenHidden=FALSE)
  
  observeEvent(dispData(), {
    
    if(dispData()$c.plt_ == 'int'){
      hide('hbmap_fst')
      show('hbmap_int')
      
    } else if(dispData()$c.plt_ == 'fst'){
      hide('hbmap_int')
      show('hbmap_fst')
    }
    
    output$t2         <- renderText(paste0('Displaying results for ', dispData()$c.gui, ':'))
    output$crs_txt    <- renderText(dispData()$c.crs)
    output$geoertbl   <- renderTable(dispData()$c.geom)
    output$dater_tbl  <- renderTable(dispData()$c.field)
    output$spaer_tbl  <- renderTable(dispData()$c.olap)
    output$spaer2_tbl <- renderTable(dispData()$c.olap2)
    output$hlptxt     <- renderText(dispData()$c.plt)
    hb_data           <- dispData()$c.data
    olap_t            <- (dispData()$c.olap[[1]][1]!='No exact overlap errors detected.')
    olap2_t           <- (dispData()$c.olap2[[1]][1]!='No partial overlap errors detected.')
    geom_t            <- (dispData()$c.geom[[1]][1]!='No geometry validity errors detected.')
    output$spec_tbl   <- renderText({
      if(dispData()$c.map!=dispData()$c.def){
        t1 <- data.frame(c('OH', 'TH', 'HD', 'SA'), c('Original Habitat', 'Translated Habitat', 'Habitats Directive', 'Study Area'))
        
        return(paste0("<font color=\"#ff8c00\"><br>Warning: Selected DEF ('", t1[t1[,1]==dispData()$c.map,2], "') does not match the DEF suggested by data fields ('", t1[t1[,1]==dispData()$c.def,2], "'). '", t1[t1[,1]==dispData()$c.def,2], "' was used as the target DEF in the data validation process. If you think this is incorrect, check the Data Exchange Formats <a href=https://www.emodnet-seabedhabitats.eu/contribute-data/data-exchange-format/ target=_blank>here</a> and try again.</font>"))
      } else {
        return(NULL)
      }
    })
    
    if(dispData()$c.def == 'TH'){
      hb_data$Habitat <- as.character(hb_data$HAB_TYPE, stringsAsFactors=F)
      
    } else if(dispData()$c.def == 'OH'){
      hb_data$Habitat <- as.character(hb_data$ORIG_HAB, stringsAsFactors=F)
      
    } else if(dispData()$c.def == 'HD'){
      hb_data$Habitat <- as.character(hb_data$ANNEXI, stringsAsFactors=F)
      
    } 
    
    if(dispData()$c.def!='SA'){
      xax <- sf::st_bbox(hb_data)[c(1,3)]
      yax <- sf::st_bbox(hb_data)[c(2,4)]
      
      if(dispData()$c.plt_ == 'int'){
        hbmap_fst <- NULL
        
        if(any(olap_t, olap2_t, geom_t)){
          olap_df <- dispData()$c.olap
          olap2_df <- dispData()$c.olap2
          geom_df <- dispData()$c.geom
          
          overlap_errors <-NULL
          if(olap_t){
            overlap_errors <- hb_data %>% filter(POLYGON %in% unique(c(olap_df[,1], olap_df[,2])))
            if(nrow(overlap_errors)>0){
              overlap_errors$Habitat <- 'Exact overlap errors'
              overlap_errors<-overlap_errors['Habitat']
            }
          }
          overlap2_errors <- NULL
          if(olap2_t){
            overlap2_errors <- hb_data %>% filter(POLYGON %in% unique(c(olap2_df[,1], olap2_df[,2])))
            if(nrow(overlap2_errors)>0){
              overlap2_errors$Habitat <- 'Partial overlap errors'
              overlap2_errors <- overlap2_errors['Habitat']
            }
          }
          geom_errors <- NULL
          if(geom_t){
            vld_p <- str_split(gsub(str_extract_all(as.data.frame(geom_df)[,2], pattern = '\\[(.*?)\\]'),pattern = '\\[|\\]', replacement = ''),pattern = ' ')
            if(length(vld_p)>0){
              vld <- sf::st_multipoint(apply(unique(do.call('rbind',vld_p)), c(1,2), as.double))
              geom_errors <- sf::st_as_sf(data.frame(Habitat = 'Validity errors', geometry = st_geometry(vld)))
              geom_errors <- sf::st_set_crs(geom_errors, sf::st_crs(hb_data))
            }
          }
          
          
          
          if(all(is.null(overlap_errors), is.null(overlap2_errors), is.null(geom_errors))){
            output$hbmap_int <- renderPlotly({
              plotly::plot_geo(locationmode='world') %>% 
                add_sf(data = hb_data[is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none', color=I('grey55')) %>%
                add_sf(data = hb_data[!is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none') %>%
                layout(geo=list(scope='europe', resolution=50, showFrame=F), dragmode='pan',
                       xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax)) %>% 
                config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
            }) 
          } else if(any(!is.null(overlap_errors), !is.null(overlap2_errors)) & is.null(geom_errors)){
            if(all(!is.null(overlap_errors), !is.null(overlap2_errors))){
              overlap_errors <- rbind(overlap_errors, overlap2_errors)
            } else if(!is.null(overlap2_errors)){
              overlap_errors <- overlap2_errors
            } 
            output$hbmap_int <- renderPlotly({
              plotly::plot_geo(locationmode='world') %>% 
                add_sf(data = hb_data[is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none', color=I('grey55')) %>%
                add_sf(data = hb_data[!is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none') %>%
                add_sf(data = overlap_errors, split = ~Habitat, hoverinfo='none', visible='legendonly') %>%
                layout(geo=list(scope='europe', resolution=50, showFrame=F), dragmode='pan',
                       xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax)) %>% 
                config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
            })
          } else if (!is.null(geom_errors) & all(is.null(overlap_errors), is.null(overlap2_errors))){
            output$hbmap_int <- renderPlotly({
              plotly::plot_geo(locationmode='world') %>% 
                add_sf(data = hb_data[is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none', color=I('grey55')) %>%
                add_sf(data = hb_data[!is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none') %>%
                add_sf(data = geom_errors, split = ~Habitat, hoverinfo='none', visible='legendonly') %>%
                layout(geo=list(scope='europe', resolution=50, showFrame=F), dragmode='pan',
                       xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax)) %>% 
                config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
            })
          } else if (!is.null(geom_errors) & any(!is.null(overlap_errors), !is.null(overlap2_errors))){
            if(all(!is.null(overlap_errors), !is.null(overlap2_errors))){
              overlap_errors <- rbind(overlap_errors, overlap2_errors)
            } else if(!is.null(overlap2_errors)){
              overlap_errors <- overlap2_errors
            } 
            output$hbmap_int <- renderPlotly({
              plotly::plot_geo(locationmode='world') %>% 
                add_sf(data = hb_data[is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none', color=I('grey55')) %>%
                add_sf(data = hb_data[!is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none') %>%
                add_sf(data = overlap_errors, split = ~Habitat, hoverinfo='none', visible='legendonly') %>%
                add_sf(data = geom_errors, split = ~Habitat, hoverinfo='none', visible='legendonly',
                       marker = list(color='rgba(139,0,0,0.1)', size=8,
                                     line = list(color='darkred', width=3))) %>%
                layout(geo=list(scope='europe', resolution=50, showFrame=F), dragmode='pan',
                       xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax)) %>% 
                config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
            })
          }
        } else {
          output$hbmap_int <- renderPlotly({
            plotly::plot_geo(locationmode='world') %>% 
              add_sf(data = hb_data[is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none', color=I('grey55')) %>%
              add_sf(data = hb_data[!is.na(hb_data$Habitat),], split = ~Habitat, hoverinfo='none') %>%
              layout(geo=list(scope='europe', resolution=50, showFrame=F), dragmode='pan',
                     xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax)) %>% 
              config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
          })
        }
        
      } else if (dispData()$c.plt_ == 'fst'){
        hbmap_int <- NULL
        output$hbmap_fst <- renderPlot({
          ggplot(hb_data)+
            geom_sf(aes(fill=Habitat), alpha=0.8)+
            theme_bw()+
            scale_fill_discrete(na.value='transparent')+
            theme(text = element_text(size=15))
        })
        
      }
    } else if(dispData()$c.def=='SA') {
      xax <- sf::st_bbox(hb_data)[c(1,3)]
      yax <- sf::st_bbox(hb_data)[c(2,4)]
      
      if(dispData()$c.plt_ == 'int'){
        hbmap_fst <- NULL
        output$hbmap_int <- renderPlotly({
          hb_data %>%
            plot_geo() %>%
            layout(geo = list(scope = 'europe', showFrame=F, resolution=50), dragmode='pan',
                   xaxis = list(autorange = F, range = xax), yaxis = list(autorange = F, range=yax))%>% 
            config(p = ., displaylogo = F, displayModeBar = F, scrollZoom = T, showTips = F, doubleClick = 'reset', autosizable=F)
        })
        
      } else if (dispData()$c.plt_ == 'fst'){
        hbmap_int <- NULL
        mps <- map_data('world')
        output$hbmap_fst <- renderPlot({
          ggplot(hb_data)+
            geom_map(data = mps, map=mps, aes(x=long, y=lat, group=group, map_id=region), fill='grey50')+
            geom_sf(aes(fill=GUI), alpha=0.8, fill='darkorange')+
            theme_bw()+
            scale_fill_discrete(na.value='transparent')+
            theme(text = element_text(size=15), axis.title = element_blank(), legend.position = 'none')+
            scale_x_continuous(limits = xax, expand = expand_scale(mult = 5))+
            scale_y_continuous(limits = yax, expand = expand_scale(mult = 5))
        })
      }
    }
  })
}


## build app
shinyApp(ui = ui, server = server)

