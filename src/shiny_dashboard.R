library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(survival)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(data.table)
library(tidyr)

library(stringr)
library(tidyverse)
library(magrittr)
library(arules)
library(visNetwork)
library(arulesViz)

library(ggthemes)
library(plotly)


#saveRDS(part_df_grp,"~/Git/predictive_maintenance/##.rds")


stopifnot(file.exists("part_data.rds"))
part_df_grp <- as.data.table(readRDS("part_data.rds"))

# top 10 manufacturer
manufacturer <- c('Alpha Therm Ltd','Baxi Heating Ltd','Glow Worm',
                  'Ideal Boilers','MAIN GAS APPLIANCES LTD','Potterton Myson Ltd',
                  'Vaillant','Vokera','Worcester Heat Systems Ltd','Default')

partIdx <- part_df_grp[,.(total=.N, event = as.integer(sum(is_ib))), 
                       by=list(manufacturer_desc, 
                               model_name, 
                               part_id, 
                               dcs_gds_desc, 
                               gc_number)][
                                   , `:=`(part=paste(part_id, dcs_gds_desc,sep=" - ")
                                          ,perc=round(100*event/total,2))] 

fullTbl <- partIdx[
    ,.(total = sum(total), event = as.integer(sum(event))),list(part_id, dcs_gds_desc)][
        ,perc:=round(100*event/total,2)][
            order(-perc)
        ]


minDate <- as.Date(min(part_df_grp$actual_start_ts, na.rm=T))
maxDate <- as.Date(max(part_df_grp$actual_start_ts, na.rm=T))
if(maxDate > today()) maxDate <- today()


getSurvData <- function(tbl){
    
    # TODO: use agreement end date as maximum date instead of max date
    #       use appointment date instead of actual start date
    
    # require:
    #   address_id 
    #   part_id 
    #   diff_addr_part - days since last installed by address and part id, change to time_since at the end
    #   prev_addr_part_actv_start_ts - part last installed time when diff_addr_part > 0
    #   rn_addr_part - the number associate to diff_addr_part
    #   actual_end_ts - the time the part finish with work request
    #   installation_dt - boiler installed time (data might not be accurate)
    
    if(!is.null(tbl)) {
        
        if(length(unique(tbl$part_id))>1) {
            
            # put all part in address together and reassign time, 
            # this assume comparing same kind of part but different part id
            # only run if > 1 part ID
            tbl %>% 
                left_join(tbl %>% 
                              select(address_id, part_id, actual_start_ts) %>% 
                              unique() %>% 
                              group_by(address_id) %>% 
                              arrange(actual_start_ts) %>% 
                              mutate(rn = 1:n()) %>% 
                              filter(max(rn) != 1) %>% 
                              mutate(time_since_new = 
                                         coalesce(floor(interval(start=actual_start_ts, end=lead(actual_start_ts))/ duration(n=1,unit="days")),
                                                  floor(interval(start=actual_start_ts, end=maxDate)/ duration(n=1,unit="days"))),
                                     is_ib_new = as.integer(!is.na(lead(actual_start_ts)))), 
                          by=c("part_id"="part_id","address_id"="address_id","actual_start_ts"="actual_start_ts")) %>% 
                mutate(time_since = coalesce(time_since_new, time_since),
                       is_ib = coalesce(is_ib_new, is_ib)) %>% select(-time_since_new, -is_ib_new) 
        } else tbl
    } else NULL
}

#---------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Predictive Maintenance"),
                sidebarLayout(
                    sidebarPanel(
                        tags$style(type='text/css', ".selectize-input { font-size: 10px;} 
                               .selectize-dropdown { font-size: 10px; } "),
                        
                        selectizeInput("PartID", "Choose Part ID:", c("",sort(unique(partIdx$part))), "", multiple = T),
                        selectInput("Manufacturer", "Choose Manufacturer:", c("",manufacturer), "", multiple = T),
                        selectInput("ModelName", "Choose Boiler:", c("",""), "", multiple = T),
                        selectInput("GCNum",  "Choose GC Number:", c("",""), "", multiple = T),
                        
                        #radioButtons("CombinePlot", "Combine Plot", c("Yes" = "Yes", "No" = "No"), "Yes", T),
                        #radioButtons("AsSelect", "Specific to Selection", c("Yes" = "Yes", "No" = "No"), "Yes", T),
                        sliderInput("rngDte", "Date Range:",
                                    min = as.Date(minDate), max = as.Date(maxDate),
                                    value = c(as.Date(minDate), as.Date(maxDate))),
                        
                        actionButton("Reset", "Reset"),
                        actionButton("Run", "Run")
                        ,width = 3),
                    
                    mainPanel(
                        tabsetPanel(id="Tabs", type = "pills",
                                    tabPanel("Data Summary", 
                                             conditionalPanel(
                                                 condition = "input.PartID != ''",
                                                 h3(textOutput("ManufaTitle")),
                                                 wellPanel(
                                                     shiny::fluidRow(
                                                         shiny::column(6,DT::dataTableOutput("PartTable")),
                                                         shiny::column(4,DT::dataTableOutput("ManufaTable"))
                                                     ), style = "text-align: center;"),
                                                 br(),
                                                 wellPanel(
                                                     shiny::fluidRow(
                                                         shiny::column(6,DT::dataTableOutput("BoilerTable")),
                                                         shiny::column(4,DT::dataTableOutput("GCTable"))
                                                     )
                                                     , style = "text-align: center;"),
                                                 br(),
                                                 DT::dataTableOutput("StatTable")
                                             ),
                                             br(),
                                             wellPanel(
                                                 h3(textOutput("FullDataTitle")),br(),
                                                 DT::dataTableOutput("FullTable")
                                             )
                                    ),
                                    tabPanel("Survival Curve", 
                                             conditionalPanel(
                                                 condition = "input.PartID != ''",
                                                 h3(textOutput("SurvPlotTitle")),
                                                 plotlyOutput("SurvPlot", width = "auto", height = "600px"),
                                                 wellPanel(
                                                     DT::dataTableOutput("SurvTable"), 
                                                     style = "text-align: center;"
                                                 ),
                                                 h4(textOutput("SurvDataTitle")),
                                                 DT::dataTableOutput("AllTable"),
                                                 br(),
                                                 wellPanel(
                                                     h4(textOutput("BoilerAgePlotTitle")),br(),
                                                     plotlyOutput("BoilerAgePlot", width = "100%", height = "600px")
                                                 )
                                             )
                                    ),
                                    
                                    tabPanel("Association Rules", 
                                             conditionalPanel(
                                                 condition = "input.PartID != ''",
                                                 h3(textOutput("NetPlotTitle")),
                                                 wellPanel(
                                                     DT::dataTableOutput("aRules")
                                                 ),
                                                 br(),
                                                 shiny::fluidRow(
                                                     shiny::column(6,
                                                                   sliderInput(inputId="nCnt",label="Choose max rules to show in the below graph",value=10,min=1,max=50,step=1)),
                                                     shiny::column(4,
                                                                   sliderInput("rngDteInterval", "Days between part:",min = 1, max = 365,value = 90, step=1))),
                                                 
                                                 visNetwork::visNetworkOutput("aGraph", width = "100%", height = "600px")
                                             )
                                    ),
                                    
                                    tabPanel("Control Chart", 
                                             conditionalPanel(
                                                 condition = "input.PartID != ''",
                                                 h3(textOutput("CtrlChartTitle")),
                                                 plotlyOutput("CtrlPlot", width = "100%", height = "600px"),
                                                 radioButtons("CtrlUnit", "Change Unit", c("Month" = "Month", "Day" = "Day"), "Month", T),
                                                 br(),
                                                 wellPanel(
                                                     tableOutput("CtrlSummary"), style = "text-align: center;"
                                                 ),br(),
                                                 DT::dataTableOutput("CtrlTable")
                                             )
                                    ),
                                    tabPanel("User Guide", 
                                             htmlOutput("SurvDesc"), br(), 
                                             htmlOutput("AppDesc")
                                    )
                        )
                    )
                ))


#shinyApp(ui = ui, server = server)

server <- function(input, output, session) {
    
    # option for table
    tblOpt <- list(deferRender = TRUE, scroller = TRUE, scrollY = 400, scrollX = 1500, scrollCollapse = TRUE)
    
    # radio button
    # check combine plot option
    #chkCombine <- reactive(input$CombinePlot == "No")
    chkCombine <- reactive(T)
    
    # check specific selection option
    #chkAsSelect <- reactive(input$AsSelect == "Yes")
    chkAsSelect <- reactive(T)
    
    chkCtrlUnitMonth <- reactive(input$CtrlUnit == "Month")
    
    chkInput <- reactive({any(input$PartID!="")|any(input$Manufacturer!="")|any(input$GCNum!="")|any(input$ModelName!="")})
    
    partsid <- reactive({as.vector(str_match(input$PartID, "[A-Z0-9]{1,6}"))})
    
    nCnt <- reactive(input$nCnt)
    
    rulesInterval <- reactive(input$rngDteInterval)
    
    
    output$FullTable <- DT::renderDataTable(isolate(
        DT::datatable(fullTbl, callback = DT::JS("
                            var tips = ['Row Names', 'Part ID', 
                            'Part name',
                            'Total installation of part after breakdown', 
                            'Reported failure after installation', 'Failure rate (%)'],
                            header = table.columns().header();
                            for (var i = 0; i < tips.length; i++) {
                                $(header[i]).attr('title', tips[i]);
                            }")))
    ) 
    
    # update selection of part index
    updateTbl <- reactive({
        if(chkInput()){ 
            
            partIdx <<- part_df_grp[as.Date(actual_start_ts) >= min(input$rngDte)&as.Date(actual_start_ts) <= max(input$rngDte),
                                    .(total=.N, event = as.integer(sum(is_ib))), 
                                    by=list(manufacturer_desc, 
                                            model_name, 
                                            part_id, 
                                            dcs_gds_desc, 
                                            gc_number)][
                                                , `:=`(part=paste(part_id, dcs_gds_desc,sep=" - ")
                                                       ,perc=round(100*event/total,2))] 
            
            fullTbl <<- partIdx[
                ,.(total = sum(total), event = as.integer(sum(event))),
                list(part_id, dcs_gds_desc)][
                    ,perc:=round(100*event/total,2)][
                        order(-perc)
                    ]
            
            as.data.table(partIdx)[
                if(any(input$PartID!="")) part %in% as.character(input$PartID) else T][
                    if(any(input$Manufacturer!="")&chkAsSelect()) manufacturer_desc %in% input$Manufacturer else T][
                        if(any(input$GCNum!="")&chkAsSelect()) gc_number %in% input$GCNum else T][
                            if(any(input$ModelName!="")&chkAsSelect()) model_name %in% input$ModelName else T
                        ]
        } else c("","")
    })
    
    # Input
    partName <- reactive({
        if(chkInput()){ 
            unique(sort(updateTbl() %>% pull(part)))
        } else c("",sort(unique(partIdx$part)))
    })
    
    manuName <- reactive({if(any(input$PartID!="")){ unique(sort(updateTbl() %>% pull(manufacturer_desc)))} else c("",manufacturer)})
    
    modelName <- reactive({if(any(input$PartID!="")){ unique(sort(updateTbl() %>% pull(model_name)))} else c("","")})
    
    gcNum <- reactive({if(any(input$PartID!="")){ unique(sort(updateTbl() %>% pull(gc_number)))} else c("","")})
    
    # update data counts
    allStat <- reactive({if(any(input$PartID!="")){updateTbl() %>% select(-part, -dcs_gds_desc)} else NULL})
    
    partStat <- reactive({if(any(input$PartID!="")){updateTbl()[,.(total=sum(total), event=sum(event)), list(part_id, dcs_gds_desc)][,perc:=round(event*100/total,2)]} else NULL})
    
    manufaStat <- reactive({if(any(input$PartID!="")){updateTbl()[,.(total=sum(total), event=sum(event)), manufacturer_desc][,perc:=round(event*100/total,2)]} else NULL})
    
    boilerStat <- reactive({if(any(input$PartID!="")){updateTbl()[,.(total=sum(total), event=sum(event)), model_name][,perc:=round(event*100/total,2)]} else NULL})
    
    gcStat <- reactive({if(any(input$PartID!="")){updateTbl()[,.(total=sum(total), event=sum(event)), gc_number][,perc:=round(event*100/total,2)]} else NULL   })
    
    # association rules 
    rulesTbl <- reactive({
        tb1 <- as.data.table(part_df_grp)[as.Date(actual_start_ts) >= min(input$rngDte)&as.Date(actual_start_ts) <= max(input$rngDte)][
            if(any(input$Manufacturer!="")&chkAsSelect()) manufacturer_desc %in% input$Manufacturer else T][
                if(any(input$GCNum!="")&chkAsSelect()) gc_number %in% input$GCNum else T][
                    if(any(input$ModelName!="")&chkAsSelect()) model_name %in% input$ModelName else T, 
                    list(address_id, part_id, actual_start_ts)]
        
        # This will only select the part that is choosen
        #parts <- as.vector(str_match(input$PartID, "[A-Z0-9]{1,6}"))
        
        tb2 <- as_tibble(tb1) %>% 
            inner_join(as_tibble(tb1) %>% filter(part_id %in% partsid()) %>% select(address_id) %>% unique()) %>% 
            arrange(address_id, actual_start_ts) %>% 
            mutate(address_id = as.character(address_id)) %>% 
            select(address_id, part_id, actual_start_ts) %>% 
            unique() %>% 
            group_by(address_id) %>% 
            mutate(dtediff = interval(start=lag(actual_start_ts), end=actual_start_ts)/ duration(n=1,unit="days"),
                   isIn = as.integer(coalesce(dtediff,0) >= rulesInterval()),
                   newNum = cumsum(isIn),
                   newAddress = paste0(address_id,"_",newNum)) %>% 
            select(newAddress, part_id) %>% ungroup()
        
        tb3 <-tb2 %>% 
            group_by(newAddress) %>% 
            filter(n() > 1)
        
        tr <- as(split(tb3$part_id, tb3$newAddress), "transactions")
        
        aRules <- apriori(tr, parameter = list(supp=0.0001, conf=0.7,maxlen=3, target= "rules"))
    })
    
    
    # update model name and gc number + stat after choosing part
    observeEvent(input$PartID,{
        if(all(input$Manufacturer=="")) updateSelectInput(session, "Manufacturer", choices = c("", manuName()))
        if(all(input$ModelName=="")) updateSelectInput(session, "ModelName", choices = c("", modelName()))
        if(all(input$GCNum=="")) updateSelectInput(session, "GCNum", choices = c("", gcNum()))
        
        output$StatTable <- DT::renderDataTable(allStat(), extensions = 'Scroller', options = tblOpt) 
        output$ManufaTable <- DT::renderDataTable({manufaStat()}, rownames=F, options = list(dom = 't'))
        output$PartTable <- DT::renderDataTable({partStat()}, rownames=F, options = list(dom = 't'))
        output$BoilerTable <- DT::renderDataTable({boilerStat()}, rownames=F, options = list(dom = 't'))
        output$GCTable <- DT::renderDataTable({gcStat()}, rownames=F, options = list(dom = 't'))
        
        output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
        output$AllTable  <- output$SurvTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
        output$CtrlSummary <- renderTable(NULL)
        output$SurvDataTitle <- output$SurvPlotTitle <- output$BoilerAgePlotTitle  <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
    })
    
    # update part after choosing Manufacturer
    observeEvent(input$Manufacturer,{
        
        if(all(input$PartID=="")) updateSelectInput(session, "PartID", choices = c("",partName()))
        if(all(input$ModelName=="")) updateSelectInput(session, inputId = "ModelName", choices = c("", modelName()))
        if(all(input$GCNum=="")) updateSelectInput(session, inputId = "GCNum", choices = c("", gcNum()))
        
        output$StatTable <- DT::renderDataTable(allStat(), extensions = 'Scroller', options = tblOpt) 
        output$ManufaTable <- DT::renderDataTable({manufaStat()}, rownames=F, options = list(dom = 't'))
        output$PartTable <- DT::renderDataTable({partStat()}, rownames=F, options = list(dom = 't'))
        output$BoilerTable <- DT::renderDataTable({boilerStat()}, rownames=F, options = list(dom = 't'))
        output$GCTable <- DT::renderDataTable({gcStat()}, rownames=F, options = list(dom = 't'))
        
        output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
        output$AllTable  <- output$SurvTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
        output$CtrlSummary <- renderTable(NULL)
        output$SurvDataTitle <- output$SurvPlotTitle <- output$BoilerAgePlotTitle  <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
    })
    
    
    # update gc number + stat after choosing part
    observeEvent(input$ModelName,{
        if(all(input$PartID=="")) updateSelectInput(session, "PartID", choices = c("",partName()))
        if(all(input$Manufacturer=="")) updateSelectInput(session, "Manufacturer", choices = c("", manuName()))
        if(all(input$GCNum=="")) updateSelectInput(session, "GCNum", choices = c("", gcNum()))
        
        output$StatTable <- DT::renderDataTable(allStat(), extensions = 'Scroller', options = tblOpt) 
        output$ManufaTable <- DT::renderDataTable({manufaStat()}, rownames=F, options = list(dom = 't'))
        output$PartTable <- DT::renderDataTable({partStat()}, rownames=F, options = list(dom = 't'))
        output$BoilerTable <- DT::renderDataTable({boilerStat()}, rownames=F, options = list(dom = 't'))
        output$GCTable <- DT::renderDataTable({gcStat()}, rownames=F, options = list(dom = 't'))
        
        output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
        output$AllTable  <- output$SurvTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
        output$CtrlSummary <- renderTable(NULL)
        output$SurvDataTitle <- output$SurvPlotTitle <- output$BoilerAgePlotTitle  <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
        
    })
    
    # update gc number + stat after choosing part
    observeEvent(input$GCNum,{
        if(all(input$PartID=="")) updateSelectInput(session, "PartID", choices = c("",partName()))
        if(all(input$Manufacturer=="")) updateSelectInput(session, "Manufacturer", choices = c("", manuName()))
        if(all(input$ModelName=="")) updateSelectInput(session, "ModelName", choices = c("", modelName()))
        
        output$StatTable <- DT::renderDataTable(allStat(), extensions = 'Scroller', options = tblOpt)
        output$ManufaTable <- DT::renderDataTable({manufaStat()}, rownames=F, options = list(dom = 't'))
        output$PartTable <- DT::renderDataTable({partStat()}, rownames=F, options = list(dom = 't'))
        output$BoilerTable <- DT::renderDataTable({boilerStat()}, rownames=F, options = list(dom = 't'))
        output$GCTable <- DT::renderDataTable({gcStat()}, rownames=F, options = list(dom = 't'))
        
        
        output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
        output$AllTable  <- output$SurvTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
        output$CtrlSummary <- renderTable(NULL)
        output$SurvDataTitle <- output$SurvPlotTitle <- output$BoilerAgePlotTitle  <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
        
    })
    
    # Reset
    observeEvent(input$Reset, {
        updateSelectInput(session, "PartID", choices = c("",sort(unique(partIdx$part))), selected = "")
        updateSelectInput(session, "Manufacturer", choices = c("",manufacturer), selected = "")
        updateSelectInput(session, "ModelName", choices = c("",""), selected = "")
        updateSelectInput(session, "GCNum", choices = c("",""), selected = "")
        
        output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
        output$PartTable <- output$BoilerTable <- output$GCTable <-  
            output$SurvTable <- output$AllTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
        output$CtrlSummary <- renderTable(NULL)
        output$SurvDataTitle <-  output$SurvPlotTitle <- output$BoilerAgePlotTitle <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
        
        
        partIdx <<- part_df_grp[,.(total=.N, event = as.integer(sum(is_ib))), 
                                by=list(manufacturer_desc, 
                                        model_name, 
                                        part_id, 
                                        dcs_gds_desc, 
                                        gc_number)][
                                            , `:=`(part=paste(part_id, dcs_gds_desc,sep=" - ")
                                                   ,perc=round(100*event/total,2))] 
        
        fullTbl <<- partIdx[
            ,.(total = sum(total), event = as.integer(sum(event))),list(part_id, dcs_gds_desc)][
                ,perc:=round(100*event/total,2)][
                    order(-perc)
                ]
        
        minDate <<- as.Date(min(part_df_grp$actual_start_ts, na.rm=T))
        maxDate <<- as.Date(max(part_df_grp$actual_start_ts, na.rm=T))
        if(maxDate > today()) maxDate <<- today()
        
        updateTabsetPanel(session, "Tabs", "Data Summary")
    })
    
    
    # Get Survival Data
    survdata_all <- shiny::reactive({
        if(any(input$PartID!="")){
            
            req(any(input$PartID!=""))
            
            tbl <- as.data.table(part_df_grp)[as.Date(actual_start_ts) >= min(input$rngDte)&as.Date(actual_start_ts) <= max(input$rngDte)][
                if(any(input$Manufacturer!="")&chkAsSelect()) manufacturer_desc %in% input$Manufacturer else T][
                    if(any(input$PartID!="")) part_id %in% partsid() else T][
                        if(any(input$GCNum!="")&chkAsSelect()) gc_number %in% input$GCNum else T][
                            if(any(input$ModelName!="")&chkAsSelect()) model_name %in% input$ModelName else T]
            
            getSurvData(tbl)
            
        } else NULL
    })
    
    # Summarise data
    survdata <- shiny::reactive({
        if(any(input$PartID!="")){
            as.data.table(survdata_all())[,.(wgt=.N), by=list(part_id, time_since, is_ib)]
        } else NULL
    })
    
    
    
    
    
    # Run
    observeEvent(input$Run,{
        if(any(input$PartID!="")){
            
            output$SurvPlotTitle <- renderText("Survival Curve")
            
            # survival curve
            output$SurvPlot <- renderPlotly({
                
                # define number of columns depends on number of graphs
                #isFacets <- chkCombine()
                isFacets <- F
                if(isFacets){
                    gridColNum <- c(1,2,3,4,5,3,4,4,3,5,4,4,5,5,5,4,5,6,5,5)
                    nc <- ifelse(length(input$PartID) > 20, 6, gridColNum[length(input$PartID)])
                } else {
                    nc <- 1
                }
                
                x <- survdata()
                
                plotTitle <- ifelse(length(manuName())>1,"Survival Curve",manuName()) 
                
                if(!is.null(x))
                    ggplotly(autoplot(
                        survfit(Surv(time_since, is_ib) ~ part_id, data = x, weights = wgt), 
                        surv.geom="line", censor.size=0.1, facets=isFacets, scales="fixed",
                        ncol = nc) + 
                            labs(title=plotTitle, fill="Parts ID", col="Parts ID")+
                            theme(legend.position = ifelse(isFacets, "none", "bottom")))
                else NULL
            })
            
            
            # summary table under the survival curve
            output$SurvTable <- DT::renderDataTable({
                
                output$SurvDataTitle <- renderText({"Life Table from Survival Model"})
                
                x <- survdata()
                
                if(!is.null(x)){
                    
                    x2 <- x %>% group_by(part_id) %>% 
                        summarise(event = sum(wgt*is_ib, na.rm=T), 
                                  avg_evt = round(sum(time_since*wgt*is_ib, na.rm=T)/event,2),
                                  right_censored=as.integer(sum(wgt*(1-is_ib), na.rm=T)),
                                  avg_evt_rc = round(sum(time_since*wgt*(1-is_ib), na.rm=T)/right_censored,2)
                        )
                    
                    DT::datatable(x2 %>% 
                                      rbind(
                                          data.frame(part_id = "Total:", 
                                                     event = sum(x2$event, na.rm=T),
                                                     avg_evt = round(sum(x2$avg_evt*x2$event, na.rm=T)/sum(x2$event),2),
                                                     right_censored = sum(x2$right_censored, na.rm=T),
                                                     avg_evt_rc = round(sum(x2$avg_evt_rc*x2$right_censored, na.rm=T)/sum(x2$right_censored),2),
                                                     stringsAsFactors = F)) %>%
                                      mutate(event_perc = scales::percent(event/(event+right_censored),0.01),
                                      ),  options = list(dom = "t"),  callback = DT::JS(paste0("
                                var tips = ['Row Names','Part ID', 
                                'Number of reported failure that occurred at time',
                                'Average failure time (days) from installation', 
                                'Number of non-failed occurence', 
                                'Average time of non-failed occurence since installation', 
                                'failure rate (%)'],
                                                header = table.columns().header();
                                                for (var i = 0; i < tips.length; i++) {
                                                $(header[i]).attr('title', tips[i]);
                                                }"))
                    )
                }
            })
            
            # data for the survival model
            output$AllTable <- 
                DT::renderDataTable(
                    if(any(input$PartID!="")) { 
                        x <- survdata()
                        
                        if(!is.null(x))
                            DT::datatable(as.data.frame(
                                summary(survfit(Surv(time_since, is_ib) ~ part_id, 
                                                data = survdata(), 
                                                weights = wgt))[
                                                    if(length(input$PartID)==1) c(2:6,14,15) else c(10,2:6,15,16)]) %>% 
                                    mutate(strata = if(length(input$PartID)==1) input$PartID else substr(strata, 9,100),
                                           surv = scales::percent(surv,0.01),
                                           lower = scales::percent(lower,0.01),
                                           upper = scales::percent(upper,0.01)
                                    )%>%
                                    rename(c("part_id"="strata", "Days"="time")) %>% 
                                    select(part_id
                                           ,Days
                                           ,n.risk
                                           ,n.event
                                           ,n.censor
                                           ,surv
                                           ,lower
                                           ,upper) %>% 
                                    mutate(part_id = ifelse(length(partsid())==1,partsid(),part_id))
                                , options = tblOpt, callback = DT::JS(paste0("
                                var tips = ['Row Names','Part ID', 
                                'Days',
                                'Number of parts at risk at time', 
                                'Number of reported failure that occurred at time', 
                                'Number of non-failed occurence', 
                                'Estimate of non-failed occurence at time', 
                                'lower confidence limits', 
                                'upper confidence limits'],
                                                header = table.columns().header();
                                                for (var i = 0; i < tips.length; i++) {
                                                $(header[i]).attr('title', tips[i]);
                                                }"))
                            )
                        else NULL
                    } else NULL ,
                    extensions = 'Scroller') 
            
            
            output$BoilerAgePlotTitle <- renderText({"Boiler Age by Time Since Installation"})
            
            output$BoilerAgePlot <- renderPlotly({
                
                x <- survdata_all()
                
                if(!is.null(x)){
                    x <- x %>%  
                        filter(boiler_age>=0, boiler_age < 30, !is.na(time_since)) %>% 
                        mutate(brkdwn = ifelse(is_ib==1,"Known Breakdown after Installed (Event)","Installation time since (Censored)"),
                               boiler_age = paste((boiler_age %/% 5)+1,". ",(boiler_age %/% 5)*5,"-",((boiler_age %/% 5)+1)*5-1, sep=""))
                    
                    g1 <- ggplot(x ,aes(time_since, fill = boiler_age)) +
                        geom_histogram(position="stack",bins = 30, col="grey20") +
                        labs(title = "",
                             fill="Boiler Age", x="Time Since Installation")
                    
                    if(chkCombine()){
                        ggplotly(g1 + facet_wrap(~brkdwn + part_id, ncol=2))
                    } else {
                        ggplotly(g1 + facet_wrap(~brkdwn , ncol=2))
                    }
                } else NULL
                
            })
            
            
            # Association Rules-----------------------------------------------------------------------------------------------------------
            
            output$NetPlotTitle <- renderText("Association Network Graph")
            
            output$aRules <- DT::renderDT({
                
                req(input$PartID)
                
                aRules <- rulesTbl()
                
                tb_rules <- DATAFRAME(aRules) %>%
                    as_tibble() %>%
                    select(LHS, RHS, confidence, count) %>% 
                    filter(count >= 5) %>% 
                    mutate(LHS = str_remove_all(LHS, "[{}]"),
                           RHS = str_remove_all(RHS, "[{}]")) %>% 
                    arrange(desc(count)) %>%       
                    datatable() %>%
                    formatRound(columns = 3, digits = 2) 
                
                as.data.table(tb_rules$x$data[,2:5]) %>% mutate(confidence = round(confidence*100,2))
                
            }, extensions = 'Scroller', options = tblOpt) # end of renderDataTable
            
            output$aGraph <- visNetwork::renderVisNetwork({
                # https://rstudio-pubs-static.s3.amazonaws.com/####.html
                req(input$PartID)
                
                parts <- partsid()
                
                aRules <- rulesTbl()
                
                ar <- arules::subset(aRules, subset = (lhs %in% parts)|(rhs %in% parts))
                
                plot(arules::head(ar, n= nCnt(), by="count"), method = "graph", 
                     shading = "confidence", engine = "htmlwidget", 
                     control = list(max = nCnt()))
            })
            
            # Control Chart ---------------------------------------------------------------------------------------------------------
            
            output$CtrlChartTitle <- renderText("Control Chart")
            
            ctrlTbl <- reactive({
                
                parts <- partsid()
                
                ctrlunit <- ifelse(chkCtrlUnitMonth(), "month", "day")
                
                tbl <- as.data.table(part_df_grp)[as.Date(actual_start_ts) >= min(input$rngDte)&as.Date(actual_start_ts) <= max(input$rngDte)][
                    if(any(input$Manufacturer!="")&chkAsSelect()) manufacturer_desc %in% input$Manufacturer else T][
                        if(any(input$PartID!="")) part_id %in% parts else T][
                            if(any(input$GCNum!="")&chkAsSelect()) gc_number %in% input$GCNum else T][
                                if(any(input$ModelName!="")&chkAsSelect()) model_name %in% input$ModelName else T
                            ]
                
                
                tbl1 <- as_tibble(tbl) %>% 
                    filter(!is.na(actual_start_ts)) %>%
                    mutate(
                        chkdte = zoo::as.Date.numeric(ifelse(is_ib==1&!is.na(prev_addr_part_actv_start_ts),
                                                             as.Date(prev_addr_part_actv_start_ts,"%Y-%m-%d"),
                                                             as.Date(actual_start_ts,"%Y-%m-%d")))
                    ) %>% 
                    filter(chkdte >= min(input$rngDte) & chkdte <= max(input$rngDte)) %>% 
                    mutate(months=floor_date(chkdte, unit=ctrlunit)) %>%
                    group_by(part_id, dcs_gds_desc, months) %>% 
                    summarise(total=n()) 
                
                tbl2 <- tbl1 %>% 
                    group_by(part_id, dcs_gds_desc) %>% 
                    tidyr::complete( months = seq.Date(min(tbl1$months), max(tbl1$months), by=ctrlunit), fill=list(total=0))
                
                
                tbl2 %>% 
                    group_by(part_id, dcs_gds_desc) %>% 
                    mutate(avg = mean(total), 
                           stdev = sd(total), 
                           UCL = avg+3*stdev,
                           LCL = ifelse(avg - 3*stdev < 0 , 0, avg - 3*stdev),
                           outside_CL = ifelse(total>UCL|total<LCL, "Yes", "No"))
                
            })
            
            output$CtrlSummary <- renderTable({
                ctrlTbl() %>% group_by(part_id, dcs_gds_desc, avg, stdev, UCL, LCL) %>% summarise(num_outside_CL = sum(outside_CL=="Yes"))
            })
            
            output$CtrlTable <- DT::renderDT({
                ctrlTbl() %>% ungroup() %>% select(part_id, months, total, outside_CL)
            })
            
            output$CtrlPlot <- renderPlotly({
                
                tbl1 <- ctrlTbl()
                
                if(!is.null(tbl1)){
                    ggplotly(
                        ggplot(tbl1, aes(months, total, group=part_id, col=part_id)) + 
                            geom_line()+
                            geom_point(aes(months, total), alpha=ifelse(tbl1$outside_CL=="Yes",1,0 ), col="red") +
                            geom_hline(aes(yintercept=avg),col="grey30",lty=2, size=0.5) +
                            geom_hline(aes(yintercept=UCL),col="dark red",lty=2, size=0.5) +
                            geom_hline(aes(yintercept=LCL),col="dark red",lty=2, size=0.5) +
                            facet_wrap(~part_id, ncol=1) +
                            theme_minimal() + 
                            theme(panel.grid.minor = element_blank(),
                                  legend.position = "none") +
                            labs(x="")
                    )
                } else NULL
            })
            
            updateTabsetPanel(session, "Tabs", selected = "Survival Curve")
            
        } else {
            output$SurvPlot <- output$BoilerAgePlot <- output$aGraph <- output$CtrlPlot <- renderPlotly(NULL)
            output$AllTable  <- output$SurvTable <- output$aRules <- output$CtrlTable <- renderDT(NULL)
            output$CtrlSummary <- renderTable(NULL)
            output$SurvDataTitle <- output$SurvPlotTitle <- output$BoilerAgePlotTitle  <- output$NetPlotTitle <- output$CtrlChartTitle <- renderText(NULL)
            updateTabsetPanel(session, "Tabs", selected = "Summary")
        }
    })
    
    observeEvent(input$rngDteInterval,{
        output$aRules <- DT::renderDT({
            
            req(input$PartID)
            
            aRules <- rulesTbl()
            
            tb_rules <- DATAFRAME(aRules) %>%
                as_tibble() %>%
                select(LHS, RHS, confidence, count) %>% 
                filter(count >= 5) %>% 
                mutate(LHS = str_remove_all(LHS, "[{}]"),
                       RHS = str_remove_all(RHS, "[{}]")) %>% 
                arrange(desc(count)) %>%       
                datatable() %>%
                formatRound(columns = 3, digits = 2) 
            
            as.data.table(tb_rules$x$data[,2:5]) %>% mutate(confidence = round(confidence*100,2))
            
        }, extensions = 'Scroller', options = tblOpt) # end of renderDataTable
        
        output$aGraph <- visNetwork::renderVisNetwork({
            # https://rstudio-pubs-static.s3.amazonaws.com/####.html
            req(input$PartID)
            
            parts <- partsid()
            
            aRules <- rulesTbl()
            
            ar <- arules::subset(aRules, subset = (lhs %in% parts)|(rhs %in% parts))
            
            plot(arules::head(ar, n= nCnt(), by="count"), method = "graph", 
                 shading = "confidence", engine = "htmlwidget", 
                 control = list(max = nCnt()))
        })
    })
    
    
    observeEvent(input$rngDteInterval,{
        output$aRules <- DT::renderDT({
            
            req(input$PartID)
            
            aRules <- rulesTbl()
            
            tb_rules <- DATAFRAME(aRules) %>%
                as_tibble() %>%
                select(LHS, RHS, confidence, count) %>% 
                filter(count >= 5) %>% 
                mutate(LHS = str_remove_all(LHS, "[{}]"),
                       RHS = str_remove_all(RHS, "[{}]")) %>% 
                arrange(desc(count)) %>%       
                datatable() %>%
                formatRound(columns = 3, digits = 2) 
            
            as.data.table(tb_rules$x$data[,2:5]) %>% mutate(confidence = round(confidence*100,2))
            
        }, extensions = 'Scroller', options = tblOpt) # end of renderDataTable
        
        output$aGraph <- visNetwork::renderVisNetwork({
            # https://rstudio-pubs-static.s3.amazonaws.com/####.html
            req(input$PartID)
            
            parts <- partsid()
            
            aRules <- rulesTbl()
            
            ar <- arules::subset(aRules, subset = (lhs %in% parts)|(rhs %in% parts))
            
            plot(arules::head(ar, n= nCnt(), by="count"), method = "graph", 
                 shading = "confidence", engine = "htmlwidget", 
                 control = list(max = nCnt()))
        })
    })
    
    
    # update title of output
    output$ManufaTitle <- reactive({
        if(any(input$PartID!="")){
            x <- manufaStat()
            
            if(nrow(x)==1) x$manufacturer_desc else "Table Counts"
        } else "Table Counts"
    })
    
    #-------------------------------------------------------------------------------------------
    # Other outputs
    output$FullDataTitle <- renderText("App's Full Data")
    
    
    output$SurvDesc <- renderText({
        HTML("<h3>Predictive Maintenance</h3><p> <b>Survival curve</b><p> The Kaplan-Meier method are used to estimate life expectancy of parts. <p>
         X-axis of the plot shows the the number of days a part is expected to last.<p>
         Y-axis shows the probability that a part will last for the corresponding number of days on the x-axis. <p><p>
         <b>Association rules</b><p><p>
         This is a rule-based machine learning technique for discovering interesting relationships between variables in data<p>
         Generates rules of the form {X} => {Y}<p>
         Often used for market basket analysis in retail industry, but here we applied it to association of part failures in breakdown jobs<p>
	     Note the implication: co-occurrence of parts, not necessarily causality <p>
	     Chosen approach: Apriori algorithm (R implementation in arules package)<p>
	     In Association Rules tab, the table shows all rules with a count of at least 10 occurrences<p>
	     Table also shows the confidence of a given rule i.e. the probability of occurrence of {Y} given that {X} is present <p>
	     Graph beneath it shows a visualisation of the top N rules (as set by the user using the slider input)<p><p><p>
         <b>Control Chart</b><p>
         With the chosen time unit, the upper/lower control limit (UCL/LCL) are 3 time the standard deviation from the average of all data<p>
        ")
    })
    
    output$AppDesc <- renderText({
        HTML(
            paste(c('Data Scientist:', 
                    sample(c('<a href="mailto:kevinhonyin.hau@centrica.com">Kevin Hau</a>',
                             '<a href="mailto:James.Gammerman@centrica.com">James Gammerman</a>',
                             '<a href="mailto:Hamzah.Javaid@centrica.com">Hamzah Javaid</a>'),3,F)), 
                  collapse="<br>")
        )
    })
}

shinyApp(ui = ui, server = server)
