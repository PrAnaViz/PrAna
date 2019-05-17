options(shiny.maxRequestSize = 5000000*1024^6)

## Login details
login_details <- data.frame(user = c("admin","fcte20","kp300","dgk30"),
                            pswd = c("Rajarajan007#","chloramphenicol1","Ckhepoler.20","r3sist-r3sistanc3"))
login <- box(
  title = "Login",
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),
  br(),
  actionButton("Login", "Log in")
)

server <- function(input, output, session) {

#To logout back to login page
login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )

histdata <- rnorm(500)

USER <- reactiveValues(Logged = F)
      observe({
          if (USER$Logged == FALSE) {
          if (!is.null(input$Login)) {
          if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
         }
        }
    }
    })
      
      ## For drug targets
      targetdata <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        fread(inFile$datapath, header = FALSE,
              sep = ",", quote = '"')
      })
      
      dform <- reactive({ 
        a1 <-read_rds(file.path("data/", "dform.rds"))
        a1$VPID <- format(as.numeric (as.character(a1$VPID)), scientific = FALSE)
        a1$FORMCD <- format(as.numeric (as.character(a1$FORMCD)), scientific = FALSE)
        a1
      })
      
      dform_desc <- reactive({ 
        a1<-  read_rds(file.path("data/", "dform_desc.rds"))
        a1$CD <- format(as.numeric (as.character(a1$CD)), scientific = FALSE)
        a1
      })
      
      drug_group <- reactive({ 
        group_data()
        #data.table::fread(file.path("data/", "drug_group.csv"), header = TRUE)
      })
      
      bnf_api_aggr1 <- reactive({
        
        if (input$searchoption == "stp_01")
        {
          if(input$selectyear == "2018") {
            read_rds(file.path("data/", "API_Aggr_02_Saltford_201811.rds"))
          }
          else if(input$selectyear == "2017") {
            read_rds(file.path("data/", "API_Aggr_02_Saltford_201712.rds"))
          }
          else if(input$selectyear == "2016") {
            read_rds(file.path("data/", "API_Aggr_02_Saltford_201612.rds"))
          }
          else if(input$selectyear == "2015") {
            read_rds(file.path("data/", "API_Aggr_02_Saltford_201512.rds"))
          }
          else if(input$selectyear == "2014") {
            read_rds(file.path("data/", "API_Aggr_02_Saltford_201412.rds"))
          }
        }
        else if (input$searchoption == "stp_04")
        {
          if(input$selectyear == "2018") {
            read_rds(file.path("data/", "API_Aggr_02_Chippenham_201811.rds"))
          }
          else if(input$selectyear == "2017") {
            read_rds(file.path("data/", "API_Aggr_02_Chippenham_201712.rds"))
          }
          else if(input$selectyear == "2016") {
            read_rds(file.path("data/", "API_Aggr_02_Chippenham_201612.rds"))
          }
          else if(input$selectyear == "2015") {
            read_rds(file.path("data/", "API_Aggr_02_Chippenham_201512.rds"))
          }
          else if(input$selectyear == "2014") {
            read_rds(file.path("data/", "API_Aggr_02_Chippenham_201412.rds"))
          }
        }
        else if (input$searchoption == "stp_02")
        {
          if(input$selectyear == "2018") {
            read_rds(file.path("data/", "API_Aggr_02_Keynsham_201811.rds"))
          }
          else if(input$selectyear == "2017") {
            read_rds(file.path("data/", "API_Aggr_02_Keynsham_201712.rds"))
          }
          else if(input$selectyear == "2016") {
            read_rds(file.path("data/", "API_Aggr_02_Keynsham_201612.rds"))
          }
          else if(input$selectyear == "2015") {
            read_rds(file.path("data/", "API_Aggr_02_Keynsham_201512.rds"))
          }
          else if(input$selectyear == "2014") {
            read_rds(file.path("data/", "API_Aggr_02_Keynsham_201412.rds"))
          }
        }
        else if (input$searchoption == "stp_05")
        {
          if(input$selectyear == "2018") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201811.rds"))
          }
          else if(input$selectyear == "2017") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201712.rds"))
          }
          else if(input$selectyear == "2016") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201612.rds"))
          }
          else if(input$selectyear == "2015") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201512.rds"))
          }
          else if(input$selectyear == "2014") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201412.rds"))
          }
        }
        else if (input$searchoption == "stp_03")
        {
          if(input$selectyear == "2018") {
            read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201811.rds"))
          }
          else if(input$selectyear == "2017") {
            read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201712.rds"))
          }
          else if(input$selectyear == "2016") {
            read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201612.rds"))
          }
          else if(input$selectyear == "2015") {
            read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201512.rds"))
          }
          else if(input$selectyear == "2014") {
            read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201412.rds"))
          }
        }
        
        
      })
      
      bnf_api_aggr2 <- reactive({
        a1 <- bnf_api_aggr1()[ which(!bnf_api_aggr1()$'CPD' %in% c(unlist_api$V1)) , ]
        a3 <- aggregate(a1$gram, by=list(CPD=a1$'CPD',PRACTICE = a1$'PRACTICE',PERIOD = a1$'PERIOD'), FUN=sum)
        a2 <- bnf_api_aggr1()[ which(bnf_api_aggr1()$'CPD' %in% c(unlist_api$V1)) , ]
        a4 <- aggregate(a2$gram, by=list(CPD=a2$'API',PRACTICE = a2$'PRACTICE',PERIOD = a2$'PERIOD'), FUN=sum)
        a5 <- rbind(a3,a4)
        a5
      })
      
      
      
      
      filt_total_group2 <- reactive({
        a2 <- cbind(Group = drug_group()$'Group'  [match(  tolower(gsub(" ", "",total_drug5()$'CPD')),gsub(" ", "",drug_group()$'API') )],total_drug5())
        a3 <- plyr::ddply(a2, .(Group), summarise, tot = length(unique(CPD)))
        a4 <- aggregate(x~Group,data = a2, FUN=sum)
        a5 <- cbind(a4,CPD_Total = a3$'tot'  [match(  (gsub(" ", "",a4$'Group')),gsub(" ", "",a3$'Group') )])
        colnames(a5)[2] <- "gram"
        a5
      })
      
      ### Filter total consumption
      total_drug5 <- reactive({
        subset(bnf_api_aggr2(), tolower(gsub(" ", "",CPD)) %in% gsub(" ", "",targetdata()$V1))
      })
      
      filt_total_api <- reactive({
        a1 <-total_drug5()[, c("CPD","PERIOD", "PRACTICE","x")]
        a2 <- aggregate(a1$x, by=list(API=gsub(" ", "",a1$'CPD')), FUN=sum)
        a3 <- cbind(Group = drug_group()$'Group'  [match(  tolower(gsub(" ", "",a2$'API')),gsub(" ", "",drug_group()$'API') )],a2)
        a4 <- cbind(a3, kg= a3$x/1000)
        a4
      })
      
      output$tab_filt_total_api <- renderDataTable({
        filt_total_api()
      })
      
      filt_total_practice <- reactive({
        a1 <-total_drug5()[, c("CPD","PERIOD", "PRACTICE","x")]
        a2 <- aggregate(a1$x, by=list(API=gsub(" ", "",a1$'CPD'),PRACTICE = a1$'PRACTICE'), FUN=sum)
        a3 <- cbind(a2, kg= a2$x/1000)
        a3
      })
      
      filt_total_period <- reactive({
        a1 <-total_drug5()[, c("CPD","PERIOD", "PRACTICE","x")]
        a2 <- aggregate(a1$x, by=list(API=gsub(" ", "",a1$'CPD'),PERIOD = a1$'PERIOD'), FUN=sum)
        a3 <- cbind(a2, kg= a2$x/1000)
        a3
      })
      
      filt_total_form <- reactive({
        a1 <- subset(bnf_api_aggr1(), tolower(CPD) %in% targetdata()$V1)
        a2 <- aggregate(a1$gram, by=list(API=gsub(" ", "",a1$'CPD'),API_form = a1$'API'), FUN=sum)
        a3 <- cbind(a2, kg= a2$x/1000)
        a3
      })
      
      filt_med_form <- reactive({
        a1 <- subset(bnf_api_aggr1(), tolower(CPD) %in% targetdata()$V1)
        a2 <- cbind(a1, DForm_desc = dform_desc()$'DESC'[match(gsub(" ", "",a1$'DFORM'), gsub(" ", "",  dform_desc()$'CD'))])
        a3 <- aggregate(a2$gram, by=list(API=gsub(" ", "",a2$'CPD'), medform = a2$'DForm_desc'), FUN=sum)
        a4 <- cbind(a3, kg= a3$x/1000)
        a4
      })
      
      
      data_api_full <- reactive({
        a1 <-subset(data_timeseries02(), tolower(gsub(" ", "",CPD)) %in% gsub(" ", "",targetdata()$V1))
        a1$"PERIOD" <- as.Date(paste0(a1$"PERIOD", "01"), format = "%Y%m%d")
        a2 <- cbind(a1,year=(substr(a1$PERIOD, 1, 4)))
        a3 <- aggregate(a2$x, by=list(STW=a2$'STW',CPD=a2$'CPD',PERIOD = a2$'PERIOD', Year = a2$'year'), FUN=sum)
        a4 <- subset(a3, Year %in%  input$selectyear)
        a4
      })
      
      data_api_full2 <- reactive({
        a1 <- data_api_full()
        a2 <- cbind(a1, kg = a1$x/1000, month=(substr(a1$PERIOD, 6, 7)))
        a2
      })
      
      data_api_full3 <- reactive({
        cast(data_api_full2(), CPD ~PERIOD, na.rm = TRUE, value = 'kg')
      })
  
      #### Time series plots #####
      
      
      data_timeseries01 <- reactive({
        
        if (input$searchoption2 == "stp_01")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201412.rds"))
          a6 <- rbind.data.frame(a1,a2,a3,a4,a5)
          a7 <- cbind(STW = "stp_01",a6)
          a7
        }
        else if (input$searchoption2 == "stp_04")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201412.rds"))
          a6 <- rbind.data.frame(a1,a2,a3,a4,a5)
          a7 <- cbind(STW = "stp_04",a6)
          a7
          
        }
        else if (input$searchoption2 == "stp_02")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201412.rds"))
          a6 <- rbind.data.frame(a1,a2,a3,a4,a5)
          a7 <- cbind(STW = "stp_02",a6)
          a7
        }
        else if (input$searchoption2 == "stp_05")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201412.rds"))
          a6 <- rbind.data.frame(a1,a2,a3,a4,a5)
          a7 <- cbind(STW = "stp_05",a6)
          a7
        }
        else if (input$searchoption2 == "stp_03")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201412.rds"))
          a6 <- rbind.data.frame(a1,a2,a3,a4,a5)
          a7 <- cbind(STW = "stp_03",a6)
          a7
        }
        
        
      })
      
      unlist_api <-   data.table::fread(file.path("data/", "unlist_api.csv"), header = FALSE)
      
      data_timeseries02 <- reactive({
        a1 <- data_timeseries01()[ which(!data_timeseries01()$'CPD' %in% c(unlist_api$V1)) , ]
        a3 <- aggregate(a1$gram, by=list(STW=a1$'STW', CPD=a1$'CPD',PRACTICE = a1$'PRACTICE',PERIOD = a1$'PERIOD'), FUN=sum)
        a2 <- data_timeseries01()[ which(data_timeseries01()$'CPD' %in% c(unlist_api$V1)) , ]
        a4 <- aggregate(a2$gram, by=list(STW=a2$'STW', CPD=a2$'API',PRACTICE = a2$'PRACTICE',PERIOD = a2$'PERIOD'), FUN=sum)
        a5 <- rbind(a3,a4)
        a5
      })
      
      group_data <- reactive({
        inFile <- input$file2
        if (is.null(inFile))
          return(NULL)
        
        fread(inFile$datapath, header = TRUE,
              sep = ",", quote = '"')
      }) 
      
      ### Filter total consumption
      data_timeseries03 <- reactive({
        subset(data_timeseries02(), tolower(gsub(" ", "",CPD)) %in% gsub(" ", "",targetdata()$V1))
      })
      
      data_timeseries04 <- reactive({
        a1 <- subset(data_timeseries03(), CPD %in% c(input$plot_api_code) )
        a1$"PERIOD" <- as.Date(paste0(a1$"PERIOD", "01"), format = "%Y%m%d")
        a1
      })
      
      data_timeseries05 <- reactive({
        a1 <- cbind(data_timeseries03(),year=(substr(data_timeseries03()$PERIOD, 1, 4)))
        a2 <- aggregate(a1$x, by=list(API=a1$'CPD',Year = a1$year), FUN=sum)
        a3 <- cbind(a2,kg = a2$x/1000)
        a3
      })
      
      data_timeseries06 <- reactive({
        subset(data_timeseries05(), API %in% c(input$plot_api_code) )
      })
      
      data_timeseries07 <- reactive({
        a1 <- cbind(data_timeseries04(),year=(substr(data_timeseries04()$PERIOD, 1, 4)))
        a2 <- subset(a1, CPD %in% c(input$plot_api_code) )
        a3 <- cbind(a2,kg = a2$x/1000)
        a3
      })
      
      output$tab_data_timeseries07 <- renderDataTable({
        data_timeseries07()
      })
      output$tab_data_timeseries08 <- renderDataTable({
        data_timeseries08()
      })
      
      data_timeseries08 <- reactive({
        s <- event_data("plotly_click", source = "time_bar01")
        if (length(s) == 0) {
          NULL
        }   
        else {
        a0 <- subset(data_timeseries03(), CPD %in% data_timeseries03()$CPD[s$pointNumber+1] )
        a0$"PERIOD" <- as.Date(paste0(a0$"PERIOD", "01"), format = "%Y%m%d")
        a1 <- cbind(a0,year=(substr(a0$PERIOD, 1, 4)))
        a2 <- subset(a1,CPD %in% a1$CPD[s$pointNumber+1]  )
        #a2 <- subset(a1, CPD %in% c(input$plot_api_code) )
        a3 <- cbind(a2,kg = a2$x/1000)
        a3
        }
      })
      
      data_api_monthwise_full <- reactive({
        a1 <- data_timeseries03()
        a1$"PERIOD" <- as.Date(paste0(a1$"PERIOD", "01"), format = "%Y%m%d")
        a2 <- cbind(a1,year=(substr(a1$PERIOD, 1, 4)))
        a3 <- aggregate(a2$x, by=list(STW=a2$'STW',CPD=a2$'CPD',PERIOD = a2$'PERIOD', Year = a2$'year'), FUN=sum)
        a4 <- cbind(a3, kg = a3$x/1000, month=(substr(a3$PERIOD, 6, 7)))
        a5 <- cbind(a4, mmdd = a4 %>% 
                      unite(mmdd, c("Year", "month"))
        )
        a5
      })
      
      data_api_monthwise_full2 <- reactive({
        cast(data_api_monthwise_full(), CPD ~mmdd.mmdd, na.rm = TRUE, value = 'kg')
      })
      
      data_api_yearwise_full1 <- reactive({
        reshape::cast(data_timeseries05(), API ~Year, na.rm = TRUE, value = 'kg')
      })
      
      data_time_comp03 <- reactive({
        a1 <- data_api_monthwise_full2()
        a1$'CPD' <- NULL
        a2 <- data_api_yearwise_full1()
        a3 <-cbind(API = as.character(a2$'API'), Y2014 = a2$'2014'/12, 
                   Y2015 <- a2$'2015'/12, Y2016 <- a2$'2016'/12,
                   Y2017 <- a2$'2017'/12, Y2018 <- a2$'2018'/11)
        colnames(a3) <- c("API","2014","2015","2016","2017","2018")
        colnames(a3) <- paste(colnames(a3), "YF", sep = "_")
        a4 <- cbind(a3,a1)
        #a5 <- reshape2::melt(a4,id.vars = "API_YF")
        a5 <- gather(a4, key, value, -API_YF)
        a6 <- cbind(a5,Year=substr(a5$key, 1, 4),Month = substr(a5$key,6,7))
        #a6 <- cbind(a5,Year=substr(a5$variable, 1, 4),Month = substr(a5$variable,6,7))
        a7 <- cbind(a6, kg= as.numeric(as.character(a6$value)))
        a8 <- cast (a7, Year+API_YF ~ Month, value = 'kg')
        a8
      })
      
      
      data_time_comp01 <- reactive({
        cbind(data_api_yearwise_full1(),data_api_monthwise_full2())
      })
      
      data_time_comp02 <- reactive({
        a1 <- data_time_comp01()[,grepl(input$comp_month, colnames(data_time_comp01())) ]
        a2 <- data_api_yearwise_full1()
        a3 <-cbind(API = as.character(a2$'API'), Y2014 = a2$'2014'/12, 
                   Y2015 <- a2$'2015'/12, Y2016 <- a2$'2016'/12,
                   Y2017 <- a2$'2017'/12, Y2018 <- a2$'2018'/11)
        colnames(a3) <- c("API","2014","2015","2016","2017","2018")
        colnames(a3) <- paste(colnames(a3), "YF", sep = "_")
        a4 <- cbind(a3,a1)
        #a5 <- reshape2::melt(a4,id.vars = "API_YF")
        a5 <- gather(a4, key, value, -API_YF)
        a6 <- cbind(a5,Year=substr(a5$key, 1, 4),Month = substr(a5$key,6,7))
        #a6 <- cbind(a5,Year=substr(a5$variable, 1, 4),Month = substr(a5$variable,6,7))
        a7 <- cbind(a6, kg= as.numeric(as.character(a6$value)))
        a7
      })
      
      data_time_comp04 <- reactive({
        a1 <- data_api_monthwise_full2()
        a1$'CPD' <- NULL
        a2 <- data_api_yearwise_full1()
        a3 <-cbind(API = as.character(a2$'API'), Y2014 = a2$'2014'/12, 
                   Y2015 <- a2$'2015'/12, Y2016 <- a2$'2016'/12,
                   Y2017 <- a2$'2017'/12, Y2018 <- a2$'2018'/11)
        colnames(a3) <- c("API","2014","2015","2016","2017","2018")
        colnames(a3) <- paste(colnames(a3), "YF", sep = "_")
        a4 <- cbind(a3,a1)
        #a5 <- reshape2::melt(a4,id.vars = "API_YF")
        a5 <- gather(a4, key, value, -API_YF)
        a6 <- cbind(a5,Year=substr(a5$key, 1, 4),Month = substr(a5$key,6,7))
        #a6 <- cbind(a5,Year=substr(a5$variable, 1, 4),Month = substr(a5$variable,6,7))
        a7 <- cbind(a6, kg= as.numeric(as.character(a6$value)))
        a8 <- subset(a7, API_YF %in% c(input$plot_api_code) )
        a8
      })
      
      data_time_comp05 <- reactive({
        a1 <- data_time_comp04()[grepl("_YF", data_time_comp04()$key ), ]
        a2 <- as.data.frame( cbind(Year =as.numeric(as.character(a1$"Year")),kg = a1$"kg"))
        a2
      })
      
      
      data_time_comp06 <- reactive({
        data_time_comp04()[!grepl("_YF", data_time_comp04()$key ),]
      })
      
      
      data_time_comp07_02 <- reactive({
        a1 <- data_api_monthwise_full2()
        a1$'CPD' <- NULL
        a2 <- data_api_yearwise_full1()
        a3 <-cbind(API = as.character(a2$'API'), Y2014 = a2$'2014'/12, 
                   Y2015 <- a2$'2015'/12, Y2016 <- a2$'2016'/12,
                   Y2017 <- a2$'2017'/12, Y2018 <- a2$'2018'/11)
        colnames(a3) <- c("API","2014","2015","2016","2017","2018")
        colnames(a3) <- paste(colnames(a3), "YF", sep = "_")
        a4 <- cbind(a3,a1)
        a5 <- reshape2::melt(a4,id.vars = "API_YF")
        a6 <- cbind(a5,Year=substr(a5$variable, 1, 4),Month = substr(a5$variable,6,7))
        a7 <- cbind(a6, kg= as.numeric(as.character(a6$value)))
        a7
      })
      
      output$tab_data_time_comp07 <- DT::renderDataTable({
        data_time_comp07()
      })
      
      data_time_comp07 <- reactive({
        a1 <- data_api_monthwise_full2()
        a1$'CPD' <- NULL
        a2 <- data_api_yearwise_full1()
        a3 <-cbind(API = as.character(a2$'API'), Y2014 = a2$'2014'/12, 
                   Y2015 <- a2$'2015'/12, Y2016 <- a2$'2016'/12,
                   Y2017 <- a2$'2017'/12, Y2018 <- a2$'2018'/11)
        colnames(a3) <- c("API","2014","2015","2016","2017","2018")
        colnames(a3) <- paste(colnames(a3), "YF", sep = "_")
        a4 <- cbind(a3,a1)
        a5 <- gather(a4, key, value, -API_YF)
        a6 <- cbind(a5,Year=substr(a5$key, 1, 4),Month = substr(a5$key,6,7))
        a7 <- cbind(a6, kg= as.numeric(as.character(a6$value)))
        a7
      })
      
      output$tab_data_time_comp07_02 <- DT::renderDataTable({
        data_time_comp07_02()
      })
      
      data_time_comp08 <- reactive({
      s <- event_data("plotly_click", source = "time_bar01")
      if (length(s) == 0) {
        NULL
      }   
      else {
        dat <- data_time_comp03()
        a1 <- subset(dat,API_YF %in% dat$API_YF[s$pointNumber+1]  )
        a1
      }
      })
      
      
      
      
#### Compare STW
      
      compare_stw01 <- reactive({
        
        if (input$selectyear02 == "2014")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201412.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201412.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201412.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201412.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201412.rds"))
          a11 <- cbind(STW = "stp_01",a1)
          a21 <- cbind(STW = "stp_04",a2)
          a31 <- cbind(STW = "stp_02",a3)
          a41 <- cbind(STW = "stp_05",a4)
          a51 <- cbind(STW = "stp_03",a5)
          a6 <- rbind.data.frame(a11,a21,a31,a41,a51)
          a6
        }
        else if (input$selectyear02 == "2015")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201512.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201512.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201512.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201512.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201512.rds"))
          a11 <- cbind(STW = "stp_01",a1)
          a21 <- cbind(STW = "stp_04",a2)
          a31 <- cbind(STW = "stp_02",a3)
          a41 <- cbind(STW = "stp_05",a4)
          a51 <- cbind(STW = "stp_03",a5)
          a6 <- rbind.data.frame(a11,a21,a31,a41,a51)
          a6
        }
        else if (input$selectyear02 == "2016")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201612.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201612.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201612.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201612.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201612.rds"))
          a11 <- cbind(STW = "stp_01",a1)
          a21 <- cbind(STW = "stp_04",a2)
          a31 <- cbind(STW = "stp_02",a3)
          a41 <- cbind(STW = "stp_05",a4)
          a51 <- cbind(STW = "stp_03",a5)
          a6 <- rbind.data.frame(a11,a21,a31,a41,a51)
          a6
        }
        else if (input$selectyear02 == "2017")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201712.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201712.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201712.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201712.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201712.rds"))
          a11 <- cbind(STW = "stp_01",a1)
          a21 <- cbind(STW = "stp_04",a2)
          a31 <- cbind(STW = "stp_02",a3)
          a41 <- cbind(STW = "stp_05",a4)
          a51 <- cbind(STW = "stp_03",a5)
          a6 <- rbind.data.frame(a11,a21,a31,a41,a51)
          a6
        }
        else if (input$selectyear02 == "2018")
        {
          a1 <- read_rds(file.path("data/", "API_Aggr_02_Saltford_201811.rds"))
          a2 <- read_rds(file.path("data/", "API_Aggr_02_Chippenham_201811.rds"))
          a3 <- read_rds(file.path("data/", "API_Aggr_02_Keynsham_201811.rds"))
          a4 <- read_rds(file.path("data/", "API_Aggr_02_Trowbridge_201811.rds"))
          a5 <- read_rds(file.path("data/", "API_Aggr_02_Avonmouth_201811.rds"))
          a11 <- cbind(STW = "stp_01",a1)
          a21 <- cbind(STW = "stp_04",a2)
          a31 <- cbind(STW = "stp_02",a3)
          a41 <- cbind(STW = "stp_05",a4)
          a51 <- cbind(STW = "stp_03",a5)
          a6 <- rbind.data.frame(a11,a21,a31,a41,a51)
          a6
        }
      })
      
      
      
      compare_stw02 <- reactive({
        a1 <- compare_stw01()[ which(!compare_stw01()$'CPD' %in% c(unlist_api$V1)) , ]
        a3 <- aggregate(a1$gram, by=list(STW=a1$'STW',CPD=a1$'CPD',PRACTICE = a1$'PRACTICE',PERIOD = a1$'PERIOD'), FUN=sum)
        a2 <- compare_stw01()[ which(compare_stw01()$'CPD' %in% c(unlist_api$V1)) , ]
        a4 <- aggregate(a2$gram, by=list(STW=a2$'STW',CPD=a2$'API',PRACTICE = a2$'PRACTICE',PERIOD = a2$'PERIOD'), FUN=sum)
        a5 <- rbind(a3,a4)
        a5
      })
      
      output$tab_compare_stw02 <- renderDataTable({
        compare_stw02()
      })
      
      
      compare_stw03 <- reactive({
        subset(compare_stw02(), tolower(gsub(" ", "",CPD)) %in% gsub(" ", "",targetdata()$V1))
      })
      
      output$tab_compare_stw03 <- renderDataTable({
        compare_stw03()
      })
      
      compare_stw04 <- reactive({
        a1 <- cbind(compare_stw03(),year=(substr(compare_stw03()$PERIOD, 1, 4)), month=(substr(compare_stw03()$PERIOD, 5, 6)))
        a2 <- aggregate(a1$x, by=list(STW=a1$'STW',API=a1$'CPD',Year = a1$year), FUN=sum)
        a3 <- cbind(a2,kg = a2$x/1000)
        a3
      })
      
      output$tab_compare_stw04 <- renderDataTable({
        compare_stw04()
      })
      
      
      stw_compare_year01<- reactive({
        reshape::cast(compare_stw04(),Year + API ~ STW, value = 'kg')
       })
      
      stw_compare_year02<- reactive({
        a1 <- aggregate(compare_stw03()$x, by=list(STW=compare_stw03()$'STW',API=compare_stw03()$'CPD',Period = compare_stw03()$PERIOD), FUN=sum)
        a2 <- cbind(a1,kg = a1$x/1000)
        a3 <- reshape::cast(a2,API + Period ~ STW, value = 'kg')
        a3
        
      })
     
      output$compare_3d_plot01 <- renderPlotly({
        withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        
        plot_ly(data = compare_stw04() , 
                x=  compare_stw04()$STW,
                y=compare_stw04()$API  , 
                z= compare_stw04()$kg ,
                color= compare_stw04()$API,
                type = "scatter3d",
                mode = "lines+markers",
                showlegend = TRUE
        ) %>%
          
          layout(autosize = T, width = 1000, height = 1000,
                 scene = list(
                   yaxis = list(title = "API"), 
                   zaxis = list(title = "Total API (kg)"), 
                   xaxis = list(title = "Catchment")) )
        
      })
      })
      
      
      
      
      output$compare_stw_bar_plot1 <- renderPlotly({ 
        withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
          
          p <- ggplot(data=compare_stw04(), aes(x=API, y=kg, fill=API, key = API )) +  
            geom_bar(stat="identity", position=position_dodge())+ 
            facet_grid( STW~. )+
            ylab("Total API (kg)") +
            xlab("API")+
            ggtitle(paste0("Total Prescription for the year ",input$selectyear02," in kg"))+
            theme(axis.title.x = element_text(face="bold", size=10),
                  axis.title.y = element_text(face="bold", size=10),
                  axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
          ggplotly(p, source = "time_bar01",tooltip =c("API","kg","Year") )# %>% layout(dragmode = "select")
        })
      })
      
      
      
      
### Outputs
output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          tags$hr(),
          uiOutput("uifile1"),
          uiOutput("uifile2"),
          bsTooltip ('uifile2',"Click here to upload your data","bottom", options = NULL),
          tags$hr(),
          menuItem(
            "Time Series",
            tabName = "timeseries",
            icon = icon("line-chart")
          ),
          menuItem("Particular Year",
                   tabName = "year_wise",
                   icon = icon("pie-chart")),
          menuItem("Compare",
                   tabName = "compare_stw",
                   icon = icon("pie-chart"))
          
        )
      )
    }
})
  
## UI File inputs

output$uifile1 <- renderUI({
  tipify(fileInput('file1', 'Upload your Targets:',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   )),
         "Click here to upload your data")
})

output$uifile2 <- renderUI({
  tipify(fileInput('file2', 'Upload your Group:',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   )),
         "Click here to upload your data")
})


## UI Inputs  - Select Inputs

## To see the Practice code
output$practiceinput <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectInput(inputId="practice_code",
                label="Select GP Practice:",
                choices=(total_drug5()$PRACTICE),selected =head(unique (total_drug5()$PRACTICE)),  multiple = FALSE)
  })
})

output$practiceinput2 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectInput(inputId="plot_practice_code",
                label="Select GP Practice:",
                choices=(data_timeseries03()$PRACTICE),selected =head(unique (data_timeseries03()$PRACTICE)),  multiple = FALSE)
  })
})

output$apiinput1 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectInput(inputId="plot_api_code",
                label="Select API:",
                choices=(data_timeseries03()$CPD),selected =head(unique (data_timeseries03()$CPD)),  multiple = FALSE)
  })
})


output$timeseries_line_plot1 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=data_timeseries06(), aes(x=Year, y=kg, color = Year)) +
      geom_point(size=2, shape=21)+
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle(paste0(input$plot_api_code, " - Total Prescription for the year 2014, 2015, 2016, 2017, 2018 in kg") )+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    
  })
})

output$timeseries_bar_plot2 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    plot_ly(data_timeseries06(), x = ~Year, y = ~kg, color = ~Year, type = "bar", source = "timeseries_bar_plot2") %>%
      layout(title = paste0(input$plot_api_code,' - Total Prescription for the year 2014, 2016, 2017, 2018 in kg'),
             xaxis = list(title = "API"), 
             yaxis = list(title = "Total API (kg)"))  
  })
})

output$timeseries_line_plot2 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "timeseries_bar_plot2")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a0 <- subset(data_timeseries07(), year %in%    s[["x"]])
      a0$PERIOD <-  as.character(a0$PERIOD)
      a1 <- aggregate(a0$'kg', by=list(STW=a0$'STW',CPD=a0$'CPD',PERIOD = a0$'PERIOD', Year = a0$'year'), FUN=sum)
      a2 <- ggplot(data=a1, aes(x=PERIOD, y=x, fill=PERIOD)) +  
        geom_bar(stat="identity", position=position_dodge())+
        ylab("API (kg)") +
        xlab("Year/Month")+
        ggtitle(paste0("Monthwise Prescription for the year - ",s[["x"]]))+
        theme(axis.title.x = element_text(face="bold", size=10),
              axis.title.y = element_text(face="bold", size=10),
              axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
      
      a2
    }
  })
})

output$timeseries_line_plot3 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    a1 <- aggregate(data_timeseries07()$'kg', by=list(STW=data_timeseries07()$'STW',CPD=data_timeseries07()$'CPD',PERIOD = data_timeseries07()$'PERIOD', Year = data_timeseries07()$'year'), FUN=sum)
    a2 <- cbind(a1,month=(substr(a1$PERIOD, 6, 7)))
    p <- ggplot(data=a2, aes(x=month, y=x, group = Year, color = Year, key = Year )) +
      geom_line()+
      geom_point(size=2, shape=21)+
      ylab("API (kg)") +
      xlab("Month")+
      ggtitle( paste0(input$plot_api_code, " - monthly prescription for the year 2014, 2016, 2017, 2018 in kg"))+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
})

output$timeseries_line_plot4 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    a1 <- aggregate(data_timeseries08()$'kg', by=list(STW=data_timeseries08()$'STW',CPD=data_timeseries08()$'CPD',PERIOD = data_timeseries08()$'PERIOD', Year = data_timeseries08()$'year'), FUN=sum)
    a2 <- cbind(a1,month=(substr(a1$PERIOD, 6, 7)))
    p <- ggplot(data=a2, aes(x=month, y=x, group = Year, color = Year, key = Year )) +
      geom_line()+
      geom_point(size=2, shape=21)+
      ylab("API (kg)") +
      xlab("Month")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
})


output$timeseries_box_plot1 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    a1 <- aggregate(data_timeseries07()$'kg', by=list(STW=data_timeseries07()$'STW',CPD=data_timeseries07()$'CPD',PERIOD = data_timeseries07()$'PERIOD', Year = data_timeseries07()$'year'), FUN=sum)
    a2 <- cbind(a1,month=(substr(a1$PERIOD, 6, 7)))
    p <- ggplot(data=a2, aes(x=month, y=x, fill = month, key = Year )) +
      geom_boxplot()+
      #geom_point(size=2, shape=21)+
      ylab("API (kg)") +
      xlab("Month")+
      ggtitle(paste0(input$plot_api_code, " - monthly prescription for the year 2014, 2016, 2017, 2018 in kg"))+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
})


output$timeseries_bar_plot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    p <- ggplot(data=data_timeseries05(), aes(x=API, y=kg, fill=Year, key = Year )) +  
      geom_bar(stat="identity", position=position_dodge())+ 
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p, source = "time_bar01",tooltip =c("API","kg","Year") )# %>% layout(dragmode = "select")
  })
})

output$timeseries_bar_plot4 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "time_bar01")
    if (length(s) == 0) {
      NULL
    }   
    else {
    a1 <- subset(data_timeseries05(),API %in% data_timeseries05()$API[s$pointNumber+1]  )
    p <- ggplot(data=a1, aes(x=API, y=kg, fill=Year, key = Year )) +  
      geom_bar(stat="identity", position=position_dodge())+ 
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle(paste0(data_timeseries05()$API[s$pointNumber+1]," for the year 2014, 2016, 2017, 2018 in kg"))+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p, source = "time_bar02",tooltip =c("API","kg","Year") ) %>% layout(dragmode = "select")
    }
    })
})


output$timeseries_line_comp_plot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "time_bar01")
    if (length(s) == 0) {
      NULL
    }   
    else {
      dat <- data_time_comp07()
      a1 <- subset(dat,API_YF %in% dat$API_YF[s$pointNumber+1]  )
      a2 <- a1[!grepl("_YF", a1$key ),]
      a3 <- a1[grepl("_YF", a1$key ), ]
      a4 <- as.data.frame( cbind(Year =as.numeric(as.character(a3$"Year")),kg = a3$"kg"))
      
    
    p <- ggplot(data=a2) +  
      geom_point( aes(x=Month, y=kg, color = Year))+ 
      geom_hline(data = a4, aes(yintercept =a4$kg))+
      facet_grid( Year~. )+
      ylab("Total API (kg)") +
      xlab("Month")+
      ggtitle(paste0(dat$API_YF[s$pointNumber+1], " prescription for the year 2014, 2016, 2017, 2018 in kg"))+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    }
  })
})

output$timeseries_comp_plot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=data_time_comp02(), aes(x=API_YF, y=kg, fill=Month)) +  
      geom_bar(stat="identity", position=position_dodge())+ 
      facet_grid(.~ Year)+
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
  })
})


output$timeseries_comp_plot2 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=data_time_comp02(), aes(x=API_YF, y=kg, color = Month)) +  
      geom_point()+ 
      facet_grid( .~Year )+
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
  })
})

output$timeseries_comp_plot3 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=data_time_comp06()) +  
      geom_point( aes(x=Month, y=kg, color = Year))+ 
      geom_hline(data = data_time_comp05(), aes(yintercept =data_time_comp05()$kg))+
      facet_grid( Year~. )+
      ylab("Total API (kg)") +
      xlab("Month")+
      ggtitle(paste0(input$plot_api_code, " - monthly prescription for the year 2014, 2016, 2017, 2018 in kg"))+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
  })
})

output$plot_compound_timeseries01 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    a1 <- data_timeseries04()
    a2 <- ggplot(data=a1, aes(x=PERIOD, y=x, group=PRACTICE,colour=PRACTICE)) +  geom_line()+ geom_point(size=2, shape=21, fill="white")
    a2 + labs(colour = "Practice", y = "grams", title = a1$'STW')
    
  })
})


output$timeseries_bar_plot3 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=data_api_monthwise_full(), aes(x=CPD, y=kg, fill=month)) +
      geom_bar(stat="identity", position=position_dodge())+
      ylab("Total API (kg)") +
      xlab("API")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
  })
})


output$filt_group_pie <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    plot_ly(filt_total_group2(), labels = ~Group, values = ~gram,  type = "pie", source = "filt_pie",
            textposition = 'inside',
            textinfo = 'label+text', #+percent
            hoverinfo = 'label+text',
            text = ~paste( CPD_Total, ' Compounds')
    ) %>%
      layout(title = 'Drug Class',
             showlegend = FALSE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }) 
})

output$filt_total_barplot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    plot_ly(filt_total_api(), x = ~API, y = ~kg,  type = "bar", source = "filt_total_barplot1") %>%
      layout(title = 'Total Drug Prescribed over year (in kg)',
             xaxis = list(title = "Drugs"), 
             yaxis = list(title = "kg"))      
  }) 
})

output$filt_total_lineplot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset(filt_total_period(), API %in%    s[["x"]])
      a1$PERIOD <-  as.character(a1$PERIOD)
      a2 <-  plot_ly(a1, x = ~PERIOD, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = s[["x"]],
               xaxis = list(title = "Year/Month"),
               yaxis = list (title = "kg"))
      a2
    }
  })
})

output$filt_total_lineplot2 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset(filt_total_practice(), API %in%    s[["x"]])
      a2 <-  plot_ly(a1, x = ~PRACTICE, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = s[["x"]],
               xaxis = list(title = "GP Practice Code"),
               yaxis = list (title = "kg"))
      a2
    }
  })
})




output$filt_total_lineplot3 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset(filt_total_form(), API %in%    s[["x"]])
      
      a2 <-  plot_ly(a1, x = ~API_form, y = ~kg, type = 'scatter',mode = 'lines+markers') %>%
        layout(title = s[["x"]],
               xaxis = list(title = "Chemical Form"),
               yaxis = list (title = "kg"))
      a2
    }
  })
})




output$filt_total_lineplot4 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset(filt_med_form(), API %in%    s[["x"]])
      
      a2 <-  plot_ly(a1, x = ~medform, y = ~kg, type = 'scatter',mode = 'lines+markers') %>%
        layout(title = s[["x"]],
               xaxis = list(title = "Medicinal Form"),
               yaxis = list (title = "kg"))
      a2
    }
  })
})

output$plot_compound_monthwise <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a0 <- subset(total_drug5(), CPD %in%    s[["x"]])
      a1 <- cbind(a0, kg= a0$x/1000)
      a2 <- ggplot(data=a1, aes(x=PERIOD, y=kg, group=PRACTICE,colour=PRACTICE)) +  geom_line()+ geom_point(size=2, shape=21, fill="white")
      a2 + labs(colour = "Practice", y = "kg", title = s[["x"]])
    }
  })
})

output$timeseries_line_plot5 <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    a1 <- data_api_full()
    a2 <- cbind(a1, kg = a1$x/1000, month=(substr(a1$PERIOD, 6, 7)))
    p <- ggplot(data=a2, aes(x=month, y=kg, group = Year, color = CPD, key = CPD )) +
      geom_line()+
      geom_point(size=2, shape=21)+
      ylab("API (kg)") +
      xlab("Month")+
      ggtitle(paste0("Total Prescription for the year ", input$selectyear," in kg") )+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
})


output$plot_cpd_monthwise <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    p <- ggplot(data=filt_total_period(), aes(x=PERIOD, y=x, group=API,colour=API)) +  geom_line()
    
    #geom_point(colour="red", size=1, shape=21, fill="white")
    p + labs(colour = "Drugs", y = "grams", title = "Drug prescibed each month (in g)")
  })
})

output$plot_practice_monthwise <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    a1 <- tot_cpd_gp_wise()
    p <- ggplot(data=a1, aes(x=PERIOD, y=kg, group=CPD,colour=CPD)) +  geom_line() +
          geom_point(size=2, shape=21, fill="white")
    p + labs(colour = "Drugs", y = "API (kg)", title = "Drug prescibed each month (in kg)")
  })
})

tot_cpd_gp_wise <- reactive({
  a0 <- subset(total_drug5(), PRACTICE %in%    c(input$practice_code))
  a1 <- cbind(a0, kg = a0$x/1000)
  a1
})

output$tab_tot_cpd_gp_wise <- renderDataTable({
  tot_cpd_gp_wise()
})

output$tab_compare_stw01 <- renderDataTable({
  compare_stw01()
})


tot_cpd_gp <- reactive({
  a0 <- subset(total_drug5())
  a1 <- cbind(a0, kg = a0$x/1000)
  a2 <- reshape::cast(a1, CPD + PRACTICE ~ PERIOD, value = 'kg', na.rm= TRUE)
  a2
})

output$tab_tot_cpd_gp <- renderDataTable({
  tot_cpd_gp()
})
tot_cpd_gp02 <- reactive({
  a0 <- subset(total_drug5(), PRACTICE %in%    c(input$practice_code))
  a1 <- cbind(a0, kg = a0$x/1000)
  a2 <- reshape::cast(a1, CPD + PRACTICE ~ PERIOD, value = 'kg', na.rm= TRUE)
  a2
})

## Click events

output$click <- renderPrint({
  d <- event_data("plotly_click")
  if (is.null(d)) "Click events appear here (double-click to clear)" else d$key
})

output$click_year <- renderPrint({
  d <- event_data("plotly_click",source = "time_bar01")
  if (is.null(d)) "Click on the Plot (double-click to clear)" else d$key
})
output$text_xaxis <- renderPrint({
  d <- event_data("plotly_click",source = "time_bar01")
  if (is.null(d)) "Click on the Plot (double-click to clear)" else as.list(d)
})
output$text_api <- renderPrint({
  d <- event_data("plotly_click",source = "time_bar01")
  if (is.null(d)) "Click on the Plot (double-click to clear)" else data_timeseries05()$API[d$pointNumber+1]
})

### Download buttons
output$downdat.time_barplot1 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_api_yearwise_full1(), file, row.names = TRUE)
  }
)

output$downdat.data_api_monthwise_full2 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_api_monthwise_full2(), file, row.names = TRUE)
  }
)

output$downdat.data_time_comp03  = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_time_comp03(), file, row.names = TRUE)
  }
)

output$downdat.data_time_comp04  = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_time_comp08(), file, row.names = TRUE)
  }
)

output$downdat.data_timeseries04 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_timeseries04(), file, row.names = TRUE)
  }
)

output$downdat.total_barplot1 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(filt_total_api(), file, row.names = TRUE)
  }
)

output$downdat.data_api_full3  = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(data_api_full3(), file, row.names = TRUE)
  }
)
output$downdat.data_api_full4  = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(tot_cpd_gp(), file, row.names = TRUE)
  }
)
output$downdat.data_api_full5  = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(tot_cpd_gp02(), file, row.names = TRUE)
  }
)


output$downdat.stw_compare_year01 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(stw_compare_year01(), file, row.names = TRUE)
  }
)
output$downdat.stw_compare_year02 = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(stw_compare_year02(), file, row.names = TRUE)
  }
)

output$downdat.cpd_monthwise = downloadHandler(
  filename = function (){ paste('data','.csv',sep = '')},
  content = function(file) {
    write.csv(filt_cpd_monthwise(), file, row.names = TRUE)
  }
)







output$body <- renderUI({
  if (USER$Logged == TRUE) {
   tabItems(
        # First tab content
        tabItem(tabName = "timeseries",
                fluidRow(
                  box(width = 2,height = 1500,
                      selectInput('searchoption2', 'Select STW:',
                                  c('stp_01' = 'stp_01',
                                    'stp_04' = 'stp_04',
                                    'stp_02' = 'stp_02',
                                    'stp_05' = 'stp_05',
                                    'stp_03' = 'stp_03'
                                  ), selected = 'stp_04'
                      ),
                      
                      tags$hr(),
                      
                      radioButtons('selplot', 'Select Plot based:',
                                   list('API'='api_plot',
                                        'GP Practice'='gp_plot'
                                   ),selected = 'api_plot'),
                      bsTooltip ('selplot',"Click here to select plot based","bottom", options = NULL),
                      conditionalPanel("input.selplot == 'gp_plot'",
                                       tags$hr(),
                                       uiOutput("practiceinput2")),
                      conditionalPanel("input.selplot == 'api_plot'",
                                       tags$hr(),
                                       uiOutput("apiinput1"))
                      
                      
                      
                  ),
                  
                  
                  tabBox(width = 10,height = 1500,
                         tabPanel('Time series - Bar Plot Complete',
                                  br(),
                                  
                                  column(9,
                                         box(width = "1300px",
                                             br(),
                                             downloadButton ('downdat.time_barplot1',label = "Yearwise"),
                                             downloadButton ('downdat.data_api_monthwise_full2',label = "Monthwise"),
                                             plotlyOutput("timeseries_bar_plot1",height="600px",width = "1000px"))
                                         
                                         
                                  ),
                                  column(12,
                                         tabBox(width = NULL,
                                                
                                                            
                                                            tabPanel("Month wise - with Average",value = "filt_tab_year_01",
                                                                     br(),
                                                                     downloadButton ('downdat.data_time_comp04',label = "Selected API"),
                                                                     downloadButton ('downdat.data_time_comp03',label = "Complete"),
                                                                     plotlyOutput("timeseries_line_comp_plot1",height="550px")
                                                                     ),
                                                #             tabPanel("Data Table",
                                                #                     dataTableOutput("tab_data_time_comp07")
                                                #                     ),
                                                # tabPanel("Data Table 2",
                                                #          dataTableOutput("tab_data_time_comp07_02")
                                                # ),
                                                            tabPanel("Month wise",value = "filt_tab_year_02",
                                                                     br(),
                                                                     # downloadButton ('downdat.data_time_comp04',label = "Selected API"),
                                                                     # downloadButton ('downdat.data_time_comp03',label = "Complete"),
                                                                     # plotlyOutput("timeseries_line_comp_plot1",height="550px")
                                                                     plotlyOutput("timeseries_line_plot4",height="450px")
                                                            )
                                                            
                                                            
                                                
                                                
                                         )
                                  )# End of column
                         ),
                         
                         tabPanel('Targeted API',
                                  column(10,
                                         tabBox(width = NULL,
                                                tabsetPanel(id = "tabs_api",
                                             tabPanel("Total Year - Bar Plot",value = "filt_api_bar",
                                                      br(),
                                                      plotlyOutput("timeseries_bar_plot2",height="450px")
                                             ),
                                             tabPanel("Total Year - Point Plot",value = "filt_api_point",
                                                      br(),
                                                      plotlyOutput("timeseries_line_plot1",height="450px")
                                             ),
                                             tabPanel("Monthwise",value = "filt_api_point_monthwise",
                                                      br(),
                                                      plotlyOutput("timeseries_line_plot3",height="450px")
                                             ),
                                             tabPanel("Monthwise with year average",value = "filt_api_point_monthwise_with_average",
                                                      br(),
                                                      plotlyOutput("timeseries_comp_plot3",height="450px")
                                             ),
                                             tabPanel("Monthwise Box plot",value = "filt_api_monthwise_box",
                                                      br(),
                                                      plotlyOutput("timeseries_box_plot1",height="450px")
                                             )
                                         ))
                                  )
                                  # column(12,
                                  #        tabBox(width = NULL,
                                  #               tabsetPanel(id = "tabs5",
                                  #                           tabPanel("Period",value = "filt_series_period",
                                  #                                    plotlyOutput("timeseries_line_plot2",height="450px")
                                  #                           )
                                  #                           
                                  #                           
                                  #                           
                                  #               )
                                  #               
                                  #        )
                                  # )
                         ),
                         
                         tabPanel("Timeseries -  GP",
                                  br(),
                                  downloadButton ('downdat.data_timeseries04'),
                                  plotlyOutput("plot_compound_timeseries01",height="450px"))
                        
                         
                  ) # End of tabBox     
                )
                ),
        
        # Second tab content
        tabItem(
          tabName = "year_wise",
          fluidRow(
            box(width = 2,height = 1200,
                selectInput('searchoption', 'Select STW:',
                            c('stp_01' = 'stp_01',
                              'stp_04' = 'stp_04',
                              'stp_02' = 'stp_02',
                              'stp_05' = 'stp_05',
                              'stp_03' = 'stp_03'
                            ), selected = 'stp_04'
                ),
                selectInput('selectyear', 'Select Prescirption Year:',
                            c('2014' = '2014',
                              '2015' = '2015',
                              '2016' = '2016',
                              '2017' = '2017',
                              '2018' = '2018'), selected = '2018'
                ),
                
                tags$hr(),
                uiOutput("practiceinput")
                
            ),
            
            
            tabBox(width = 10,height = 1200,
                   tabsetPanel(
                     id = "tabs4",
                     tabPanel('Total Filtered Plots',
                              br(),
                              column(3,
                                     box(width = NULL,
                                         br(),
                                         plotlyOutput("filt_group_pie",height="400px")
                                     )),
                              column(9,
                                     box(width = NULL,
                                         br(),
                                         downloadButton ('downdat.total_barplot1'),
                                         plotlyOutput("filt_total_barplot1",height="400px"))
                              ),
                              column(12,
                                     tabBox(width = NULL,
                                            tabsetPanel(id = "tabs3",
                                                        tabPanel("Period",value = "filt_tab_period",
                                                                 br(),
                                                                 plotlyOutput("filt_total_lineplot1",height="450px")
                                                        ),
                                                        
                                                        tabPanel("Practice",value = "filt_tab_practice",
                                                                 br(),
                                                                 plotlyOutput("filt_total_lineplot2",height="450px")
                                                        ),
                                                        tabPanel("Chemcial Form",value = "filt_tab_chemform",
                                                                 br(),
                                                                 plotlyOutput("filt_total_lineplot3",height="450px")
                                                        ),
                                                        tabPanel("Medicinal Form",value = "filt_tab_medform",
                                                                 br(),
                                                                 plotlyOutput("filt_total_lineplot4",height="450px")
                                                        ),
                                                        tabPanel("Practice - Monthwise",value = "filt_tab_monthwise",
                                                                 br(),
                                                                 plotlyOutput("plot_compound_monthwise",height="450px")
                                                        )
                                            )
                                            
                                     )
                              )# End of column
                              
                              
                     ),
                     # tabPanel('BNF API aggr1',
                     #          dataTableOutput('tab_bnf_api_aggr1')),
                     # tabPanel('BNF API aggr2',
                     #          dataTableOutput('tab_bnf_api_aggr2')),
                     # tabPanel('Total Drug 5',
                     #          dataTableOutput('tab_filt_total_drug5')),
                     # tabPanel('Total Group',
                     #          dataTableOutput('tab_filt_total_group')),
                     # tabPanel('Total Group2',
                     #          dataTableOutput('tab_filt_total_group2')),
                     # tabPanel('Total Medicinal form',
                     #          dataTableOutput('tab_filt_med_form')),
                     # tabPanel('Monthwise CPD',
                     #          dataTableOutput('tab_filt_cpd_monthwise')),
                     # tabPanel('Final Groupwise',
                     #          dataTableOutput('tab_filt_total_api')),
                     # tabPanel('Total API GP wise',
                     #          dataTableOutput('tab_tot_cpd_gp_wise')),
                     # tabPanel('Total API GP',
                     #          dataTableOutput('tab_tot_cpd_gp')),
                     # tabPanel('Final Total period',
                     #          dataTableOutput('tab_filt_total_period')),
                     # tabPanel('Monthwise Practice',
                     #          dataTableOutput('tab_filt_practice_monthwise')),
                     # tabPanel('Total Practice',
                     #          dataTableOutput('tab_filt_total_practice')),
                     # tabPanel("Monthwise CPD Plot",value = "cpd_monthwise",
                     #          downloadButton ('downdat.cpd_monthwise'),
                     #          plotlyOutput("plot_cpd_monthwise")
                     # ),
                     tabPanel("Monthwise - API Full",value = "filt_tab_full_monthwise",
                              br(),
                              downloadButton ('downdat.data_api_full3', label = "Monthwise"),
                              downloadButton ('downdat.data_api_full4', label = "Monthwise - with GPs"),
                              plotlyOutput("timeseries_line_plot5",height="600px")
                     ),
                     tabPanel("Practice wise CPD Plot", #value = "practice_monthwise",
                              br(),
                              downloadButton ('downdat.data_api_full5', label = "Monthwise - with GPs"),
                              plotlyOutput("plot_practice_monthwise", height= "600px")
                     )
                     # tabPanel("API Vs Time", 
                     #          dataTableOutput("tab_data_api_full3")
                     # )
                   )
            )      
          )
          
        ),
        
        # Third tab content
        tabItem(
          tabName = "compare_stw",
          fluidRow(
            box(width = 2,height = 1200,
                selectInput('selectyear02', 'Select Prescirption Year:',
                            c('2014' = '2014',
                              '2015' = '2015',
                              '2016' = '2016',
                              '2017' = '2017',
                              '2018' = '2018'), selected = '2018'
                ),
                tags$hr()
               
                
            ),
            
            
            tabBox(width = 10,height = 1200,
                   # tabPanel("Data - Compare STW 01", 
                   #            dataTableOutput("tab_compare_stw01")
                   #          ),
                   # tabPanel("Data - Compare STW 02", 
                   #          dataTableOutput("tab_compare_stw02")
                   # ),
                   # tabPanel("Data - Compare STW 03", 
                   #          dataTableOutput("tab_compare_stw03")
                   # ),
                   # tabPanel("Data - Compare STW 04", 
                   #          dataTableOutput("tab_compare_stw04")
                   # ),
                   tabPanel("2D Plot - Compare STW", 
                            downloadButton ('downdat.stw_compare_year01', label = "Yearwise"),
                            downloadButton ('downdat.stw_compare_year02', label = "Monthwise"),
                            plotlyOutput("compare_stw_bar_plot1",height="600px",width = "1000px")
                   ),
                   tabPanel("3D Plot - Compare STW", 
                            plotlyOutput("compare_3d_plot01",height="600px",width = "1000px")
                   )
                   
                   
            )      
          )
          
        )
      )
    } else {
      login
    }
  })
}