server<-function(input,output,session){
  
  tab_list <- NULL
  
  output$total_noc <- renderValueBox({
  
    if(input$time=='All') {
      url<-paste0(base_url,'?count=noc_date:year')
    }else{
      year_start=as.numeric(input$time)
      year_end=year_start+1
      
      url <-paste0(base_url,'?search=noc_date:[',year_start,'+TO+',year_end,']&count=noc_date:month')
    }
    
    result<-url%>%
            add_api_key()%>%
            hc_result(T)
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Total Number of NOC")
  })
  
  js_line_clicked <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$group_totals <- renderHighchart({
    
    if(input$time=='All') {
      url <-paste0(base_url,'?count=noc_date:year')
      
      result<-url%>%
        add_api_key()%>%
        hc_result(F)%>%
        mutate(time=year(as.Date(substr(key_as_string,1,10),'%Y-%m-%d')))
      
      group_name <- "Annual"
    } else {
      year_start=as.numeric(input$time)
      year_end=year_start+1
      
      url <-paste0(base_url,'?search=noc_date:[',year_start,'+TO+',year_end,']&count=noc_date:month')
      
      result<-url%>%
        add_api_key()%>%
        hc_result(F)%>%
        mutate(time=month(as.Date(substr(key_as_string,1,10),'%Y-%m-%d')))
      group_name <- "Monthly"
    } 
    
    
    highchart() %>%
      hc_add_series(
        data=result,
        type = "line",
        hcaes(x=time,y=doc_count),
        name = paste(group_name, " total NOC submission"),
        events = list(click = js_line_clicked)) 
})
  
  # Tracks the JavaScript event created by `js_click_line`
  observeEvent(input$line_clicked != "",
               if(input$time == 'All')
                 updateSelectInput(session, "time", selected = input$line_clicked),
               ignoreInit = TRUE)
  
  
  
  
  js_col_clicked <- JS("function(event) {Shiny.onInputChange('col_clicked', [event.point.category]);}")

  output$product_type <- renderHighchart({
    # The following code runs inside the database
    if(input$time == 'All'){
      url <- paste0(base_url,'?count=noc_eng_product_type')
    }else{
      year_start=as.numeric(input$time)
      year_end=year_start+1
      
      url <- paste0(base_url,'?search=noc_date:[',year_start,'+TO+',year_end,']&count=noc_eng_product_type')
    }
    
    result<-url%>%
            add_api_key()%>%
            hc_result(F)

    highchart() %>%
      hc_add_series(
        data=result,
        type = "bar",
        hcaes(x=key,y=doc_count),
        name = paste("No. of NOC"),
        events = list(click = js_col_clicked))%>%
      hc_xAxis(
        categories = result$key,
        tickmarkPlacement="on")

    
  })

  observeEvent(input$col_clicked,
               {
                 category <- input$col_clicked[1]
                 tab_title <- paste(category,
                                    if(input$time != 'All') paste("-" ,input$time))

                 if(tab_title %in% tab_list == FALSE){
                   #return first 1000 for now:
                   if(input$time!='All'){
                     
                     year_start=as.numeric(input$time)
                     year_end=year_start+1
                     
                   url <- paste0(base_url,'?search=noc_eng_product_type:',category,'+AND+','noc_date:[',year_start,'+TO+',year_end,']')
                   }else{
                   url<-paste0(base_url,'?search=noc_eng_product_type:',category)
                   }
                     
                   details<-url%>%          
                            add_api_key()%>%
                            hc_result(F)%>%
                            `[[`('_source')
                   if(is.null(details$noc_eng_reason_submission)) details$noc_eng_reason_submission<-NA
                   
                   details<-data.frame(noc_date=details$noc_date,
                                       noc_num=details$noc_number,
                                       manufacturer=details$noc_manufacturer_name,
                                       brandname=sapply(details$brandnames,paste,collapse=','),
                                       submission_reason=details$noc_eng_reason_submission)%>%
                            mutate(noc_date=ymd(substr(noc_date,1,10)))

                  
                   
                     appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               DT::renderDataTable(details)
                             ))

                   tab_list <<- c(tab_list, tab_title)

                 }

                 updateTabsetPanel(session, "tabs", selected = tab_title)

               })
  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
  
  output$manufacturer <- renderHighchart({
    # The following code runs inside the database
    if(input$time == 'All'){
     url<- paste0(base_url,'?count=noc_manufacturer_name')
    }else{
      year_start=as.numeric(input$time)
      year_end=year_start+1
      url<- paste0(base_url,'?search=noc_date:[',year_start,'+TO+',year_end,']&count=noc_manufacturer_name')
    }
    
    result<-url%>%
            add_api_key()%>%
            hc_result(F)
    
    highchart() %>%
      hc_add_series(
        data=result%>%slice(1:10),
        type = "pie",
        hcaes(x=key,y=doc_count),
        name = paste("No. of NOC"))
    
  })
  
  js_col_clicked2 <- JS("function(event) {Shiny.onInputChange('col_clicked2', [event.point.category]);}")
  
  output$submission_type<-renderHighchart({
    if(input$time == 'All'){
      url <- paste0(base_url,'?count=noc_eng_submission_type')
      
    }else{
      year_start=as.numeric(input$time)
      year_end=year_start+1
      url <- paste0(base_url,'?search=noc_date:[',year_start,'+TO+',year_end,']&count=noc_eng_submission_type')
    }
    
    result<-url%>%
            add_api_key()%>%
            hc_result(F)
    
    highchart() %>%
      hc_add_series(
        data=result,
        type = "column",
        hcaes(x=key,y=doc_count),
        name = paste("No. of NOC"),
        events = list(click = js_col_clicked2))%>%
        hc_xAxis(
        categories = result$key,
        tickmarkPlacement="on")
  })
  
  observeEvent(input$col_clicked2,
               {
                 submission <- input$col_clicked2[1]
                 tab_title <- paste(submission,
                                    if(input$time != 'All') paste("-" ,input$time))
                 
                 if(tab_title %in% tab_list == FALSE){
                   #return first 1000 for now:
                   if(input$time == 'All'){
                     url <- paste0(base_url,'?search=noc_eng_submission_type:',clean_string(submission))
                     
                   }else{
                     year_start=as.numeric(input$time)
                     year_end=year_start+1
                     url <- paste0(base_url,'?search=noc_eng_submission_type:',clean_string(submission),
                                  '+AND+','noc_date:[',year_start,'+TO+',year_end,']')
                   }
                   
                   details<-url%>%          
                     add_api_key()%>%
                     hc_result(F)%>%
                     `[[`('_source')
                   
                   details<-data.frame(noc_date=details$noc_date,
                                       noc_num=details$noc_number,
                                       brandname=sapply(details$brandnames,paste,collapse=','),
                                       manufacturer=details$noc_manufacturer_name)%>%
                     mutate(noc_date=ymd(substr(noc_date,1,10)))
                   
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               DT::renderDataTable(details)
                             ))
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  
  noc_tb<-reactive({
    if(input$product!=""){
      ing<-clean_string(input$product)
      url<-paste0(base_url,'?search=ingredients:',ing)
      
      details<-url%>%
              add_api_key()%>%
              hc_result(F)%>%
              `[[`('_source')
    if(!is.null(details$noc_crp_product_name)){
      details<-data.frame(noc_date=details$noc_date,
                          noc_num=details$noc_number,
                          brandname=sapply(details$brandnames,paste,collapse=','),
                          ingredient=sapply(details$ingredients,paste,collapse=','),
                          manufacturer=details$noc_manufacturer_name,
                          reference_product=details$noc_crp_product_name,
                          din=sapply(details$drug_identification_numbers,paste,collapse=','))%>%
               mutate(noc_date=ymd(substr(noc_date,1,10)))
      
      num_crp<-length(unique(details$reference_product))
      crps<-unique(details$reference_product)
    }else{
      details<-data.frame(noc_date=details$noc_date,
                          noc_num=details$noc_number,
                          brandname=sapply(details$brandnames,paste,collapse=','),
                          ingredient=sapply(details$ingredients,paste,collapse=','),
                          manufacturer=details$noc_manufacturer_name,
                          reference_product=NA,
                          din=sapply(details$drug_identification_numbers,paste,collapse=','))%>%
        mutate(noc_date=ymd(substr(noc_date,1,10)))
      
      num_crp<-length(unique(details$brandname))
      crps<-unique(details$brandname)
    }
      
    }else{
    url<-''
    crps=c('')
    num_crp<-0
    details<-data.frame(`Please select an ingredient`='')
    }
    
    
    return(list(
      details=details,
      num_crp=num_crp,
      crp=crps
    ))
  })
  
  output$noc_output<-DT::renderDataTable(
     DT::datatable(
       noc_tb()$details,
       options = list(
         scrollX = TRUE
         )
     )
  )
  
  output$total_crp<-renderValueBox(
    valueBox(value = prettyNum(noc_tb()$num_crp, big.mark = ","),
             subtitle = "Total Number of Innovator Product",color='aqua',width=2)
  )
  
  output$innovator_output<-DT::renderDataTable({
      details<-noc_tb()$details
      
      if(noc_tb()$num_crp!=0)
      details<-details%>%
               dplyr::filter(is.na(reference_product))%>%
               dplyr::select(c(1:5,7))
    
    
      DT::datatable(details,
                    options = list(
                      scrollX = TRUE
                    ))
  })
  
  
  
}