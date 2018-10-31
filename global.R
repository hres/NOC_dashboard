library(shiny)
library(shinydashboard)
library(elastic)
library(dplyr)
library(jsonlite)
library(highcharter)
library(lubridate)
library(purrr)

connect(es_host = "elastic-gate.hc.local", es_port = 80,errors = "complete")


#function to query data:
base_url<-'https://node.hres.ca/drug/licence'
hc_result<-function(url,summation=TRUE){
  
  result = httr::GET(url)
  httr::stop_for_status(result)
  
  if(summation){
    hc_result<-fromJSON(httr::content(result, as='text'))$total
  }else{
    hc_result<-fromJSON(httr::content(result, as='text'))$results
  }
  
  return(hc_result)
}

add_api_key<-function(url){
  #url<-paste0(url,'&key=40e40966014eb7ac')
  url<-paste0(url,'&limit=50000')
  url<-gsub(' ','%20',url)
  return(url)
}

clean_string<-function(string){
  string <- gsub("%", "%20", string)
  string <- gsub("'", "%27", string)
  string <- gsub("!", "", string)
  string <- gsub("&", "", string)
  string <- gsub(' ', '%20', string)
  
  string<-paste0('\"',string,'\"')
  
  string
}



#list of years:
years<-paste0(base_url,'?count=noc_date:year')%>%
       add_api_key()%>%
       hc_result(F)%>%
       dplyr::pull(key_as_string)%>%
       substr(1,4)
       
#list of brand names to search for:
#pull list of brandnames using elastic query:
query<-'{ 
  "size":0,
  "aggs": {
    "brandname": {
      "nested": {
        "path": "drug_products.ingredients"
      },
      "aggs": {
        "brand": {
          "terms": {
            "field": "drug_products.ingredients.ingredient_name",
            "size" : 10000
          }
        }
      }
    }
  }
}'

ingredients<-Search(index='notice_of_compliance',body=query)$aggregations$brandname$brand$buckets
ingredients<-sapply(ingredients,'[','key')%>%unlist(use.names=F)