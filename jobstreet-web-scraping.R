#Title : JobStreet Web Scraping

# Script by Wazeef
install.packages("pacman")
Packages <- c("rvest","RSelenium","tidyverse","data.table","RPostgreSQL")
Packages %in% loadedNamespaces() # check if the packages are loaded
# [1] FALSE FALSE

pacman::p_load(Packages, character.only = TRUE)

Packages %in% loadedNamespaces()



date = gsub("-", "_", Sys.Date())

url = "https://www.jobstreet.com.my/en/job-search/job-vacancy.php?sort=createdAt"

driver = rsDriver(port = as.integer(sample(1000:10000, 1)), browser = "chrome", chromever = "106.0.5249.61")

remDr = driver[["client"]]

remDr$navigate(url)

Sys.sleep(.5)

######################################################################################################################################################

# links = read_html(remDr$getPageSource()[[1]]) %>%
#   html_nodes("a._1hr6tkx5._1hr6tkx8._1hr6tkxb.sx2jih0.sx2jihf.zcydq8h") %>% html_attr("href")
# 
# links = paste0("https://www.jobstreet.com.my/",links2)
# class(links)
# length(links)
# 
# links = links[!is.na(links)]
# class(links)
# length(links)
# 
# links = links[!duplicated(links)]
# class(links)
# length(links)


######################################################################################################################################################

joblinks = vector()

for(i in 1:100){

  linkstart = ("https://www.jobstreet.com.my/en/job-search/job-vacancy/")
  linknumber = i
  linkend = ("/?sort=createdAt")
  
  pagelink = paste0(linkstart,linknumber,linkend)
  
  #cat(pagelink,"\n")
  
  remDr$navigate(pagelink)
  
  Sys.sleep(.1)
  
  job_link = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("a._1hr6tkx5._1hr6tkx8._1hr6tkxb.sx2jih0.sx2jihf.zcydq8h") %>% html_attr("href")
  
  job_link = grep("/job/", job_link, value = T)
  # 
  # job_link = job_link[!grepl("job-search", job_link)]
  
  # class(job_link)
  # cat(job_link)
  # length(job_link)
  
  comjoblink = paste0("https://www.jobstreet.com.my",job_link)
  
  # class(comjoblink)
  # cat(comjoblink)
  # length(comjoblink)
  
  # comjoblink = data.frame(LINK  = comjoblink)
  
  # class(comjoblink)
  # nrow(comjoblink)
  
  joblinks = append(joblinks,comjoblink)
  
   # class(joblinks)
  # nrow(joblinks)
  #class(joblinks)
   
   # joblinks = na.omit(joblinks)
  
  #joblinks = joblinks[!is.na(joblinks)]
  # joblinks = data.frame(joblinks)
  # class(joblinks)
  # length(joblinks)
   
   #joblinks = joblinks[!duplicated(joblinks$LINK),]
  # joblinks = data.frame(LINK = joblinks)
  
  joblinks = joblinks[!duplicated(joblinks)]
  # joblinks = data.frame(joblinks)
   #class(joblinks)
  # length(joblinks)
   
  cat("Iteration = ",i,"\n")
  
  
  

}

#backup
# fwrite(as.data.frame(joblinks), paste0("//10.13.10.22/dataSandbox/scraping/misc/jobstreet/links/jobstreet_links_", date, ".csv"))


# joblinks = fread(paste0("//10.13.10.22/dataSandbox/scraping/misc/jobstreet/links/jobstreet_links_", date, ".csv"))

######################################################################################################################################################

# for(i in 30:40){
#   
#   remDr$navigate(joblinks[i])
#   
# }

######################################################################################################################################################

jobdata = data.frame()

# for(i in 1:length(joblinks)){
for(i in 1:100){
  remDr$navigate(joblinks[i])
  
  Sys.sleep(.2)
  
  #1
  jobname = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("h1.sx2jih0._18qlyvc0._18qlyvch._1d0g9qk4._18qlyvcp._18qlyvc1x") %>% html_text() 
  if(length(jobname)!=0){
    jobname = jobname
  }else{
    jobname = NA 
  }
  #cat("JOB TITLE = ",jobname,"\n")
  
  detail = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("span.sx2jih0.zcydq84u._18qlyvc0._18qlyvc1x._18qlyvc1._18qlyvca") %>% html_text()
  
  #2
  jobstate = detail[1]
  if(length(jobstate)!=0){
    jobstate = jobstate
  }else{
    jobstate = NA 
  }
  #cat("STATE = ",jobstate,"\n")
  
  #2.5
  salary = detail[2]
  if(length(salary)!=0){
    salary = salary
  }else{
    salary = NA 
  }
  #cat("SALARY = ",salary,"\n")
  
  
  
  #3
  responsibilitiesrequirements = detail[3]
  if(length(responsibilitiesrequirements)!=0){
    responsibilitiesrequirements = responsibilitiesrequirements
  }else{
    responsibilitiesrequirements = NA 
  }
  #cat("RESPONSIBILITIES AND REQS = ",responsibilitiesrequirements,"\n")
  
  #4
  compoverview = detail[4]
  if(length(compoverview)!=0){
    compoverview = compoverview
  }else{
    compoverview = NA 
  }
  #cat("COMPANY OVERVIEW = ",compoverview,"\n")
  
  detail2 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("span.sx2jih0.zcydq84u._18qlyvc0._18qlyvc1x._18qlyvc1._1d0g9qk4._18qlyvcb") %>% html_text()
  
  #5
  jobspecialization = detail2[23]
  if(length(jobspecialization)!=0){
    jobspecialization = jobspecialization
  }else{
    jobspecialization = NA 
  }
  #cat("JOB SPECIALIZATION = ",jobspecialization,"\n")
  
  #6
  careerlevel = detail2[19]
  if(length(careerlevel)!=0){
    careerlevel = careerlevel
  }else{
    careerlevel = NA 
  }
  #cat("CAREER LEVEL = ",careerlevel,"\n")
  
  #7
  industry = detail2[26]
  if(length(industry)!=0){
    industry = industry
  }else{
    industry = NA 
  }
  #cat("INDUSTRY = ",industry,"\n")
  
  #8
  benefits = detail2[27]
  if(length(benefits)!=0){
    benefits = benefits
  }else{
    benefits = NA 
  }
  #cat("BENEFITS & OTHERS = ",benefits,"\n")
  
  #9
  experience = detail2[21]
  if(length(experience)!=0){
    experience = experience
  }else{
    experience = NA 
  }
  #cat("YEARS OF EXPERIENCE = ",experience,"\n")
  
  #10
  specificlocation = detail2[28]
  if(length(specificlocation)!=0){
    specificlocation = specificlocation
  }else{
    specificlocation = NA 
  }
  #cat("SPECIFIC LOCATION = ",specificlocation,"\n")
  
  detail3 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("span.sx2jih0.zcydq84u._18qlyvc0._18qlyvc1x._18qlyvc2._1d0g9qk4._18qlyvcb") %>% html_text()
  
  #11
  companyname = detail3[1]
  if(length(companyname)!=0){
    companyname = companyname
  }else{
    companyname = NA 
  }
  #cat("COMPANY = ",companyname,"\n")
  
  #12
  jobbblink = joblinks[i]
  
  df = data.frame(JobTitle = jobname,
                  Salary = salary,
                  Company = companyname,
                  State = jobstate,
                  Location = specificlocation,
                  Industry = industry,
                  JobSpecialization = jobspecialization, 
                  CareerLevel = careerlevel,
                  YearsOfExperience = experience,
                  Benefits = benefits,
                  CompanyOverview = compoverview,
                  RespAndReq = responsibilitiesrequirements,
                  JobLink = jobbblink)
  
  
  jobdata = rbind(jobdata, df)
  
  
  
  cat("Iteration = ",i,"\n")
  
  
  values <- paste( " jobdata[  , c(",
                   paste( names(jobdata),collapse=",") ,
                   ")] ", collapse="" )
  print(values)

  cmd <- paste("insert into description values ", values)

  print(cmd)
  
  dbSendQuery(con, cmd, as.is=TRUE)

}


values <- paste( " jobdata[  , c(",
                 paste( names(jobdata),collapse=",") ,
                 ")] ", collapse="" )
print(values)

cmd <- paste("insert into description values ", values)

print(cmd)

dbSendQuery(con, cmd, as.is=TRUE)

dbListTables(con)



pw <- {
  "wtgwrgergerg3354"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "jobstreet",
                 host = "10.13.11.18", port = 5432,
                 user = "docker", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "public")

values <- paste( " jobdata[  , c(", 
                 paste( names(jobdata),collapse=",") ,
                 ")] ", collapse="" ) 

cmd <- paste("insert into descr values ", values)

result <- sqlQuery(con, cmd, as.is=TRUE)


# df_postgres <- dbGetQuery(con, "SELECT * from jupem_point")

queryBuilding <- paste('SELECT * from public') 

buildingFootprint <- st_read(con, query = queryBuilding)

dbListTables(con) 

dbWriteTable(con, name="description", value=jobdata)

dbReadTable(con, 'job-description')

dbSendQuery(connec, "INSERT INTO Employees VALUES(1,'Aakash')")


fwrite(as.data.frame(jobdata), paste0("//10.13.10.22/dataSandbox/scraping/misc/jobstreet/jobstreet_jobdataraw_", date, ".csv"))



  #############################################################################################################.lknmok#########################################
  
for(i in 1:length(detail)){
     cat(i,detail[i])
}
  
  
for(i in 1:10){
  remDr$navigate(joblinks$LINK[5])
  
  Sys.sleep(.2)
  
}

######################################################################################################################################################

#CLEANING AND PROCESSING

jobdatacopy = jobdata

cutlink = head(jobdatacopy$JobLink)

cutlink = df.cutlink()


sum(is.na(jobdata$JobTitle))

jobdata = jobdata[!is.na(jobdata$JobTitle),]
  
jobdata_new1 = jobdata[!grepl("Post",jobdata$Salary),] 

jobdata_new2 = jobdata[grepl("MYR|USD|SGD",jobdata$Salary),]
  
row.names(jobdata_new2) = NULL

data.table::fwrite(jobdata, paste0("//10.13.10.22/dataSandbox/scraping/misc/jobstreet/jobstreet_scrape_", date, ".csv"))


######################################################################################################################################################
nrow(joblinks)

joblinkscopy = joblinks

nrow(joblinkscopy)

joblinks2 = data.frame(joblinks2)

nrow(joblinks2)


length(joblinks2)

class(joblinks2)

joblinks2[1]

for(i in 1:50){

remDr$navigate(joblinks$LINK[i])
  
}

######################################################################################################################################################

if(length(click1)!=0){
  Sys.sleep(0.5)
  x_btn = remDr$findElement(using = "css selector",
                            value = "a._1hr6tkx5._1hr6tkx8._1hr6tkxb.sx2jih0.sx2jihf.zcydq8h")
  x_btn$clickElement()
}
