#Title : JobStreet Web Scraping

# Script by Wazeef
# Packages <- c("RSelenium","RPostgreSQL","dplyr") 

library("pacman")

p_load("RSelenium","RPostgreSQL","dplyr","rvest")

date = gsub("-", "_", Sys.Date())

url = "https://www.jobstreet.com.my/en/job-search/job-vacancy.php?sort=createdAt"

remDr <- RSelenium::remoteDriver(remoteServerAddr = "10.13.11.18",
                      port = 4444L,
                      browserName = "chrome")
remDr$open()

#driver = rsDriver(port = as.integer(sample(1000:10000, 1)), browser = "chrome", chromever = "106.0.5249.61")

#remDr = driver[["client"]]

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




pw <- {
  "wtgwrgergerg3354"
}

# loads the PostgreSQL driver
drv <- RPostgreSQL::PostgreSQL()
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "jobstreet",
                 host = "10.13.11.18", port = 5432,
                 user = "docker", password = pw)
rm(pw) # removes the password 


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


}
# https://stackoverflow.com/questions/47543247/how-to-append-update-a-row-from-a-data-frame-to-a-table-in-postgresql-db-table-t
dbWriteTable(con, "description", jobdata, append = TRUE, row.names = FALSE)
