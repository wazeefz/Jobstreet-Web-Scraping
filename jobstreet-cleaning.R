
#Getting the 'link' column from dataframe

link = jobdata$JobLink

sample(link, 100) 

#Getting the start location of pattern

id_loc_s = str_locate(link, "jobId=jobstreet-[:alpha:]{2}-job-")[,2]

head(id_loc_s)

#Getting the end location of pattern

id_loc_e = str_locate(link, "&sectionRank")[,1]

head(id_loc_e)

#Slicing JobID usig str_sub

jobid = str_sub(link, start = id_loc_s+1, end = id_loc_e-1)  

# id = data.frame(job_id, jid, stringsAsFactors = F)

jobdatacopy = jobdata

jobdatacopy = jobdatacopy %>% mutate(`Job ID` = jobid)

#################################################################################################################################################
# class(as.character(link))
#
# library(magrittr)
# 
# link %<>% as.character
# 
# link = as.character(link)
# 
# link = link %>% as.character()

# head(jobdatacopy)

################################################################################################################################################

#Removing duplicates in 'Job ID'

jobdatacopy = jobdatacopy[!duplicated(jobdatacopy$`Job ID`),]

jobdatacopy = jobdatacopy2

jobdatacopy = jobdatacopy %>%
  mutate(across(everything(), as.character))

#################################################################################################################################################

#Taking only data that contains 3 or more occurences of 


jobdatacopy = jobdatacopy %>% mutate(Salary = ifelse(grepl("[[::]]{3,}",Salary), Salary, NA))

# x = jobdatacopy$Salary[grepl("[[:digit:]]{2,}",jobdatacopy$Salary)]

#################################################################################################################################################

#taking only data that contains the word 'years' in YearsOfExperience column

jobdatacopy = jobdatacopy %>% mutate(YearsOfExperience2 = ifelse(grepl("years",YearsOfExperience), YearsOfExperience, NA))

#################################################################################################################################################

negeri = c("Johor",
           "Kuala Lumpur",
           "Kedah",
           "Kelantan",
           "Negeri Sembilan",
           "Pahang",
           "Penang",
           "Perak",
           "Perlis",
           "Sabah",
           "Sarawak",
           "Terengganu",
           "Labuan",
           "Putrajaya",
           "Selangor",
           "Melaka",
           "")

#data$keep <- ifelse(data$animal %in% matches, "Keep", "Discard")


length(negeri)
type(negeri)
class(negeri)


jobdatacopy = jobdatacopy %>% mutate(State2 = ifelse(State %in% negeri, State, NA))

unique(jobdatacopy$State2)

length(unique(jobdatacopy$State2))

table(unique(jobdatacopy$State2))

table(jobdatacopy$State2)

count(jobdatacopy$State2)


#################################################################################################################################################



jobdatacopy = jobdatacopy %>% mutate(Location2 = ifelse(State %in% negeri, Location, State))


#################################################################################################################################################



jobdatacopy = jobdatacopy %>% mutate(RespAndReq2 = ifelse(grepl("Posted",RespAndReq), NA, RespAndReq))

jobdatacopy = jobdatacopy %>% mutate(RespAndReq2 = ifelse(RespAndReq2 %in% negeri, NA, RespAndReq2))

kawasan = jobdatacopy$Location2

head(kawasan)

class(kawasan)

jobdatacopy = jobdatacopy %>% mutate(RespAndReq2 = ifelse(RespAndReq2 %in% kawasan, NA, RespAndReq2))

jobdatacopy = jobdatacopy %>% mutate(RespAndReq2 = ifelse(grepl("Multiple work locations",RespAndReq2), NA, RespAndReq2))

#################################################################################################################################################

jobdatacopy = jobdatacopy %>%
  mutate(across(everything(), as.character))

jobdatacopy = jobdatacopy %>% mutate(CompanyOverview2 = ifelse(grepl("[[:digit:]]{3,}",CompanyOverview), NA, CompanyOverview))

#################################################################################################################################################

cleanjobdata = jobdatacopy %>% select(-CompanyOverview2)

cleanjobdata = cleanjobdata %>% mutate(CompanyOverview2 = ifelse(grepl("Posted",CompanyOverview), NA, CompanyOverview))

cleanjobdata = cleanjobdata %>% mutate(CompanyOverview2 = ifelse(grepl("Multiple work locations",CompanyOverview2), NA, CompanyOverview2))

cleanjobdata = cleanjobdata %>% mutate(CompanyOverview2 = ifelse(CompanyOverview2 %in% negeri, NA, CompanyOverview2))

cleanjobdata = cleanjobdata %>% mutate(CompanyOverview2 = ifelse(CompanyOverview2 %in% kawasan, NA, CompanyOverview2))

cleanjobdata = cleanjobdata %>% mutate(CompanyOverview2 = ifelse(grepl("MYR|sgd|usd|Responsibilities|job description|job role|requirement|candidate must poss|more jobs from this employer",
                                                                       ignore.case = TRUE, CompanyOverview2), NA, CompanyOverview2))

cleanjobdata = cleanjobdata %>% mutate(RespAndReq2 = ifelse(grepl("job role|requirement|responsibilities",
                                                                       ignore.case = TRUE, CompanyOverview), CompanyOverview, RespAndReq2))



#################################################################################################################################################


jobstreetdata = cleanjobdata %>% select(c(`Job ID`,`Job Title` = JobTitle, Company, Salary, State = State2, Location = Location2, `Company Overview` = CompanyOverview2,`Responsibilities & Requirements` = RespAndReq2,
                                           Industry, `Job Specialization` = JobSpecialization, `Career Level` = CareerLevel, `Years of Exp` = YearsOfExperience2, `Company Overview` = CompanyOverview2,
                                          Benefits, Industry, `Job Link` = JobLink
                                           ))


#################################################################################################################################################

date = gsub("-", "_", Sys.Date())


fwrite(as.data.frame(jobstreetdata), paste0("//10.13.10.22/dataSandbox/scraping/misc/jobstreet/jobstreet_jobstreetclean1_", date, ".csv"))




























