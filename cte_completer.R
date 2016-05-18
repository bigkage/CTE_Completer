
##### tech prep completer
# tech prep course
# B or better
# single class if sem only, two classes if year long
# (group: course key, year, CIP, student, school)
# (filter: b or better, if yearlong then must have course count of two)

##### cte completer
# cte course
# D or better
# 2 credits or more
# in same pathway
# (group: student, pathway)
# (filter: d or better)


library("dplyr")
library(openxlsx)

cedars_grade_history_2015 <- read.delim("cedars_grade_history_2015.txt")
cedars_grade_history_2014 <- read.delim("cedars_grade_history_2014.txt")
skyward_grade_history <- read.csv("skyward_grade_history.csv")
pathway <- read.csv("courses_for_pathway.csv")

# count(cedars_grade_history_2014, IsVocationalCompleter)
# count(cedars_grade_history_2014, IsTechPrepCompleter)

courses_cjh_2015 <- read.csv("courses_cjh_2015.csv")
courses_cjh_2014 <- read.csv("courses_cjh_2014.csv")
courses_cjh_2013 <- read.csv("courses_cjh_2013.csv")
courses_cjh_2012 <- read.csv("courses_cjh_2012.csv")
courses_fhs_2015 <- read.csv("courses_fhs_2015.csv")
courses_fhs_2014 <- read.csv("courses_fhs_2014.csv")
courses_fhs_2013 <- read.csv("courses_fhs_2013.csv")
courses_fhs_2012 <- read.csv("courses_fhs_2012.csv")
courses_slms_2015 <- read.csv("courses_slms_2015.csv")
courses_slms_2014 <- read.csv("courses_slms_2014.csv")
courses_slms_2013 <- read.csv("courses_slms_2013.csv")
courses_slms_2012 <- read.csv("courses_slms_2012.csv")

##### fix columns #####
fix_columns <- function(df) {
  df$Sec <- NULL
  df$Attendance.Days.Meet <- as.factor(df$Attendance.Days.Meet)
  df$Class.Meeting.Times <- as.factor(df$Class.Meeting.Times)
  df$Display.Days.Meet <- as.factor(df$Display.Days.Meet)
  df$Dsp.Terms <- as.factor(df$Dsp.Terms)
  df$Instr.Staff <- as.factor(df$Instr.Staff)
  df$Room.Nbr <- as.factor(df$Room.Nbr)
  df$Teacher.Indicator <- as.factor(df$Teacher.Indicator)
  df$Teacher.Long.Name <- as.factor(df$Teacher.Long.Name)
  df$Teacher.Name.Key <- as.factor(df$Teacher.Name.Key)
  df$Teacher.Shrt.Name <- as.factor(df$Teacher.Shrt.Name)
  df$Teacher.Type <- as.factor(df$Teacher.Type)
  df$Sch.Terms <- as.factor(df$Sch.Terms)
  df$Class.Status <- as.factor(df$Class.Status)
  df$Tch.Mod <- as.factor(df$Tch.Mod)
  df$Asgn.Seats <- NULL
  df$Room.Type <- NULL
  df$Lun.Cd <- NULL
  df$Scheduling.Categories <- NULL
  df$Sch.Prd <- as.factor(df$Sch.Prd)
  df$TQS <- as.factor(df$TQS)
  df$Coreq.s <- NULL
  df$Scheduling.Days.Meet <- NULL
  df$Atn.Prd <- NULL
  df$Dsp.Prd <- NULL
  df$Instr.Staff <- NULL
  df$Room.Nbr <- NULL
  df$Teacher.Indicator <- NULL
  df$Teacher.Long.Name <- NULL
  df$Acad.Mins <- NULL
  df$Adv.Place.Code <- NULL
  df$Art.Tch.Prep <- NULL
  df$Rank <- NULL
  df$Keep.Atnd <- NULL
  df$Keep.Grds <- NULL
  df$Grad.Req.Crdts <- NULL
  df$Content.Area.Cd <- as.factor(df$Content.Area.Cd)
  df$Curric.Master <- as.factor(df$Curric.Master)
  df$Prds.Poss <- NULL
  df$Blk.Sec <- NULL
  df$ClsDf.Max <- NULL
  df$ClsDf.Min <- NULL
  df <- distinct(df)
  return(df)
}

courses_fhs_2012 <- fix_columns(courses_fhs_2012)
courses_fhs_2013 <- fix_columns(courses_fhs_2013)
courses_fhs_2014 <- fix_columns(courses_fhs_2014)
courses_fhs_2015 <- fix_columns(courses_fhs_2015)
courses_cjh_2012 <- fix_columns(courses_cjh_2012)
courses_cjh_2013 <- fix_columns(courses_cjh_2013)
courses_cjh_2014 <- fix_columns(courses_cjh_2014)
courses_cjh_2015 <- fix_columns(courses_cjh_2015)
courses_slms_2012 <- fix_columns(courses_slms_2012)
courses_slms_2013 <- fix_columns(courses_slms_2013)
courses_slms_2014 <- fix_columns(courses_slms_2014)
courses_slms_2015 <- fix_columns(courses_slms_2015)

skyward_grade_history$District.Code <- NULL
skyward_grade_history$District.Code.1 <- NULL
skyward_grade_history$X1st.Grd.Rq.Area <- NULL
skyward_grade_history$X1st.Grd.Req.Crd <- NULL
skyward_grade_history$X2nd.Grd.Rq.Area <- NULL
skyward_grade_history$X2nd.Grd.Rq.Crd <- NULL
skyward_grade_history$X3rd.Grd.Rq.Area <- NULL
skyward_grade_history$X3rd.Grd.Rq.Crd <- NULL

skyward_grade_history$Withdrawal.Date <- as.Date(skyward_grade_history$Withdrawal.Date, format = "%m/%d/%Y")

skyward_grade_history <- filter(skyward_grade_history, Withdrawal.Date > "2014-09-03" | is.na(Withdrawal.Date))

##### merge course files #####

courses_fhs <- full_join(courses_fhs_2012, courses_fhs_2013)
courses_fhs <- full_join(courses_fhs, courses_fhs_2014)
courses_fhs <- full_join(courses_fhs, courses_fhs_2015)

courses_cjh <- full_join(courses_cjh_2012, courses_cjh_2013)
courses_cjh <- full_join(courses_cjh, courses_cjh_2014)
courses_cjh <- full_join(courses_cjh, courses_cjh_2015)

courses_slms <- full_join(courses_slms_2012, courses_slms_2013)
courses_slms <- full_join(courses_slms, courses_slms_2014)
courses_slms <- full_join(courses_slms, courses_slms_2015)

courses_fhs$School <- "FHS"
courses_fhs$Entity.ID <- 400
courses_cjh$School <- "CJH"
courses_cjh$Entity.ID <- 300
courses_slms$School <- "SLMS"
courses_slms$Entity.ID <- 200

courses <- full_join(courses_fhs, courses_cjh)
courses <- full_join(courses, courses_slms)

courses <- distinct(courses)

courses <- rename(courses, School.Year = Schl.Year)
pathway <- rename(pathway, School.Year = Schl.Year)
pathway <- rename(pathway, cte_pathway = Subj)

courses <- filter(courses, CIP.Code > 0) %>% 
  select(School.Year, Long.Description, CIP.Code, Dept, Subj, School, Course.Key, Entity.ID, Short.Description, Content.Area.Cd, State.Code, Tech.Prep) %>% 
  distinct()

##### join grade history and course data

skyward_grade_history <- skyward_grade_history %>% 
  mutate(Student.Grades = gsub("Q[1,2,3,4]:\\[.+?\\]", "", Student.Grades)) %>% 
  mutate(Student.Grades = gsub("P[2,4]:\\[.+?\\]", "", Student.Grades)) %>% 
  mutate(Student.Grades = gsub("S[1,2]:\\[", "", Student.Grades)) %>% 
  mutate(Student.Grades = gsub("\\]", "", Student.Grades)) %>% 
  filter(School.Year != "2016") %>% 
  filter(Entity.ID > 199) %>% 
  mutate(Student.Grades = as.factor(Student.Grades))

skyward_grade_history <- droplevels(skyward_grade_history)

skyward_grade_history$Semester.Mark <- NULL
skyward_grade_history$Term.Mark <- NULL
skyward_grade_history$Final.Mark <- NULL

cte_history <- left_join(skyward_grade_history, courses, by = c("School.Year", "Course.Key", "Entity.ID")) %>% 
  filter(CIP.Code > 0) %>% 
  filter(Student.Grades != "") %>% 
  filter(Student.Grades != "W") %>% 
  filter(Student.Grades != "N") %>% 
  filter(Student.Grades != "NC") %>% 
  filter(Student.Grades != "P") %>% 
  filter(Student.Grades != "S")
  
cte_history <- cte_history %>% 
  mutate(Yearlong = ifelse((grepl("^\\d\\d\\d\\d$", cte_history$Course.Key)), "Semester","Yearlong")) %>% 
  mutate(Base_Course_Key = gsub("\\s\\d$", "", cte_history$Course.Key)) %>% 
  mutate(Sem = gsub("^\\d\\d\\d\\d\\s", "", cte_history$Course.Key))

cte_history$Dept <- NULL
cte_history$Subj <- NULL
cte_history$Subj.Short.Desc <- NULL

##### tech prep #####

tech_prep <- filter(cte_history, Student.Grades == "B" | Student.Grades == "B+" | Student.Grades == "A-" | Student.Grades == "A") %>% 
  filter(Tech.Prep == "Yes") %>% 
  filter(School.Year == 2015) %>% 
  filter(Tchr.User.Name != "") %>% 
  filter(School != "SLMS")

tech_prep_sem <- filter(tech_prep, Yearlong == "Semester") %>% 
  droplevels()
tech_prep_year <- filter(tech_prep, Yearlong == "Yearlong") %>% 
  droplevels()

tech_prep_year <- group_by(tech_prep_year, Stu.Full.Name, Entity.ID, Crs.Long.Desc, Base_Course_Key, Other.ID) %>% 
  mutate(sem_count = n()) %>% 
  filter(sem_count == 2)

write.csv(tech_prep_year, "tech prep yearlong.csv")
write.csv(tech_prep_sem, "tech prep semester.csv")
write.xlsx(tech_prep_year, "tech prep yearlong.xlsx")
write.xlsx(tech_prep_sem, "tech prep semester.xlsx")

##### CTE Completer #####

cte_comp <- filter(cte_history, Student.Grades != "F")

cte_comp <- left_join(cte_comp, pathway, by = c("School.Year", "Course.Key", "School", "CIP.Code"))

write.csv(cte_comp, "cte_completers_raw.csv")

cte_comp_counted <- cte_comp %>% 
  arrange(Stu.Full.Name, cte_pathway, School.Year, Sem) %>% 
  group_by(Stu.Full.Name, Other.ID, cte_pathway) %>% 
  mutate(cte_count = 1:n()) %>% 
  mutate(cte_comp_total = n()) %>% 
  droplevels()

# cte_comp_counted <- group_by(cte_comp, Stu.Full.Name, Other.ID, cte_pathway) %>% 
#   summarise(cte_count = n()) %>% 
#   filter(cte_count >= 4)

write.csv(cte_comp_counted, "cte completers.csv")
write.xlsx(cte_comp_counted, "cte completers.xlsx")

cte_min <- filter(cte_comp_counted, cte_count == 4)
cte_min$completer <- "CTE"
tech_prep_year_min <- filter(tech_prep_year, Sem == 2)
tech_prep_year_min$completer <- "Tech Prep"
tech_prep_sem$completer <- "Tech Prep"

data_entry <- full_join(tech_prep_sem, tech_prep_year_min) 
data_entry <- full_join(data_entry, cte_min) %>% 
  select(Stu.Full.Name, Entity.ID, Student.Grade, Course.Key, Crs.Short.Desc, School.Year, Start.Term, completer, CIP.Code, cte_pathway) %>% 
  filter(School.Year == 2015)

data_entry$Entered.Into.Skyward <- ""

write.xlsx(data_entry, "completer_data_entry.xlsx")

forassessment <- select(cte_history, Stu.Full.Name, Other.ID) %>% 
  distinct()