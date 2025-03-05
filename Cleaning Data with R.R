ambulancesData <- read.csv("./data/nigeria_ambulances.csv")
names(ambulancesData)
tolower(names(ambulancesData))
fileUrl <- 
download.file(fileUrl, )
#############################
foods
tolower(names(foods)
peace<- list(letters = c("Serious", "Studious", "Stable", "Others"), numbers = 1:6, matrix(1:12, ncol = 3))
head(peace)
peace$letters[[3]]

splitNames=strsplit(names(foods), "\\ ")
splitNames
#firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)
View(foods)
sub("-", "_", names(foods))
maria <- "so_long_a_letter"

sub("_", "", maria)

gsub("_", " ", maria)
grep("Tomato", foods$ItemLabel)

table(grepl("Tomato", foods$ItemLabel))

foods2 <- foods[!grepl("Tomato",foods$ItemLabel),]

foods2
grep("Tomato", foods$ItemLabel, value=TRUE)

grep("Plantain(ripe)", foods$ItemLabel)

length(grep("Plantain(ripe)", foods$ItemLabel)
sa <- "Senator Adetokunbo"
       
nchar(sa)   
sa_b4 <- substr(sa, 8,18)

sa_b4 
paste0("Senator","Adetokunbo")
paste("Senator", "Adetokunbo")
str_trim("Senator      ")
###############################
#ambulances <- read.csv("./data/nigeria_ambulances.csv")

foods2 <- foods %>% 
  mutate(ItemLabel = str_replace(ItemLabel, "Tomato", "Timati"))

foods2
View(foods2)
ambulances <- read.csv("./data/nigeria_ambulances.csv")

rep_str = c('Abia'='South East','Anambra'='South East','Taraba'='North East')

ambulances$state_name <- str_replace_all(ambulances$state_name, rep_str)

ambulances
###############################
aipapers <- read.csv("C:/Users/Open User/Downloads/ai_sample_papers.csv")
table(grepl("AI", aipapers$abstract))

ai2 <- aipapers[!grepl("AI", aipapers$abstract),]

ai2
tolu=Sys.Date()
tolu
format(tolu,"%d %b %a")
tess=as.Date(17-08-2024, "%d%m%Y")
tess

aba = c("14feb2023", "22feb2023", "18jun2023", "28jun2023", "14jul2023")

layi = as.Date(aba, "%d%b%Y")

layi

layi[2] - layi[1]

as.numeric(layi[2]-layi[1])

yomi = Sys.Date()

weekdays(yomi)

months(yomi)

julian(yomi)
library(lubridate)

ymd("230628")

mdy("06/28/2023")

dmy("28-06-2023"),
ymd_hms("2011-08-03 10:15:03"),

ymd_hms("2011-08-03 10:15:03",tz="GMT")

?Sys.timezone
aba = dmy(c("14feb2023", "22feb2023", "18jun2023", "28jun2023", "14jul2023"))

wday(aba[1])
wday(aba[1], label=TRUE)
#############################
aipapers
head(aipapers)
View(aipapers)
names(aipapers)
tolower(names(aipapers))
splitNames=strsplit(names(aipapers), "\\_")
splitNames
splitNames[[16]]
splitNames[[15]][1]
firstElement <- function(x){x[1]}
sapply(splitNames, firstElement)
names(aipapers)
sub('_', ' ', names(aipapers))

grep("Hassan Satori", aipapers$submitter)
table(grepl("Hassan Satori", aipapers$submitter))
aipapers2 <- aipapers[!grepl("Hassan Satori", aipapers$submitter),]
aipapers2

aipapers2 <- aipapers %>%
mutate(submitter = str_replace(submitter, "Elena Orlando", "Helena"))  
aipapers2

aipapers2 <- aipapers %>% 
  mutate(submitter = str_replace(submitter, "Elena Orlando", "Helena"))

aipapers2
ambu <- read.csv("C:/Users/Open User/Downloads/nigeria_ambulances.csv")
rep_str = c('Abia'='South East','Anambra'='South East','Taraba'='North East')

ambu$state_name <- str_replace_all(ambu$state_name, rep_str)

ambu
