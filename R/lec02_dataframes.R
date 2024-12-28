# Building matrices in R

# Using matrix()
# Matrix (data, nrow, ncol, byrow, dimnames)

m <- matrix(
  c(2,4,3,1,5,7), # the data elements
  nrow = 2,       # number of rows
  ncol = 3,       # number of columns
  byrow = TRUE    # fill matrix by rows

)
m

matrix(1:12,3,4)
matrix(1:12,ncol = 4,nrow = 3)

ID <- 1:15
weight <- rnorm(15,mean=75, sd=4)
height <- rnorm(15,mean=1.65, sd=0.2)

#combine into matrix

study.group <- matrix(c(ID,weight,height), ncol = 3, byrow = FALSE)
study.group <- cbind(ID,weight,height)

dim(study.group)

# Add a row to the formed matrix... use rbind

study.group <- rbind(study.group,c(16,77,1.6))

study.group[3,2]
study.group[3,2:3]
study.group[3,c(1,3)]
study.group[3,c(2,3)]
study.group[,2]
study.group[3,]

study.group[16,2] <- 87
study.group[16,2]


#matrix math

m*2
m**2

#objects and their properties
class(study.group)
cowName <- c(
  "Daisy","Bia","Mimosa","Princess",
  "lili","Nora","earTag1378","ceci",
  "kiki","Amelia"
)
lactation <- c(1,2,5,3,2,2,4,1,4,3)
vaccinated <- c(TRUE,FALSE,FALSE,TRUE,TRUE,
                TRUE,TRUE,FALSE,TRUE,TRUE)
ELISA <- c(0.1,0.5,0,2.5,1.2,0,1.5,0,0,2.5)

class(cowName)
class(lactation)
is.integer(lactation)
lactation <- as.integer(lactation)
class(vaccinated)
class(ELISA)
as.integer(ELISA)


v <- 1:5
v[10] <- 10
v
is.na(v)
which(is.na(v)) # drops missing values
sum(is.na(v)) # sums up none NA
v[is.na(v)] <- 3 # replaces NAs with 3
v

# using cbind to matrix will return them as
# characters and therefore a dataframe is the solution
IDD <- 1:10
cow.study <- data.frame(IDD,cowName,lactation,vaccinated)
class(cow.study)
class(cow.study[,'cowName'])

cow.study[,"cowName"] <- as.factor(cow.study[,'cowName'])
class(cow.study[,'cowName'])

aggregate(lactation ~ cowName, FUN = mean, data = cow.study)

feed.group <- c(rep("A",5))
class(feed.group)

cow.study <- cbind(cow.study,feed.group)
class(cow.study[,'feed.group'])
dim(cow.study)


# indexing

cow.study[3,"lactation"]
cow.study[cow.study$cowName=="Mimosa",]

# more properties of data frames and matrices

dim(cow.study)
nrow(cow.study)
ncol(cow.study)
dim(cow.study)[1]
dim(cow.study)[2]

original.names <- colnames(cow.study)
colnames(cow.study) <- c("name","lact","vacc","feed")
cow.study

# Remember, R is not meant to SEE data

str(cow.study)
summary(cow.study)
head(cow.study)
tail(cow.study)
table(cow.study$vacc)
table(cow.study$vacc,cow.study$lact)


### Arrays

result <- array(rpois(18,5),dim = c(2,3,3))


