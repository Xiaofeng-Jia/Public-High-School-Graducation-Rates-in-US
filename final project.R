library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

d = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/acgr-lea-sy2020-21-long.csv')
str(d)

geo = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/location.csv')
str(geo)

#Notes: 9/29 meeting with Dr. Lehmann
# https://nces.ed.gov/programs/coe/indicator/coi/high-school-graduation-rates
#1. will location information work in a map?
#state level is fine
#dplyr interjoin
#inner_join()
#inner_join(d, new_data, by=‘lead’)

#match the zipcode with location

#1-2. do you think map will be a good form to display the information?

#2. catagory: can I divid them into 2 variables?
#3. the rate is a range, how to deal with it?
#if else there is one column



#-------------------------------------------------------------------
# STEP1: Data cleaning
#-------------------------------------------------------------------


# use inner join function to add the geo information to the dataset
d1 = merge(x = d, y = geo, by = 'LEAID')

# deal with the rate to get a average number
d1 = d1 %>%
  separate('RATE', c('L_RATE', 'H_RATE'),'\\-' ) %>%
  mutate(
    L_RATE = str_remove_all(L_RATE, 'GE'),
    L_RATE = str_remove_all(L_RATE, 'PS'),
    L_RATE = as.numeric(L_RATE),
    H_RATE = as.numeric(H_RATE)
  )

#replacing n/a into 0
d1[is.na(d1)] = 0

str(d1)

#calculate the Rate
d1$Rate = rowMeans(d1[ , c(9, 10)], na.rm=TRUE)

#replace 0 into n/a

d1['Rate'][d1['Rate'] == 0] <- NA

#drop some unused variables to make it easier to check
df = subset(d1, select = -c(1,4,5,8,9,10))


# Define breaks for cut function
breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# Use cut function to create a new variable
df$RateLevel <- cut(df$Rate, breaks = breaks, labels = letters[1:10], include.lowest = TRUE)


df = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/df.csv')

unique(df$RateLevel)

df = df %>%
  mutate(
    RateLevel = str_replace_all(RateLevel, 'a', '0-10'),
    RateLevel = str_replace_all(RateLevel, 'b', '10-20'),
    RateLevel = str_replace_all(RateLevel, 'c', '20-30'),
    RateLevel = str_replace_all(RateLevel, 'd', '30-40'),
    RateLevel = str_replace_all(RateLevel, 'e', '40-50'),
    RateLevel = str_replace_all(RateLevel, 'f', '50-60'),
    RateLevel = str_replace_all(RateLevel, 'g', '60-70'),
    RateLevel = str_replace_all(RateLevel, 'h', '70-80'),
    RateLevel = str_replace_all(RateLevel, 'i', '80-90'),
    RateLevel = str_replace_all(RateLevel, 'j', '90-100'),
    
    CATEGORY = str_replace_all(CATEGORY, 'CWD', 'Children with disabilities'),
    CATEGORY = str_replace_all(CATEGORY, 'ECD', 'Economically disadvantaged students'),
    CATEGORY = str_replace_all(CATEGORY, 'FCS', 'Foster Care students'),
    CATEGORY = str_replace_all(CATEGORY, 'HOM', 'Homeless Enrolled students'),    
    CATEGORY = str_replace_all(CATEGORY, 'LEP', 'English Learner students'),
    CATEGORY = str_replace_all(CATEGORY, 'MAM', 'American Indian/Alaska Native students'),    
    CATEGORY = str_replace_all(CATEGORY, 'MAS', 'Asian/Pacific Islander students'),    
    CATEGORY = str_replace_all(CATEGORY, 'MBL', 'Black students'),    
    CATEGORY = str_replace_all(CATEGORY, 'MHI', 'Hispanic/Latino students'),    
    CATEGORY = str_replace_all(CATEGORY, 'MTR', 'Multiracial Students'),   
    CATEGORY = str_replace_all(CATEGORY, 'MWH', 'White students')
    )


df = df %>%
  mutate(
    CATEGORY = str_replace_all(CATEGORY, 'ALL', 'U.S. average')
  )


write.csv(df, '/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/df.csv', row.names=FALSE)







#----------------------------------------------------------------
# STEP 2: Make Plots
#----------------------------------------------------------------

All = subset(df, CATEGORY == 'ALL')
#disadvantage groups
CWD = subset(df, CATEGORY == 'CWD')
ECD = subset(df, CATEGORY == 'ECD')
FCS = subset(df, CATEGORY == 'FCS')
HOM = subset(df, CATEGORY == 'HOM')
LEP = subset(df, CATEGORY == 'LEP')
#races
MAM = subset(df, CATEGORY == 'MAM')
MAS = subset(df, CATEGORY == 'MAS')
MBL = subset(df, CATEGORY == 'MBL')
MHI = subset(df, CATEGORY == 'MHI')
MWH = subset(df, CATEGORY == 'MWH')
MTR = subset(df, CATEGORY == 'MTR')



#install.packages('rgdal') 
library(leaflet)
#library(sp)
#library(rgdal)

#https://www.youtube.com/watch?v=oJhvoXjeyso&list=PLaZuFfQk5gXMaQVhYkGwM0tlg9kHDZ7Pm&index=4

#make data spatial
# in my understanding, this step is to convert the address to the LON and LAT??? So I may do not need to use this step because I already have the LON and LAT
#??? On the Youtube video, it should have a lot of points. In effect, it changed the dataset into a weird format

#coordinates(df) = c('LON', 'LAT')
#crs.geo1 = CRS('+proj=longlat')
#proj4string(df) = crs.geo1



#shapefile
#download the shapefile from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# I downloaded the county level shapefile

require(rgdal)
#could not find function "readOGR"
shape = readOGR(dsn = '/Users/crazycat/Documents/academic/5th year/MAS 627 R/final project/cb_2018_us_county_500k', layer = '1')

plot(shape)

points(df, pch = 20, col = 'Black')


bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
qpal = colorBin('Reds', df$Rate, bins=bins)

labels = sprintf(
  '<strong>%s</strong>',
  df$Rate) %>%
  lapply (htmltools:: HTML)


#Error in polygonData.default(data) : 
#Don't know how to get path data from object of class data.frame

leaflet(df) %>%
  addPolygons(stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5, color = 'white', fillColor = ~qpal(Rate), weight = 1) %>%
  addLegend(values = ~Rate, pal = qpal, title = 'Graduate Rate')

