postal_cluster <- function(df) {      
  out <- kmeans(c(df$Latitude, df$Longitude), 3, iter.max=10)
  return(data.frame(Latitude=df$Latitude, Longitude=df$Longitude, Cluster=out$cluster))
}


getOutputSchema <- function() {      
  return (data.frame (
    Latitude = prep_decimal (),
    Longitude = prep_decimal (),
    Cluster = prep_int ()));
}

install.packages("read.xlsx")
install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
schools <- read.xlsx("C:/Users/Ryan.M.Allen/Documents/My Tableau Repository/Client Trainings/OIG/Student Files/CA Public Schools - Active and Pending Jan 2019.xlsx")
head(schools)
View(schools)

df <- tail(schools, -5)
View(df)

colnames(df) <- as.character(unlist(df[1,]))
df = df[-1, ]
dim(df)
df <- df[,c(17,18,19)]
df$Latitude <- df$School_Lat
df$Longitude <- df$School_Long
df$School_Lat <- NULL
df$School_Long <- NULL
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df$Enrollment <- as.numeric(df$Enrollment)
df <- df[complete.cases(df),]

str(df)

predictions <- postal_cluster(df)

ggplot(predictions, aes(x= Cluster))+ geom_histogram() + stat_bin(bidwidth = 1, geom = "text", aes(label = ..count..), vjust = -1.5) +
  ylim(0, 15000)
ggplot(data = clusters, aes(Longitude, Latitude)) + 
  geom_point(aes(shape = as.character(Cluster), color = as.character(Cluster)), alpha = .25) + 
  scale_shape_manual(values = c(5, 16, 17)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_minimal() +
  theme(legend.position = "top")