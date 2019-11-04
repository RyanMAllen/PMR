postal_cluster <- function(df) {      
  out <- kmeans(c(df$Latitude, df$Longitude), 3, iter.max=10)
  return(data.frame(CDSCode = df$CDSCode, Enrollment = df$Enrollment, Latitude=df$Latitude, Longitude=df$Longitude, Cluster=out$cluster))
}


getOutputSchema <- function() {      
  return (data.frame (
    CDSCode = prep_string(),
    Enrollment = prep_int(),
    Latitude = prep_decimal (),
    Longitude = prep_decimal (),
    Cluster = prep_int ()));
}

