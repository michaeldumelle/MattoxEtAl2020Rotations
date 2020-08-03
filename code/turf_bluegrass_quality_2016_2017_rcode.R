library(tidyverse)
library(stringr)
library(dunn.test)


mc_method = "holm"

e1_y1 <- read_csv("turfquality_2016_trial1.csv")
head(e1_y1)

#function test
test <- dunn.test(x = e1_y1$Nov, g = e1_y1$Trt, altp = TRUE, method = mc_method)
test_output <- tibble(group1 = str_sub(test$comparisons, 1, 1), 
                group2 = str_sub(test$comparisons, -1, -1),
                p = test$altP, 
                padj = test$altP.adjusted, 
                month = "Nov")
test_output

#writing the function
dunn_results <- function(x, g, altp = TRUE, method = mc_method, month = NULL) {
  #running the dunns test function
  dunn <- dunn.test(x = x, g = g, altp = altp, method = method)
  #getting the characters from the strings (str_trim() removes whitespace)
  group1 <- str_trim(str_sub(dunn$comparisons, 1, 2))
  group2 <- str_trim(str_sub(dunn$comparisons, -2, -1))
  output <- tibble(group1 = group1, 
                   group2 = group2,
                   p = dunn$altP, 
                   padj = dunn$altP.adjusted)
  if (!is.null(month)) output <- output %>% mutate(month = month)
  return(output)
}

#now running real test

#experiment 1 year 2
#making the list for the map
e1_y1 <- read_csv("turfquality_2016_trial1.csv")

#making the list for the map
data_list_e1_y1 <- list(list(month = "Nov", data = e1_y1$Nov),
                  list(month = "Dec", data = e1_y1$Dec),
                  list(month = "Jan", data = e1_y1$Jan),
                  list(month = "Feb", data = e1_y1$Feb),
                  list(month = "Mar", data = e1_y1$Mar))

#making the list of reslts
dunn_lists_e1_y1 <- map(.x = data_list_e1_y1, 
                  .f = function(a) dunn_results(x = a$data, g = e1_y1$Trt, altp = TRUE, method = mc_method, month = a$month))

#getting the final output
dunn_output_e1_y1 <- do.call(rbind, dunn_lists_e1_y1)  %>% 
  arrange(as.numeric(group1), as.numeric(group2)) %>%
  filter(padj <= 0.1)
dunn_output_e1_y1

#writing to a csv
#write_csv(dunn_output_e1_y1, "dunn_output_e1_y1.csv")




#experiment 1 year 2
#making the list for the map
e1_y2 <- read_csv("turfquality_2017_trial1.csv")

data_list_e1_y2 <- list(list(month = "Nov", data = e1_y2$Nov),
                        list(month = "Dec", data = e1_y2$Dec),
                        list(month = "Jan", data = e1_y2$Jan),
                        list(month = "Feb", data = e1_y2$Feb),
                        list(month = "Mar", data = e1_y2$Mar))

#making the list of reslts
dunn_lists_e1_y2 <- map(.x = data_list_e1_y2, 
                        .f = function(a) dunn_results(x = a$data, g = e1_y2$Trt, altp = TRUE, method = mc_method, month = a$month))

#getting the final output
dunn_output_e1_y2 <- do.call(rbind, dunn_lists_e1_y2)  %>% 
  arrange(as.numeric(group1), as.numeric(group2)) %>%
  filter(padj <= 0.1)
dunn_output_e1_y2

#writing to a csv
#write_csv(dunn_output_e1_y2, "dunn_output_e1_y2.csv")




#experiment 2 year 2
#making the list for the map
e2_y1 <- read_csv("turfquality_2016_trial2.csv")

#making the list for the map
data_list_e2_y1 <- list(list(month = "Nov", data = e2_y1$Nov),
                        list(month = "Dec", data = e2_y1$Dec),
                        list(month = "Jan", data = e2_y1$Jan),
                        list(month = "Feb", data = e2_y1$Feb),
                        list(month = "Mar", data = e2_y1$Mar),
                        list(month = "Apr", data = e2_y1$Apr))

#making the list of reslts
dunn_lists_e2_y1 <- map(.x = data_list_e2_y1, 
                        .f = function(a) dunn_results(x = a$data, g = e2_y1$Trt, altp = TRUE, method = mc_method, month = a$month))

#getting the final output
dunn_output_e2_y1 <- do.call(rbind, dunn_lists_e2_y1)  %>% 
  arrange(as.numeric(group1), as.numeric(group2)) %>%
  filter(padj <= 0.1)
dunn_output_e2_y1 

#writing to a csv
#write_csv(dunn_output_e2_y1, "dunn_output_e2_y1.csv")




#experiment 2 year 2
#making the list for the map
e2_y2 <- read_csv("turfquality_2017_trial2.csv")

data_list_e2_y2 <- list(list(month = "Nov", data = e2_y2$Nov),
                        list(month = "Dec", data = e2_y2$Dec),
                        list(month = "Jan", data = e2_y2$Jan),
                        list(month = "Feb", data = e2_y2$Feb),
                        list(month = "Mar", data = e2_y2$Mar),
                        list(month = "Apr", data = e2_y2$Apr))

#making the list of reslts
dunn_lists_e2_y2 <- map(.x = data_list_e2_y2, 
                        .f = function(a) dunn_results(x = a$data, g = e2_y2$Trt, altp = TRUE, method = mc_method, month = a$month))

#getting the final output
dunn_output_e2_y2 <- do.call(rbind, dunn_lists_e2_y2)  %>% 
  arrange(as.numeric(group1), as.numeric(group2)) %>%
  filter(padj <= 0.1)
dunn_output_e2_y2

#writing to a csv
#write_csv(dunn_output_e2_y2, "dunn_output_e2_y2.csv")




#bluegrass thinning

e2_y1_blue <- read.csv("bluegrassquality_2016_trial2.csv")
dunn_output_e2_y1_blue <- dunn_results(x = e2_y1_blue$Percent, g = e2_y1_blue$Trt, altp = TRUE, method = mc_method)
dunn_output_e2_y1_blue


#write_csv(dunn_output_e2_y1_blue, "dunn_output_e2_y1_blue.csv")



e2_y2_blue <- read.csv("bluegrassquality_2017_trial2.csv")
dunn_output_e2_y2_blue <- dunn_results(x = e2_y2_blue$Percent, g = e2_y2_blue$Trt, altp = TRUE, method = mc_method)
dunn_output_e2_y2_blue

#write_csv(dunn_output_e2_y2_blue, "dunn_output_e2_y2_blue.csv")
