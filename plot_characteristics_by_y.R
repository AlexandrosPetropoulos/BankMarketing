plot_characteristics_by_y<- function(bank, characteristic_name, save_to_file){
  
#se periptosi pou den douleui, isos thelei na ginete prota clean ta proigoumena plots
#diaforetika copy paste ta kommatia pou endiaferoun

  #ggplot Define aesthetic mappings programmatically
  #search ggplot define aes from function
  #http://www.katiejolly.io/blog/2018-02-05/aes-string
  
  #get the string of a column
  #colnames(trainTransformed)[18] <- "y"
  #to access a column instead of $ dollar sign
  #ggplot(bank) + geom_histogram(aes(x = age), binwidth = 2, col = "lightgreen") +
        #facet_grid(y~., scales = "free")  
  
  #ggplot(bank) + geom_bar(aes(x = job), col = "white") +
        #facet_grid(y~., scales = "free")
  
  #convert data frame name to string r
  #deparse(substitute(my.data))
  
  if(class(bank[[characteristic_name]])=="factor"){
    ggplot(bank) + geom_bar(aes_string(x = characteristic_name), col = "lightgreen") +
       facet_grid(y~., scales = "free")
    if(save_to_file==TRUE){
      #concatenate strings with paste
      ggsave(paste(characteristic_name,"versus_y.png",sep="_")) 
    }
    
  }
  else{
    ggplot(bank) + geom_histogram(aes_string(x = characteristic_name), binwidth = 2, col = "lightgreen") +
      facet_grid(y~., scales = "free") 
    
  }
  
  
  
  
}