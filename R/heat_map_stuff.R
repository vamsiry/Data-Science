
#Objects
#df = Your data.frame (with all data)
#df$cluster_group = column in data.frame identifying cluster group


#Create Heat Map Plot Function
heat_plot <- function(df = data, x = "X_Column", y = "Y_Column", value = "Value_Column", 
                      lab = c("Title","X label","Y label"), facet = NULL)
{
  
  #Arguments
  #df = data.frame for each cluster group
  #x = name of column used for x axis
  #y = name of column used for y axis
  #value = name of numeric column used to denote intensity
  #lab = character vector c("title of plot","x axis label","y axis label")
  
  outplot <- ggplot2::ggplot(df, aes_string(x, y ))
  
  outplot <- outplot + ggplot2::geom_tile(aes_string(fill = value), color = "white")
  
  if( !is.null(facet) & is.character(facet)){
  outplot <- outplot + ggplot2::facet_wrap(facet)
  } else if (!is.character(facet)) {
    warning("Please verify facet argument is a a character string")
    stop()
  }
  
  outplot <- outplot + ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    
  outplot  <- outplot + ggplot2::labs(title = lab[1], x = lab[2] ,y = lab[3])
  
  outplot <- outplot + ggplot2::theme(legend.title = element_text(size = 10),
                                      legend.text = element_text(size = 12),
                                      plot.title = element_text(size=16),
                                      axis.title=element_text(size=14, face = "bold"),
                                      axis.text.x = element_text(angle = 90, hjust = 1))
}


clusters <- split(df, df$cluster_group)

plots <- lapply(clusters, function(i) heat_plot(df = i, x = "X_Column", y = "Y_Column", 
                                                value = "Value_Column", lab = c("Title","X lable","Y lable"))
                )