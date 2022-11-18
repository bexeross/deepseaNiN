library(viridis)

gg_ordisurf<-function(ord, 
                      env.var, 
                      groups=NA, 
                      choices=c(1,2), 
                      var.label="Level", 
                      binwidth, 
                      pt.size=3,
                      gen.text.size = 20,
                      title.text.size = 20,
                      leg.text.size = 20,
                      plot=TRUE) {
  
  groups <- as.factor(groups)
  
  # Extract ordisurf data for plotting
  ordi <- vegan::ordisurf(ord ~ env.var, plot=FALSE) # creates the ordisurf object
  ordi.grid <- ordi$grid # extracts the ordisurf object
  ordi.data <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) # get x and y
  ordi.data$z <- as.vector(ordi.grid$z) # unravel the matrix for the z scores
  df_surf <- data.frame(na.omit(ordi.data)) # gets rid of the NAs
  
  # Extract site coordinates for plotting.
  df_ord <- as.data.frame(scores(ord, choices = choices, display = "sites"))
  if (is.na(groups)[1]) {
    df_ord <- data.frame(x=df_ord[ , 1], y=df_ord[ , 2])
  } else {
    df_ord <- data.frame(x=df_ord[ , 1], y=df_ord[ , 2], Group=groups)
  }
  df_ord<-cbind(df_ord,env.var)
  
  # Make axis labels.
  axis.labels <- ord_labels(ord)[choices]
  xlab <- axis.labels[1]
  ylab <- axis.labels[2]
  
  # Calculate default binwidth
  if(missing(binwidth)) {
    r <- range(env.var, na.rm = TRUE)
    binwidth <- (r[2]-r[1])/15
  }
  
  # Plotting in ggplot2
  if (is.na(groups)[1]) {
    plt <- ggplot() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = leg.text.size),
            text = element_text(size = gen.text.size),
            plot.title = element_text(size = title.text.size)) +
      labs(title = var.label) +
      geom_point(data=df_ord[!is.na(env.var),], aes(x=x, y=y), color = "#DCC8F5", size=pt.size+2) +
      geom_point(data=df_ord, aes(x=x, y=y, colour=env.var), size=pt.size) +
      scale_color_gradientn(colours=viridis(n=6)) +
      xlab(xlab) + ylab(ylab) +
      stat_contour(data=df_surf, aes(x=x, y=y, z=z, color= ..level..), binwidth=binwidth) +
      labs(color=var.label) +
      coord_fixed(ratio=1)
  } else {
    plt <- ggplot() +
      theme(legend.title = element_blank(),
            # legend.text = element_text(size = gen.text.size),
            text = element_text(size = gen.text.size),
            plot.title = element_text(size = title.text.size)) +
      labs(title = var.label) +
      geom_point(data=df_ord[!is.na(env.var),], aes(x=x, y=y), color = "#DCC8F5", size=pt.size+2) +
      geom_point(data=df_ord, aes(x=x, y=y, colour=env.var), shape=21, size=pt.size) +
      xlab(xlab) + ylab(ylab) +
      scale_color_gradientn(colours=viridis(n=6)) +
      stat_contour(data=df_surf, aes(x=x, y=y, z=z, color= ..level..), binwidth=binwidth) +
      labs(color=var.label) +
      coord_fixed(ratio=1)
  }
  
  # can change the binwidth depending on how many contours you want
  
  # Plot?
  if (plot) {print(plt)}
  
  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, df_surf=df_surf, plot=plt))
}
