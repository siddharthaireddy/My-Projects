
list.of.packages <- c("ggplot2", "RSpectra","dplyr","FactoMineR",
                      "DT", 
                      "GGally",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "tabplot")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all these
lapply(list.of.packages, require, character.only = TRUE)

options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  
  # read in the CSV
  the_data_fn <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    the_data <-   read.csv(inFile$datapath, header = (input$header == "Yes"),
                               sep = input$sep, quote = input$quote, stringsAsFactors=FALSE)
    return(the_data)
  })

  
  # tableplot
  output$tableplot <- renderPlot({
    if(is.null(the_data_fn())) return()
    the_data <- the_data_fn()
    tabplot::tableplot(the_data)
    
  })
  
  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })
  
  # display a summary of the CSV contents
  output$summary <-  renderTable({
    the_data <- the_data_fn()
    psych::describe(the_data)
  })
  
  
  
  
  
  # corr tables
  output$corr_tables <- renderTable({
    the_data <- the_data_fn()
    # we only want to show numeric cols
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    
      res <- Hmisc::rcorr(as.matrix(the_data_num))
      cormat <- res$r
      pmat <- res$P
      ut <- upper.tri(cormat)
     df <- data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
     with(df, df[order(-cor), ])
    
  })
 

output$kmo <- renderPrint({
  the_data <- the_data_fn()
  the_data_num <- the_data[,sapply(the_data,is.numeric)]
  # exclude cols with zero variance
  the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
 
  kmo = function( data ){ 
    
    library(MASS) 
    X <- cor(as.matrix(data)) 
    iX <- ginv(X) 
    S2 <- diag(diag((iX^-1))) 
    AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix 
    IS <- X+AIS-2*S2                         # image covariance matrix 
    Dai <- sqrt(diag(diag(AIS))) 
    IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix 
    AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix 
    a <- apply((AIR - diag(diag(AIR)))^2, 2, sum) 
    AA <- sum(a) 
    b <- apply((X - diag(nrow(X)))^2, 2, sum) 
    BB <- sum(b) 
    MSA <- b/(b+a)                        # indiv. measures of sampling adequacy 
    
    AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the 
    # correlation matrix. That is the 
    # negative of the partial correlations, 
    # partialling out all other variables. 
    
    kmo <- BB/(AA+BB)                     # overall KMO statistic 
    
    # Reporting the conclusion 
    if (kmo >= 0.00 && kmo < 0.50){ 
      test <- 'The KMO test yields a degree of common variance 
      unacceptable for FA.' 
    } else if (kmo >= 0.50 && kmo < 0.60){ 
      test <- 'The KMO test yields a degree of common variance miserable.' 
    } else if (kmo >= 0.60 && kmo < 0.70){ 
      test <- 'The KMO test yields a degree of common variance mediocre.' 
    } else if (kmo >= 0.70 && kmo < 0.80){ 
      test <- 'The KMO test yields a degree of common variance middling.' 
    } else if (kmo >= 0.80 && kmo < 0.90){ 
      test <- 'The KMO test yields a degree of common variance meritorious.' 
    } else { 
      test <- 'The KMO test yields a degree of common variance marvelous.' 
    } 
    
    ans <- list(  overall = kmo, 
                  report = test, 
                  individual = MSA, 
                  AIS = AIS, 
                  AIR = AIR ) 
    return(ans) 
    
  }    # end of kmo() 
  kmo(na.omit(the_data_num))
  
}) 
  
 
  
  # Check boxes to choose columns
  output$choose_columns_pca <- renderUI({
    
    the_data <- the_data_fn()
    
    # Get the data set with the appropriate name
    
    # we only want to show numeric cols
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    
    colnames <- names(the_data_num)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_data <- the_data_fn()

              
  # for grouping we want to see only cols where the number of unique values are less than 
  # 10% the number of observations
    grouping_cols <- sapply(seq(1, ncol(the_data)), function(i) length(unique(the_data[,i])) < nrow(the_data)/10 )
    
    the_data_group_cols <- the_data[, grouping_cols, drop = FALSE]
    # drop down selection
    selectInput(inputId = "the_grouping_variable", 
                label = "Grouping variable:",
                choices=c("None", names(the_data_group_cols)))

  })
  
  
pca_objects <- reactive({
  # Keep the selected columns
  columns <-    input$columns
  the_data <- na.omit(the_data_fn())
  the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
  
  
  pca_output <- prcomp(na.omit(the_data_subset), 
                       center = (input$center == 'Yes'), 
                       scale. = (input$scale. == 'Yes'))
  # data.frame of PCs
  pcs_df <- cbind(the_data, pca_output$x)
  
  return(list(the_data = the_data, 
       the_data_subset = the_data_subset,
       pca_output = pca_output, 
       pcs_df = pcs_df))
  
})

output$the_pcs_to_plot_x <- renderUI({
  pca_output <- pca_objects()$pca_output$x
  
  # drop down selection
  selectInput(inputId = "the_pcs_to_plot_x", 
              label = "X axis:",
              choices= colnames(pca_output), 
              selected = 'PC1')
})

output$the_pcs_to_plot_y <- renderUI({
  pca_output <- pca_objects()$pca_output$x
  
  # drop down selection
  selectInput(inputId = "the_pcs_to_plot_y", 
              label = "Y axis:",
              choices= colnames(pca_output), 
              selected = 'PC2')
})
  


  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })
  
  
  # PC plot
 pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable

    if(grouping == 'None'){
      # plot without grouping variable
      pc_plot_no_groups  <- ggplot(pcs_df, 
                                   aes_string(input$the_pcs_to_plot_x, 
                                              input$the_pcs_to_plot_y
                                                  )) +
        
        
        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
      # the plot
      pc_plot_no_groups
      
      
    } else {
    # plot with grouping variable
      
      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
    pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                          input$the_pcs_to_plot_y, 
                                          fill = 'fill_', 
                                          colour = 'fill_'
                                          )) +
      stat_ellipse(geom = "polygon", alpha = 0.1) +
    
      geom_text(aes(label = labels),  size = 5) +
      theme_bw(base_size = 14) +
      scale_colour_discrete(guide = FALSE) +
      guides(fill = guide_legend(title = "groups")) +
      theme(legend.position="top") +
      coord_equal() +
      xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
      ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    # the plot
    pc_plot_groups
    }
    
    
  })
  
  output$brush_info <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })
  

  # for zooming
  output$z_plot1 <- renderPlot({
    
    pca_biplot() 

  })
  
  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$z_plot1Brush
    if (!is.null(brush)) {
      zooming$x <- c(brush$xmin, brush$xmax)
      zooming$y <- c(brush$ymin, brush$ymax)
    }
    else {
      zooming$x <- NULL
      zooming$y <- NULL
    }
  })
  

  # for zooming
  output$z_plot2 <- renderPlot({
    
    pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y) 

    
  })
  
  output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })
  
  output$pca_details <- renderPrint({
    # 
    print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)
    
  })
  
  
  
  ###########
  
  
  
  output$faceplot1 <- renderPlot({
    df_new <- data.frame()
    # Read the pixel data for face from the csv file
    image_data <- read.csv("C://Users//user//Downloads//R Project//face_data.csv",header = F) %>% as.matrix()
    
    # Function to plot image data
    image_plot <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
    
    # Set the graphical parameters
    par(mfrow=c(2,2))
    par(mar=c(2,2,2,2))
    
    if (!is.null(input$imagerow)){
      # We will display the image(data from the row parsed) from the image dataset which has 400 images
      image_1 <- matrix(as.numeric(image_data[as.integer(input$imagerow), ]), nrow=64, byrow=T)
      image_plot(image_1)
    }
    
    if (!is.null(input$imagerow)){
      straight_image <- t(apply(matrix(as.numeric(image_data[as.integer(input$imagerow), ]), nrow=64, byrow=T), 2, rev))
      image_plot(straight_image)
    }
    
    if (!is.null(input$startimagerow)){
      if (!is.null(input$endimagerow)){
        # All the images in the dataset are then rotated to make them straight and are then saved into a new file
        for(i in 1:nrow(image_data))
        {
          # Each row is looped through to convert the merge the pixels into a matrix and then rotate the resultant image by 90 degree
          straight_image <- as.numeric((apply(matrix(as.numeric(image_data[i, ]), nrow=64, byrow=T), 2, rev)))
          
          # Bind the new data iteratively with the new dataset
          df_new <- rbind(df_new,straight_image)
        }
        
        # Store the newly obtained data in a dataframe
        df_straight_image =as.data.frame(df_new)
        
        # Calculate the average of the data which are to be taken into consideration and parse it to the image_plot function to construct the average image
        average_data <- colMeans(data.matrix(df_straight_image[input$startimagerow:input$endimagerow,]))
        image_plot(matrix(average_data, nrow=64, byrow=T))
      }
    }
    
  }, height = 500, width = 500)
  
  output$faceplot2 <- renderPlot({
    df_new <- data.frame()
    
    image_data <- read.csv("C://Users//user//Downloads//R Project//face_data.csv",header = F) %>% as.matrix()
    
    
    image_plot <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
    
    
    par(mfrow=c(2,2))
    par(mar=c(2,2,2,2))
    
     
    for(i in 1:nrow(image_data))
    {
      
      straight_image <- as.numeric((apply(matrix(as.numeric(image_data[i, ]), nrow=64, byrow=T), 2, rev)))
      
      
      df_new <- rbind(df_new,straight_image)
    }
    
    
    df_straight_image =as.data.frame(df_new)
    
    
    scaled_data_new <- scale(data.matrix(df_straight_image))
    
    
    cov_mat <- t(scaled_data_new) %*% scaled_data_new / (nrow(scaled_data_new)-1)
    
        
    eigs <- eigs(cov_mat, 40, which = "LM")
    
    
    eigenvalues <- eigs$values
    
    
    eigenvectors <- eigs$vectors
    
    par(mfrow=c(1,1))
    par(mar=c(2.5,2.5,2.5,2.5))
    
    
    
    y_cord=eigenvalues[1:40]
    
    plot(1:40, y_cord, type="o", log = "y", main="Weight associated with each Eigen Value", xlab="Eigenvalue Number", ylab="Weight")
  }, height = 500, width = 500)
  
  output$faceplot3 <- renderPlot({
    df_new <- data.frame()
    
    image_data <- read.csv("C://Users//user//Downloads//R Project//face_data.csv",header = F) %>% as.matrix()
    
    image_plot <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
    
    
    par(mfrow=c(2,2))
    par(mar=c(2,2,2,2))
    
     
    for(i in 1:nrow(image_data))
    {
      
      straight_image <- as.numeric((apply(matrix(as.numeric(image_data[i, ]), nrow=64, byrow=T), 2, rev)))
      
      
      df_new <- rbind(df_new,straight_image)
    }
    
    df_straight_image =as.data.frame(df_new)
    
    
    average_face=colMeans(df_straight_image)
    AVF=matrix(average_face,nrow=1,byrow=T)
    
    
    scaled_data_new <- scale(data.matrix(df_straight_image))
    
    
    cov_mat <- t(scaled_data_new) %*% scaled_data_new / (nrow(scaled_data_new)-1)
    
        
    eigs <- eigs(cov_mat, 40, which = "LM")
    
    
    eigenvalues <- eigs$values
    
    
    eigenvectors <- eigs$vectors
    
    sum(eigenvalues)/sum(eigen(cov_mat)$values)
    
    
    D_new <- scaled_data_new %*% eigenvectors
    
    # Set the graphical parameters the matrix is is 1*2 1 rows and 2 columns and a standard margin of 2 everywhere
    par(mfrow=c(1,2))
    par(mar=c(2, 2, 2, 2))
    
    
    for (i in 1:2){
      image_plot(matrix(as.numeric(eigenvectors[, i]),nrow=64,byrow=T))
    }
    
  }, height = 500, width = 500)
  
  
  #########
  
  
}
  


