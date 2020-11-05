ui <- bootstrapPage(
  mainPanel(
    
    titlePanel("A Comprehensive View of Principle Componenet Analysis: Dashboard"),
    tabsetPanel(
      
      tabPanel("Data input", 
               p("Select the options that match your CSV file, then upload your file:"),
               
               
               radioButtons(inputId = 'header',  
                            label = 'Header',
                            choices = c('Columns have headers'='Yes',
                                        'Columns do not have headers'='No'), 
                            selected = 'Yes'),
               
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
               
               radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
               
               tags$hr(),
               
               fileInput('file1', 'Choose a CSV file to upload:',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )),
               p("Click on Data insights for further reference")
               
      ), # end file  tab
      
      tabPanel("Data Insights",
               
               p("Summary of the data"),
               tableOutput('summary'),
               tags$hr(),
               p("Raw data from the CSV file"),
               DT::dataTableOutput('contents'),
               #  ), # end  tab
               
               
               # tabPanel("Correlation Plots",
               uiOutput("choose_columns_biplot"),
               tags$hr()
            
      ), # end  tab
      
      
      tabPanel("Customize Your PCA",
               
               p("Choose the columns of your data to include in the PCA."),
               
               uiOutput("choose_columns_pca"),
               tags$hr(),
               p("Select options for the PCA computation"),
               radioButtons(inputId = 'center',  
                            label = 'Center',
                            choices = c('Shift variables to be zero centered'='Yes',
                                        'Do not shift variables'='No'), 
                            selected = 'Yes'),
               
               radioButtons('scale.', 'Scale',
                            choices = c('Scale variables to have unit variance'='Yes',
                                        'Do not scale variables'='No'), 
                            selected = 'Yes')
               
      ), # end  tab
      
      tabPanel("PCA output and Plots",
               verbatimTextOutput("pca_details"),
               
              
               h2("Scree plot"),
               p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
               plotOutput("plot2", height = "300px"),
               tags$hr(),
               h2("PC plot: zoom and select points"),
               p("Select the grouping variable."),
               p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
               uiOutput("the_grouping_variable"),
               tags$hr(),
               p("Select the PCs to plot"),
               uiOutput("the_pcs_to_plot_x"),
               uiOutput("the_pcs_to_plot_y"),
               tags$hr(),
               
               p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
               p("Then select points on zoomed plot below to get more information about the points."),
               
               plotOutput ("z_plot1", height = 400,
                           brush = brushOpts(
                             id = "z_plot1Brush",
                             resetOnNew = TRUE)),
               tags$hr(),
               
               p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
               
               plotOutput("z_plot2", height = 400,
                          brush = brushOpts(
                            id = "plot_brush_after_zoom",
                            resetOnNew = TRUE)),
               tags$hr()
              
      
      ), # end  tab 
      
      tabPanel("Eigen Faces",
               
               
               textInput(inputId = 'imagerow', label = 'Enter any Row Number', placeholder = 'Enter the row number to view Image'),
                       textInput(inputId = 'startimagerow', label = 'Enter Start Row Numbers for Average Faces', placeholder = 'Enter the Begin row number to view average of Image'),
                       textInput(inputId = 'endimagerow', label = 'Enter End Row Number for Average Faces', placeholder = 'Enter the ending row number to view average of Image'),
                       p('Image 1: Face of the mentioned row  Image 2: Rotated Face of the mentioned row Image 3: Average Faces of the entered Row Numbers'),
                       plotOutput('faceplot1'),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       p('First two Eigen Faces'),
                       plotOutput('faceplot3'),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       p('Plot to show the eigen values'),
                       plotOutput('faceplot2')
               )
               
               
               
      ) # end  tab 
      
      
    ))