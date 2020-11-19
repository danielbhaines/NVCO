### LIBRARIES ###

# call all the necessary libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(naniar)
library(ggplot2)
library(plotly)
library(caret)
library(mathjaxr)
set.seed(15)

### DATA ###

# import the full NAVCO dataset
NAVCO <- readxl::read_xls('RProj3_data/NAVCO2-1_ForPublication.xls')

# trim the orginal dataset to contain only non-violent campaigns
NVCO_temp <- NAVCO[NAVCO$prim_meth==1, ]

# keep the variables we intend to use in our final dataset
NVCO_num <- data.frame(id_num = NVCO_temp$id,
                       id_name = NVCO_temp$camp_name,
                       country = NVCO_temp$location,
                       year = NVCO_temp$year,
                       status_year = NVCO_temp$cyear,
                       campaign_size = NVCO_temp$camp_size_cat,
                       media = NVCO_temp$media_outreach,
                       repression = NVCO_temp$repression,
                       fatalities = NVCO_temp$fatalities_range,
                       international_support = NVCO_temp$regime_support,
                       success = NVCO_temp$success
)

# clean data to turn NA values from '-99' to 'NA'
NVCO_num[,-c(2,3)] <-  replace_with_na_all(NVCO_num[,-c(2,3)], condition=~.x==-99)

# create a copy of the data set with variables correctly described as factors
NVCO_fac <- data.frame(id_num = NVCO_num$id_num,
                       id_name = NVCO_num$id_name,
                       country = NVCO_num$country,
                       year = NVCO_num$year,
                       status_year = as.factor(NVCO_num$status_year),
                       campaign_size = as.factor(NVCO_num$campaign_size),
                       media = as.factor(NVCO_num$media),
                       repression = as.factor(NVCO_num$repression),
                       fatalities = as.factor(NVCO_num$fatalities),
                       international_support = as.factor(NVCO_num$international_support),
                       success = as.factor(NVCO_num$success)
)

### UI ###

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title="Nonviolent Campaigns and Outcomes", titleWidth=450),
    
    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        width=275,
        # create the tabs
        sidebarMenu(
            menuItem("Information", tabName='information', icon = icon("info-circle")),
            menuItem("Data Exploration (Boxplot/Histogram)", tabName='explor_box', icon = icon("table")),
            menuItem("Data Exploration (Scatterplot)", tabName='explor_scatter', icon = icon("table")),
            menuItem("Principal Component Analysis", tabName='pca', icon = icon("project-diagram")),
            menuItem("K-Nearest Neighbors", tabName='KNN', icon = icon("chart-area")),
            menuItem("Random Forest", tabName='forest', icon = icon("chart-area")),
            menuItem("Data Manipulation / Export", tabName='export', icon = icon("save"))
        )
    ),
    
    dashboardBody(
        tabItems(
            # create the information tab
            tabItem(tabName='information',
                    h1(strong('NVCO'), align='center'),
                    h3("Nonviolent Campaigns and Outcomes (NVCO) is a subset of the", 
                    a(href="https://dataverse.harvard.edu/dataverse/navco",
                      'Nonviolent and Violent Campaigns and Outcomes'),
                    "(NAVCO) dataset from Harvard University. The project obtained data
                    on 323 major campaigns from 1900 to 2006. This dataset was subset
                    in order to create models pertaining to nonviolent protest. Of the 
                    original 142 variables, 11 potential variables were selected in the
                    creation of this app."),
                    # describe each of the variables used in these models
                    h4(strong('\U25CB id_num:'), 
                       'A unique numerical ID associated with each campaign.'),
                    h4(strong('\U25CB id_name:'),
                       'The name of the campaign.'),
                    h4(strong('\U25CB country:'),
                       'Which country the campaign took place in.'),
                    h4(strong('\U25CB year:'),
                       'The year the data was collected on. Note that many of these campaigns
                       spanned multiple years.'),
                    h4(strong('\U25CB status_year:'),
                       'A factor variable with 4 levels describing the status of the campaign that
                       year.',
                       br(),
                       em('\U2043 Factor levels:'), 
                       '0 = onset, 1 = ongoing, 2 = end year, 3 = post campaign'),
                    h4(strong('\U25CB campaign_size:'),
                       'A factor variable with 4 levels describing the estimated size of the
                       campaign.',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = small (hundreds to thousands), 1 = medium (tens of thousands),
                       2 = large (above one hundred thousand), 3 = extremely large (above one
                       million), NA = unknown'),
                    h4(strong('\U25CB media:'),
                       'A factor variable with 2 levels describing whether or not the campaign uses
                       resources for traditional media coverage and public relations',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = none, 1 = information campaign, NA = unknown'),
                    h4(strong('\U25CB repression:'),
                       'A factor variable with 4 levels describing the extent of state repression
                       in response to campaign activity',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = none, 1 = mild repression, 2 = moderate repression, 3 = extreme repression,
                       NA = unknown'),
                    h4(strong('\U25CB fatalities:'),
                       'A factor variable with 7 levels describing the range of estimated fatalities
                       among protestors/dissidents.',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = no known fatalities, 1 = 1-10 fatalities, 2 = 11-25 fatalities, 3 = 26-100
                       fatalities, 4 = 101-1,000 fatalities, 5 = 1,001-10,000 fatalities, 6 = 10,001+
                       fatalities, NA = unknown'),
                    h4(strong('\U25CB international_support:'),
                       'A factor variable with 2 levels describing whether the campaign has formal
                       support from other countries',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = no, 1 = yes, NA = unknown'),
                    h4(strong('\U25CB success:'),
                       'A factor variable with two levels and the response variable of interest 
                       describing whether the campaign\'s ultimate goal was achieved as a direct
                       result of the campaign',
                       br(),
                       em('\U2043 Factor levels:'),
                       '0 = not successful, 1 = successful'),
                    br(),
                    h4(strong('Note:'),
                       'the original dataset used \'-99\' to indicate missing values. For our analysis,
                       these are replaced as \'NA\''),
                    h4(strong('Note:'),
                       'KNN was fit utilizing the Euclidean distance, i.e.',
                       withMathJax(helpText('$$\\sqrt{\\sum_{i=1}^{k}(x_{i}-y_{i})^{2}}$$')))
            ),
            # create the exploratory data tab containing the boxplot and histogram
            tabItem(tabName='explor_box', 
                    h1(strong('Data Exploration - Boxplot/Histogram'), align='center'),
                    box(
                        # select which variable to summarize
                        selectInput('histo_var',
                                    'Select variable to summarize.',
                                    choices=c(
                                        'year'=4,
                                        'status_year'=5,
                                        'campaign_size'=6,
                                        'media'=7,
                                        'repression'=8,
                                        'fatalities'=9,
                                        'international_support'=10,
                                        'success'=11
                                    )
                        ),
                        # create buttons to download each of the plots
                        downloadButton('download_histo', 'Save histogram'),
                        downloadButton('download_box', 'Save boxplot'),
                        width=6
                    ),
                    box(
                        # create a second box containing summary statistics
                        verbatimTextOutput('histo_sum'),
                        width=6
                    ),
                    box(
                        # plot the histogram using a plotly output
                        width=7,
                        plotlyOutput('histo')
                    ),
                    box(
                        # plot the boxplot
                        width=5,
                        plotOutput('box')
                    )
            ),
            # create an exploratory tab containing a plotly scatterplot
            tabItem(tabName='explor_scatter', 
                    h1(strong('Data Exploration - Scatterplot'), align='center'),
                    box(
                        # choose which x-variable to look at
                        selectInput('scatter_x',
                                    'Select x variable',
                                    choices=c(
                                        'year'=4,
                                        'status_year'=5,
                                        'campaign_size'=6,
                                        'media'=7,
                                        'repression'=8,
                                        'fatalities'=9,
                                        'international_support'=10,
                                        'success'=11
                                    )
                        ),
                        # choose which y variable to look at
                        selectInput('scatter_y',
                                    'Select y variable',
                                    choices=c(
                                        'year'=4,
                                        'status_year'=5,
                                        'campaign_size'=6,
                                        'media'=7,
                                        'repression'=8,
                                        'fatalities'=9,
                                        'international_support'=10,
                                        'success'=11
                                    )
                        ),
                        # create a button to download the scatterplot
                        downloadButton('download_scatter', 'Save scatterplot'),
                        width=4
                    ),
                    box(
                        # plot the scatterplot using plotly
                        plotlyOutput('scatter'),
                        width=8
                    )
            ),
            # create a principal components analysis tab
            tabItem(tabName='pca', 
                    h1(strong('Principal Component Analysis'), align='center'),
                    box(
                        # choose pca predictors
                        checkboxGroupInput('pca_pred', 
                                           'Select predictor variables',
                                           choices=c(
                                               'year',
                                               'status_year',
                                               'campaign_size',
                                               'media',
                                               'repression',
                                               'fatalities',
                                               'international_support'
                                            )),
                        # create am action button to run the pca analysis
                        actionButton('action_pca', 'Run PCA'),
                        width=2
                    ),
                    box(
                        # display pca summary statistics
                        verbatimTextOutput('pca_sum')
                    ),
                    box(
                        # display a biplot
                        plotOutput('pca_plot')
                    )
            ),
            # create a k-nearest neighbors tab
            tabItem(tabName='KNN', 
                    h1(strong('Model - K-Nearest Neighbors'), align='center'),
                    box(
                        # choose which variables to include for knn
                        checkboxGroupInput('knn_pred', 
                                           'Select predictor variables',
                                           choices=c(
                                               'year',
                                               'status_year',
                                               'campaign_size',
                                               'media',
                                               'repression',
                                               'fatalities',
                                               'international_support'
                                           )),
                        # create an action button to run our analysis
                        actionButton('action_knn', 'Fit model'),
                        width=2
                    ),
                    box(
                        # create a conditional panel to predict values based on the model
                        checkboxInput('prediction_knn', 
                                      strong('Predict values using fitted model? - must fit model first')),
                        conditionalPanel(
                            condition='input.prediction_knn',
                            # create number inputs for prediction
                            h4('Enter values for prediction'),
                            numericInput('year', label='year', value=NA, min=1945, max=2013),
                            numericInput('status_year', label='status_year', value=NA, min=0, max=2),
                            numericInput('campaign_size', label='campaign_size', value=NA, min=0, max=3),
                            numericInput('media', label='media', value=NA, min=0, max=1),
                            numericInput('repression', label='repression', value=NA, min=0, max=3),
                            numericInput('fatalities', label='fatalities', value=NA, min=0, max=5),
                            numericInput('international_support', label='international_support', value=NA, min=0, max=1),
                        ),
                        width=2
                    ),
                    box(
                        # display the knn summary 
                        verbatimTextOutput('knn_sum'),
                        width=8
                    ),
                    box(
                        # display the knn prediction if values are entered
                        verbatimTextOutput('knn_prediction_val')
                    )
            ),
            # create a random forest tab
            tabItem(tabName='forest', 
                    h1(strong('Model - Random Forest'), align='center'),
                    box(
                        # choose predictors for the model
                        checkboxGroupInput('forest_pred',
                                           'Select predictor variables',
                                           choices=c(
                                               'year',
                                               'status_year',
                                               'campaign_size',
                                               'media',
                                               'repression',
                                               'fatalities',
                                               'international_support'
                                           )),
                        # create an action button to fit the model
                        actionButton('action_forest', 'Fit model'),
                        width=2
                    ),
                    box(
                        # give the option to predict values on our model
                        checkboxInput('prediction_forest', 
                                      strong('Predict values using fitted model? - must fit model first')),
                        conditionalPanel(
                            condition='input.prediction_forest',
                            # create number inputs for prediction
                            h4('Enter values for prediction'),
                            numericInput('year_f', label='year', value=NA, min=1945, max=2013),
                            numericInput('status_year_f', label='status_year', value=NA, min=0, max=2),
                            numericInput('campaign_size_f', label='campaign_size', value=NA, min=0, max=3),
                            numericInput('media_f', label='media', value=NA, min=0, max=1),
                            numericInput('repression_f', label='repression', value=NA, min=0, max=3),
                            numericInput('fatalities_f', label='fatalities', value=NA, min=0, max=5),
                            numericInput('international_support_f', label='international_support', value=NA, min=0, max=1),
                        ),
                        width=2
                        ),
                    box(
                        # display the random forest summary
                        verbatimTextOutput('forest_sum'),
                        width=8
                    ),
                    box(
                        # display the predicted results if selected
                        verbatimTextOutput('forest_prediction_val')
                    )
            ),
            # create a subset/export data tab
            tabItem(tabName='export', 
                    h1(strong('Subset / Export Dataset'), align='center'),
                    # create a box for displaying the data
                    box(
                        dataTableOutput('table_subset'),
                        width=9
                    ),
                    # create a box for choosing variables
                    box(
                        checkboxGroupInput('variables', 
                                           'Select variables for output',
                                           choices=colnames(NVCO_fac)),
                        # allow the user to specify a name for the data
                        textInput('download_name', 'Name your dataset to download'),
                        # create a button to download the data
                        downloadButton('download_data', 'Download'),
                        width=3
                    )
            )
        )
    )
)

### SERVER ###

# Define server logic required to draw a histogram
server <- function(input, output){
    # create a download link for the subset data
    output$download_data <- downloadHandler(
        filename=function(){
            paste0(input$download_name, '.csv')
        },
        content=function(file){
            write.csv(select(NVCO_fac, input$variables), file)
        }
    )
    
    # create the subset data table for the data download tab
    output$table_subset <- renderDataTable(select(NVCO_fac, input$variables))
    
    # create a summary table for the data exploration page
    output$histo_sum <- renderPrint({
        summary(NVCO_num[,as.numeric(input$histo_var)])
    })
    
    # create a histogram for the data exploration page
    output$histo <- renderPlotly({
        g <- ggplot(data=NVCO_num, aes(x=NVCO_num[,as.numeric(input$histo_var)],
                                       fill='#CC0000'))
        g1 <- g + geom_histogram() + xlab('') + theme(legend.position='none')
        
        ggplotly(g1)
    })
    
    # create a boxplot for the data exploration page
    output$box <- renderPlot({
        g <- ggplot(data=NVCO_num, aes(x=NVCO_num[,as.numeric(input$histo_var)],
                                       fill='#000000'))
        g2 <- g + geom_boxplot() + xlab('') + theme(legend.position='none')
        
        g2
    })
    
    # save the histogram and boxplots
    output$download_histo <- downloadHandler(
        file=paste0('histogram.png'),
        content=function(file){
            ggsave(file, 
                   plot=ggplot(data=NVCO_num,aes(x=NVCO_num[,as.numeric(input$histo_var)],
                                                 fill='#CC0000')) +
                       geom_histogram() +
                       xlab('') +
                       theme(legend.position='none')
            )
        }
    )
    output$download_box <- downloadHandler(
        file=paste0('boxplot.png'),
        content=function(file){
            ggsave(file, 
                   plot=ggplot(data=NVCO_num,aes(x=NVCO_num[,as.numeric(input$histo_var)],
                                                 fill='#000000')) +
                       geom_boxplot() +
                       xlab('') +
                       theme(legend.position='none')
            )
        }
    )
    
    # create scatterplot
    output$scatter <- renderPlotly({
        g3 <- ggplot(data=NVCO_num, 
                    aes(x=NVCO_num[,as.numeric(input$scatter_x)],
                        y=NVCO_num[,as.numeric(input$scatter_y)]))
        g4 <- g3 + geom_point(position='jitter') + xlab('') + ylab('')
        
        ggplotly(g4)
    })
    
    # create option to download scatterplot
    output$download_scatter <- downloadHandler(
        file=paste0('scatterplot.png'),
        content=function(file){
            ggsave(file, 
                   plot=ggplot(data=NVCO_num, 
                               aes(x=NVCO_num[,as.numeric(input$scatter_x)],
                                   y=NVCO_num[,as.numeric(input$scatter_y)])) +
                       geom_point(position='jitter') +
                       xlab('') +
                       ylab('')
            )
        }
    )
    
    # create an actionable section of code pertaining to KNN analysis
    knn <- eventReactive(input$action_knn, {
        # create a data partition
        NVCO_fac$success <- factor(NVCO_fac$success, labels=c('no','yes'))
        index_train <- createDataPartition(y=NVCO_fac$success, p=0.8, list=FALSE)
        train_dat <- NVCO_fac[index_train,]
        test_dat <- NVCO_fac[-index_train,]
        
        # obtain the formula for a knn fit
        knn_formula <- as.formula(paste('success~',paste(as.character(input$knn_pred), collapse='+')))
        
        # fit the model
        knn_fit <- train(knn_formula,
                         data=train_dat,
                         method='knn',
                         na.action=na.omit,
                         trControl=trainControl(method='repeatedcv', repeats=3),
                         preProcess=c('center','scale'))
        
        knn_fit
    })
    
    # create the output associated with the summary values for knn
    output$knn_sum <- renderPrint(knn())
    
    # create an actionable section of code pertaining to random forest analysis
    random_forest <- eventReactive(input$action_forest, {
        # create a data partition
        index_train <- createDataPartition(y=NVCO_fac$success, p=0.8, list=FALSE)
        train_dat <- NVCO_fac[index_train,]
        test_dat <- NVCO_fac[-index_train,]
        
        # obtain the formula for a knn fit
        forest_formula <- as.formula(paste('success~',paste(as.character(input$forest_pred), collapse='+')))
        
        # fit the model
        forest_fit <- train(forest_formula,
                         data=train_dat,
                         method='rf',
                         metric='Accuracy',
                         na.action=na.omit,
                         trControl=trainControl(method='repeatedcv', number=10, repeats=3),
                         preProcess=c('center','scale'))
        
        forest_fit
    })
    
    # create data.frame from forest prediction responses
    data_forest <- reactive({
        data.frame(year=input$year_f,
                   status_year=factor(input$status_year_f),
                   campaign_size=factor(input$campaign_size_f),
                   media=factor(input$media_f),
                   repression=factor(input$repression_f),
                   fatalities=factor(input$fatalities_f),
                   international_support=factor(input$international_support_f)
        )
    })
    
    # output prediction for forest
    output$forest_prediction_val <- renderPrint({
        if(input$prediction_forest==FALSE){
            print('Prediction not selected')
        } else {
            predict(random_forest(), newdata=data_forest())
        }
    })
    
    # create the output associated with the summary values for random forest
    output$forest_sum <- renderPrint(random_forest())
    
    # create data.frame from knn prediction responses
    data_knn <- reactive({
        data.frame(year=input$year,
                   status_year=factor(input$status_year),
                   campaign_size=factor(input$campaign_size),
                   media=factor(input$media),
                   repression=factor(input$repression),
                   fatalities=factor(input$fatalities),
                   international_support=factor(input$international_support)
                   )
    })
    
    # output prediction for knn
    output$knn_prediction_val <- renderPrint({
        if(input$prediction_knn==FALSE){
            print('Prediction not selected')
        } else {
        predict(knn(), newdata=data_knn())
        }
    })
    
    # run pca analysis
    pca <- eventReactive(input$action_pca, {
        pca_analysis <- prcomp(as.formula(paste0('~',paste(as.character(input$pca_pred), collapse='+'))),
                               data=NVCO_num)
        
        pca_analysis
    })
    
    # output the pca summary
    output$pca_sum <- renderPrint(pca())
    
    # output a biplot for the pca
    output$pca_plot <- renderPlot(biplot(pca()))
}
    
### RUN FILE ###

# Run the application 
shinyApp(ui, server)
