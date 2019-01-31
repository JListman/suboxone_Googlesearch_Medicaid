#           
# This is a Shiny web application. 
## load necessary packages
library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(scales)
library(plyr)

## get data
gathered.opioidforshiny<-read.csv("gathered.opioidforshiny.csv", header=TRUE)
medianSuboxoneShoppingOverAdjODRatePerK<-median(gathered.opioidforshiny$SuboxoneShoppingOverAdjODRatePerK)
meanSuboxoneShoppintOverAsjODRatePerK<-mean(gathered.opioidforshiny$SuboxoneShoppingOverAdjODRatePerK)

# Define UI for application that draws a lollipop plot w subplot barplot
ui <- fluidPage(
        
        # Application title
        titlePanel("State Medicaid Policy & Google Searches for Opiate Addiction Treatment in 2015"),
        
        # Sidebar with a button input for factor contributing to suboxone search rate
        sidebarLayout(
                sidebarPanel(
                        
                        ## radio button options
                        radioButtons("factorInput", "State Action or Policy",
                                     choiceNames = c("Medicaid Expansion",
                                                     "Methadone On List of Medicaid-covered Medications", 
                                                     "Percentage of Suboxone Prescriptions Paid by Medicaid or Medicare",
                                                     "No Lifetime Limit on Medicaid Suboxone Prescriptions",
                                                     "Potential Suboxone Patients Per 1000 State Residents"
                                     ),
                                     choiceValues = c("MedicaidExpansion",
                                                      "Methadone.On.List", 
                                                      "MedicaidPlusCareLevels",
                                                      "Suboxone.Lifetime.Limits",
                                                      "PotentialSuboxPatientsPerKLevels"
                                                      
                                     )
                        ),
                        ## helptext displayed for additional information corresponding to radio button options
                        uiOutput('helptext'
                        )
                ),
                # lollipop plot and bar plot from opioid data using ggplot2()
                mainPanel(
                        plotOutput("lolliplot", height = "600px")
                )
        )
)

# Define server logic required to draw lollipop plot
server <- function(input, output) {
        output$helptext <- renderUI({
                switch(input$factorInput, ## use HTML-like commands (p,strong, span, and style) to control font color, etc.
                       PotentialSuboxPatientsPerKLevels = helpText(p('Number per thousand state residents 
                                                                     who could possibly receive a Suboxone prescription 
                                                                     from a certified MD was'), 
                                                                     h3(strong(span('ABOVE', style= "color: #8dd3c7;")) , 
                                                                    'or', 
                                                                     strong(span('BELOW', style="color:#fb8072;"))), 
                                                                     'the median.'),
                       MedicaidPlusCareLevels = helpText(p('Percentage of Suboxone prescriptions 
                                                           paid by State Medicaid or Medicare, 
                                                           versus out of pocket or private insurance, was'), 
                                                           h3(strong(span('ABOVE', style= "color: #8dd3c7;")) , 
                                                           'or',
                                                           strong(span('BELOW', style="color:#fb8072;"))), 
                                                           'the median.'),
                       MedicaidExpansion = helpText(p('Did state participate in Medicaid expansion:'), 
                                                      h3(strong(span('YES', style= "color: #8dd3c7;")), 
                                                      'or', 
                                                      strong(span('NO',style="color:#fb8072;")))),
                       Methadone.On.List = helpText(p('Did state Medicaid cover Methadone treatment 
                                                      for opioid abuse or addiction:'), 
                                                      h3(strong(span('YES', style= "color: #8dd3c7;")), 
                                                      'or', 
                                                      strong(span('NO', style="color:#fb8072;")))),
                       Suboxone.Lifetime.Limits = helpText(p('Did state Medicaid cover
                                                             Suboxone, as long as needed:'), h3(strong(span('YES', style= "color: #8dd3c7;")), 
                                                             'or', strong(span('NO', style="color:#fb8072;"))))
                )
        })
        ## lolipop plot with sub-barplot
        output$lolliplot <- renderPlot({
                ## use dplyr::filter to plot only the factor chosen with radio button
                filtered<-
                        gathered.opioidforshiny %>%
                        filter(Factor==input$factorInput) %>%
                        arrange(desc(SuboxoneShoppingOverAdjODRatePerK))
                
                filtered2<-
                        gathered.opioidforshiny %>%
                        filter(Factor==input$factorInput)
                filtered3<-filtered2 %>%
                        filter(SuboxoneShoppingOverAdjODRatePerK>mean(SuboxoneShoppingOverAdjODRatePerK))
                filtered4<-filtered2 %>%
                        filter(SuboxoneShoppingOverAdjODRatePerK<=mean(SuboxoneShoppingOverAdjODRatePerK))
                plot2data<-as.data.frame(table(filtered3$Effect))
                plot3data<-as.data.frame(table(filtered4$Effect))
                plot2data<-mutate(plot2data,percents= round((Freq/sum(Freq)* 100), digits=0) )
                plot3data<-mutate(plot3data,percents= round((Freq/sum(Freq)* 100), digits=0))
                  plot2data$status<-as.factor(c("Below Average", "Below Average"))
                plot3data$status<-as.factor(c("Above Average", "Above Average"))
                plottotal<-rbind(plot3data, plot2data)
                
                ggplot(filtered, aes(x=State,y=SuboxoneShoppingOverAdjODRatePerK))+
                        geom_segment(aes(x=reorder(State, -SuboxoneShoppingOverAdjODRatePerK),
                                         y=mean(SuboxoneShoppingOverAdjODRatePerK), 
                                         xend=reorder(State,-SuboxoneShoppingOverAdjODRatePerK), 
                                         yend=SuboxoneShoppingOverAdjODRatePerK), color="gray")+
                        geom_point(stat="identity",aes(col=Effect), size=3)+
                        scale_color_discrete(labels=c("Negative", "Positive"))+
                    
                        labs(x="", y="Suboxone Shopping vs. OD Deaths \n  \n \n(Ratio of State Rank in Google Shopping for Suboxone to \nAge-Adjusted Drug Overdose Deaths Per 100 Thousand)")+
                        ## use ifelse to match y-axis label color to data point color
                        theme(panel.grid.minor.x = element_blank())+
                        theme(panel.grid.major.y = element_blank())+
                        theme(axis.text.y=element_text(size=12, colour = ifelse(filtered$Effect == "Bad", "#fb8072", "#8dd3c7"), face="bold"))+
                        theme(axis.title.x = element_text(size=12, face="bold"))+
                        theme(legend.position = 'none')+
                        ## use annotation_custom to insert separate ggplot inside main plot, but still be reactive
                        annotation_custom(grob=ggplotGrob(ggplot(data=plottotal,aes(x = status, y=percents, fill=Var1))+
                                                                  geom_bar(stat="identity",width=.45)+
                                                                  geom_text(aes(x = status, y=percents, label = paste0(percents,"%")),
                                                                            position = position_stack(vjust=.5),size = 4)+
                                                                  theme(axis.title.y=element_blank())+
                                                                  theme(axis.title.x=element_text(face="bold",size=12))+
                                                                  theme(legend.position = 'none')+
                                                                  theme(axis.text.y = element_blank())+
                                                                  theme(axis.text.x = element_text(face="bold", size=12))+
                                                                  scale_x_discrete(name="Suboxone Searches Were",labels=c("LESS \nThan Expected", 
                                                                                            "MORE \nThan Expected"), position="top" )+
                                                                  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank())+
                                                                  theme(axis.ticks = element_blank())+
                                                                  theme(plot.background=element_rect(fill = "transparent",colour = NA))
                                                                                                   ),
                                          xmin=35, xmax=50, ymin=2.15, ymax=3.9)+
                                          geom_segment(color="gray",alpha=0.1,aes(x = 36, y = 3.25, xend = 20.5, yend = 1.7))+
                                          geom_segment(color="gray",alpha=0.1,aes(x = 36, y = 3.57, xend = 1, yend = 3.87))+
                                          geom_segment(color="gray",alpha=0.1,aes(x = 45.2, y = 2.49, xend = 51, yend = 1.62))+
                                          geom_segment(color="gray",alpha=0.1,aes(x = 36, y = 2.49, xend = 21.3, yend = 1.62))+
                        coord_flip()
        }, height = 600, units="px")
        }
# Run the application 
shinyApp(ui = ui, server = server)