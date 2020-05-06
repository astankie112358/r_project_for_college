library(dplyr)
library(ggplot2)
library(shiny)
library(stringr)
danepogoda<-read.csv(file="D:/R projekt/danepogoda.csv",header = TRUE, sep = ",")

pom<-danepogoda %>% distinct(Nazwa_stacji)
miesiace<-danepogoda %>% distinct(Miesiac)
miesiace$nazwa<-rbind("Styczeń","Luty","Marzec","Kwiecień","Maj","Czerwiec","Lipiec","Sierpień","Wrzesień","Październik","Listopad","Grudzień")
ui <- fluidPage(
    titlePanel("Pogoda w Polsce"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Nazwa_stacji",
                        "Nazwa stacji:",pom,multiple = TRUE),
            selectInput("wartosc","Co chcesz sprawdzic",colnames(danepogoda[5:17])),
            selectInput("typ","Typ wykresu",c("Slupkowy"="slup","Pudelkowy"="box"))
        ),
        
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
    
)



server <- function(input, output,session) {
    observe({
        
        x<-input$Nazwa_stacji
        a=length(x)
        y<-as.numeric(input$range)
        z<-input$wartosc

        if(is.null(x)==FALSE){
            dane<-filter(danepogoda,Nazwa_stacji %in% x)
            pom<-input$ad
            if(input$typ=="slup"){
                plot1<-ggplot(dane,aes(x=Rok,y=dane[,z]))+geom_bar(stat = "summary", fun.y = "mean",aes(fill=Nazwa_stacji),position = "dodge")+ggtitle(paste(str_replace_all(z,"_"," "),"w miejscowości",x,sep=" "))
            }
            if(a==1){
            if(input$typ=="box"){
               plot1<-ggplot(dane,aes(x=Rok,y=dane[,z]))+geom_boxplot(aes(group=Rok,colour=Nazwa_stacji))+ggtitle(paste(str_replace_all(z,"_"," "),"w miejscowości",x,sep=" "))
            }
            }
            else
            {
                plot1<-ggplot(dane,aes(x=Rok,y=dane[,z]))+geom_bar(stat = "summary", fun.y = "mean",aes(fill=Nazwa_stacji),position = "dodge")+ggtitle(paste(str_replace_all(z,"_"," "),"w porównaniu miast",sep=" "))
                
            }
            
            output$distPlot <- renderPlot({
               plot1+scale_y_continuous("Średnia roczna")
                
            })
        }
    })
}

shinyApp(ui = ui, server = server)

