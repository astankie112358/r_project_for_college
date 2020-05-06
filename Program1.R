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
            selectInput("Miesiac","Miesiąc:",miesiace$nazwa),
            selectInput("wartosc","Co chcesz sprawdzić",colnames(danepogoda[5:17])),
            sliderInput("range",label = "Czas:", min = 1951, max = 2018, value = c(1951, 2018)),
            selectInput("typ","Typ wykresu",c("Liniowy"="lin","Punktowy"="point","Punktowy z linią regresji"="reg"))
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
        y<-input$Miesiac
        y<-match(y,miesiace$nazwa)
        z<-input$wartosc
        
        if(is.null(x)==FALSE){
        dane<-filter(danepogoda,Nazwa_stacji %in% x & Miesiac %in% y & Rok>input$range[1]-1 & Rok<input$range[2]+1 )
        pom<-input$ad
        plot1<-ggplot(dane,aes(x=Rok,y=dane[,z]))
        if(input$typ=="lin"){
            plot1<-plot1+geom_line(aes(color=Nazwa_stacji,size='qsec'))+
                geom_point()}
        if(input$typ=="point"){
            plot1<-plot1+geom_point(aes(shape=Nazwa_stacji))
        }
        if(input$typ=="reg")
        {
            plot1<-plot1+geom_smooth(aes(color=Nazwa_stacji),method="lm")+geom_point(aes(color=Nazwa_stacji,size='qsec'))
        }
        plot1<-plot1+scale_y_continuous(str_replace_all(z,"_"," "))+scale_x_continuous("Rok")
        if(a=="1")
        {
            plot1<-plot1+ggtitle(paste(str_replace_all(z,"_"," "),"w miejscowości",x,"w miesiącu",input$Miesiac,sep=" "))
        }
        else{
            plot1<-plot1+ggtitle(paste(str_replace_all(z,"_"," "),"w porównaniu miast","w miesiącu",input$Miesiac,sep=" "))
        }
        output$distPlot <- renderPlot({
           plot1 
            
    })
        }
    })
}

shinyApp(ui = ui, server = server)
