#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(pROC)
library(tidyverse)
library(gridExtra)
library(rpart)
library(rpart.plot)

#Load CSV file
d <- read.csv("marvel_superheroes.csv", header=TRUE, sep=",")
str(d)

#Change "-" to "Genderless" for column "Gender"
levels(d$Gender) <- c(levels(d$Gender), "Genderless") 
d$Gender[d$Gender=='-'] <- 'Genderless'

#Change "-" to NA
column_names <- colnames(d)
for (i in column_names) {
    d[[i]][d[[i]]=='-'] <- NA
}

#Check if column "Alignment" has any NA values
anyNA(d$Alignment)

#Count NA occurences
sum(is.na(d$Alignment))/nrow(d)

#As there are barely any occurences of NA for Alignment column, drop the records that contain it
d <- d[is.na(d$Alignment) == FALSE,]

#Utworzenie dychotomicznej zmiennej celu 'is_bad', przyjmującej 1 dla złych bohaterów, oraz 0 dla innych
d$is_bad <- ifelse(d$Alignment == 'bad',1,0)

#Utworzenie powyzszej zmiennej, ale typu factor
d$is_bad_factor <- factor(d$is_bad)

#Utworzenie wektora supermocy
powers <- c("Intelligence", "Strength", "Speed", "Durability", "Power", "Combat")

#Utworzenie wektora zmiennych do wykresu zależności
variables <- c("Intelligence", "Strength", "Speed", "Durability", "Power", "Combat", "Race","EyeColor","HairColor","SkinColor")

#Rekategoryzacja zmiennej Race, EyeColor, HairColor i SkinColor
Race_old <- levels(d$Race)
Race_new <- c("Other","Alien","Other","Other","Asgardian","Other","Other","Other","Other","Other","Demon","Other","Other","Other","God / Eternal","(partly) Human","(partly) Human", "(partly) Human", "(partly) Human", "(partly) Human", "Inhuman", "Other", "(partly) Mutant", "(partly) Mutant", "Other", "Symbiote", "Other")
d <- d %>%
    mutate(Race=plyr::mapvalues(Race,from=Race_old,to=Race_new))

Eyes_old <- levels(d$EyeColor)
summary(d$EyeColor)
Eyes_new <- c("Other","Other","Black","Blue","Blue","Brown","Other","Green","Other","Brown","Other","Red","Other","Other","White","Other","Yellow","Yellow","Yellow")
d <- d %>%
    mutate(EyeColor=plyr::mapvalues(EyeColor,from=Eyes_old,to=Eyes_new))

Hair_old <- levels(d$HairColor)
Hair_old
summary(d$HairColor)
Hair_new <- c("Other","Brown","Black","Black","Blond","Blond","Other","Brown","Brown","Brown","Other","Other","Other","Other","Other","No Hair","Other","Other","Other","Other","Red","Red","Red","Other","Other","White","Other")
d <- d %>%
    mutate(HairColor=plyr::mapvalues(HairColor,from=Hair_old,to=Hair_new))

#Podział zbioru na uczący i testowy
n <- nrow(d)

i <- floor(0.75*n)

set.seed(1708)

s <- sample.int(n, i, replace = F)

d.train <- d[s,]
d.test <- d[-s,]

#Tworzenie modelu - ponieważ zbiór ma dość dużo braków danych, wybrałam drzewa decyzyjne
m <- rpart(data = d.train, is_bad ~ Gender + Power + Combat + Race)

predict.m <- predict(m,newdata = d.test, type='vector')

#Tworzenie krzywej ROC
roc.m <- roc(d.test$is_bad, predict.m)


# Warstwa UI programu Shiny
ui <- fluidPage(

    # Tytuł aplikacji
    titlePanel("Analiza superbohaterów z uniwersum Marvela"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            h3("Analiza zmiennych"),
            selectInput(inputId = "hist",
                        label = "Wykreśl histogram dla zmiennej:",
                        choices = powers),
            
            selectInput(inputId = "x",
                        label = "Zbadaj zależność między zmienną X:",
                        choices = variables),
            
            selectInput(inputId = "y",
                        label = "a zmienną Y:",
                        choices = variables),
            
            checkboxInput('plec', 'Grupuj wg. płci'),
            
            h3("Model drzewa decyzyjnego"),
            p("Ze względu na dużą ilość braków danych wybrano model drzewa decyzyjnego. Model ma postać: is_bad = Gender + Power + Combat + Race"),
            selectInput(inputId = "model",
                        label = "Wybierz wykres do wygenerowania:",
                        choices = c("wizualizacja drzewa", "krzywa ROC"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotgraph"),
           plotOutput("model")
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    plot1 <- reactive({
        #rysuje histogram wybranej zmiennej
        zmienna = d[[input$hist]]
        ggplot(d, aes(x=zmienna)) + 
            geom_histogram(aes(y=..density..), color="black",fill="white", binwidth=10, na.rm=TRUE)+
            geom_density(color="#bf1515") +
            ggtitle(paste("Histogram dla zmiennej", input$hist, sep=" "))+
            ylab("Gęstość")+
            xlab(input$hist)
            })
    
    plot2 <- reactive({
        # analizuje relacje miedzy dwoma wybranymi zmiennymi
        zmienna_x = d[[input$x]]
        zmienna_y = d[[input$y]]
        if (input$plec) {
        ggplot(data=d, aes(x = zmienna_x, y = zmienna_y)) +
            geom_point(position='jitter') +
            geom_smooth(method="lm") +
            facet_wrap(~ Gender, nrow = 1) +
            xlab(input$x) +
            ylab(input$y) +
            ggtitle(paste("Zależność między zmienną X =", input$x, "a zmienną Y", input$zmienna_y, "z podziałem na płeć", sep=" "))
        } else {
            ggplot(data=d, mapping = aes(x = zmienna_x, y = zmienna_y)) +
                geom_point(position='jitter') +
                geom_smooth(method="lm") +
                xlab(input$x) +
                ylab(input$y) +
                ggtitle(paste("Zależość między zmienną X =", input$zmienna_x, "a zmienną Y", input$zmienna_y, sep=" "))
        }
    })
    
   
    output$model <- renderPlot( {
        if(input$model == "wizualizacja drzewa") {
        #Model drzew decyzyjnych - wizualizacja drzewa
        prp(m, main="Wizualizacja drzewa decyzyjnego")
        }
        
        #krzywa ROC
        if(input$model == "krzywa ROC") {
            plot(roc.m, main="Krzywa ROC")
        }
        
    })

    output$plotgraph = renderPlot({
        plots <- list(plot1(),plot2())
        #grid.arrange(grobs=plots)
        grid.arrange(grobs=plots)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
