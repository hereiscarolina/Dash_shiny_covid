#Código final


rm(list=ls())
# Carregar dados ----------------------------------------------------------
load("dados.Rdata")

# dados auxiliares --------------------------------------------------------
minimo <- 100
options(scipen=999)
populacao <- 7.6*10^9

# Cores pro dash ----------------------------------------------------------
cor_confirmados <- "purple"
cor_mortos <- "red"
cor_recuperados <- "olive"


# Bibliotecas -------------------------------------------------------------

packages <-  c("tidyverse",
               "lubridate",
               "ggplot2",
               "shiny",
               "shinydashboard",
               "shinythemes",
               "leaflet",
               "gghighlight",
               "viridis",
               "readr")
if (length(setdiff(packages,rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages,rownames(installed.packages())))
}

lapply(packages,require,character.only = TRUE)
rm(packages)


# Carregar pacotes --------------------------------------------------------
library(rgdal)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(viridis)
library(readr)

# Funcções ----------------------------------------------------------------


# Download dos dados do git -----------------------------------------------

download.file(url = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
              , destfile = "Labs/dados_covid/time_series_covid19_confirmed_global.csv")


download.file(url = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
              , destfile = "Labs/dados_covid/time_series_covid19_recovered_global.csv")

download.file(url = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
              , destfile = "Labs/dados_covid/time_series_covid19_deaths_global.csv")



infectados_ <- read.csv("Labs/dados_covid/time_series_covid19_confirmed_global.csv",header = TRUE,stringsAsFactors=FALSE)
recuperados_ <- read.csv("Labs/dados_covid/time_series_covid19_recovered_global.csv",header = TRUE,stringsAsFactors=FALSE)
mortos_ <- read.csv("Labs/dados_covid/time_series_covid19_deaths_global.csv",header = TRUE,stringsAsFactors=FALSE)


 



infectados_1 <- infectados_ %>% 
  mutate(tipo = "Confirmados")

recuperados_1 <- recuperados_ %>% 
  mutate(tipo = "Recuperados")

mortos_1 <- mortos_ %>% 
  mutate(tipo = "Mortos")

casos_empilhados <-  plyr::rbind.fill(infectados_1,recuperados_1,mortos_1)




# Funções pra tratar os dados ---------------------------------------------


#Transforma em longo

transformar_longo_new <- function(dados, tipo){ #usado na obtencao de dados
  dados %>%
    pivot_longer(cols = -c(`Province.State`, `Country.Region`, Lat, Long,tipo),
                 names_to = "datas",
                 values_to = "count") %>%
    separate(datas,c("x","data_unica"), sep= "X") %>% 
    separate(data_unica,c("mes","dia","ano")) %>% 
    mutate(date = as.Date(ISOdate(as.numeric(ano)*101,mes,dia))) %>% 
    group_by(`Country.Region`, date,tipo) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    rename(country = `Country.Region`)
}

transformar_longo_ok <- function(dados, tipo){ #usado na obtencao de dados
  dados %>%
    pivot_longer(cols = -c(`Province.State`, `Country.Region`, Lat, Long),
                 names_to = "datas",
                 values_to = "count")  %>%
    separate(datas,c("x","data_unica"), sep= "X") %>% 
    separate(data_unica,c("mes","dia","ano")) %>% 
    mutate(date = as.Date(ISOdate(as.numeric(ano)*101,mes,dia))) %>% 
    group_by(`Country.Region`, date)%>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    mutate(
           tipo = tipo) %>% 
    rename(country = `Country.Region`) 
}


transformar_longo <- function(dados, tipo){ #usado na obtencao de dados
  dados %>%
    pivot_longer(cols = -c(`Province/State`, `Country/Region`, Lat, Long),
                 names_to = "date",
                 values_to = "count") %>%
    group_by(`Country/Region`, date) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    mutate(date = mdy(date),
           tipo = tipo) %>%
    rename(country = `Country/Region`)
}

obter_diff <- function(pais, tipo_){ #usado na aba pais - ele calcula o crescimento de casos dia a dia - o 0 é pra juntar na data inicial
  c(0, 
    covid_longo %>% 
      filter(country == pais, tipo==tipo_) %>%
      arrange(date) %>% 
      select(count) %>% 
      pull() %>% 
      diff())
}

obter_data_diff <- function(pais, tipo_){ #usado na aba pais
  covid_longo %>% 
    filter(country == pais, tipo==tipo_) %>% 
    arrange(date) %>%
    mutate(diff = obter_diff(pais, tipo_)) %>% 
    select(date, diff)
}


obter_numero_casos <- function(caso, pais){ #usado na aba pais
  covid_longo %>% 
    filter(tipo == caso,  #só quero o tipo que eu selecionar
           country == pais, #count no país
           date == max(covid_longo$date)) %>% #quero o último caso 
    select(count) %>%
    pull() %>% 
    format(big.mark = ".", digits = 0)
}

obter_total_mais_recente <- function(tipo_){ #usado na aba panorama
  covid_longo %>%
    filter(tipo == tipo_, date==ifelse(tipo_ != "Recuperados", max(covid_longo$date), max(covid_longo$date)-1)) %>% 
    group_by(date) %>% 
    summarise(max = sum(count, na.rm = TRUE)) %>% 
    select(max) %>% 
    pull()
}

arredonda <- function(x) {10^(ceiling(log10(x)))} #usado na aba panorama


# Transformação dos dados -------------------------------------------------
covid_longo1 <-  transformar_longo_new(casos_empilhados) 
covid_longo <-   plyr::rbind.fill(
                 transformar_longo_ok(infectados_,"Confirmados"),
                 transformar_longo_ok(mortos_,"Mortos"),
                 transformar_longo_ok(recuperados_,"Recuperados")
)
  
covid_longo %>% filter(country %in% "US")

# lista de paises com mais do que _minimo_ de casos confirmados
paises_mais_confirmados <- 
  covid_longo %>% 
  filter(tipo == "Confirmados",
         count >= minimo,
         country != "Todos") %>% 
  group_by(country) %>% 
  summarize(n = n()) %>% 
  arrange(country) %>%
  top_n(30) %>% 
  select(country) %>% 
  pull()

# Cabeçalho ---------------------------------------------------------------

header <- dashboardHeader(title = "Coronavaírus")


# Barra lateral -----------------------------------------------------------
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Panorama", icon=icon("globe"), tabName = "panorama"),
    menuItem("Análise por país", icon=icon("flag"), 
             menuSubItem(selectInput("pais_select", label = "Selecione o país",
                                     choices = unique(covid_longo$country)),
                         tabName = "pais", newtab = FALSE, icon = ""),
             menuSubItem(checkboxGroupInput("variaveis_select", 
                                            label = "Selecione as variáveis",
                                            choices = unique(covid_longo$tipo),
                                            inline = FALSE,
                                            selected = unique(covid_longo$tipo)),
                         newtab = FALSE, icon = ""),
             menuSubItem(numericInput("max_y", "Limite eixo y:", 0, 
                                      min = 1, max = max(covid_longo$count)),
                         tabName = "pais", newtab = FALSE, icon = ""), #Pra não ter uma aba nova pra cada subitem que eu selecionar
             menuSubItem(dateInput("data_inicio", "Mostrar dados a partir de", 
                                   value = min(covid_longo$date), 
                                   min = min(covid_longo$date), 
                                   max = max(covid_longo$date),
                                   format = "dd-mm-yyyy", 
                                   startview = "month", weekstart = 0,
                                   language = "pt", autoclose = TRUE,
                                   datesdisabled = NULL, daysofweekdisabled = NULL), 
                         tabName = "pais", newtab = FALSE, icon = "")
    ),
    menuItem("Comparativo", icon=icon("chart-line"), tabName = "comparativo"),
    menuItem("Brasil", icon=icon("home"), tabName = "brasil"),
    #Tag de baixo
    tags$footer(
      tags$p(
        tags$i(
          paste0("Atualizado em ", format(max(covid_longo$date),"%d/%m"))),
        align = "center"))
  )
)



# Corpo -------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    #Aba 1: panorama
    tabItem(tabName = "panorama",
            fluidRow(width = 12, style = "height:430px",
                     leafletOutput("map", height = "100%")),
            fluidRow(width=12,
                     valueBox(width = 4,
                              value = format(obter_total_mais_recente("Confirmados"), big.mark = "."),
                              subtitle = "Confirmados",
                              icon = icon("microscope"),
                              color = cor_confirmados),
                     valueBox(width = 4,
                              value = format(obter_total_mais_recente("Recuperados"), big.mark = "."),
                              subtitle = "Recuperados",
                              icon = icon("check"),
                              color = cor_recuperados),
                     valueBox(width = 4,
                              value = format(obter_total_mais_recente("Mortos"), big.mark = "."),
                              subtitle = "Mortos",
                              icon = icon("dizzy"),
                              color = cor_mortos)
            )
    ),
    #Aba 2: pais
    tabItem(tabName = "pais",
            fluidRow(width = 12,
                     strong(h1(textOutput("titulo_grafico"), align = "center"))), #strong = vem do html pra dixar negrito. H1 deixa um título bem grande. Pós aqui eu defino no server.
            fluidRow(column(width = 8, style ="height: 500px;background: white",
                            plotOutput(outputId = "plot_line",
                                       height = "100%")),
                     column(width = 4,
                            valueBox(width = 6,
                                     value = textOutput("confirmados_pais"),
                                     subtitle = "Confirmados",
                                     icon = icon("microscope"),
                                     color = cor_confirmados),
                            valueBox(width = 6,
                                     value = textOutput("mortos_pais"), #agora preciso ir no server e dar um nome pra  essas variáveis
                                     subtitle = "Mortos",
                                     icon = icon("dizzy"),
                                     color = cor_mortos),
                            column(width = 12, style ="height: 400px; background:white",
                                   plotOutput(outputId = "plot_diario")))), #vai ter um output diario, com esse nome. Vou definir ele lá no servidor.
            fluidRow(width = 12,
                     HTML("<b>Fontes:</b>
                   Casos COVID-19: <a href='https://github.com/CSSEGISandData/COVID-19'>Johns Hopkins University</a>,
                   Mapa: <a href='https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/'>Natural Earth Data</a>")
            )
    ),
    # Aba 3: comparativo
    tabItem(tabName = "comparativo",
            fluidRow(width = 12,
                     strong(h1(textOutput("titulo_comparativo"), align = "center"))),
            fluidRow(width = 12,
                     checkboxGroupInput("paises_select", 
                                        label = "Selecione os países",
                                        choices = paises_mais_confirmados,
                                        inline = TRUE,
                                        selected = paises_mais_confirmados[1:3]
                                        # width = NULL, choiceNames = NULL,
                                        # choiceValues = NULL
                     )),
            fluidRow(width = 12, style ="height: 400px;background: white",
                     plotOutput(outputId = "plot_comparativo"))
    ),
    # Aba 4: brasil
    tabItem(tabName = "brasil",
            fluidRow(width = 12,
                     strong(h1(textOutput("titulo_brasil"), align = "center"))),
            fluidRow(width = 12,
                     checkboxGroupInput("estados_select", 
                                        label = "Selecione os estados",
                                        choices = unique(brasil$estado),
                                        inline = TRUE,
                                        selected = "SP"
                                        # width = NULL, choiceNames = NULL,
                                        # choiceValues = NULL
                     )),
            fluidRow(width = 12, style ="height: 400px;background: white",
                     plotOutput(outputId = "plot_estados"))
    )
  ))

ui <- dashboardPage(header, sidebar, body,
                    skin = "black")


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  

# Reatividade da aba 1 - Mapa ---------------------------------------------

  output$map <- renderLeaflet({
    max_total <- max(covid_longo$count)
    
    pal <- colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), 
                        domain = c(0,log(arredonda(max_total))))
    
    countries2 <- merge(countries, 
                        covid_longo %>% filter(tipo=="Confirmados", date == max(covid_longo$date)), 
                        by.x = "NAME", 
                        by.y = "country",                    
                        sort = FALSE)
    
    country_popup <- paste0("<strong>País: </strong>",
                            countries2$NAME,
                            "<br><strong>Total de casos confirmados: <strong>",
                            round(countries2$count,2))
    
    leaflet(data = countries2) %>%
      setView(0, 30, zoom = 2) %>% 
      addPolygons(fillColor = pal(log(countries2$count+1)),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = country_popup) %>% 
      addLegend(position = "bottomright",
                pal = pal, opacity = 1,
                bins = log(10^(0:log10(arredonda(max_total)))),
                value = log(1:10^(log10(arredonda(max_total)))),
                data = log(10^(0:log10(arredonda(max_total)))),
                labFormat = labelFormat(transform =  exp ))
  })
  
  # Reatividade aba 2  - País -------------------------------------------------------
  
  pais_selecionado <- reactive(input$pais_select) # O input lá da barralaetrarl vai ser o país select, e quais são as opções? O unique (pra pegar sem dupli) nos nomes dos países
  data_selecionada <- reactive(input$data_inicio)
  tipos_selecionados <- reactive(input$variaveis_select)
  limites_selecionados <- reactive(input$limites_y)
  max_y <- reactive(input$max_y) 
  
  output$plot_line = renderPlot({
    covid_longo %>% 
      filter(country == pais_selecionado(),
             date >= data_selecionada(),
             tipo %in% tipos_selecionados()) %>% 
      ggplot()+
      geom_line(aes(x = date, y = count, 
                    col = tipo),
                key_glyph = "timeseries",
                size=1)+
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,ifelse(max_y()==0, NA, max_y())))+
      scale_x_date(date_breaks = "2 days",
                   date_labels = "%d/%m")+
      labs(x = "", y="Número de casos", col = "")+
      scale_color_manual(values=c("gray", "red", "green"))+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45),
            text = element_text(size=rel(5)),
            legend.text=element_text(size=rel(5)),
            legend.position="bottom")
  })
  
  output$confirmados_pais <- renderText(obter_numero_casos("Confirmados", 
                                                           pais_selecionado())) #Para os confirmados, eu passo os confirmados e qual país eu selecionei
  
  output$mortos_pais <- renderText(obter_numero_casos("Mortos", 
                                                      pais_selecionado()))
  
  output$recuperados_pais <- renderText(obter_numero_casos("Recuperados", 
                                                      pais_selecionado())) #isso aqui que vai preencher as caixinhas
  
  output$titulo_grafico <- renderText(ifelse(pais_selecionado() == "",
                                             "Selecione um país para gerar o gráfico",
                                             paste0("País selecionado: ", 
                                                    pais_selecionado()))) #Ifelse - se o valor for vazio, o título vai ser "seleione o país para gerar o gráfico", se não for vazio ele coloca "país selecionadoe o nome do país selecionado
  #O que eu defini de titulo gráfico aqui ele tem que puxar lá em cima no body.
  
  output$plot_diario <- renderPlot({
    obter_data_diff(pais_selecionado(), "Confirmados") %>% #ele chama a função obter data diff 
      filter(date >= data_selecionada()) %>% 
      ggplot() + 
      geom_col(aes(x = date, y = diff)) +
      labs(title = "Novos casos diários", x = "", y = "") + 
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(date_breaks = "5 days",
                   date_labels = "%d/%m")+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45),
            text = element_text(size=rel(4)),
            plot.title = element_text(size=rel(5)),
            legend.text=element_text(size=rel(4)))
  })
  
 # Reatividade da aba comparativos -----------------------------------------
    
    paises_selecionados <- reactive(input$paises_select)
  
  output$titulo_comparativo <- renderText(
    ifelse(length(paises_selecionados()) == 0,
           "Selecione os países para comparar o crescimento dos casos confirmados",
           "Crescimento de casos a partir do centésimo"))
  
  output$plot_comparativo <- renderPlot({
    data_minima <- covid_longo %>% #ata que atingiu o minimo de casos
      group_by(country) %>%
      filter(tipo == "Confirmados", count >= minimo) %>%
      summarise(minimo = min(covid_longo$date))
    
    covid_longo %>% 
      filter(tipo=="Confirmados", count >= 100) %>% 
      left_join(data_minima, by = "country") %>%
      mutate(
        rank = as.numeric(date - minimo)
      ) %>%
      filter(rank >= 0) %>%
      group_by(country) %>%
      filter (n() >= 7) %>% 
      ungroup() %>% 
      filter(country %in% paises_selecionados()) %>% 
      ggplot(aes(x = rank, color = country, y = count)) +
      geom_line() +
      labs(x = "Dias a partir da confirmação do centésimo caso", 
           y="Casos confirmados (escala log)", col = "")+
      scale_y_log10()+
      scale_colour_discrete() +
      theme_minimal()+
      theme(text = element_text(size=rel(5)))+
      gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1))
  })
  

# Reatividade da aba Brasil -----------------------------------------------

  estados_selecionados <- reactive(input$estados_select)
  
  output$titulo_brasil <- renderText(
    ifelse(length(estados_selecionados()) == 0,
           "Selecione os estados para comparar o crescimento dos casos confirmados",
           "Crescimento de casos nos estados"))
  
  output$plot_estados <- renderPlot({
    brasil %>% 
      filter(estado %in% estados_selecionados()) %>% 
      ggplot(aes(x = data, y = count, color = estado)) +
      geom_line() +
      labs(x = "", 
           y="Casos confirmados", col = "")+
      scale_x_date(date_breaks = "2 days",
                   date_labels = "%d/%m")+
      theme_minimal()+
      theme(text = element_text(size=rel(5)))+
      gghighlight(TRUE,  label_key = estado, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1))
  })  
  
  
}

#atualizar os dados do Brasil

# shiny app ---------------------------------------------------------------
shinyApp(ui, server)

