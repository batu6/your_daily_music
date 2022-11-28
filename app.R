# V 2.1

library(extrafont)
library(shiny)
library(googlesheets4)
library(tidyverse)
library(shinythemes)
library(htmltools)

dir.create('~/.fonts')
file.copy("www/xkcd.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

## googlesheets4 related necessary part ----------
m <- read.delim("mail.txt")

options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = m)

gs4_auth()

# Old google sheets
#options(gargle_oauth_cache = ".secrets" )
#    
#   # check the value of the option, if you like
#   gargle::gargle_oauth_cache()
#    
#     # trigger auth on purpose to store a token in the specified cache
#     #googlesheets4::gs4_auth()
#
#       # sheets reauth with specified token and email address
#       gs4_auth(
#         cache = ".secrets",
#         email = ""
#       )
       
       # Find the used google docs file-------------
       Data <- gs4_find("Data")

       url <- "https://twitter.com/intent/tweet?text=Check%20this%20out&url=https://batu6.shinyapps.io/app_music/"
       
       
ui <- navbarPage(theme = shinytheme("superhero"),
  
                 
  # App title -----
  title = "Your Daily Music! v2.1",
  
  tabPanel("Your Daily Music",
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(width = 3,
      

        # Input: Tempo --
        radioButtons("tempo", tags$h4(tags$strong("Choose Tempo")), choices = c("slow", "fast", "all"), selected = "all", inline = T) ,
  
        # Input: Songs with zero listening so far ----
        checkboxInput("rare", tags$em("Undiscovered Songs Only", style = "font-size:15px;" ), value = F),
        
        div(id = "a4",
            tags$em("(Songs with 0 listening history)",style = "color:lightgray;", style = "font-size:12px;")
        ),
        
        br(),
      # Input: Rating of the song ----
      checkboxGroupInput("rate", tags$h4(tags$strong("Rating")), choices = c("Meh", "OK", "AWESOME"), inline = T),
      
        # Submit answers ----
        actionButton("submit", "Customized Search", class="btn btn-success btn-lg"),
      div(id = "a",
        tags$p(tags$em("(Search according to above specifications)",style = "color:lightgray;", style = "font-size:12px;"))
      ),
    
      div(id = "a3",
      tags$h5(tags$em("OR"))
      ),
      
        # Or choose completely random song without criteria. ----
      actionButton("random", "Random Song",class="btn btn-success btn-lg"),
      div(id = "a2",
      tags$p(tags$em("(Be bold and choose a random song)", style = "color:lightgray;", style = "font-size:12px;"))
      ),
      
      tags$hr(),
      
                # Create url with the 'twitter-share-button' class
                tags$a(href=url, "Tweet", class="twitter-share-button" ),
                # Copy the script from https://dev.twitter.com/web/javascript/loading into your app
                # You can source it from the URL below. It must go after you've created your link
                includeScript("http://platform.twitter.com/widgets.js"),
      
      tags$hr(),    
          # For later submit2 input in server side. ----
          tags$div(id = 'placeholder' ),
          br(),
        br(),
      br(),
      
      tags$div(
          id = 'placecomment'),
      
      uiOutput(outputId = "yeter")
     
    ),
  
  # Main panel ----  
  mainPanel(
     
    uiOutput(outputId = "yeter2"),
    
      # Output of initial statistics plots.
      fluidRow(
               column(4,offset = 0, plotOutput("plotStat")),
               column(4,offset = 0 ,plotOutput("plotStat2")),
               column(4,offset = 0, plotOutput("plotStat3")),
               
               column(6,offset = 0,htmlOutput("frame")),
               column(6,offset = 0, span(textOutput("year"), style = "font-size:40px;",
                                                                  style= "font-style: italic;"))
      ),
              

  
    
      # Outputs for the embedded videos ----
      
      
      br(),
        # output for the plot of ratings.
      fluidRow(
        column(5, offset = 1, plotOutput("plotYears")),
        column(5, offset = 0, plotOutput("plotYearsAll")),
        column(6, offset = 0, plotOutput("plotRating")),
        column(6, offset = 0, plotOutput("plotRating2"))
      ),
      
      
      br(),
      br(),
   
      tags$footer(tags$em("-Batuhan Akçabozan tarafından geliştirildi.- Packages Used: 'extrafont', 'googlesheets4', 'tidyverse', 'shinythemes' and 'shiny' DUH.."), 
                  align = "right", style = "font-size:12px;" )   
  )
  )
  ),
  
  tabPanel("SSS (FAQ)",
           tags$ol(
             tags$li("Bu ne şimdi?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("Seçimlerinize göre size bir liste içerisinden rastgele bir müzik seçip gösteren bir site.")),
             tags$li("Şarkılar nereden geliyor?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("Şarkılar şimdilik benim zevkime göre sevdiğim, keşfettiğim şarkılardan geliyor. 
                       Genel olarak dünyanın farklı yerlerinden farklı türlerden müzikler.")),
             tags$li("Ne kadar bilgi toplanıyor?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("Gösterilen grafikleri çizmeye yetecek bilgiler kadar. 
                       Site kullanımı tamamen anonim ve benim herhangi bir şekilde kimin kullandığını ne seçtiğini bilmeme imkan yok.")),
             tags$li("Neden?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("İnteraktif grafikler her zaman eğlencelidir bence. R shiny de aklımın bir köşesindeydi. 
                       Bir şeyler yapmaya çalışayım dedim. Hoş, üzerinde oynamalık grafikler yok ama güzel bir deneyim oldu.
                       Benim gibi değişik müzikler keşfetmeyi sevenlerin hoşuna gideceğini düşünüyorum.")),
             tags$li("Neden günlük, neden arka arkaya birkaç şarkı çalacak şekilde değil?", style = "color:cyan;", style = "font-size:16px;"), 
             tags$p(em("Öncelikle şarkıları ben seçtiğim için, büyük bir şarkı listesi mevcut değil; bundan dolayı çabucak bütün şarkılar bitebilir. 
                       İkincisi ise bu kod shinyapps.io üzerinden sizlerle buluşuyor. 
                       Orada ise aylık 24 saatlik aktif kullanım limiti var (Bedava olarak kullanmak istiyorsam eğer.)
                       O yüzden sitede saatlerinizi geçirmenizi istemiyorum. :)")),
             tags$li("Hiç 'easter egg' var mı?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("Var tabii.")),
             tags$li("İlham nereden geldi?", style = "color:cyan;", style = "font-size:16px;"),
             tags$p(em("'radiooooo.com'un güzel etkileri oldu. Öneririm."))
           ) 
          )
  
  
  )


server <- function(input,output){
  
  
  set.seed(Sys.time())
  # Read in the necessary sheets from the "Data"----------
  
  # Sheet 1: Info about people's input -- 
  choices <- range_read(Data, sheet = 1)

  # Sheet 2: Info on the songs. --
  songs <- range_read(Data, sheet = 2)
  
  # Sheet 3: Record on song ratings. --
  songs_data <- range_read(Data, sheet = 3)
  
    # Function to get mean rating values in each session. Then it will be used to update mean rating in sheet 2.-------
    meanRating <- songs_data %>% 
      group_by(Song) %>%
      summarize(mean = mean(Rating))
  
    # Updates the mean rating values in sheet 2. It filters sheet 2 according to the presence of values in sheet 3. And updates. --
    songs[songs$Index %in% meanRating$Song,]$Rating <- meanRating$mean
  
  # Initial Plots ----
    if(sum(choices$submit) != 0 & nrow(choices != 0)){
  # Choices data manipulation for first plot --  
  choices <- choices %>% 
    pivot_longer(cols = c(submit, random))
  
        # First plot random or submit ?  
        output$plotStat <- renderPlot({
    
  
            ggplot(choices, aes(x = name, y = value, fill = name))+
              geom_col()+
              theme(legend.position = "none")+
              scale_x_discrete(breaks=c("random", "submit"),
                            labels=c("Random", "Specified"))+
              xlab("Search Preferance")+
              ggtitle("What does people prefer?")+ 
            theme_dark()+
            theme(plot.background=element_rect(fill = "#768ca6"),
                  legend.background = element_rect(fill = "#768ca6", color = NA),
                  legend.text = element_text(color = "white"),
                  axis.text = element_text(color = "white"),
                  text = element_text(size = 16, family = "xkcd"))+ 
            scale_fill_discrete(labels = c("random" = "Random", "submit" = "Specified"))
        })
  
  # Choices data manipulation for second plot --
        choices2 <- choices %>% filter(name == "submit" & value == 1) 
          
  
        # Second plot: Rare songs checked or not
        output$plotStat2 <- renderPlot({
          
            ggplot(choices2,aes(x = `Rare songs` , fill = `Rare songs`))+
              geom_bar()+
              theme(legend.position = "none")+
              scale_x_discrete(breaks=c("TRUE", "FALSE"),
                            labels=c("Yes", "NO"))+
              xlab("Undiscovered Songs Checked")+
              ggtitle("What does people prefer?")+ 
            theme_dark()+
            theme(plot.background=element_rect(fill = "#768ca6"),
                  legend.background = element_rect(fill = "#768ca6", color = NA),
                  legend.text = element_text(color = "white"),
                  axis.text = element_text(color = "white"),
                  text = element_text(size = 16, family = "xkcd"))+ 
            scale_fill_discrete(labels = c("FALSE" = "No","TRUE" = "Yes"))
        })
            
        output$plotStat3 <- renderPlot({
          
            ggplot(choices2, aes(x = tempo , fill = tempo))+
              geom_bar()+
              theme(legend.position = "none")+
              xlab("Tempo")+
              ggtitle("What does people prefer?")+ 
            theme_dark()+
            theme(plot.background=element_rect(fill = "#768ca6"),
                  legend.background = element_rect(fill = "#768ca6", color = NA),
                  legend.text = element_text(color = "white"),
                  axis.text = element_text(color = "white"),
                  text = element_text(size = 16, family = "xkcd"))
        })
  
    }
    
    output$plotYears <- renderPlot({
      ggplot(filter(songs, Rating != 0), aes(x = Year, y = Rating))+
        geom_point(color= "cyan3", size = 3)+
        ggtitle("Song release date vs Rating")+ 
        theme_dark()+
        theme(plot.background=element_rect(fill = "#768ca6"),
              axis.text = element_text(color = "white"),
              text = element_text(size = 16, family = "xkcd"))+
        coord_cartesian(ylim = c(0,10), xlim = c(1950,2020))
    })
    
    output$plotYearsAll <- renderPlot({
      ggplot(songs, aes(x = Year))+
        geom_histogram(fill= "cyan3",col = "cyan4", binwidth = 5,boundary=-0.01)+
        ggtitle("Songs are coming from these years:")+ 
        theme_dark()+
        theme(plot.background=element_rect(fill = "#768ca6"),
              axis.text = element_text(color = "white"),
              text = element_text(size = 16, family = "xkcd"))+
        scale_x_continuous(breaks = seq(1950,2030,5))+
        ylab("Number of Songs")
    })
 
   # Removes the buttons of submit and random if pressed either of them. To prevent further abuse. ----
  observeEvent(c(input$submit,input$random),{
    
    if (input$submit == 1 | input$random == 1){
      
      Data  <- Data  %>%                                                                      
        range_write(data = data.frame(songs$Rating), range = paste0("E",2), sheet = 2, col_names = F)  ## I moved the rating update to here, now it will at least update when either submit or random is pressed. I hope
        
        removeUI(selector='#submit', immediate=TRUE)
        removeUI(selector='#random', immediate=TRUE)
        removeUI(selector='#plotStat', immediate=TRUE)
        removeUI(selector='#plotStat2', immediate=TRUE) 
        removeUI(selector='#plotStat3', immediate=TRUE)
        removeUI(selector='#plotYears', immediate=TRUE) 
        removeUI(selector='#plotYearsAll', immediate=TRUE)
        removeUI(selector='#a', immediate=TRUE) 
        removeUI(selector='#a2', immediate=TRUE) 
        removeUI(selector='#a3', immediate=TRUE)
        }
      }, autoDestroy=TRUE)

  
  
  # IF ELSE BURDEN. All just to give the desired song for lovely users.----
  # What happens if submit is pressed.
  video2 <- eventReactive(input$submit,{
  
      if(is.null(input$rate) ){
        
        if(input$rare == T){
          
                   songs2 <- songs[songs$Listened == 0, ]
                   
                   if(input$tempo == "slow"){
                     
                     songs2 <- songs2[songs2$Tempo == "slow", ]
                     video <- sample(songs2$Link, 1)
                     
                   } else if(input$tempo == "fast"){
                     
                     songs2 <- songs2[songs2$Tempo == "fast", ]
                     video <- sample(songs2$Link, 1)
                     
                   } else {
                     
                     video <- sample(songs2$Link, 1)# search for all ratings
                   }
            
          }else if(input$rare == F){
            
                   if(input$tempo == "slow"){
                     
                     songs2 <- songs[songs$Tempo == "slow", ]
                     video <- sample(songs2$Link, 1)
                     
                   } else if(input$tempo == "fast"){
                     
                     songs2 <- songs[songs$Tempo == "fast", ]
                     video <-  sample(songs2$Link, 1)
                     
                   } else {
                     
                     video <-  sample(songs$Link, 1)# search for all ratings
                     
                   }   
            
          }
          
        
        
                  
   
  
 } else {
    
          meh      <- "Meh"       %in% input$rate
          ok       <- "OK"        %in% input$rate
          awesome  <- "AWESOME"   %in% input$rate
     
      if(input$rare == T){
        
          songs2 <- songs[songs$Listened == 0, ]
          
          if(input$tempo == "slow"){
            
              songs2 <- songs2[songs2$Tempo == "slow", ]
               video <- sample(songs2$Link, 1)
        
          } else if(input$tempo == "fast"){
            
              songs2 <- songs2[songs2$Tempo == "fast", ]
               video <- sample(songs2$Link, 1)
      
          } else {
            
               video <- sample(songs2$Link, 1)# search for all ratings
            
              }
        

 } else if(input$rare == F){ 
      
          if (meh & ok & awesome){
        
                          if(input$tempo == "slow"){
                
                              songs2 <- songs[songs$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
          
                  } else if(input$tempo == "fast"){
            
                              songs2 <- songs[songs$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
          
                      } else {
            
                               video <- sample(songs$Link, 1)# search for all ratings
          
                          }
          
          
   } else if (meh & ok){
        
        songs2 <- songs[between(songs$Rating,0.01,6.66),]
        
                          if(input$tempo == "slow"){
                            
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                                
                   } else if(input$tempo == "fast"){
                     
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
          
                      } else {
          
                               video <- sample(songs2$Link, 1)
          
                           }
        
  } else if (ok & awesome){
        
        songs2 <- songs[songs$Rating >= 3.33 ,]
        
                          if(input$tempo == "slow"){
                            
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                               
                   } else if(input$tempo == "fast"){
                     
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
                          
                      } else {
                        
                               video <- sample(songs2$Link, 1)
                        
                         }
                          
  } else if (meh & awesome) {
    
        songs2 <- songs[between(songs$Rating,0.01,3.33) | songs$Rating >= 6.66 ,]
        
                         if(input$tempo == "slow"){
                           
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                              
                  } else if(input$tempo == "fast"){
                    
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
                         
                     } else {
                       
                               video <- sample(songs2$Link, 1)
                     
                        }
      
                
                             
                             
  } else if (meh){
    
       songs2 <- songs[between(songs$Rating,0.01,3.33),]
        
                         if(input$tempo == "slow"){
                           
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                             
                  } else if(input$tempo == "fast"){
                    
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
                            
                     } else {
                       
                               video <- sample(songs2$Link, 1)
                            
                          }
       
  } else if (awesome){
                          
      songs2 <- songs[songs$Rating >= 6.66,]
                                           
                          if(input$tempo == "slow"){
                            
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                            
                         } else if(input$tempo == "fast"){
                           
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
                           
                         } else {
                           
                           video <- sample(songs2$Link, 1)
                           
                         }
        
  } else if (ok) {
                          
      songs2 <- songs[between(songs$Rating,3.33,6.66),]
        
                          if(input$tempo == "slow"){
                           
                              songs2 <- songs2[songs2$Tempo == "slow", ]
                               video <- sample(songs2$Link, 1)
                           
                   } else if(input$tempo == "fast"){
                           
                              songs2 <- songs2[songs2$Tempo == "fast", ]
                               video <- sample(songs2$Link, 1)
                           
                      } else {
                           
                               video <- sample(songs2$Link, 1)
                           
                          }
                       
      }
      
    }
    
  }  
})

  # Recording the result if Random is pressed. ----
  videoR <- eventReactive(input$random,{
    
        videoR <- sample(songs$Link, 1)
  })

 

  # To save the inputs and update them in google docs. (sheet 1) ----
  observeEvent(c(input$submit, input$random), { 
    
    if(input$submit > 1 | input$random > 1){
       # ------------------------------------------------
      print("no")
      
      
      }else if(input$submit ==1 | input$random == 1){
        results <- data.frame(input$tempo, paste0(".",paste0(".",input$rate, collapse = ",")), input$rare ,as.integer(input$submit),as.integer(input$random), Sys.time())
        
        Data  <- Data  %>%                                                                      
          sheet_append( data = results, sheet = 1)
      }
    
    
    

                                                        
  }, ignoreInit = T
  )

  # The following codes create the slider inputs for rating and submit button 2. Can be improvent----------------
  observeEvent(input$random, {
    
    if(input$random == 1){
      insertUI(
        selector = '#placeholder',
        ui = tagList(sliderInput('ratethis', tags$h3('Give this song a rating'), 0, 10, 5),
                     actionButton('submit2', 'Submit', class="btn btn-info btn-lg")),where = "beforeEnd"
      )
      
      insertUI(
        selector = '#placecomment',
        ui = tagList(hr(),
                     textAreaInput(inputId = "comment", label = tags$em("Comments, suggestions, questions:", style = "font-size:14px;"), rows=3, resize = "vertical"),
                     actionButton("commentSubmit", "Submit",class="btn btn-info"))
      )
    }
    
    
             })
  
  observeEvent(input$commentSubmit,{ # -------
    
    if(input$comment != ""){
      Data  <- Data  %>%                                                                      
        sheet_append(data = data.frame(input$comment, Sys.time()),  sheet = 4 )
      
      
      removeUI(selector = "div:has(> #commentSubmit)", immediate = T)
      removeUI(selector = "div:has(> #comment)", immediate = T)
    } else if(input$comment == "" & input$commentSubmit == 3){
      
      removeUI(selector = "div:has(> #commentSubmit)", immediate = T)
      removeUI(selector = "div:has(> #comment)", immediate = T)
      
      output$yeter <- renderUI({
        img(src='yeter.png', height = '350px')
      })
    }
    
  })
  
 
  
  
  observeEvent(input$submit, {
    
    if(input$submit == 1){ ## --------
      
      insertUI(
        selector = '#placeholder',
        ui = tagList(sliderInput('ratethis', tags$h3('Give this song a rating'), 0, 10, 5),
                     actionButton('submit2', 'Submit', class="btn btn-info btn-lg")),where = "beforeEnd"
      )
      
      insertUI(
        selector = '#placecomment',
        ui = tagList(textAreaInput(inputId = "comment", label ="Comments, suggestions, questions: ", rows=3, resize = "vertical"),
                     actionButton("commentSubmit", "Submit",class="btn btn-info"))
      )
    }
    
              
             })


  # Saving the results from submit2 button
  observeEvent(input$submit2, {
  
      if (input$submit == 1){
        
            
            index <- songs[songs$Link == video2(),1] # Index of the displayed song
          newvote <- songs[songs$Link == video2(),6] + 1 # New vote value if voted.
    
            Data  <- Data  %>%                                                                      
              sheet_append(data = data.frame(as.integer(input$ratethis), index),  sheet = 3) # Rating saved to sheet 3 with index
            Data  <- Data  %>% 
              range_write(data = data.frame(newvote), range = paste0("F", index+1), sheet = 2, col_names = F) # vote # updated in sheet2
  
            # Updates the mean rating in sheet 2
          
                  
                  # Removes the slider and submit2 button.
                  removeUI(selector = "div:has(> #submit2)", immediate = T)
                  removeUI(selector = "div:has(> #ratethis)", immediate = T)
                  
    
    
  } else if( input$random == 1){
    
            index <- songs[songs$Link == videoR(),1] 
          newvote <- songs[songs$Link == videoR(),6] + 1
    
            Data  <- Data  %>%                                                                      
              sheet_append(data = data.frame(c(as.integer(input$ratethis), index )), sheet = 3) 
            Data  <- Data  %>% 
              range_write(data = data.frame(newvote), range = paste0("F", index+1), sheet = 2, col_names = F)
    

            # Saves the data frame.-----

    
                  removeUI(selector = "div:has(> #ratethis)", immediate = T)
                  removeUI(selector = "div:has(> #submit2)", immediate = T)
        }

  }, autoDestroy=TRUE
  )



  # record the times a song is displayed. Sheet2 = Listened.
  observeEvent(c(input$submit, input$random), {
    
               if (input$submit == 1){
                 
                 # Output of Submit ---
                 output$frame <- renderUI({
                   
                   tags$iframe(width="560", height="315", src= video2(),
                               frameborder="0", allow= "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                 })
                   
                    index <- songs[songs$Link == video2(),1] 
                      new <- songs[songs$Link == video2(),7] + 1
                 
                    Data  <- Data  %>%                                                                      
                       range_write(data = data.frame(new), range = paste0("G", index+1), sheet = 2, col_names = F) 
                    
                  output$year <- renderText({ 
                     paste0("'", songs[songs$Index == as.integer(index),"Year"]  ,"'")})
  
                 
        } else if( input$random == 1){
          
          # Output of Random ---
          output$frame <- renderUI({
            
            tags$iframe(width="560", height="315", src= videoR(),
                        frameborder="0", allow= "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
          })
                 
                    index <- songs[songs$Link == videoR(),1] 
                      new <- songs[songs$Link == videoR(),7] + 1
                 
                    Data  <- Data  %>%                                                                      
                       range_write(data = data.frame(new), range = paste0("G",index+1), sheet = 2, col_names = F)
                    
                    output$year <- renderText({ 
                      paste0("'", songs[songs$Index == as.integer(index),"Year"]  ,"'")})
                  
        }else if(input$submit >= 3 | input$random >= 3){#<-------
          
          output$yeter2 <- renderUI({
            img(src='yeter.png', height = '800px')
          })
               }
               
      })
  
  # Plotting the Ratings of current song and other songs.
  observeEvent(c(input$submit, input$random, input$submit2) ,{
  
        if(input$submit== 1){
    
              index <- songs[songs$Link == video2(),1]
              
         songs_data <- songs_data %>% 
              mutate(durum = ifelse(Song == as.integer(index) ,"This Song", "Other Songs"))
    
                output$plotRating <- renderPlot({ 
                  
                      ggplot(songs_data, aes(x = Rating, fill = durum))+
                          geom_histogram(binwidth = 1,position = "identity", alpha = 0.5, aes(y = ..density..))+
                          geom_density(alpha=.1, aes(color = durum), show.legend = F) +
                          coord_cartesian(xlim = c(0,10), ylim = c(0,1))+
                          scale_x_continuous(breaks = seq(0,10,1))+ 
                    theme_dark()+
                    theme(plot.background=element_rect(fill = "#768ca6"),
                          legend.background = element_rect(fill = "#768ca6", color = NA),
                          legend.text = element_text(color = "white"),
                          axis.text = element_text(color = "white"),
                          text = element_text(size = 16, family = "xkcd"))+
                    scale_fill_discrete(name = "Songs")+
                    ggtitle("Rating comparison")
                  
    }) 
                
                
                songs3 <-  songs %>% 
                  filter(Rating != 0) %>%
                  mutate(durum = ifelse(as.integer(index) == Index, "This song", "not this song"))
                
                if(nrow(subset(songs3, durum == "This song"))== 0){
                  output$plotRating2 <- renderPlot({
                    ggplot( data = songs3, mapping  = aes(x = Rating, y = Tempo))+
                      geom_boxplot(aes(fill = Tempo), show.legend = F)+ 
                      theme_dark()+
                      theme(plot.background=element_rect(fill = "#768ca6"),
                            legend.background = element_rect(fill = "#768ca6", color = NA),
                            legend.text = element_text(color = "white"),
                            axis.text = element_text(color = "white"),
                            text = element_text(size = 16, family = "xkcd")) + 
                      ggtitle("Rating and song Tempo")+
                      coord_cartesian(xlim = c(0,10))
                    
                  }) 
                }else{
                  output$plotRating2 <- renderPlot({
                    ggplot( data = songs3, mapping  = aes(x = Rating, y= Tempo))+
                      geom_boxplot(aes(fill = Tempo), show.legend = F)+
                      geom_vline(data = subset(songs3, durum == "This song")  ,aes(xintercept = Rating), linetype = 2, color = "white")+
                      geom_text(data = subset(songs3, durum == "This song")  ,aes(angle = 90, label= Song, y = 1.5), size = 7,nudge_x = -0.22, color = "white")+ 
                      theme_dark()+
                      theme(plot.background=element_rect(fill = "#768ca6"),
                            legend.background = element_rect(fill = "#768ca6", color = NA),
                            legend.text = element_text(color = "white"),
                            axis.text = element_text(color = "white"),
                            text = element_text(size = 16, family = "xkcd")) + 
                      ggtitle("Rating and song Tempo")+
                      coord_cartesian(xlim = c(0,10))
                    
                  }) 
                }
                
              
    
            if( is.null(input$submit2)){
                  
            }else if(input$submit2 == 1){
          
             songs_data <- songs_data %>% 
                add_row(Rating = as.numeric(input$ratethis), Song = as.integer(index) , durum = "This Song")
             
                  output$plotRating <- renderPlot({ 
                    
                    ggplot(songs_data, aes(x = Rating, fill = durum ))+
                      geom_histogram(binwidth = 1, position = "identity", alpha = 0.5, aes(y = ..density..))+
                      geom_density(alpha=.1, aes(color = durum), show.legend = F) +
                      coord_cartesian(xlim = c(0,10), ylim = c(0,1))+
                      scale_x_continuous(breaks = seq(0,10,1))+ 
                      theme_dark()+
                      theme(plot.background=element_rect(fill = "#768ca6"),
                            legend.background = element_rect(fill = "#768ca6", color = NA),
                            legend.text = element_text(color = "white"),
                            axis.text = element_text(color = "white"),
                            text = element_text(size = 16, family = "xkcd"))+
                      scale_fill_discrete(name = "Songs")+
                      ggtitle("Rating comparison")
                    })   
           }
    
    
  } else if(input$random == 1){
    
      index <- songs[songs$Link == videoR(),1]
      
      songs_data <- songs_data %>% 
        mutate(durum = ifelse(Song == as.integer(index) ,"This Song", "Other Songs"))
    
            output$plotRating <- renderPlot({ 
              
              ggplot(songs_data, aes(x = Rating, fill = durum))+
                geom_histogram(binwidth = 1, position = "identity", alpha = 0.5, aes(y = ..density..))+
                geom_density(alpha=.1, aes(color = durum), show.legend = F) +
                coord_cartesian(xlim = c(0,10), ylim = c(0,1))+
                scale_x_continuous(breaks = seq(0,10,1))+ 
                theme_dark()+
                theme(plot.background=element_rect(fill = "#768ca6"),
                      legend.background = element_rect(fill = "#768ca6", color = NA),
                      legend.text = element_text(color = "white"),
                      axis.text = element_text(color = "white"),
                      text = element_text(size = 16, family = "xkcd"))+
                scale_fill_discrete(name = "Songs")+
                ggtitle("Rating comparison")
              }) 
                
            songs3 <-  songs %>% 
              filter(Rating != 0) %>%
              mutate(durum = ifelse(as.integer(index) == Index, "This song", "not this song"))
            
            if(nrow(subset(songs3, durum == "This song")) == 0){
              output$plotRating2 <- renderPlot({
                ggplot( data = songs3, mapping  = aes(x = Rating, y = Tempo))+
                  geom_boxplot(aes(fill = Tempo), show.legend = F)+ 
                  theme_dark()+
                  theme(plot.background=element_rect(fill = "#768ca6"),
                        legend.background = element_rect(fill = "#768ca6", color = NA),
                        legend.text = element_text(color = "white"),
                        axis.text = element_text(color = "white"),
                        text = element_text(size = 16, family = "xkcd")) + 
                  ggtitle("Rating and song Tempo")+
                  coord_cartesian(xlim = c(0,10))
                }) 
              
            }else{
              output$plotRating2 <- renderPlot({
                ggplot( data = songs3, mapping  = aes(x = Rating, y = Tempo))+
                  geom_boxplot(aes(fill = Tempo), show.legend = F)+
                  geom_vline(data = subset(songs3, durum == "This song")  ,aes(xintercept = Rating), linetype = 2, color = "white")+
                  geom_text(data = subset(songs3, durum == "This song")  ,aes(angle = 90, label= Song, y = 1.5), size = 7,nudge_x = -0.22, color = "white")+ 
                  theme_dark()+
                  theme(plot.background=element_rect(fill = "#768ca6"),
                        legend.background = element_rect(fill = "#768ca6", color = NA),
                        legend.text = element_text(color = "white"),
                        axis.text = element_text(color = "white"),
                        text = element_text(size = 16, family = "xkcd")) + 
                  ggtitle("Rating and song Tempo")+
                  coord_cartesian(xlim = c(0,10))
                
              }) 
            }
                  if( is.null(input$submit2)){
              
                  }else if(input$submit2 == 1){
                
                   songs_data <- songs_data %>% 
                     add_row(Rating = as.numeric(input$ratethis), Song = as.integer(index), durum = "This Song" )
                
                            output$plotRating <- renderPlot({ 
                              
                            ggplot(songs_data, aes(x = Rating, fill = durum))+
                            geom_histogram(binwidth = 1, position = "identity", alpha = 0.5, aes(y = ..density..))+
                            geom_density(alpha=.1, aes(color = durum), show.legend = F) +
                            coord_cartesian(xlim = c(0,10), ylim = c(0,1))+
                            scale_x_continuous(breaks = seq(0,10,1))+ 
                                theme_dark()+
                                theme(plot.background=element_rect(fill = "#768ca6"),
                                      legend.background = element_rect(fill = "#768ca6", color = NA),
                                      legend.text = element_text(color = "white"),
                                      axis.text = element_text(color = "white"),
                                      text = element_text(size = 16, family = "xkcd"))+
                                scale_fill_discrete(name = "Songs")+
                                ggtitle("Rating comparison")})   
                }           
      }            
  
  }
  )
  
  showNotification(strong("Updates - Güncellemeler: -- Tweeter'da paylaşma butonu eklendi.
                          -- SSS sayfası eklendi, kimse sıkça bir şey sormamasına rağmen.(Yeni)
                          -- RASTGELE GELMESİ GEREKEN ŞEYLER RASTGELE GELMİYORDU. BU DÜZELTİLDİ. (düzelmiş gibi duruyor..)
                          -- Sayfa yüklenirken sabırsızlananlar için önlem alındı.(alınan önlemle başka şeyleri bozmuşuz, o düzeltildi.)
                          -- Atık böyle bildirimler çıkıyor 
                          -- bug fixes"), duration = NULL,
                    closeButton = T, type = "message")
  
}

shinyApp(ui = ui, server = server)
