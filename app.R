#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(compiler)
library(dplyr)
library(progress)
library(broom)
library(zcurve)
library(thematic)
library(shinycssloaders)
library(bslib)
library(statcheck)
library(fulltext)
library(roadoi)
library(pdftools)
library(stringr)


ui <- navbarPage("Meta Yourself",   # title

                 # set theme (choose theme at: https://bootswatch.com/3/ )
                 theme = bs_theme(version = 4, bootswatch = "darkly"),


                 tabPanel("Instructions",
                          fluidPage(column(6,
                                           offset = 4,
                                           align="left",
                                           includeMarkdown('markdown_files/instructions.Rmd'))
                          )
                 ),


                 tabPanel("Set up",
                          fluidRow(             # fluid row for simulation control

                              column(4,
                                     offset = 3,
                                     style='padding:0px;',
                                     sliderInput("limit",
                                                 "Maximum number of studies:",
                                                 min = 50,
                                                 max = 1000,
                                                 value = 100),
                              ),
                              column(4,
                                     textInput('query', 'What do you want to inspect?', value = "neuromarketing", width = NULL, placeholder = NULL)

                              )
                          ),
                          fluidRow(
                              column(6,
                                     offset = 4,
                                     align="left",
                                     includeMarkdown('markdown_files/set_up.Rmd'))
                          ),
                          fluidRow(
                              column(12,
                                     verbatimTextOutput("articles")%>% withSpinner(color="#0dc5c1"))
                          )
                 ),




                 tabPanel("Results",
                          fluidRow(
                              column(6,
                                     offset = 3,
                                     plotOutput("z_plot_c")%>% withSpinner(color="#0dc5c1"))),
                          fluidRow(
                              column(6,
                                     offset = 4,
                                     align="left",
                                     includeMarkdown('markdown_files/results.Rmd')))
                 )
)




# Define server logic required to draw a histogram
server <- function(input, output) {


    results <- reactive({

        dir.create("uploads/")
        res <- ft_search(query = input$query, limit=input$limit)

        doi = unlist(res$plos$data)

        len = length(doi)

        library = roadoi::oadoi_fetch(dois = doi,
                                      email = "najko.jahn@gmail.com")

        for (i in 1:len){
            skip_to_next = FALSE
            url = unlist(library[2]$best_oa_location[i])[2]
            tryCatch(download.file(url, sprintf('uploads/article_%s.pdf',i), mode="wb"), error = function(e) { skip_to_next <<- TRUE})

            if(skip_to_next) { next }

        }
        txt = ''
        for (pdf_path in (list.files("uploads/", full.names = TRUE))){
            out <- tryCatch(
                {
                    pdftools::pdf_text(pdf_path) %>%
                        paste(sep = " ") %>%
                        stringr::str_replace_all(fixed("\n"), " ") %>%
                        stringr::str_replace_all(fixed("\r"), " ") %>%
                        stringr::str_replace_all(fixed("\t"), " ") %>%
                        stringr::str_replace_all(fixed("\""), " ") %>%
                        paste(sep = " ", collapse = " ") %>%
                        stringr::str_squish() %>%
                        stringr::str_replace_all("- ", "")
                },
                error=function(cond) {
                    return(' ')
                })
            txt = paste(txt, out, sep = ' ')
        }
        stats = statcheck(txt)

        zip = list('mydata' = stats, 'library' = library)
        do.call(file.remove, list(list.files("uploads/", full.names = TRUE)))
        return(zip)
    })

    output$z_plot_c = renderPlot({

        zip = results()                                          # get the data
        mydata = zip$mydata
        z = zcurve(p = mydata$Computed[!is.na(mydata$Computed)], method = 'EM', bootstrap = 100)   # apply the z-curve

        plot(z, annotation = TRUE, CI = TRUE, x_text = 0)          # plot the z-curve,
        height = 12

    })

    output$articles = renderText({

        zip = results()
        library = zip$library
        title = library$title
        authors = library$authors
        text = ''
        for (i in 1:length(title)){
            text = paste(text, paste(title[i], '\n', authors[[i]]$given[1] ,authors[[i]]$family[1], sep = ' '), sep = '\n\n')
        }
        return(text)

    })
}

# Run the application
shinyApp(ui = ui, server = server)

