library(shinyWidgets)
library(httr)
library(jsonlite)
library(rvest)
# Reference: https://jtr13.github.io/cc21fall2/comparison-among-base-r-tidyverse-and-datatable.html
library(tidyverse)
library(stringi)

GETfunc = function(URL) {
    GET(
        url = URL,
        config = add_headers(
            "Host" = "data.usajobs.gov",
            "User-Agent" = "jialong7@illinois.edu",
            "Authorization-Key" = "Cuht8z1/tYxh7Lr6pfdk24oXpYddEeKjD0kUkBHFfgE="
        )
    )
}

Search_query = function(Key_word, location, URL) {
    Key_word = stri_replace_all_fixed(Key_word, " ", "")
    location = stri_replace_all_fixed(location, " ", "")
    query = paste0(URL,"?Keyword=",Key_word,"&LocationName=",location,"&ResultsPerPage=1000")
    query
}

# key_word = "data", location = "NYC", URL is fixed, "https://data.usajobs.gov/api/search".
test_query_1 = Search_query("data", "NYC","https://data.usajobs.gov/api/search")
# read = GETfunc(test_query_1)
# data = fromJSON(rawToChar(read$content))
# data

GetDataFrame = function(query) {
  # read data
  read = GETfunc(query)
  data = fromJSON(rawToChar(read$content))
  Research_result = data$SearchResult$SearchResultItems$MatchedObjectDescriptor
  selected_df = Research_result |>
    select(PositionTitle, PositionLocationDisplay, OrganizationName, DepartmentName)
  
  # columns in first layer
  result_df = selected_df |>
    select(PositionTitle, PositionLocationDisplay, PositionLocationDisplay, OrganizationName, DepartmentName)
  
  for (i in 1:nrow(result_df)) {
    salary_range = unlist(data$SearchResult$SearchResultItems$MatchedObjectDescriptor$PositionRemuneration[i])
    result_df$MinimumRange[i] = salary_range[[1]]
    result_df$MaximumRange[i] = salary_range[[2]]
  }
  
  # add columns from deeper layer. 
  text = rep(0,nrow(Research_result))
  x = rep(0,nrow(Research_result))
  y = rep(0,nrow(Research_result))
  Education_degree = rep(0,nrow(Research_result))
  for(i in 1:nrow(Research_result)) {
    # gpa requirement
    text[i] = Research_result$QualificationSummary[[i]]
    x[i] = unlist(str_extract_all(text[i], "[\\d]+[\\.]+[\\d]+[\\/]+[\\d]+[\\.]+[\\d]"))[1]
    y[i] = as.numeric(str_split(x[i], pattern = "/")[[1]])[1]
    
    # education degree
    Education_degree[i] = str_extract(text[i],"([B|b]+[a]+[c]+[h]+[e]+[l]+[o]+[r])|([M|m]+[a]+[s]+[t]+[e]+[r])|([B|b]+[a]+[c]+[c]+[a]+[l]+[a]+[u]+[r]+[e]+[a]+[t]+[e])|([G|g]+[r]+[a]+[d]+[u]+[a]+[t]+[e])+([D|d]+[o]+[c]+[t]+[o]+[r])|([P|p]+[H|h]+[\\.]+[D|d])|([D|d]+[o]+[c]+[t]+[o]+[r]+[a]+[l])")
  }
  
  # need 3.0 instead of 3
  GPA_requirement = ifelse(is.na(y),"GPA not required",sprintf("%.1f",y))
  result_df = cbind(result_df,GPA_requirement)
  
  # standardized terms we use
  Education_degree = ifelse(is.na(Education_degree),"not required",Education_degree)
  Education_degree = ifelse(toupper(Education_degree) == "BACCALAUREATE", "bachelor", Education_degree)
  Education_degree = ifelse(toupper(Education_degree) == "PH.D", "doctoral", Education_degree)
  Education_degree = str_to_lower(Education_degree)
  result_df = cbind(result_df,Education_degree)
  
  result_df
}

# The DF
selected_df = GetDataFrame(test_query_1)

ui = navbarPage(
    title = "STAT 440 final project ",
    tabPanel(
        title = " Visualization",
        titlePanel(title = "2022 NYC data-oriented federal jobs research visualization"),
        sidebarLayout(
            sidebarPanel(
                uiOutput("input_1"),
                actionButton("view1", "submit")
            ),
            mainPanel(
                plotOutput("plot2")
            )
        )),
    tabPanel(
        title = "NYC Job research table",
        titlePanel(title = "2022 NYC data-oriented federal jobs research summarised table"),
        mainPanel(
            dataTableOutput("table2")
        )
    ),
    tabPanel(title = "Description", includeMarkdown("Description.Rmd"))
)

server = function(input, output){
    output$table2 <- renderDataTable({
        dataset_new()
    })
    dataset_new = eventReactive(input$view1,{
        if (input$pick == "Job Location"){
            selected_df %>%
                group_by(PositionLocationDisplay) %>%
                summarise("y" = n()) %>%
                arrange(desc(y)) %>%
                rename("Job Location" = PositionLocationDisplay,"Number of Jobs offered" = y)
        } else if (input$pick =="Department Name") {
            selected_df %>%
                group_by(DepartmentName) %>%
                summarise("y" = n()) %>%
                arrange(desc(y)) %>%
                rename("Department name"=DepartmentName,"Number of Jobs offered" = y)
        } else if (input$pick =="Minimum educational requirement") {
            selected_df %>%
                group_by(Education_degree) %>%
                summarise("y" = n()) %>%
                arrange(desc(y)) %>%
                rename("Minimum educational requirement" = Education_degree,"Number of Jobs offered" = y)
        }})
    
    output$input_1 = renderUI({
        pickerInput(inputId = "pick", label = c('Please choose one of the variables, then click the "submit" button'), choices= c("Job Location","Department Name","Minimum educational requirement"), 
                    options=list(`actions-box`= TRUE), multiple = F, width = "fit")
    })
    
    dataset = eventReactive(input$view1, {
      if (input$pick == "Job Location") {
        cutted = selected_df %>%
        select(PositionLocationDisplay) %>%
        arrange()
          
        for (i in 1:length(cutted$PositionLocationDisplay)) {
          string = cutted$PositionLocationDisplay[i]
          shorted = str_remove_all(string, ", United States")
          shorted1 = str_remove_all(shorted, "May be filled in ")
          shorted2 = str_remove_all(shorted1, "(remote job)")
          shorted3 = str_remove_all(shorted2, "[()]")
          shorted4 = str_remove_all(shorted3, " Upon Request")
          shorted5 = str_remove_all(shorted4, " After Selection")
          cutted$PositionLocationDisplay[i] = shorted5
        }
      
        cutted %>%
          group_by(PositionLocationDisplay) %>%
          summarise("y" = n()) %>%
          arrange(desc(y)) |>
          slice_head(n = 7) |>
          ggplot(aes(x=reorder(`PositionLocationDisplay`,+`y`), y=`y`)) +
          coord_flip() + 
          geom_bar(stat = "identity") +
          geom_text(aes(label= y), hjust = -0.08, size = 3.5) +
          labs(x="Jobs Location", y="Jobs offered") +
          theme_light() +
          theme(panel.grid = element_blank(), text = element_text(size = 17)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
          scale_y_continuous(breaks=seq(0, 300, 50)) +
          guides(fill=guide_legend(title="Job locations"))
          
        } else if(input$pick == "Department Name") {
          cutted = selected_df %>%
            group_by(DepartmentName) %>%
            summarise("y" = n()) %>%
            arrange(desc(y)) %>%
            slice_head(n=7)
          # 
          for (i in 1:length(cutted$DepartmentName)) {
            string = cutted$DepartmentName[i]
            shorted = str_remove_all(string, "Department of ")
            if (shorted == "Other Agencies and Independent Organizations") shorted = "Other Agencies"
            if (shorted == "Health And Human Services") shorted = "Health & Human Services"
            cutted$DepartmentName[i] = shorted
          }
          cutted |>
            ggplot(aes(x=reorder(`DepartmentName`,+`y`), y=`y`)) +
            coord_flip() + 
            geom_bar(stat = "identity") +
            geom_text(aes(label= y), hjust = -0.08, size = 3.5) +
            labs(x="Department Name", y="Jobs offered") +
            theme_light() +
            theme(panel.grid = element_blank(), text = element_text(size = 17)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(breaks=seq(0, 275, 50)) +
            guides(fill=guide_legend(title="Department Name"))
            
        } else if(input$pick == "Minimum educational requirement"){
            selected_df %>%
            group_by(Education_degree) %>%
            summarise("y" = n()) %>%
            arrange(desc(y)) %>%
            slice_head(n = 8) %>%
            ggplot(aes(x=reorder(`Education_degree`,+`y`), y=`y`)) +
            coord_flip() + 
            geom_bar(stat = "identity") +
            geom_text(aes(label= y), hjust = -0.08, size = 4) +
            labs(x="Minimum educational degree", y="Jobs offered") +
            theme_light() +
            theme(panel.grid = element_blank(), text = element_text(size = 17)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(breaks=seq(0, 470, 50)) +
            guides(fill=guide_legend(title="Education degree"))
        }

    })
    output$plot2<- renderPlot({
        dataset()
    })
}


shinyApp(ui=ui, server=server)
