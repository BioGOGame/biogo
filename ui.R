# Copyright © 2019, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2019 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

ui <- miniPage(
  gadgetTitleBar(img(src='biogo_logo_transp_xs.png'),  
                   right = miniTitleBarButton("tasks", "Tasks", 
                                              primary = T),
                    left = miniTitleBarButton("rules", "Rules", 
                                            primary = T)),
  
  tags$head(tags$style(
    type="text/css",
    " img {max-width: 100%; height: auto}
    .tabbable {z-index : 999999}
    .nav-tabs {
      background-color: #006747;
    }"
  )),
  
  miniTabstripPanel(
    
    #----------------------------------------------------------------------------
    # STORE PANEL 
    # This panel is for the storage, in the group's chess, of individual bounties
    #----------------------------------------------------------------------------
    
    miniTabPanel("Store", icon = icon("image"),
      miniContentPanel(
        helpText(help_store),
        tags$hr(),
        fileInput("file", label = "Needs to be a jpg file"),
        selectInput("store_quest", label = "Choose quest", choices = c(1,2)),
        selectInput("store_zone", label = "Choose zone", choices = c(1,2)),
        h3("Your image"),
        imageOutput("store_img", width="auto"),
        h3("Localisation"),
        leafletOutput("store_map")

      ),
      miniButtonBlock(actionButton("store", label = "Store image", icon("paper-plane"), 
                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      border = "top")
    ),
    
    
    #----------------------------------------------------------------------------
    # SUBMIT PANEL 
    # In this panel, users can choose on of their stored bounty and submit it for evaluation
    # This action cannot be undone. 
    #----------------------------------------------------------------------------
    
    miniTabPanel("Submit", icon = icon("check"),
       miniContentPanel(
          helpText(help_submit),
          tags$hr(),
          selectInput("submit_img", label = "Choose a stored bounty", choices = c(1,2,3)),
          selectInput("submit_quest", label = "Choose quest", choices = c(1,2,3)),
          # selectInput("submit_group", label = "Choose group", choices = c(1,2,3)),
          selectInput("submit_zone", label = "Choose zone", choices = c(1,2,3)),
          imageOutput("submit_img", width="auto")
      ),
      miniButtonBlock(
        
        # Button to submit the bounty for evaluation
        actionButton("submit", 
                     label = "Submit bounty", 
                     icon("check"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        # Button to delete a bounty from the user's chest
        actionButton("delete", 
                     label = "Delete bounty", 
                     icon("trash-o"), 
                     style="color: #fff; background-color: #d83429; border-color: #98231b"),
        border = "top")
    ),
    
    
    #----------------------------------------------------------------------------
    # BOUNTY
    # In this panel, users can see their own bounties, submitted or not
    # together with their evaluation status. Users can also see the ranking of all the groups
    #----------------------------------------------------------------------------
    
    miniTabPanel("Bounty", icon = icon("star outline"),
      miniContentPanel(
        helpText(help_bounty),
        tags$hr(),
        materialSwitch(inputId = "show_all_bounties",
                       label = "Show all groups",
                       status = "success", right = TRUE, value = FALSE),
        conditionalPanel(
          condition = "input.show_all_bounties == true",
          tags$hr(),
          htmlOutput("summary_all_bounties"),
          tags$hr(),
          dataTableOutput("all_bounties")
        ),
        conditionalPanel(
          condition = "input.show_all_bounties == false",
          tags$hr(),
          htmlOutput("summary_group_bounties"),
          tags$hr(),
          dataTableOutput("group_bounties"), 
          tags$hr(), 
          imageOutput("bounty_img", width="auto") # Display the selected image
          
        )
      )
    ),

    #----------------------------------------------------------------------------
    # QUESTS /  ADMIN PANEL
    # This panel changes if the users is a "player" or a "master"
    # Player : list of all quests and zones
    # Master : admin panel (dashboard, table view, table update, correction interface and game settings)
    #----------------------------------------------------------------------------
    
    miniTabPanel("Quests", icon = icon("list alternate outline"),
       miniContentPanel(
         
         #--------------------------------
         # QUESTS PANEL, if the user is a player
         #--------------------------------
         conditionalPanel(
          condition = "output.role == 1",
            materialSwitch(inputId = "show_zones",
                           label = "Show map of zones",
                           status = "success", right = TRUE, value = FALSE),
            tags$hr(),
            conditionalPanel(
              condition = "input.show_zones == false",
              htmlOutput("summary_all_quests"),
              tags$hr(),
              materialSwitch(inputId = "show_inactive",
                             label = "Show inactive quests",
                             status = "success", right = TRUE, value = FALSE),
              tags$hr(),
              dataTableOutput("all_quests")
            ),
            conditionalPanel(
              condition = "input.show_zones == true",
              leafletOutput("zones_map")
            )
          ),
         
         # ADMIN PANEL, if the user is a master of a GOD
         conditionalPanel(
           condition = "output.role <= 0",
           
           # if user is master, then access to basic admin functions
           # conditionalPanel(
           #   condition = "output.role == 0", # if only master
           #    selectInput("select_view", label = "Choose view", 
           #             choices = c("Dashboard" = 0,
           #                          "Correction interface" = 1,
           #                          "View DB tables" = 2))
           # ),
           # # if the user is GOD, then access to higher admin functions
           # conditionalPanel(
           #   condition = "output.role == -1", # if GOD
             selectInput("select_view", label = "Choose view", 
                         choices = c("Dashboard" = 0,
                                     "Correction interface" = 1,
                                     "View DB tables" = 2,
                                     "Update DB table" = 3,
                                     "Game parameters" = 4)),
           # ),
           tags$hr(),
           
           
           # DASHBOARD PANEL
           # This is a panel showing synthetic data about the users 
           conditionalPanel(
             condition = "input.select_view == 0",
             box(
               tags$h3("Submitted quests"),
               plotOutput("dashQuests")
             ),
             
             box(
               tags$h3("Corrections"),
               plotOutput("dashCorrections")
             ) , 
             
             box(
               tags$h3("Bounty storage"),
               plotOutput("dashSubmissions")
             ), 
             
             box(
               tags$h3("Connections"),
               plotOutput("dashConnections")
             )
           ),
           
           
           # GAME PARAMETERS
           # update the parameters of the game
           conditionalPanel(
             condition = "input.select_view == 4",
             helpText("Number of quests each group needs to collect during the game"),
             sliderInput("req_bounties", "Number of needed quests", 0, 200, req_bounties),
             helpText("Number of quests of each type each group needs to collect during the game"),
             sliderInput("req_types", "Number of needed quests type", 0, req_bounties, req_types),
             helpText("Number of zones each group needs to explore during the game"),
             sliderInput("req_zones", "Number of needed zones", 0, 10, req_zones),
             helpText("Maximum number of out-of-zone bounties allowed for each group"),
             sliderInput("req_out_zones", "Maximum out-of-zone bounties ", 0, req_bounties, req_types),
             helpText("Maximum number of time a single quest can be found. If a quest reach this limit, it will be listed as inactive"),
             sliderInput("max_found_quest", "Maximum bounties for a quest ", 0, max_found_quest, req_types),
             
             miniButtonBlock(actionButton("update_params", label = "Update parameters", icon("cogs"), 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             border = "top")
             
           ),
           
           
           # VIEW DATA
           # View the data contained in the different tables of the database
           conditionalPanel(
             condition = "input.select_view == 2",
             selectInput("select_table", label = "Choose table", choices = all_tables),
             downloadButton("download_table_data", "Download"),
             dataTableOutput("table_data")
             
           ),
           
           # UPLOAD INTERFACE
           # Allow the user (if GOD), to upload new data to a datatable 
           # The cana can also be from a backup file
           conditionalPanel(
             condition = "input.select_view == 3",
             materialSwitch(inputId = "from_backup_file",
                            label = "Update from backup file",
                            status = "success", right = TRUE, value = FALSE),
             conditionalPanel(
               condition = "!input.from_backup_file",
               fileInput("input_file", label = "Choose file to upload")
             ),
             conditionalPanel(
               condition = "input.from_backup_file",
               selectInput("select_table_bu", label = "Choose backup table", choices = c(1,2))
             ),
            
             tags$hr(),
             
             dataTableOutput("input_table_data"),
             selectInput("select_table_1", label = "Choose table to update", choices = all_tables),
             selectInput("append_data", label = "Choose action", 
                         choices = c("Override existing data" = 1,
                                     "Append data" = 2)),
             tags$hr(),
             miniButtonBlock(actionButton("update_table", label = "Update table", icon("database"), 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             border = "top")
             
           ),
           
           
           # CORRECTION INTERFACE
           # Allow the masters to correct the bounties in the database.
           # The bounties can be filter by type, status, etc.
           conditionalPanel(
             condition = "input.select_view == 1",
             fluidRow(
               column(4,
                  wellPanel(
                    selectInput("correction_type", "Which quest type(s) to display?", unique(all_quests$type),
                                multiple = T, selected = unique(all_quests$type)),
                    
                    selectInput("correction_quest", "Choose quest", "wait"),
                    selectInput("correction_val", "Which quest validation type(s) to display?", 
                                c("to correct"=-1, 
                                  "false"=0, 
                                  "correct"=1, 
                                  "not visible"=-2,
                                  "to keep"=4,
                                  "to check"=-4,
                                  "whatthefuck"=-3),
                                multiple = T, selected = -1),
                    
                    tags$hr(),
                    htmlOutput("to_do"),
                    htmlOutput("done")
                  )       
               ),
               column(4,
                  actionButton("button_correct", "Correct", icon = icon("check"), 
                               style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                  actionButton("button_tocheck", "To keep", icon = icon("thumbs-o-up"),
                               style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                  actionButton("button_false", "False", icon = icon("remove"), 
                               style="color: #fff; background-color: #d83429; border-color: #d83429"),
                  actionButton("button_flou", "Not visible", icon = icon("eye-slash"), 
                               style="color: #fff; background-color: #e57c21; border-color: #e57c21"),
                  actionButton("button_marrant", "WTF???", icon = icon("rocket"), 
                               style="color: #fff; background-color: #f260e0; border-color: #f260e0"),
                  
                  
                  actionButton("button_unknown", "NEXT", icon = icon("chevron-right")),
                  
                  tags$hr(),
                  htmlOutput("img_title"),
                  imageOutput("myImage", width="100%"),   
                  #uiOutput('myImage'),
                  tags$hr()
                  
               ),
               column(4, 
                  tags$h3("Correct images (from previous year)"),
                  sliderInput("n_correct_img", "Number of images to show", 1, 20, 5),
                  uiOutput('knowledge_base')
              )
             )
           )
         )
       )
    )

  )
  
)