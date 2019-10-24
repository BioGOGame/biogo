# Copyright © 2019, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2019 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.


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
    
    
    # STORE PANEL 
    # This panel is for th storage, in the group's chess, individual pictures
    miniTabPanel("Store", icon = icon("image"),
      miniContentPanel(
        helpText(help_store),
        fileInput("file", label = "Needs to be a jpg file"),
        selectInput("store_quest", label = "Choose quest", choices = c(1,2)),
        # selectInput("store_group", label = "Choose your group", choices = c(1,2)),
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
    
    
    
    # SUBMIT
    miniTabPanel("Submit", icon = icon("check"),
       miniContentPanel(
          helpText(help_submit),
          selectInput("submit_img", label = "Choose a stored bounty", choices = c(1,2,3)),
          selectInput("submit_quest", label = "Choose quest", choices = c(1,2,3)),
          # selectInput("submit_group", label = "Choose group", choices = c(1,2,3)),
          selectInput("submit_zone", label = "Choose zone", choices = c(1,2,3)),
          imageOutput("submit_img", width="auto")
      ),
      miniButtonBlock(actionButton("submit", 
                                   label = "Submit bounty", 
                                   icon("check"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      
                      actionButton("delete", 
                                   label = "Delete bounty", 
                                   icon("trash-o"), 
                                   style="color: #fff; background-color: #d83429; border-color: #98231b"),
                      border = "top")
    ),
    
    
    
    # BOUNTY
    miniTabPanel("Bounty", icon = icon("star outline"),
      miniContentPanel(
        materialSwitch(inputId = "show_all_bounties",
                       label = "Show all groups",
                       status = "success", right = TRUE, value = FALSE),
        conditionalPanel(
          condition = "input.show_all_bounties == true",
          # h3("All groups' bounties"),
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
          dataTableOutput("group_bounties")
        )
      )
    ),

    # QUESTS /  ADMIN PANEL
    miniTabPanel("Quests", icon = icon("list alternate outline"),
       miniContentPanel(
         # QUESTS
         conditionalPanel(
          condition = "output.role == 1",
            materialSwitch(inputId = "show_zones",
                           label = "Show map of zones",
                           status = "success", right = TRUE, value = FALSE),
            tags$hr(),
            conditionalPanel(
              condition = "input.show_zones == false",
              dataTableOutput("all_quests")
            ),
            conditionalPanel(
              condition = "input.show_zones == true",
              leafletOutput("zones_map")
            )
          ),
         
         # ADMIN
         conditionalPanel(
           # VIEW DATATABLES
           condition = "output.role == 0",
           selectInput("select_view", label = "Choose view", 
                       choices = c("Dashboard" = 0,
                                    "Correction interface" = 1,
                                   "Table view" = 2,
                                   "Admin" = 3)),
           # materialSwitch(inputId = "show_correction",
           #                label = "Show correction interface",
           #                status = "success", right = TRUE, value = FALSE),
           tags$hr(),
           
           
           # DASHBOARD
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
           
           
           # VIEW DATA
           conditionalPanel(
             condition = "input.select_view == 2",
             selectInput("select_table", label = "Choose table", choices = c(1,2)),
             downloadButton("download_table_data", "Download"),
             tags$hr(),
             dataTableOutput("table_data")
             
           ),
           # UPLOAD INTERFACE
           
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
             selectInput("select_table_1", label = "Choose table to update", choices = c(1,2)),
             selectInput("append_data", label = "Choose action", 
                         choices = c("Override existing data" = 1,
                                     "Append data" = 2)),
             tags$hr(),
             miniButtonBlock(actionButton("update_table", label = "Update table", icon("database"), 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             border = "top")
             
           ),
           
           
           # CORRECTION INTERFACE
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
                    
                    selectInput("correction_week", "Which week(s) to display?", c(1:4),
                                multiple = T, selected = c(1:4)),
                    
                    tags$hr(),
                    htmlOutput("to_do"),
                    htmlOutput("done")
                  )       
               ),
               column(4,
                  actionButton("button_correct", "Correct", icon = icon("check"), 
                               style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                  actionButton("button_tocheck", "To keep", icon = icon("thumbs-o-up"),style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                  actionButton("button_false", "False", icon = icon("remove"), 
                               style="color: #fff; background-color: #d83429; border-color: #d83429"),
                  actionButton("button_flou", "Not visible", icon = icon("eye-slash"), 
                               style="color: #fff; background-color: #e57c21; border-color: #e57c21"),
                  actionButton("button_marrant", "WTF???", icon = icon("rocket"), 
                               style="color: #fff; background-color: #f260e0; border-color: #f260e0"),
                  
                  
                  actionButton("button_unknown", "NEXT", icon = icon("chevron-right")),
                  
                  tags$hr(),
                  htmlOutput("img_title"),
                  # imageOutput("myImage", width="100%"),   
                  uiOutput('myImage'),
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