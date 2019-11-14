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

library(shiny)

shinyServer(function(input, output, clientData, session) {
  
  
  rs <- reactiveValues(current_user = 1,
                       current_role = "player",
                       current_group = 1,
                       current_group_name = NULL,
                       current_user_name = NULL,
                       resized_img = NULL,
                       current_img_name  = NULL,
                       current_img_gps = NULL,
                       all_bounties = all_bounties,
                       active_bounties = NULL,
                       update = 1,
                       input_file = NULL, 
                       to_correct = NULL, 
                       current_img = NULL,
                       update_correction = 1,
                       to_correct_current = NULL,
                       correction_done = NULL)
  
  passw <- "test"
  usrname <- "test"
  observe(modalLogin()) # If this is commented, not login is required to access the app
  
  
  
  #--------------------------------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #--------------------------------------------------
  observe({
    req(input$submit_img)
    req(rs$update)
    
    # Get the name of the current group of the user, based on the loggin information
    rs$current_group_name <- all_groups$name[all_groups$id == rs$current_group] 
    
    # Get the name of the current user, based on the loggin information
    rs$current_user_name <- all_users$name[all_users$id == rs$current_user] 
    
    # Get all the active boutines, from the current group
    rs$active_bounties <- dbReadTable(con, "bounties") %>%  
      filter(id_group == rs$current_group, submitted == 0) 
    
  })
  

  

  
  #--------------------------------------------------------------------------------
  #----------------------  LOGIN FUNCTIONS ----------------------------------------
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------
  # LOGIN WITH MODAL DIALOGS
  # Modals are used to manage the users login. 
  # Not sure this is the best option, but it works
  #--------------------------------------------------
  
  
  # This is the basic modal login. it appears when the app is open
  # Disapears when the Action button is clicked. Then the login and password
  # are checked. If they are not OK, this modal is re-openned.
  modalLogin <- function(){
    showModal(modalDialog(
      textInput("username", "Username",
                placeholder = 'Sacha'
      ),
      passwordInput("password", "Password",
                placeholder = 'Pikachu'
      ),
      footer = tagList(
        actionButton("ok", "OK")
      )
    ))
  }
  
  # Failed login modal dialog. Appears if the password and login check is not successful. 
  modalFailed <- function(){
    showModal(modalDialog(
      tags$b("Wrong username or password. Try again"),
      footer = tagList(
        actionButton("ok_2", "OK")
      )
    ))
  }
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    user <- dbReadTable(con, "users") %>%  filter(username == input$username)
    if(nrow(user) == 0){
      removeModal()
      sendSweetAlert(session, title = "Password or login incorrect", 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalLogin()
    }else if (digest(input$password, algo = c("sha256")) == user$password) { # TODO
      removeModal()
      rs$current_user = user$id
      rs$current_group = user$group_id
      rs$current_role = user$role
      # If this is the first time the user logs in, then 
      # the user needs to reset the password. 
      if(user$login == 0){
        statement <- paste0("UPDATE users ",
                            "SET login = 1 ",
                            "WHERE username == '",input$username,"'")
        dbSendStatement(con, statement)
        modalPassword()
      } 

      # Record the user loggin in the log table
      dbWriteTable(con, "log", data.frame(datetime = as.character(now()), 
                                          username = input$username, 
                                          user_id = rs$current_user), 
                   append = TRUE)   
      
      
      
    }else{
      # IF loggin is incorrect, then send Failed modal message
      removeModal()
      sendSweetAlert(session, title = "Password or login incorrect", 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalLogin()
    }
  })
  
  
 # Change the password when login for the first time 
  modalPassword <- function(){
    showModal(modalDialog(
      helpText("This is the first you log into the system. You need to change your password"),
      textInput("current_noma", "NOMA",
                placeholder = "noma"
      ),
      passwordInput("new_password", "New password",
                placeholder = "password"
      ),
      footer = tagList(
        actionButton("psw_change", "OK")
      )
    ))
  }
  
  
  # Update the password of the user
  observeEvent(input$psw_change, {
    
      if(input$current_noma != rs$current_user){
        sendSweetAlert(session, title = "NOMA incorrect", 
                       text = NULL, type = "error",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        modalPassword()
      }else{
        statement <- paste0("UPDATE users ",
                            "SET password = '",digest(input$new_password, algo = c("sha256")),"' ",
                            "WHERE id == '",input$current_noma,"'")
        dbSendStatement(con, statement)
        removeModal()
        sendSweetAlert(session, title = "Password name changed succefully", 
                       text = NULL, type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    
  })
  
  # Set the current user's role, for access from the UI
  # This is needed as the UI changes depending on the user's role
  output$role <- reactive({
    req(rs$current_role)
    switch(rs$current_role,
           "player" = 1,
           "master" = 0, 
           "god" = -1
    )
  })
  
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  
  
  
  #--------------------------------------------------
  # CREATE A MODAL WITH CURRENT TASKS
  # This modal can be triggered by the users
  # It shows the remaining tasks to finish the game. 
  # These tasks are defined by the game parameters. 
  #--------------------------------------------------
  
  observeEvent(input$tasks, {
    
    print("Task modal")

    # Get the group data
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group)# 

    bounties <- merge(temp, dbReadTable(con, "quests"), by.x = "id_quest", by.y = "id")
    n_out <- nrow(bounties[bounties$id_zone == 0,])
    n_quests <- nrow(bounties)
    n_zones <- length(unique(bounties$id_zone))
    
    mess <- paste0("Hello <b>",rs$current_user_name,"</b>, here is your task list for the game:</br></br>",
                   "<ul><li><b>bounties</b> still needed: <b>",
                   (input$req_bounties - n_quests),"</b></li>")
    for(t in unique(all_quests$type)){
      n_type <- nrow(bounties[bounties$type == t,])
      mess <- paste0(mess, "<li><b>",t," bounties</b> still needed: <b>",
                     (input$req_types - n_type),"</b></li>")
    }
    mess <- paste0(mess, "<li><b>out-of-zone bounties</b> still allowed: <b>",
                   (input$req_out_zones - n_out),"</b></li>")
    mess <- paste0(mess, "<li><b>zones</b> still needed: <b>",
                   (input$req_zones - n_zones),"</b></li></ul>")
    
    showModal(modalDialog(
      title = paste0("Progress of team ",rs$current_group_name),
      HTML(mess),
      easyClose = TRUE,
      footer = tagList(
        actionButton("group_name", "Change group name", icon=icon("cogs")),
        modalButton("Dismiss")
      )
    ))
  })
  
  

  
  
  #--------------------------------------------------
  # CREATE A MODAL TO CHANGE GROUP NAME
  # Allows the user to change the name of the group
  # A name is assigned to the group by default
  #--------------------------------------------------
  
  # Not sure this is the best was to do it. But trying to fit functions in the limited space... 
  observeEvent(input$group_name, {
    removeModal()
    modalGroup()
  })
  modalGroup <- function(){
    showModal(modalDialog(
      textInput("new_group_name", "New Group Name",
                placeholder = rs$current_group_name
      ),
      footer = tagList(
        actionButton("group_change", "OK")
      )
    ))
  }
  # Update the name of the group
  observeEvent(input$group_change, {
    if(input$new_group_name %in% dbReadTable(con, "groups")$name){
      removeModal()
      sendSweetAlert(session, title = "Group name already taken", 
                     text = "Sorry this group name already exist. Please choose an other one.", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      statement <- paste0("UPDATE groups ",
                          "SET name = '",input$new_group_name,"' ",
                          "WHERE id == '",rs$current_group,"'")
      dbSendStatement(con, statement)
      rs$current_group_name = input$new_group_name
      removeModal()
      sendSweetAlert(session, title = "Group name changed succefully", 
                     text = paste0("New name is ",input$new_group_name), type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }

  })
  
  
  
   
  #--------------------------------------------------
  # CREATE A MODAL WITH THE RULES
  # Can be triggered by the users. 
  # Displays the rules of the game. 
  #--------------------------------------------------
  
  observeEvent(input$rules, {
    # Check that data object exists and is data frame.
    showModal(modalDialog(
      title = NULL,
      includeMarkdown("www/rules.md"),
      easyClose = TRUE
    ))
  })

  
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  STORE PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #--------------------------------------------------
  observe({
    req(rs$update)
    
    # Get all the active quests
    active_quests <- dbReadTable(con, "quests") %>%  filter(found < input$max_found_quest)
    quest_list <- split(active_quests$id, active_quests$name)
    
    # Update the STORE panel
    updateSelectInput(session, "store_quest", choices = quest_list, selected=1)
    updateSelectInput(session, "store_zone", choices = zone_list, selected=1)
    
  })
  
  
  #--------------------------------------------------
  # LOAD THE IMAGE
  # The image is loaded, resize, then stored in the 
  # dedicated forlder for later use. 
  #--------------------------------------------------
  observe({
    req(input$file)
    inFile <- input$file
    if(is.null(inFile)) return(NULL)
    
    # GET THE IMAGE
    img <- readImage(inFile$datapath)
    new_height <- round(dim(img)[2] / dim(img)[1] * 300)
    img <- resizeImage(image = img, width=300, height = new_height)
    # the image is assigned a random id. That id is stored. 
    rs$current_img_name <- paste0(stri_rand_strings(1, 15), ".jpg")
    writeImage(img, paste0("www/temp/", rs$current_img_name))
    
    # GET THE EXIF
    temp <- read_exif(inFile$datapath)
    dat <- data.frame(image=rs$current_img_name,
                      longitude=0,
                      latitude=0,
                      date="-")
    if(!is.null(temp$GPSLongitude)) dat$longitude <- temp$GPSLongitude
    if(!is.null(temp$GPSLatitude)) dat$latitude <- temp$GPSLatitude
    if(!is.null(temp$DateTimeOriginal)) dat$date <- temp$DateTimeOriginal
    rs$current_img_gps <- dat
    
    # FIND THE DEFAULT ZONE (by finding if the photo is within a predefined zone)
    updateSelectInput(session, "store_zone", choices = zone_list, selected=0)
    for(z in unique(all_zones$id)){
      if(z != 0){
        temp <- all_zones_delim %>% filter(id == z)
        if(point.in.polygon(dat$latitude, dat$longitude, temp$latitude, temp$longitude) == 1){
          updateSelectInput(session, "store_zone", choices = zone_list, selected=z)
        }
      }
    }
    
    
    
  })
  
  #--------------------------------------------------
  # DISPLAY THE IMAGE IN THE STORE PANEL
  #--------------------------------------------------
  output$store_img <- renderImage({
    req(rs$current_img_name)
    filename <- normalizePath(file.path('./www/temp/', rs$current_img_name))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image number", rs$current_img_name))
  }, deleteFile = F)
  
  
  #--------------------------------------------------
  # DISPLAY THE MAP OF THE CURRENT IMAGE
  #--------------------------------------------------
  output$store_map <- renderLeaflet({
    
    current_zone <- all_zones_delim %>% 
      filter(zone_id == input$store_zone)
    zone_to_plot <- merge(current_zone, all_zones, by.x = "zone_id", by.y = "id")
    
    if(!is.null(rs$current_img_gps)){
      map <-  leaflet(rs$current_img_gps) %>%
        addTiles() %>%
        addMarkers(~ longitude, ~ latitude,
                   options = popupOptions(closeButton = FALSE),
                   clusterOptions = markerClusterOptions()) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      for(i in unique(zone_to_plot$group)){
        temp <- zone_to_plot %>% filter(group == i)
        map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                   opacity = 0.0, fillOpacity = 0.02,
                                   color = temp$color, 
                                   # label = as.character(temp$zone_id),
                                   popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                   popupOptions(closeButton = F)
        )
      }
    }else{
      map <-  leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      for(i in unique(zone_to_plot$group)){
        temp <- zone_to_plot %>% filter(group == i)
        map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                   opacity = 0.0, fillOpacity = 0.02,
                                   color = temp$color, 
                                   # label = as.character(temp$zone_id),
                                   popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                   popupOptions(closeButton = F)
        )
      }
    }
    map
    
    

  })
    
  
  
  #-------------------------------------------------
  # STORE THE BOUNTY
  # Store the current image in the database. The users
  # will then be able to submit this image for evaluation
  #-------------------------------------------------
  
  observeEvent(input$store, {
    # Check that data object exists and is data frame.
    
    temp <- data.frame(id = rs$current_img_name, 
                       id_quest = input$store_quest,
                       id_group = rs$current_group,
                       id_zone = input$store_zone,
                       code = stri_rand_strings(1, 12),
                       name = paste(input$store_quest, input$store_zone, rs$current_group, sep="_"),
                       date = rs$current_img_gps$date,
                       longitude = rs$current_img_gps$longitude,
                       latitude = rs$current_img_gps$latitude,
                       submitted = 0,
                       validated = -1,
                       date_submitted = NA,
                       date_stored = as.character(today()),
                       date_validated = NA, 
                       stored_by = rs$current_user,
                       submitted_by = NA,
                       validated_by = NA)
    
    
    active_bounties <- dbReadTable(con, "bounties")   %>%  filter(id_group == rs$current_group) 
    
    rs$update = -rs$update
    

      # Update the database
      dbWriteTable(con, "bounties", temp, append = TRUE)   
      
      # Store the image 
      # rs$all_bounties <- dbReadTable(con, "bounties")   
      img <- readImage(normalizePath(file.path('./www/temp/', rs$current_img_name)))
      writeImage(img, paste0("www/img/", temp$code,".jpg"))
      
      sendSweetAlert(session, title = "Bounty stored", text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  SUBMIT PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #--------------------------------------------------
  observe({
    req(input$submit_img)
    req(rs$update)
    
    # Get all the active quests
    active_quests <- dbReadTable(con, "quests") %>%  filter(found < input$max_found_quest)
    quest_list <- split(active_quests$id, active_quests$name)
    
    selected_bounty <- dbReadTable(con, "bounties") %>%  filter(name == input$submit_img)
    
    
    # Enrich the list of bouties with the information from the Quest and Zone tables
    temp <- merge(rs$active_bounties, all_quests, 
                  by.x = "id_quest", 
                  by.y = "id") %>% 
      mutate(name = name.x, quest = name.y) %>% 
      select(-c(name.y, name.x))
    
    temp <- merge(temp, all_zones, 
                  by.x = "id_zone", 
                  by.y = "id") %>% 
      mutate(name = name.x, zone = name.y) %>% 
      select(-c(name.y, name.x))
    
    if(nrow(temp) > 0){
      bounty_list <- temp$name
      names(bounty_list) <-  paste(temp$quest, " - ", temp$zone )
    }else{
      bounty_list <- c()
    }

    #  Update the SUBMIT panel
    updateSelectInput(session, "submit_img", choices = bounty_list, selected=input$submit_img)
    updateSelectInput(session, "submit_quest", choices = quest_list, selected = selected_bounty$id_quest)
    updateSelectInput(session, "submit_zone", choices = zone_list, selected = selected_bounty$id_zone)
    
  })
  
  
  
  
  #-------------------------------------------------
  # DISPLAY THE IMAGE IN THE SUBMIT PANEL
  # Display the selected image 
  #-------------------------------------------------
  output$submit_img <- renderImage({
    req(input$submit_img)
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group, 
             name == input$submit_img)
    
    filename <- normalizePath(file.path(paste0('./www/img/', temp$code,".jpg")))

    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", temp$code))
  }, deleteFile = F)
  
  
  #-------------------------------------------------
  # SUBMIT THE BOUNTY
  # The users can submit stored bounties for evaluation
  # Once a bounty is submitted, this cannot be undone
  # Bounties can only be submitted once for each quest
  # and only if that quest is still active. 
  #-------------------------------------------------
  
  observeEvent(input$submit, {
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group, 
             name == input$submit_img)
    
    current_quest <- dbReadTable(con, "quests") %>% 
      filter(id == input$submit_quest)
    
    
    active_bounties <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group & submitted == 1) 

    if(input$submit_quest %in% active_bounties$id_quest){
      # Check that data object exists and is data frame.
      sendSweetAlert(session, title = "Bounty already submitted", 
                     text = "You already submitted a bounty for this quest. Please select an other quest.",
                     type = "error",
                     btn_labels = "Ok", html = FALSE, 
                     closeOnClickOutside = TRUE)
    }else if (current_quest$found < input$max_found_quest){
      # Check if the quest is still active
      sendSweetAlert(session, title = "Quest not active anymore", 
                     text = paste0("This quest is not active anymore. It was used for ",
                                   input$max_found_quest, " bounties already."),
                     type = "error",
                     btn_labels = "Ok", html = FALSE, 
                     closeOnClickOutside = TRUE)
      
    }else{
      # if the bounty has not been submitted and and still active, then the players can submit it.
      statement <- paste0("UPDATE bounties ",
                          "SET submitted = 1, ",
                          "id_quest = ",input$submit_quest," ,",
                          " id_zone = ",input$submit_zone," ,",
                          " name = '", paste(input$submit_quest, input$submit_zone, rs$current_group, sep="_"), "' ,",
                          " date_submitted = '",as.character(today()),"' ,",
                          " submitted_by = ",rs$current_user," ",
                          "WHERE code == '",temp$code,"'")
      dbSendStatement(con, statement)
      rs$update <- -rs$update    
      
      # update the number of found bounties submitted to this quests, and make it
      # inactive if necessary (if nmber of bounties is above the threshold)
      statement <- paste0("UPDATE quests ",
                          "SET found = ",(current_quest$found + 1)," ",
                          "WHERE id == ",input$submit_quest)
      dbSendStatement(con, statement)

      sendSweetAlert(session, title = "Bounty submitted", text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  
  #-------------------------------------------------
  # DELETE THE BOUNTY
  # This allows the users to delete stored, unsubmitted
  # bounties from their collection
  #-------------------------------------------------
  observeEvent(input$delete, {
    statement <- paste0("DELETE FROM bounties ",
                        "WHERE name == '",input$submit_img,"'")
    dbSendStatement(con, statement)
    rs$update <- -rs$update
    sendSweetAlert(session, title = "Bounty delete", text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  QUESTS PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #-----------------------------------
  # DISPLAY A TABLE WITH ALL QUESTS
  # this table simply display all the
  # quests from the game
  #-----------------------------------
  output$all_quests = renderDataTable({
    
    
    # Check wether this user wants to see the 
    # active or inactive quests
    
    temp <- dbReadTable(con, "quests") %>% 
      filter(found < input$max_found_quest) %>% 
      select(id, type, group, name, points, found)
    
    if(input$show_inactive){
      temp <- dbReadTable(con, "quests") %>% 
        filter(found >= input$max_found_quest) %>% 
        select(id, type, group, name, points, found)
    }
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      found = color_tile("#e5f5f9", "#2ca25f")
    )),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  
  
  
  
  #-------------------------------------------------
  # DISPLAY A MAP WITH THE DIFERENT ZONES
  # Show the active zones in the game
  #-------------------------------------------------
  output$zones_map <- renderLeaflet({
    
    to_plot <- merge(all_zones_delim, all_zones, by.x = "zone_id", by.y = "id")
    
    map <-  leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    for(i in unique(to_plot$group)){
      temp <- to_plot %>% filter(group == i)
      map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                 opacity = 0.0, fillOpacity = 0.02,
                                 color = temp$color, 
                                 # label = as.character(temp$zone_id),
                                 popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                 popupOptions(closeButton = F)
      )
    }
    map
    
  })
  
  
  #-------------------------------------------------
  # DISPLAY A TEXT DESCRIBING THE QUESTS
  # Say how many quests are left and when they are removed
  #-------------------------------------------------
  output$summary_all_quests <- renderUI({
    
    temp <- dbReadTable(con, "quests")
    
    actives <- temp %>% filter(found < input$max_found_quest) %>% nrow()
    inactives <- temp %>% filter(found >= input$max_found_quest) %>% nrow()
    
    text <- paste0("<b>",actives," quests</b> are still active.</br>",
                   "<b>",inactives," quests</b> are inactive.</br>",
                   "Quests become inactive when they have been found <b>",input$max_found_quest," times</b></br>"
                   )
    
    HTML(text)
    
    
  })
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  BOUNTY PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #----------------------------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  # This table diplays the points of all the 
  # different groupes
  #----------------------------------------------
  
  output$all_bounties = renderDataTable({
    req(rs$update)
    temp1 <- dbReadTable(con, "bounties") %>% filter(submitted == 1)
    temp <- merge(temp1, all_quests, by.x = "id_quest", by.y = "id") %>% 
      ddply(.(id_group), summarise, 
                  bounties = length(points), 
                  theoretical_points = sum(points[submitted == 1]),
                  real_points = sum(points[validated == 1])) 
    
    temp <- merge(temp, dbReadTable(con, "groups"), by.x = "id_group", by.y = "id") %>% 
      mutate(group = name) %>% 
      select(group, bounties, theoretical_points, real_points) %>% 
      arrange(desc(theoretical_points))
    
    dt <- as.datatable(formattable(temp, list(
      real_points = color_tile("#e5f5f9", "#2ca25f")
    )),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  
  #-------------------------------------------
  # DISPLAY A SUMMARY WITH THE GROUP BOUNTIES
  # This text informs the user about synthetic
  # information regarding his/her bounties
  #-------------------------------------------
  
  output$summary_group_bounties <- renderUI({
    temp1 <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      select(id_quest, submitted, validated)
    temp2 <- all_quests %>% 
      select(id, type, name, points)
    temp <- merge(temp1, temp2, by.x = "id_quest", by.y = "id")
    temp1 <- temp %>% filter(submitted == 1)
    temp2 <- temp %>% filter(validated == 1)
    
    text <- paste0("Your group has collected <b>",nrow(temp),"</b> bounties.</br>",
                   "Your group has submitted <b>",nrow(temp1),"</b> bounties, for a total of <b>",sum(temp1$points),"</b> theoretical points.</br>",
                   "Your group has <b>",nrow(temp2),"</b> validated bounties, for a total of <b>",sum(temp2$points),"</b> real points.</br>")
    
    HTML(text)
                   
    
  })
  
  #-------------------------------------------
  # DISPLAY A TABLE WITH THE GROUP BOUNTIES
  # This table displays the bounties stored by
  # by the user. 
  #-------------------------------------------
  output$group_bounties = renderDataTable({
    req(rs$update)
    req(rs$update_correction)
    temp1 <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      select(id_quest, submitted, validated)
    
    temp2 <- all_quests %>% 
      select(id, type, group, name, points)
    
    temp <- merge(temp1, temp2, by.x = "id_quest", by.y = "id") %>% 
      mutate(sub = submitted,
             val = validated) %>% 
      select(type, name, group, points, sub, val)
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      sub = formatter("span", 
                       style = x ~ style(color = ifelse(x == 1, "green", "red")),
                       x ~ icontext(ifelse(x == 1, "ok", "remove"))),
      val = formatter("span", 
                        style = x ~ style(color = ifelse(x ==1, "green", 
                                                         ifelse(x == -1, "orange", "red"))),
                        x ~ icontext(ifelse(x == 1, "ok", 
                                            ifelse(x == -1, "time", "remove"))))#,
    )),
    rownames = F,
    options = list(pageLength = 50), selection=list(mode="single"))
    
    dt
  })
  
  #---------------------------------------------
  # DISPLAY THE IMAGE IN THE BOUNTY PANEL
  # Display the image of the quest selected in 
  # the 'group_bounties' table
  #---------------------------------------------
  output$bounty_img <- renderImage({
    req(input$group_bounties_rows_selected)
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) 
    temp <- temp[input$group_bounties_rows_selected, ] # Get the select row only
    
    
    filename <- normalizePath(file.path(paste0('./www/img/', temp$code,".jpg")))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", temp$code))
  }, deleteFile = F)
  
  
  #--------------------------------------------------------------------------------
  #----------------------  ADMIN PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  #-------------------------------------------------
  # PARAMETERS PANEL  
  # This panel allows GOD to change the game parameters
  # This should not be done during the game!
  #-------------------------------------------------
  
  observeEvent(input$update_params, {
  
      statement <- paste0("UPDATE params ",
                          "SET value = ",input$req_bounties," ",
                          "WHERE param == 'req_bounties'")
      dbSendStatement(con, statement)
      
      statement <- paste0("UPDATE params ",
                          "SET value = ",input$req_types," ",
                          "WHERE param == 'req_types'")
      dbSendStatement(con, statement)
      
      statement <- paste0("UPDATE params ",
                          "SET value = ",input$req_zones," ",
                          "WHERE param == 'req_zones'")
      dbSendStatement(con, statement)
      
      statement <- paste0("UPDATE params ",
                          "SET value = ",input$req_out_zones," ",
                          "WHERE param == 'req_out_zones'")
      dbSendStatement(con, statement)
      
      statement <- paste0("UPDATE params ",
                          "SET value = ",input$max_found_quest," ",
                          "WHERE param == 'max_found_quest'")
      dbSendStatement(con, statement)
      
      sendSweetAlert(session, title = "Parameters changed", text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  
  #-------------------------------------------------
  # CORRECTION PANEL
  # This section allows the masters to correct the 
  # quests submitted by the users. The database is then 
  # directly updated and the corrections visible by the users. 
  #-------------------------------------------------
  
  # Update the different fields of the corrections section
  observe({
    
    # This can be trigger on demand by the different actions in the section
    req(rs$update_correction)
    
    # Load only if the current user is a master
    if(rs$current_role != "master") return(NULL) 
    
    # Load the images to correct. Images are selected based 
    # on the user choices. Only submitted images are selected
    rs$to_correct  <- merge(dbReadTable(con, "bounties") , 
                            all_quests, 
                            by.x = "id_quest", by.y = "id") %>% 
      mutate(name = name.x, 
             quest = name.y) %>% 
      filter(submitted == 1,
             validated %in% input$correction_val,
             type %in% input$correction_type, 
             !code %in% rs$correction_done)
    
    # Get the list of quests to correct 
    # (to be used in the dropdown menu)
    to_correct_quests <- rs$to_correct %>% 
      distinct(id_quest, quest)
    
    # Create the list of quests to be used on the menu
    to_correct <- to_correct_quests$id
    names(to_correct) <- to_correct_quests$quest
    
    # Update the drop down menu based on the user choices
    updateSelectInput(session, "correction_quest", choices = to_correct, selected = to_correct[1])
    
    # Update the game parameters
    rani <- input$req_animal
    updateSliderInput(session, "req_animal", min = 0, max = input$req_bounties, value = rani)
    rveg <- input$req_vegetal
    updateSliderInput(session, "req_vegetal", min = 0, max = input$req_bounties, value = rveg)

  })
  
  
  # Load the data already corrected in this session
  observe({
    if(rs$current_role != "master") return(NULL)
    req(input$correction_quest)
    rs$to_correct_current <-rs$to_correct %>% 
        filter(id_quest == input$correction_quest)
  })
  
  # Define the image that is diplayed for correction
  # 
  observe({
    req(rs$update_correction)
    req(rs$to_correct_current)
    rs$current_img <- rs$to_correct_current %>%
      slice(1)
    
  })
  
  
  # The master is UNABLE TO CORRECT the image
  # Move to the next one without updating the database
  observeEvent(input$button_unknown, {
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is CORRECT
  # Store the result in the database, with a value = 1
  observeEvent(input$button_correct, {
    statement <- paste0("UPDATE bounties ",
                        "SET validated = 1 ,",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code == '",rs$current_img$code,"'")
    dbSendStatement(con, statement)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is CORRECT and VERY NICE (and therefore worth keeping on the side)
  # Store the result in the database, with a value = 1
  observeEvent(input$button_tocheck, {
    statement <- paste0("UPDATE bounties ",
                        "SET validated = 4, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code == '",rs$current_img$code,"'")
    dbSendStatement(con, statement)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is INCORRECT 
  # Store the result in the database, with a value = -0
  observeEvent(input$button_false, {
    statement <- paste0("UPDATE bounties ",
                        "SET validated = 0, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code == '",rs$current_img$code,"'")
    dbSendStatement(con, statement)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is INCORRECT and ABSURD (and therefore worth keeping on the side)
  # Store the result in the database, with a value = -3
  observeEvent(input$button_marrant, {
    statement <- paste0("UPDATE bounties ",
                        "SET validated = -3, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code == '",rs$current_img$code,"'")
    dbSendStatement(con, statement)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The quest in the image is NOT VISIBLE (and therefore considered as false)
  # Store the result in the database, with a value = -2
  observeEvent(input$button_flou, {
    statement <- paste0("UPDATE bounties ",
                        "SET validated = -2,",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code == '",rs$current_img$code,"'")
    dbSendStatement(con, statement)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  
  # Display the current image title, to be displayed on top of the image
  # The title include other information about the image
  
  output$img_title <- renderText({
    req(rs$current_img)
    text <- paste0("<h3>",rs$current_img$quest,"</h3>",
                   "<b>title</b> = ",rs$current_img$name,
                   " / <b>code</b> = ",rs$current_img$code,
                   " / <b>date</b> = ", rs$current_img$date_submitted)
    if(rs$current_img$validated == 1){
      text <- paste0(text, "</br> STATUS = <b>CORRECT")
    }else if(rs$current_img$validated == -1){
      text <- paste0(text, "</br> STATUS = <b>Non-corrigé</b>")
    }
    else if(rs$current_img$validated == 0){
      text <- paste0(text, "</br> STATUS = <b>FAUX</b>")
    }
    else if(rs$current_img$validated == -2){
      text <- paste0(text, "</br> STATUS = <b>FLOU</b>")
    }else if(rs$current_img$validated == -3){
      text <- paste0(text, "</br> STATUS = <b>WTF</b>")
    }
    text
  })
  
  
  # Display the number of images left to check in this section
  output$to_do <- renderText({
    req(rs$to_correct_current)
    n <- rs$to_correct_current %>% 
      filter(validated %in% input$correction_val,
             type %in% input$correction_type)  %>% 
      nrow()
    paste0(n, " remaining images for this quest")
  })
  
  # Display the number of images done in this correction session
  output$done <- renderText({
    req(rs$correction_done)
    paste0(length(rs$correction_done), " image(s) done")
  })
  
  
  # Display the image to correct
  output$myImage <-  renderUI({
    
    req(rs$current_img)
    images <- data.frame(src = paste0(rs$current_img$code, ".jpg")) 
    lightbox_gallery(images,"gallery",display = TRUE, path = "img", width="100%")
  })
  

  # Display the correct images. These images come from a previous
  # BioGO game. They serve a guideline for the masters during the corrections.
  
  output$knowledge_base <-  renderUI({
    
    if(rs$current_role != "master") return(NULL)
    require(input$correction_quest)
    
    q <- input$correction_quest
    
    # Get the correct image
    img_correct <- all_corr %>% 
      filter(quest_id == q & check >= 1)  
    
    images <- data.frame(src = list.files('www/img_corr/')) %>% 
      mutate(src2 = gsub(".jpg", "", src)) %>% 
      filter(src2 %in% img_correct$img) 
    
    images <- images %>% 
      sample_n(min(input$n_correct_img, nrow(images)))

    images$key <- c(1:nrow(images))
    
    lightbox_gallery(images,  "gallery",display = TRUE, path = "img_corr")
  })
  
  
  
  
  #--------------------------------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  # This section allow the masters to visualize 
  # the data in the database. 
  #--------------------------------------------------
  
  # Visualize the selected datatable (via input$select_table)
  output$table_data = renderDataTable({
    if(input$select_table == 1) return(NULL)
    
    as.datatable(formattable(dbReadTable(con, input$select_table) ),
      rownames = F,
      options = list(pageLength = 50))
    
  })
  
  # Download a csv file of the selected dataset
  output$download_table_data <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", input$select_table, ".csv")
    },
    content = function(file) {
      temp <- dbReadTable(con, input$select_table) 
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  #--------------------------------------------------
  # LOAD NEW DATA IN THE DATABASE
  # This section allow the masters to add new data in the 
  # database. The data can be either append to or replace
  # the current one. 
  #--------------------------------------------------
  
  # Display the data to be added into the database. 
  # The data can come either from a new CSV file
  # of from a back datafiles stored in the app. 
  # The data is then stored in rs$input_file
  output$input_table_data <- renderDataTable({
    
    # Load data from a loaded csv files
    if(!input$from_backup_file){
      inFile <- input$input_file
      if (is.null(inFile)) return(NULL)
      temp <- read.csv(inFile$datapath)  
    # Load data from a backup file
    }else{
      temp <- read.csv(paste0("www/bu_db/", input$select_table_bu))  
    }
    
    dt <- as.datatable(formattable(temp),
                       rownames = F,
                       options = list(pageLength = 50))
    rs$input_file = temp
    
    dt
  })
  
  # Update the list of available backup datafiles
  # These will be displayed and can be used to replace the 
  # current data in the database
  observe({
    req(input$from_backup_file)
    all_bu <- list.files("www/bu_db/")
    updateSelectInput(session, "select_table_bu", choices = all_bu, selected = all_bu[1])
  })
  
  
  # Select the table in the DB that match the input file
  # This is done based on the structure of the loaded table
  # The loaded table should have the same column names as one of
  # the tables in the database. This is done only when
  # a table is displayed. Work with both new data (from csv)
  # and with the backup data (as the data come from rs$input_file)
  observe({
    req(rs$input_file)
    temp <- colnames(rs$input_file)
    match <- NULL 
    k <- 0
    for(i in tables_str){
      k <- k+1
      if(all(temp == i)){
        match <- k
      }
    }
    updateSelectInput(session, "select_table_1",  
                      choices = all_tables[match], 
                      selected = all_tables[match])
  })
  
  # # Select the table in the DB that match the backup file
  # # This is done based on the name of the backup table
  # # This is done only when a table is loaded. 
  # observe({
  #   req(input$select_table_bu)
  #   print("backup update")
  #   match <- substr(".csv", "", strsplit(input$select_table_bu, "_")[[1]][2])
  #   updateSelectInput(session, "select_table_1",  
  #                     choices = all_tables[match], 
  #                     selected = all_tables[match])
  # })
  
  
  # Update one of the data table with the loaded data (from CSV file)
  # The data can either replace the current data (input$append_data == 1), or
  # be appened to it (input$append_data == 0)
  observeEvent(input$update_table, {
    
    if(input$append_data == 1){
      # replace the current data in the DB by the new one. 
      # make a backup of current datatable before erasing it
      temp <- dbReadTable(con, input$select_table_1) 
      write_csv(temp, paste0("www/bu_db/", Sys.time(), "_", input$select_table_1, ".csv"))
      dbWriteTable(con, input$select_table_1, rs$input_file, overwrite = TRUE)      
      
    }else{
      # Add the new data to the current one in the DB. 
      # Update the id field in the new data, for consistency. 
      rs$input_file$id <- rs$input_file$id + max(temp$id) + 1 
      dbAppendTable(con, name = input$select_table_1, value = rs$input_file)      
    }
    
    # Success alert
    sendSweetAlert(session, title = paste0("Table ",input$select_table_1," updated"), 
                   text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, 
                   closeOnClickOutside = TRUE)
    
  })
  
  
  
  #--------------------------------------------------
  # PLOT DASHBOARD DATA
  # Plot different information accessibles via the admin tab 
  # in the interface. 
  #--------------------------------------------------
  
  # Plot the repartition of the different types of quests
  output$dashQuests <- renderPlot({
    
    merge(dbReadTable(con, "bounties"), 
                  dbReadTable(con, "quests"), 
                  by.x="id_quest", by.y = "id") %>% 
      ddply(.(difficulty), summarise, prop=length(difficulty)) %>%
      arrange(desc(difficulty)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
      ggplot(aes(x = 2, y = prop, fill = difficulty)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+
        geom_text(aes(y = lab.ypos, label = prop), color = "white", size=10)+
        scale_fill_manual(values = mycols) +
        theme_void() +
        theme(text = element_text(size=6),
              legend.text = element_text(size=10),
              legend.title = element_text(size=10)) + 
        xlim(0.5, 2.5)

  })
  
  
  # Plot the repartition of the different corrections types
  # for all the quests submitted
  
  output$dashCorrections <- renderPlot({
    
    dbReadTable(con, "bounties") %>% 
      mutate(corr = ifelse(validated <= 0, "false", "correct")) %>% 
      mutate(corr = ifelse(validated == -1, "waiting", corr)) %>% 
      ddply(.(corr), summarise, prop=length(corr)) %>%
      arrange(desc(corr)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
      ggplot(aes(x = 2, y = prop, fill = corr)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+
        geom_text(aes(y = lab.ypos, label = prop), color = "white", size=10)+
        scale_fill_manual(values = mycols) +
        theme_void() +
        theme(text = element_text(size=6),
              legend.text = element_text(size=10),
              legend.title = element_text(size=10)) + 
        xlim(0.5, 2.5)
      
  })
  
  
  # Plot the number of submissions per days since
  # the beginning of the game
  
  output$dashSubmissions <- renderPlot({
    
    dbReadTable(con, "bounties") %>% 
      filter(date_stored != "string") %>% 
      mutate(date = ymd(date_stored)) %>% 
      ddply(.(date), summarise, prop=length(date)) %>% 
      ggplot(aes(ymd(date), prop)) + 
        geom_bar(stat = "identity", fill="lightgrey")+
        xlab("Date") +
        ylab("Number of stored bounties")
    
  })
  
  
  # Plot the number of connections to the app per days since
  # the beginning of the game
  
  output$dashConnections <- renderPlot({
  
      dbReadTable(con, "log") %>% 
      filter(datetime != "test") %>% 
      mutate(date = ymd(substr(datetime, 0, 10))) %>% 
      ddply(.(date), summarise, prop=length(date)) %>% 
      ggplot(aes(ymd(date), prop)) + 
        geom_bar(stat = "identity", fill="lightgrey")+
        xlab("Date") +
        ylab("Number of connections")
    
  })
  
  
})