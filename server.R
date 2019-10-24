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


library(shiny)

shinyServer(function(input, output, clientData, session) {
  
  
  rs <- reactiveValues(current_user = 7,
                       current_role = "master",
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
                       current_week = 1, 
                       to_correct = NULL, 
                       current_img = NULL,
                       update_correction = 1,
                       to_correct_current = NULL,
                       correction_done = NULL)
  
  passw <- "test"
  usrname <- "test"
  
  
  #-------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #-------------------------
  observe({
    
    rs$current_group_name <- all_groups$name[all_groups$id == rs$current_group]
    rs$current_user_name <- all_users$name[all_users$id == rs$current_user]
    
    req(input$submit_img)
    req(rs$update)
    
    
    temp <- planning %>% 
      filter(date == today())
    rs$current_week <- temp$week
    
    active_quests <- all_quests %>%  filter(status == "active")
    quest_list <- split(active_quests$id, active_quests$name)
    
    active_groups <- all_groups %>% filter(id != 0)
    group_list <- split(active_groups$id, active_groups$name)
    
    rs$active_bounties <- dbReadTable(con, "bounties") %>%  filter(id_group == rs$current_group, submitted == 0) 

    temp <- merge(rs$active_bounties, all_quests, by.x = "id_quest", by.y = "id") %>% 
      mutate(name = name.x, quest = name.y) %>% 
      select(-c(name.y, name.x))
    
    temp <- merge(temp, all_zones, by.x = "id_zone", by.y = "id") %>% 
      mutate(name = name.x, zone = name.y) %>% 
      select(-c(name.y, name.x))
    
    if(nrow(temp) > 0){
      bounty_list <- temp$name
      names(bounty_list) <-  paste(temp$quest, " - ", temp$zone )
    }else{
      bounty_list <- c()
    }
    
    # STORE PANEL
    updateSelectInput(session, "store_quest", choices = quest_list, selected=1)
    updateSelectInput(session, "store_zone", choices = zone_list, selected=1)

    # SUBMIT PANEL
    updateSelectInput(session, "submit_img", choices = bounty_list, selected=input$submit_img)
    selected_bounty <- dbReadTable(con, "bounties") %>%  filter(name == input$submit_img)
    updateSelectInput(session, "submit_quest", choices = quest_list, selected = selected_bounty$id_quest)
    updateSelectInput(session, "submit_zone", choices = zone_list, selected = selected_bounty$id_zone)
    
  })
  
  observe({
    req(input$from_backup_file)
    all_bu <- list.files("www/bu_db/")
    updateSelectInput(session, "select_table_bu", choices = all_bu, selected = all_bu[1])
  })
  
  # Select the talbe that match the input file
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
  
  
  #---------------------------------------------   LOGIN FUNCTIONS  --------------------------------------------------
  
  
  #-------------------------
  # LOGIN WITH MODAL DIALOGS
  #-------------------------
  observe(modalLogin())
  
  modalLogin <- function(){
    showModal(modalDialog(
      textInput("username", "Username",
                placeholder = 'Sacha'
      ),
      textInput("password", "Password",
                placeholder = 'Pikachu'
      ),
      footer = tagList(
        actionButton("ok", "OK")
      )
    ))
  }
  
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
      if(user$login == 0){
        statement <- paste0("UPDATE users ",
                            "SET login = 1 ",
                            "WHERE username == '",input$username,"'")
        dbSendStatement(con, statement)
        modalPassword()
      } 

      temp <- data.frame(datetime = as.character(now()), username = input$username, user_id = rs$current_user)
      dbWriteTable(con, "log", temp, append = TRUE)   
      
      
      
    }else{
      removeModal()
      sendSweetAlert(session, title = "Password or login incorrect", 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalLogin()
    }
  })
  
  # # when closing the failled message, go back to login modal
  # observeEvent(input$ok_2, {
  #     modalLogin()
  # })
  # 
  
 # Change the password when login for the first time 
  modalPassword <- function(){
    showModal(modalDialog(
      helpText("This is the first you log into the system. You need to change your password"),
      textInput("current_noma", "NOMA",
                placeholder = "noma"
      ),
      textInput("new_password", "New password",
                placeholder = "password"
      ),
      footer = tagList(
        actionButton("psw_change", "OK")
      )
    ))
  }
  # Update the name of the group
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
                       text = NULL,
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    
  })
  
  
  output$role <- reactive({
    req(rs$current_role)
    switch(rs$current_role,
           "player" = 1,
           "master" = 0
    )
  })
  
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  
  
  
  
  #-------------------------
  #-------------------------
  # CREATE A MODAL WITH CURRENT TASKS
  #-------------------------
  observeEvent(input$tasks, {
    
    print("Task modal")
    print(rs$current_week)
    print(rs$current_group)
    # Get the group data
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group & week_submitted == rs$current_week) 
    print(temp)
    week_bounties <- merge(temp, dbReadTable(con, "quests"), by.x = "id_quest", by.y = "id")
    
    n_quests <- nrow(week_bounties)
    n_animal <- nrow(week_bounties[week_bounties$type == "animal",])
    n_vegetal <- nrow(week_bounties[week_bounties$type == "végétal",])
    n_zones <- length(unique(week_bounties$id_zone))
    
    showModal(modalDialog(
      title = paste0("Week's progress of team ",rs$current_group_name),
      HTML(paste0("Hello <b>",rs$current_user_name,"</b>, here is your task list for the week:</br></br>",
                  "<ul><li>Bounties left this week: <b>",(req_bounties-n_quests),"</b></li>",
                  "<li>Animal bounties still needed: <b>",(req_animal - n_animal),"</b></li>",
                  "<li>Vegetal bounties still needed: <b>",(req_vegetal - n_vegetal),"</b></li>",
                  "<li>Zones still needed: <b>",(req_zones - n_zones),"</b></li></ul>")),
      easyClose = TRUE,
      footer = tagList(
        actionButton("group_name", "Change group name", icon=icon("cogs")),
        modalButton("Dismiss")
      )
    ))
  })
  
  
  
  #-----------------------------------------------------------------------------------------------
  
  
  
  #-------------------------
  # CREATE A MODAL TO CHANGE GROUP NAME
  #-------------------------
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
  
  
  
  
  #-----------------------------------------------------------------------------------------------
 

   
  #-------------------------
  # CREATE A MODAL WITH THE RULES
  #-------------------------
  observeEvent(input$rules, {
    # Check that data object exists and is data frame.
    showModal(modalDialog(
      title = NULL,
      includeMarkdown("www/rules.md"),
      easyClose = TRUE
    ))
  })
  
  
  
  #-----------------------------------  STORE PANEL FUNCTIONS ------------------------------------------------------------
  
  
  
  #-------------------------
  # LOAD THE IMAGE, RESIZE IT, GET THE EXIF DATA  AND SAVE IT IN A TEMP FOLDER
  #-------------------------
  observe({
    req(input$file)
    inFile <- input$file
    if(is.null(inFile)) return(NULL)
    
    # GET THE IMAGE
    img <- readImage(inFile$datapath)
    new_height <- round(dim(img)[2] / dim(img)[1] * 300)
    img <- resizeImage(image = img, width=300, height = new_height)
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
          print(paste0("zone = ", z))
        }
      }
    }
    
    
    
  })
  
  #-------------------------
  # DISPLAY THE IMAGE IN THE STORE PANEL
  #-------------------------
  output$store_img <- renderImage({
    req(rs$current_img_name)
    filename <- normalizePath(file.path('./www/temp/', rs$current_img_name))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image number", rs$current_img_name))
  }, deleteFile = F)
  
  
  #-------------------------
  # DISPLAY THE MAP OF THE CURRENT IMAGE
  #-------------------------
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
    
  
  
  #-------------------------
  # STORE THE BOUNTY
  #-------------------------
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
                       week_submitted = NA,
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
  
  
  
  
  
  #-----------------------------------  SUBMIT PANEL FUNCTIONS ------------------------------------------------------------
  
  
  
  #-------------------------
  # DISPLAY THE IMAGE IN THE SUBMIT PANEL
  #-------------------------
  output$submit_img <- renderImage({
    req(input$submit_img)
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group, 
             name == input$submit_img)
    
    filename <- normalizePath(file.path(paste0('./www/img/', temp$code,".jpg")))

    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", temp$code))
  }, deleteFile = F)
  
  
  #-------------------------
  # SUBMIT THE BOUNTY
  #-------------------------
  observeEvent(input$submit, {
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group, 
             name == input$submit_img)
    
    active_bounties <- dbReadTable(con, "bounties")   %>%  filter(id_group == rs$current_group & submitted == 1) 
    week_bounties <- active_bounties   %>%  
      filter(week_submitted == rs$current_week) 

    if(input$submit_quest %in% active_bounties$id_quest){
      # Check that data object exists and is data frame.
      sendSweetAlert(session, title = "Bounty already submitted", 
                     text = "You already submitted a bounty for this quest. Please select an other quest.", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      
    }else if(nrow(week_bounties) >= req_bounties){
      sendSweetAlert(session, title = "Total number of bounties reached", 
                     text = paste0("You already submitted ",req_bounties," bounties this week. Congratulation! You can still store new bounties but have to wait next week to submit more. "), 
                     type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      
      
      statement <- paste0("UPDATE bounties ",
                          "SET submitted = 1, ",
                          "id_quest = ",input$submit_quest," ,",
                          " id_zone = ",input$submit_zone," ,",
                          " name = '", paste(input$submit_quest, input$submit_zone, rs$current_group, sep="_"), "' ,",
                          " date_submitted = '",as.character(today()),"' ,",
                          " submitted_by = ",rs$current_user," ,",
                          " week_submitted = ",rs$current_week," ",
                          "WHERE code == '",temp$code,"'")
      print(statement)
      dbSendStatement(con, statement)
      rs$update <- -rs$update    
      sendSweetAlert(session, title = "Bounty submitted", text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  
  #-------------------------
  # DELETE THE BOUNTY
  #-------------------------
  observeEvent(input$delete, {
    statement <- paste0("DELETE FROM bounties ",
                        "WHERE name == '",input$submit_img,"'")
    dbSendStatement(con, statement)
    rs$update <- -rs$update
    sendSweetAlert(session, title = "Bounty delete", text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  
  
  #-----------------------------------  QUESTS PANEL FUNCTIONS ------------------------------------------------------------
  
  
  
  #-------------------------
  # DISPLAY A TABLE WITH ALL QUESTS
  #-------------------------
  output$all_quests = renderDataTable({
    
    temp <- all_quests %>% 
      select(id, type, name, points)
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      type = formatter("span", 
                       style = x ~ style(color = ifelse(x =="végétal", "green", "magenta")),
                       x ~ icontext(ifelse(x == "végétal", "leaf", "piggy-bank"), ifelse(x=="végétal", "", "")))
    )),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  
  
  
  #-------------------------
  # DISPLAY A MAP WITH THE DIFERENT ZONES
  #-------------------------
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
  
  
  
  #-----------------------------------  BOUNTY PANEL FUNCTIONS ------------------------------------------------------------
  
  
  
  #-------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  #-------------------------
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
  
  
  #-------------------------
  # DISPLAY A SUMMARY WITH THE GROUP BOUNTIES
  #-------------------------
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
  
  #-------------------------
  # DISPLAY A TABLE WITH THE GROUP BOUNTIES
  #-------------------------
  output$group_bounties = renderDataTable({
    req(rs$update)
    req(rs$update_correction)
    temp1 <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      select(id_quest, week_submitted, submitted, validated)
    
    temp2 <- all_quests %>% 
      select(id, type, name, points)
    
    temp <- merge(temp1, temp2, by.x = "id_quest", by.y = "id") %>% 
      mutate(sub = submitted,
             val = validated,
             week = week_submitted) %>% 
      select(type, week, name, points, sub, val)
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      type = formatter("span", 
                       style = x ~ style(color = ifelse(x =="végétal", "green", "magenta")),
                       x ~ icontext(ifelse(x == "végétal", "leaf", "piggy-bank"), ifelse(x=="végétal", "", ""))),
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
    options = list(pageLength = 50))
    
    dt
  })
  
  
  
  #-----------------------------------  ADMIN PANEL FUNCTIONS ------------------------------------------------------------
  
  
  
  
  #-------------------------
  # CORRECTION PANEL
  #-------------------------
  
  
  
  observe({
    
    req(rs$update_correction)
    if(rs$current_role != "master") return(NULL)
    
    # GET ALL THE IMAGE TO CORRECT
    rs$to_correct  <- merge(dbReadTable(con, "bounties") , all_quests, by.x = "id_quest", by.y = "id") %>% 
      mutate(name = name.x, 
             quest = name.y) %>% 
      filter(submitted == 1,
             validated %in% input$correction_val,
             week_submitted %in% input$correction_week,
             type %in% input$correction_type) %>% 
      filter(!code %in% rs$correction_done)
    
    
    to_correct_quests <- rs$to_correct %>% 
      distinct(id_quest, quest)
    to_correct <- to_correct_quests$id
    names(to_correct) <- to_correct_quests$quest
    
    active_quests <- all_quests %>%  
      filter(id %in% to_correct_quests$id &
               status == "active")
    
    # CORRECTION PANEL
    updateSelectInput(session, "select_table", choices = all_tables, selected = "groups")
    updateSelectInput(session, "select_table_1", choices = all_tables, selected = "groups")
    updateSelectInput(session, "correction_quest", choices = to_correct, selected = to_correct[1])

  })
  
  
  
  # Load the data already done
  observe({
    req(input$correction_quest)
    rs$to_correct_current <-rs$to_correct %>% 
        filter(id_quest == input$correction_quest)
  })
  
  
  
  # Define the current image
  observe({
    req(rs$update_correction)
    req(rs$to_correct_current)
    rs$current_img <- rs$to_correct_current %>%
      slice(1)
    
  })
  
  
  # Correct
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
  
  
  # WHAT THE FUDGE ?
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
  
  # UNKNOWN
  observeEvent(input$button_unknown, {
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # FALsE
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
  
  # NOT VISIBLE
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
  
  
  # TO CHECK
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
  
  
  # The image title
  output$img_title <- renderText({
    req(rs$current_img)
    text <- paste0("<h3>",rs$current_img$quest,"</h3>",
                   "<b>title</b> = ",rs$current_img$name,
                   " / <b>code</b> = ",rs$current_img$code,
                   " / <b>date</b> = ", rs$current_img$date_submitted, 
                   " / <b>week</b> = ", rs$current_img$week_submitted)
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
  
  # The number of images left to check
  output$to_do <- renderText({
    req(rs$to_correct_current)
    n <- rs$to_correct_current %>% 
      filter(validated %in% input$correction_val,
             week_submitted %in% input$correction_week,
             type %in% input$correction_type)  %>% 
      nrow()
    paste0(n, " remaining images for this quest")
  })
  
  # The number of images done in this correction
  output$done <- renderText({
    req(rs$correction_done)
    paste0(length(rs$correction_done), " image(s) done")
  })
  
  
  # Display the image to correct
  
  output$myImage <-  renderUI({
    
    req(rs$current_img)
    
    # if(nrow(rs$current_img) == 0){
    #   filename <- normalizePath(file.path('./www/img/', "done.png"))
    # }else{
    #   toshow <- paste0(rs$current_img$code, ".jpg")
    #   filename <- normalizePath(file.path('./www/img/', toshow))
    # } 
    
    images <- data.frame(src = paste0(rs$current_img$code, ".jpg")) 

    
    lightbox_gallery(images,"gallery",display = TRUE, path = "img", width="100%")
  })
  
  # 
  # output$myImage <- renderImage({
  #   req(rs$current_img)
  #   
  #   if(nrow(rs$current_img) == 0){
  #     filename <- normalizePath(file.path('./www/img/', "done.png"))
  #   }else{
  #     toshow <- paste0(rs$current_img$code, ".jpg")
  #     filename <- normalizePath(file.path('./www/img/', toshow))
  #   }
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename, alt = paste("Image number", input$n))
  # }, deleteFile = F)
  # 
  # Display correct images
  
  output$knowledge_base <-  renderUI({
    
    if(rs$current_role != "master") return(NULL)
    require(input$correction_quest)
    
    print("HELLO")
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
  
  
  
  
  
  #-------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  #-------------------------
  output$table_data = renderDataTable({
    if(input$select_table == 1) return(NULL)
    temp <- dbReadTable(con, input$select_table) 
    
    dt <- as.datatable(formattable(temp),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  # Downloadable csv of selected dataset ----
  output$download_table_data <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", input$select_table, ".csv")
    },
    content = function(file) {
      temp <- dbReadTable(con, input$select_table) 
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  #-------------------------
  # LOAD NEW DATA IN THE DATABASE
  #-------------------------
  
  output$input_table_data <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    # req(input$input_file)
    
    # if (is.null(inFile))
    #   return(NULL)
    
    if(!input$from_backup_file){
      inFile <- input$input_file
      if (is.null(inFile)) return(NULL)
      temp <- read.csv(inFile$datapath)  
    }else{
      temp <- read.csv(paste0("www/bu_db/", input$select_table_bu))  
    }
    
    
    dt <- as.datatable(formattable(temp),
                       rownames = F,
                       options = list(pageLength = 50))
    rs$input_file = temp
    
    dt
  })
  
  
  observeEvent(input$update_table, {

    if(input$append_data == 1){
      # make a bckup of current datatable before erasing it
      temp <- dbReadTable(con, input$select_table_1) 
      write_csv(temp, paste0("www/bu_db/", Sys.time(), "_", input$select_table_1, ".csv"))
      dbWriteTable(con, input$select_table_1, rs$input_file, overwrite = TRUE)      
      
    }else{
      temp <- dbReadTable(con, input$select_table_1) 
      rs$input_file$id <- rs$input_file$id + max(temp$id) + 1
      dbAppendTable(con, name = input$select_table_1, value = rs$input_file)      
    }
    
    sendSweetAlert(session, title = paste0("Table ",input$select_table_1," updated"), text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    
  })
  
  
  
  
})