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

# The aim of this script is to create the database for the game BioGO

# install.packages("googlesheets")



# REQUIED LIBRARIES 
library(RSQLite)
library(DBI)
library(googlesheets)
library(tidyverse)
library(rjson)



#--------------------------------------------------
# SETUP DATABASE
con <- dbConnect(RSQLite::SQLite(), "www/database.sql")
dbListTables(con)


#--------------------------------------------------
# QUEST TABLE
# Get the list of quests from google sheet

  ti <- gs_title("LBIR1151 - Questions ")
  
  # vegetal quests
  veg <- gs_read(ti, ws = 1) %>% 
    mutate(type = "végétal")
  
  # animal quests
  ani <- gs_read(ti, ws = 2) %>% 
    mutate(id = id+max(veg$id),
           type = "animal")
  
  quests <- rbind(veg, ani) %>% 
    mutate(status = "active")
  
  dbWriteTable(con, "quests", quests, overwrite = TRUE)

  
#--------------------------------------------------  
# ZONES TABLE
# Get the list of zones form the JSON delim files
  
  zones_raw <- fromJSON(file = "www/maps/map_small.json")[[2]]
  zones_delim <- NULL
  ti <- gs_title("LBIR1151 - Questions ")
  zones <-  gs_read(ti, ws = 3)
  
  for( i in c(1:length(zones_raw))){
    temp <- data.frame(matrix(unlist(zones_raw[[i]][[3]][2]), ncol=2, byrow=T))
    max_id <- 0
    if(!is.null(zones_delim)) max_id <- max(zones_delim$id)
    zones_delim <- rbind(zones_delim, 
                         data.frame(id = c((max_id + 1) : (max_id + nrow(temp))),
                                    group=i, 
                                    latitude=temp$X2, 
                                    longitude = temp$X1, 
                                    zone_id = zones$id[i]))
  }
  dbWriteTable(con, "zones", zones, overwrite = TRUE)
  dbWriteTable(con, "zones_delim", zones_delim)

  
  
#--------------------------------------------------
# GROUPS TABLE
# Get the list of groups from google sheet
  
  ti <- gs_title("LBIR1151 - Questions ")
  groups <- gs_read(ti, ws = 4) 
  dbWriteTable(con, "groups", groups, overwrite = TRUE)  
  
#--------------------------------------------------
# USER TABLE
# Get the list of users from google sheet
  
  ti <- gs_title("LBIR1151 - Questions ")
  users <- gs_read(ti, ws = 5) 
  dbWriteTable(con, "users", users, overwrite = TRUE)
  
#--------------------------------------------------
# BOUNTIES TABLE
# Get the list of bounties from google sheet
  
  ti <- gs_title("LBIR1151 - Questions ")
  bounties <- gs_read(ti, ws = 6) 
  dbWriteTable(con, "bounties", bounties, overwrite = TRUE)    
  
  
#--------------------------------------------------
# PLANNING TABLE
# Get the planning of the game
  
  ti <- gs_title("LBIR1151 - Questions ")
  planning <- gs_read(ti, ws = 7) 
  dbWriteTable(con, "planning", planning, overwrite = TRUE)    
  
  
  
  

#--------------------------------------------------
# LOG TABLE
# get user logging history
  
  ti <- gs_title("LBIR1151 - Questions ")
  log <- gs_read(ti, ws = 8) 
  dbWriteTable(con, "log", log, overwrite = TRUE)    
  
  


dbListTables(con)
