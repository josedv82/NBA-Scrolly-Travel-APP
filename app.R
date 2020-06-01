
#NBA Travel Scrolling APP
#Jose Fernandez
# May 2020

##################

#import library#####

library(shiny)
library(scrollytell)
library(shinyWidgets)
library(plotly)
library(maps)
library(geosphere)
library(tidyverse)
library(feather)


#import feathers####
articles <- read_feather("article.feather")
sche <- read_feather("sche.feather")
acities <- read_feather("acities.feather")
toronto <- c("Toronto", 43.65, -79.38) #for map




#### UI code####

ui <- fluidPage(
  
  setBackgroundColor("#e9ebee"),
  
  # Code to suppress warning messages while data is loading on-screen 
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  
  # Parallax introductory image
  fluidRow(
    
    HTML(
      '<style>
      .parallax {
      /* The image used */
      background-image: url("https://a.espncdn.com/photo/2017/0105/r168695_1296x729_16-9.jpg");
      
      /* Set a specific height */
      height: 500px;
      
      /* Create the parallax scrolling effect */
      background-attachment: fixed;
      background-position: center;
      background-repeat: no-repeat;
      background-size: cover;
      filter: grayscale(100%) blur(0px) sharpen(2px);
      -webkit-filter: grayscale(100%);
      }
      </style>
      <!-- Container element -->
      <div class="parallax"></div>')
    ),
  
  # Article title and subtitle
  fluidRow(HTML("<center>
                <h1>UP IN THE AIR</h1>
                <h3>What a season of traveling looks like in the NBA</h3>
                <p style='size:18px'> by <a href='https://twitter.com/jfernandez__' target='_blank'>Jose Fernandez</a></p>
                </center>")
  ),
  
  br(),
  br(),
  
  fluidRow(
    column(1),
    column(10,
    # Introduction
    fluidRow(
    column(2),
    column(8, 
    br(),
    p("The NBA has one of the most demanding traveling schedules in American sports.
    Unlike the NFL where teams play once a week or MLB with series over multiple days, 
    NBA teams can play on average 3 games per week with multiple travels in short periods of time."),
    br(),
    p("The purpose of this app is to highlight differences in overall mileage covered in a season by NBA teams, 
      as well as a visual estimation of what a year of traveling may look like for each team."),
    br(),
    p("For a deeper look into the density of the schedule in the NBA, including individual player loads please visit my",
      a("NBA Game Density APP.", href = "https://josedv.shinyapps.io/NBASchedule/", target="_blank")),
    br()
    ),
    column(2)
    ))),
           
    br(), 
    br(),
  
  fluidRow(column(width = 12, align = "center",
                  
                  
                  radioGroupButtons(
                    inputId = "season",
                    label = "Select Season",
                    choices = c("2017-18", "2018-19", "2019-20"),
                    selected = "2019-20",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: #29487d"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: #29487d"))
                  )
                  
                  )),
  
  br(),
  br(),
  h1(icon("angle-down"), align = "center"),
  br(),
  br(),
  
  fluidRow(style="padding:25px",
  
             
   scrolly_container("scr",
                     
      scrolly_graph(
        br(),
        br(),
        br(),
        br(),
        br(), 
        
        plotlyOutput("dotPlot", width = "80%", height = "200px"),
        plotOutput("mapPlot", width = "90%", height = "400px")
        
        ), 
      
                     
      scrolly_sections(
      
      HTML('<center>'),
        
      scrolly_section(id = "Atlanta Hawks",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/atlanta-hawks-logo.png', width = "200px"),
                      tags$h2("Atlanta Hawks", style="color:#29487d")),
      
      scrolly_section(id = "Boston Celtics",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/boston-celtics-logo.png', width = "200px"),
                      tags$h2("Boston Celtics", style="color:#29487d")),
      
      scrolly_section(id = "Brooklyn Nets",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png', width = "200px"),
                      tags$h2("Brooklyn Nets", style="color:#29487d")),
      
      scrolly_section(id = "Charlotte Hornets",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/charlotte-hornets-logo.png', width = "200px"),
                      tags$h2("Charlotte Hornets", style="color:#29487d")),
      
      scrolly_section(id = "Chicago Bulls",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/chicago-bulls-logo.png', width = "200px"),
                      tags$h2("Chicago Bulls", style="color:#29487d")),
      
      scrolly_section(id = "Cleveland Cavaliers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/cleveland-cavaliers-logo.png', width = "200px"),
                      tags$h2("Cleveland Cavaliers", style="color:#29487d")),
      
      scrolly_section(id = "Dallas Mavericks",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/dallas-mavericks-logo.png', width = "200px"),
                      tags$h2("Dallas Mavericks", style="color:#29487d")),
      
      scrolly_section(id = "Denver Nuggets",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/denver-nuggets-logo.png', width = "200px"),
                      tags$h2("Denver Nuggets", style="color:#29487d")),
      
      scrolly_section(id = "Detroit Pistons",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/detroit-pistons-logo.png', width = "200px"),
                      tags$h2("Detroit Pistons", style="color:#29487d")),
      
      scrolly_section(id = "Golden State Warriors",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/golden-state-warriors-logo.png', width = "200px"),
                      tags$h2("Golden State Warriors", style="color:#29487d")),
      
      scrolly_section(id = "Houston Rockets",
                      tags$img(src='https://cdn.clipart.email/f62c1b980afe560d4ab30f9a43dd22a5_houston-rockets-wikipedia_1200-1591.png', width = "200px"),
                      tags$h2("Houston Rockets", style="color:#29487d")),
      
      scrolly_section(id = "Indiana Pacers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/indiana-pacers-logo.png', width = "200px"),
                      tags$h2("Indiana Pacers", style="color:#29487d")),
      
      scrolly_section(id = "Los Angeles Clippers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-clippers-logo.png', width = "200px"),
                      tags$h2("Los Angeles Clippers", style="color:#29487d")),
      
      scrolly_section(id = "Los Angeles Lakers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-lakers-logo.png', width = "200px"),
                      tags$h2("Los Angeles Lakers", style="color:#29487d")),
      
      scrolly_section(id = "Memphis Grizzlies",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/memphis-grizzlies-logo.png', width = "200px"),
                      tags$h2("Memphis Grizzlies", style="color:#29487d")),
      
      scrolly_section(id = "Miami Heat",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png', width = "200px"),
                      tags$h2("Miami Heat", style="color:#29487d")),
      
      scrolly_section(id = "Milwaukee Bucks",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/milwaukee-bucks-logo.png', width = "200px"),
                      tags$h2("Milwaukee Bucks", style="color:#29487d")),
      
      scrolly_section(id = "Minnesota Timberwolves",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-timberwolves-logo.png', width = "200px"),
                      tags$h2("Minnesota Timberwolves", style="color:#29487d")),
      
      scrolly_section(id = "New Orleans Pelicans",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/new-orleans-pelicans-logo.png', width = "200px"),
                      tags$h2("New Orleans Pelicans", style="color:#29487d")),
      
      scrolly_section(id = "New York Knicks",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/new-york-knicks-logo.png', width = "200px"),
                      tags$h2("New York Knicks", style="color:#29487d")),
      
      scrolly_section(id = "Oklahoma City Thunder",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/oklahoma-city-thunder-logo.png', width = "200px"),
                      tags$h2("Memphis Grizzlies", style="color:#29487d")),
      
      scrolly_section(id = "Orlando Magic",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/orlando-magic-logo.png', width = "200px"),
                      tags$h2("Orlando Magic", style="color:#29487d")),
      
      scrolly_section(id = "Philadelphia 76ers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/philadelphia-76ers-logo.png', width = "200px"),
                      tags$h2("Philadelphia 76ers", style="color:#29487d")),
      
      scrolly_section(id = "Phoenix Suns",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/phoenix-suns-logo.png', width = "200px"),
                      tags$h2("Phoenix Suns", style="color:#29487d")),
      
      scrolly_section(id = "Portland Trail Blazers",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/portland-trail-blazers-logo.png', width = "200px"),
                      tags$h2("Portland Trail Blazers", style="color:#29487d")),
      
      scrolly_section(id = "Sacramento Kings",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/sacramento-kings-logo.png', width = "200px"),
                      tags$h2("Sacramento Kings", style="color:#29487d")),
      
      scrolly_section(id = "San Antonio Spurs",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/san-antonio-spurs-logo.png', width = "200px"),
                      tags$h2("San Antonio Spurs", style="color:#29487d")),
      
      scrolly_section(id = "Toronto Raptors",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/toronto-raptors-logo.png', width = "200px"),
                      tags$h2("Toronto Raptors", style="color:#29487d")),
      
      scrolly_section(id = "Utah Jazz",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/utah-jazz-logo.png', width = "200px"),
                      tags$h2("Utah Jazz", style="color:#29487d")),
      
      scrolly_section(id = "Washington Wizards",
                      tags$img(src='https://cdn.freebiesupply.com/images/thumbs/2x/washington-wizards-logo.png', width = "200px"),
                      tags$h2("Washington Wizards", style="color:#29487d")),
      
      HTML('<center>') 
    
    )#scrolly sections
   )#scrolly container
 ),#fluidrow
 
 tags$hr(style="border-color: #29487d;"),
 
 fluidRow(style="padding:30px", 
      
      HTML('<iframe width="100%" height = "600px" src="https://www.youtube.com/embed/Cl9OJWp6kwQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
 
 ),
 
 tags$hr(style="border-color: #29487d;"),
 
 fluidRow(style="padding:30px",
          
 p("Disclaimer: Total mileage and flight paths are just estimations and may not accurately represent 
   how teams actually manage their travels."),
 p("Thanks to others for posting examples of scrollytelling in shiny. Especially Scott Davis as his",
   a("Dream Team App", href = "https://sdavis.shinyapps.io/dream_team/", target="_blank"), "was a source of inspiration."),
 p("This app relies on multiple packages including",
   a("Tidyverse", href = "https://www.tidyverse.org/", target="_blank"), "for data manipuilation and plotting.",
   a("Geosphere", href = "https://cran.r-project.org/web/packages/geosphere/geosphere.pdf", target="_blank"), "and",
   a("Maps", href = "https://cran.r-project.org/web/packages/maps/maps.pdf", target="_blank"), "to calculate distance
   between cities and flight paths representations. And of course the", 
   a("scrollytell package", href = "https://github.com/statistiekcbs/scrollytell", target="_blank"), "for scrollying effects."),
 p("Video: NBA Rooks. Life on the Road. Sourced from the official National Basketball Association",
   a("Youtube channel", href = "https://www.youtube.com/user/NBA/about", target="_blank")),
 p("Header Image. Source: ",
   a("ESPN", href = "https://www.espn.co.uk/nba/story/_/id/18415599/travel-take-toll-athletes-lose-sleep-it", target="_blank"))

),#fluidrow


br(),
br()
 
 
)#fluid page

# Define server logic####
server <- function(input, output, session) {
  
  
  #create reactive datasets####
  
  
  #add months and rolling average for stress
  sche1 <- reactive({ 
    
    sche %>% 
    
    filter(Season == input$season) %>%
    
    mutate(Season = as.factor(Season)) %>%
    group_by(Team) %>%
    
    mutate(Month = ifelse(Month == 1, 'January',
                          ifelse(Month == 2, 'February',
                                 ifelse(Month == 3, 'March',
                                        ifelse(Month == 4, 'April',
                                               ifelse(Month == 5, 'May',
                                                      ifelse(Month == 10, 'October',
                                                             ifelse(Month == 11, 'November', 'December')))))))) %>%
    
    select(-Time) %>%
    
    mutate(elapsed = Date - lag(Date)) %>%
    
    mutate(Travel = ifelse(City != lag(City), "y", "n")) %>%
    
    mutate(Zone = ifelse(City == "Toronto", "Eastern",
                  ifelse( City == "New Orleans", "Central",
                  ifelse( City == "Houston", "Central",
                  ifelse( City == "Oklahoma", "Central",
                  ifelse( City == "New York", "Eastern",
                  ifelse( City == "Charlotte", "Eastern",
                  ifelse( City == "Miami", "Eastern",
                  ifelse( City == "Phoenix", "Mountain",
                  ifelse( City == "Utah", "Mountain",
                  ifelse( City == "Los Angeles", "Pacific",
                  ifelse( City == "Dallas", "Central",
                  ifelse( City == "Milwaukee", "Central",
                  ifelse( City == "Philadelphia", "Eastern",
                  ifelse( City == "Minnesota", "Central",
                  ifelse( City == "San Francisco", "Pacific",
                  ifelse( City == "Portland", "Pacific",
                  ifelse( City == "Denver", "Mountain",
                  ifelse( City == "Sacramento", "Pacific",
                  ifelse( City == "Boston", "Eastern",
                  ifelse( City == "Detroit", "Eastern",
                  ifelse( City == "Memphis", "Central",
                  ifelse( City == "Cleveland", "Eastern",
                  ifelse( City == "Chicago", "Central",
                  ifelse( City == "Orlando", "Eastern",
                  ifelse( City == "Atlanta", "Eastern",
                  ifelse( City == "Washington D.C.", "Eastern",
                  ifelse( City == "Indiana", "Eastern",
                  ifelse( City == "San Antonio", "Central", "other")))))))))))))))))))))))))))))
    
   
  
  })
  
  
  #code to add opponent rolling stress
  
  sche2 <- reactive ({
  
  a <- sche1() %>%
    mutate(conc = paste(Team, Date, Opponent)) %>%
    select(aSeason = Season, aTeam = Team, aMonth = Month, aDate = Date, aOpponent = Opponent, aLocation = Location, aCity = City, aArena = Arena, aTeam_pts = Team_pts, aOpp_pts = Opp_pts, conc)
  
  b <- sche1() %>%
    mutate(conc = paste(Opponent, Date, Team)) %>%
    select(bSeason = Season, bTeam = Team, bMonth = Month, bDate = Date, bOpponent = Opponent, bLocation = Location, bCity = City, bArena = Arena, bTeam_pts = Team_pts, bOpp_pts = Opp_pts, conc)
  
  
  #final table with both local and opponent rolling stress
   full_join(a,b, by = "conc") %>%
    select(Season= aSeason, Month = aMonth, Date = aDate, Location = aLocation, City = aCity, Arena = aArena, Team = aTeam, Opponent = aOpponent, Team_pts = aTeam_pts, Opp_pts = aOpp_pts) %>%
    mutate(`W/L` = ifelse(Team_pts > Opp_pts, "W", "L")) %>%
    ungroup() %>%
    select(Season, Month, Date, Location, City, Arena, Team, Opponent, `W/L`, Team_pts, Opp_pts) %>%
    arrange(Season, Team)
   
  })
  
  
  #code with cities coordinates and travel details for mapping
  
  cities <- reactive ({
    
    us.cities %>% 
    filter(name == "Houston TX" | 
             name == "Oklahoma City OK" |
             name == "New York NY" |
             name == "Charlotte NC" | 
             name == "Miami FL" |
             name == "Phoenix AZ" | 
             name == "Salt Lake City UT" |
             name == "Los Angeles CA" | 
             name == "Dallas TX" |
             name == "Milwaukee WI" | 
             name == "Philadelphia PA" | 
             name == "Minneapolis MN" |
             name == "San Francisco CA" | 
             name == "Portland OR" |
             name == "Denver CO" |
             name == "Sacramento CA" | 
             name == "Boston MA" |
             name == "Detroit MI" | 
             name == "Memphis TN" |
             name == "Cleveland OH" | 
             name == "Chicago IL" |
             name == "Atlanta GA" |
             name == "WASHINGTON DC" |
             name == "Indianapolis IN" |
             name == "San Antonio TX" |
             name == "New Orleans LA" |
             name == "Orlando FL") %>%
    
    select(City = name, Latitude = lat, Longitude = long) %>%
    
    mutate(City = ifelse( City == "New Orleans LA", "New Orleans", 
                  ifelse( City == "Houston TX", "Houston",
                  ifelse( City == "Oklahoma City OK", "Oklahoma",
                  ifelse( City == "New York NY", "New York",
                  ifelse( City == "Charlotte NC", "Charlotte",
                  ifelse( City == "Miami FL", "Miami",
                  ifelse( City == "Phoenix AZ", "Phoenix",
                  ifelse( City == "Salt Lake City UT", "Utah",
                  ifelse( City == "Los Angeles CA", "Los Angeles",
                  ifelse( City == "Dallas TX", "Dallas",
                  ifelse( City == "Milwaukee WI", "Milwaukee",
                  ifelse( City == "Philadelphia PA", "Philadelphia",
                  ifelse( City == "Minneapolis MN", "Minnesota",
                  ifelse( City == "San Francisco CA", "San Francisco",
                  ifelse( City == "Portland OR", "Portland",
                  ifelse( City == "Denver CO", "Denver",
                  ifelse( City == "Sacramento CA", "Sacramento",
                  ifelse( City == "Boston MA", "Boston",
                  ifelse( City == "Detroit MI", "Detroit",
                  ifelse( City == "Memphis TN", "Memphis",
                  ifelse( City == "Cleveland OH", "Cleveland",
                  ifelse( City == "Chicago IL", "Chicago",
                  ifelse( City == "Orlando FL", "Orlando",
                  ifelse( City == "Atlanta GA", "Atlanta",
                  ifelse( City == "WASHINGTON DC", "Washington D.C.",
                  ifelse( City == "Indianapolis IN", "Indiana",
                  ifelse( City == "San Antonio TX", "San Antonio", "Toronto")))))))))))))))))))))))))))) %>%
    
    rbind(toronto) %>%
    
    ungroup() %>%
    
    
    #joining master table
    
    full_join(sche2(), by = c("City")) %>%
    
    
    select(Season, Date, Location, Team, Opponent, City, Latitude, Longitude, everything()) %>%
    
    arrange(Season, Team, Date) %>%
    
    group_by(Team, Season) %>%
    
    mutate(destLat = lag(Latitude), destLon = lag(Longitude)) %>%
    
    
    #correcting for missing coordinates for first games of the season for each team
    
    mutate(destLat = ifelse(is.na(destLat) & Team == "Atlanta Hawks", acities %>% filter(City == "Atlanta") %>% select(Latitude) %>% as.numeric(), 
                     ifelse(is.na(destLat) & Team == "Boston Celtics", acities %>% filter(City == "Boston") %>% select(Latitude) %>% as.numeric(),      
                     ifelse(is.na(destLat) & Team == "New Orleans Pelicans", acities %>% filter(City == "New Orleans") %>% select(Latitude) %>% as.numeric(), 
                     ifelse(is.na(destLat) & Team == "Houston Rockets", acities %>% filter(City == "Houston") %>% select(Latitude) %>% as.numeric(), 
                     ifelse(is.na(destLat) & Team == "Oklahoma City Thunder", acities %>% filter(City == "Oklahoma") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "New York Knicks", acities %>% filter(City == "New York") %>% select(Latitude) %>% as.numeric(),  
                     ifelse(is.na(destLat) & Team == "Charlotte Hornets", acities %>% filter(City == "Charlotte") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Miami Heat", acities %>% filter(City == "Miami") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Phoenix Suns", acities %>% filter(City == "Phoenix") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Utah Jazz", acities %>% filter(City == "Utah") %>% select(Latitude) %>% as.numeric(),  
                     ifelse(is.na(destLat) & Team == "Los Angeles Lakers", acities %>% filter(City == "Los Angeles") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Los Angeles Clippers", acities %>% filter(City == "Los Angeles") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Dallas Mavericks", acities %>% filter(City == "Dallas") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Milwaukee Bucks", acities %>% filter(City == "Milwaukee") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Philadelphia 76ers", acities %>% filter(City == "Philadelphia") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Minnesota Timberwolves", acities %>% filter(City == "Minnesota") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Golden State Warriors", acities %>% filter(City == "San Francisco") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Portland Trail Blazers", acities %>% filter(City == "Portland") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Denver Nuggets", acities %>% filter(City == "Denver") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Sacramento Kings", acities %>% filter(City == "Sacramento") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Detroit Pistons", acities %>% filter(City == "Detroit") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Memphis Grizzlies", acities %>% filter(City == "Memphis") %>% select(Latitude) %>% as.numeric(),       
                     ifelse(is.na(destLat) & Team == "Cleveland Cavaliers", acities %>% filter(City == "Cleveland") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Chicago Bulls", acities %>% filter(City == "Chicago") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Orlando Magic", acities %>% filter(City == "Orlando") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Washington Wizards", acities %>% filter(City == "Washington D.C.") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Indiana Pacers", acities %>% filter(City == "Indiana") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "San Antonio Spurs", acities %>% filter(City == "San Antonio") %>% select(Latitude) %>% as.numeric(),
                     ifelse(is.na(destLat) & Team == "Toronto Raptors", acities %>% filter(City == "Toronto") %>% select(Latitude) %>% as.numeric(), 
                     ifelse(is.na(destLat) & Team == "Brooklyn Nets", acities %>% filter(City == "New York") %>% select(Latitude) %>% as.numeric(),       
                     destLat))))))))))))))))))))))))))))))) %>%
    
    mutate(destLon = ifelse(is.na(destLon) & Team == "Atlanta Hawks", acities %>% filter(City == "Atlanta") %>% select(Longitude) %>% as.numeric(), 
                     ifelse(is.na(destLon) & Team == "Boston Celtics", acities %>% filter(City == "Boston") %>% select(Longitude) %>% as.numeric(),      
                     ifelse(is.na(destLon) & Team == "New Orleans Pelicans", acities %>% filter(City == "New Orleans") %>% select(Longitude) %>% as.numeric(), 
                     ifelse(is.na(destLon) & Team == "Houston Rockets", acities %>% filter(City == "Houston") %>% select(Longitude) %>% as.numeric(), 
                     ifelse(is.na(destLon) & Team == "Oklahoma City Thunder", acities %>% filter(City == "Oklahoma") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "New York Knicks", acities %>% filter(City == "New York") %>% select(Longitude) %>% as.numeric(),  
                     ifelse(is.na(destLon) & Team == "Charlotte Hornets", acities %>% filter(City == "Charlotte") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Miami Heat", acities %>% filter(City == "Miami") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Phoenix Suns", acities %>% filter(City == "Phoenix") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Utah Jazz", acities %>% filter(City == "Utah") %>% select(Longitude) %>% as.numeric(),  
                     ifelse(is.na(destLon) & Team == "Los Angeles Lakers", acities %>% filter(City == "Los Angeles") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Los Angeles Clippers", acities %>% filter(City == "Los Angeles") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Dallas Mavericks", acities %>% filter(City == "Dallas") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Milwaukee Bucks", acities %>% filter(City == "Milwaukee") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Philadelphia 76ers", acities %>% filter(City == "Philadelphia") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Minnesota Timberwolves", acities %>% filter(City == "Minnesota") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Golden State Warriors", acities %>% filter(City == "San Francisco") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Portland Trail Blazers", acities %>% filter(City == "Portland") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Denver Nuggets", acities %>% filter(City == "Denver") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Sacramento Kings", acities %>% filter(City == "Sacramento") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Detroit Pistons", acities %>% filter(City == "Detroit") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Memphis Grizzlies", acities %>% filter(City == "Memphis") %>% select(Longitude) %>% as.numeric(),       
                     ifelse(is.na(destLon) & Team == "Cleveland Cavaliers", acities %>% filter(City == "Cleveland") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Chicago Bulls", acities %>% filter(City == "Chicago") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Orlando Magic", acities %>% filter(City == "Orlando") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Washington Wizards", acities %>% filter(City == "Washington D.C.") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Indiana Pacers", acities %>% filter(City == "Indiana") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "San Antonio Spurs", acities %>% filter(City == "San Antonio") %>% select(Longitude) %>% as.numeric(),
                     ifelse(is.na(destLon) & Team == "Toronto Raptors", acities %>% filter(City == "Toronto") %>% select(Longitude) %>% as.numeric(), 
                     ifelse(is.na(destLon) & Team == "Brooklyn Nets", acities %>% filter(City == "New York") %>% select(Longitude) %>% as.numeric(),       
                     destLon))))))))))))))))))))))))))))))) %>%
    
    
    mutate(Route = ifelse(is.na(lag(City)) & Team == "Atlanta Hawks", paste(acities %>% filter(City == "Atlanta") %>% select(City), City, sep = " - "), 
                   ifelse(is.na(lag(City)) & Team == "Boston Celtics", paste(acities %>% filter(City == "Boston") %>% select(City), City, sep = " - "),      
                   ifelse(is.na(lag(City)) & Team == "New Orleans Pelicans", paste(acities %>% filter(City == "New Orleans") %>% select(City), City, sep = " - "), 
                   ifelse(is.na(lag(City)) & Team == "Houston Rockets", paste(acities %>% filter(City == "Houston") %>% select(City), City, sep = " - "), 
                   ifelse(is.na(lag(City)) & Team == "Oklahoma City Thunder", paste(acities %>% filter(City == "Oklahoma") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "New York Knicks", paste(acities %>% filter(City == "New York") %>% select(City), City, sep = " - "),  
                   ifelse(is.na(lag(City)) & Team == "Charlotte Hornets", paste(acities %>% filter(City == "Charlotte") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Miami Heat", paste(acities %>% filter(City == "Miami") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Phoenix Suns", paste(acities %>% filter(City == "Phoenix") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Utah Jazz", paste(acities %>% filter(City == "Utah") %>% select(City), City, sep = " - "),  
                   ifelse(is.na(lag(City)) & Team == "Los Angeles Lakers", paste(acities %>% filter(City == "Los Angeles") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Los Angeles Clippers", paste(acities %>% filter(City == "Los Angeles") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Dallas Mavericks", paste(acities %>% filter(City == "Dallas") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Milwaukee Bucks", paste(acities %>% filter(City == "Milwaukee") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Philadelphia 76ers", paste(acities %>% filter(City == "Philadelphia") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Minnesota Timberwolves", paste(acities %>% filter(City == "Minnesota") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Golden State Warriors", paste(acities %>% filter(City == "San Francisco") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Portland Trail Blazers", paste(acities %>% filter(City == "") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Denver Nuggets", paste(acities %>% filter(City == "Portland") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Sacramento Kings", paste(acities %>% filter(City == "Sacramento") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Detroit Pistons", paste(acities %>% filter(City == "Detroit") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Memphis Grizzlies", paste(acities %>% filter(City == "Memphis") %>% select(City), City, sep = " - "),       
                   ifelse(is.na(lag(City)) & Team == "Cleveland Cavaliers", paste(acities %>% filter(City == "Cleveland") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Chicago Bulls", paste(acities %>% filter(City == "Chicago") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Orlando Magic", paste(acities %>% filter(City == "Orlando") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Washington Wizards", paste(acities %>% filter(City == "Washington D.C.") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Indiana Pacers", paste(acities %>% filter(City == "Indiana") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "San Antonio Spurs", paste(acities %>% filter(City == "San Antonio") %>% select(City), City, sep = " - "),
                   ifelse(is.na(lag(City)) & Team == "Toronto Raptors", paste(acities %>% filter(City == "Toronto") %>% select(City), City, sep = " - "), 
                   ifelse(is.na(lag(City)) & Team == "Brooklyn Nets", paste(acities %>% filter(City == "Brooklyn") %>% select(City), City, sep = " - "),       
                   paste(lag(City), City, sep = " - ")))))))))))))))))))))))))))))))) %>%
    
    mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude), destLat = as.numeric(destLat), destLon = as.numeric(destLon)) %>%
    
    rowwise() %>% 
    mutate(dist = geosphere::distm(c(destLon, destLat), c(Longitude, Latitude), fun=distHaversine)) %>% 
    mutate(Distance = round(dist * 0.000621,0)) %>%
    
    mutate(Route = ifelse(dist == 0, "No Travel", Route)) %>%
    
    select(Season, Month, Date, Location, Arena, Team, Opponent, City, `W/L`, Team_pts, Opp_pts, Latitude, Longitude, destLat, destLon, Route, Distance)
  
  })
  
  ################
  
  
  #map plot############
  output$mapPlot <- renderPlot({
   
    req(input$scr)
    
    acitiesc <- cities() %>% filter(Team == input$scr)
    
    #create basemap
    maps::map("world", regions=c("usa"), fill=T, col="#898f9c", bg="transparent", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
    points(acities$Longitude, acities$Latitude, pch=7, cex=1, col="#e9ebee")
    
    
    for (i in (1:dim(acitiesc)[1])) { 
      
      inter <- geosphere::gcIntermediate(c(acitiesc$destLon[i], acitiesc$destLat[i]), c(acitiesc$Longitude[i], acitiesc$Latitude[i]), n=500)
      lines(inter, lwd=0.5, col="#29487d", lty=1)
      
    }  
  
    
  })
  
  #bar chart#######
  output$dotPlot <- renderPlotly({
    
  req(input$scr) 
  
  acity <- cities() %>%
    as.data.frame() %>%
    group_by(Team) %>%
    summarise(Distance = sum(Distance)) %>%
    ungroup() %>%
    mutate(zscore = round((Distance - mean(Distance))/sd(Distance),1)) %>%
    arrange(desc(Distance))
  
  bcity <- acity %>%
    as.data.frame() %>%
    group_by(Team) %>%
    summarise(Distance = sum(Distance)) %>%
    ungroup() %>%
    mutate(zscore = round((Distance - mean(Distance))/sd(Distance),1)) %>%
    filter(Team == input$scr)
    
      
  
 a2 <- ggplot() +
    #geom_hline(aes(yintercept = 0), color = "#4c4c4c") +
    geom_point(data = acity, aes(x = reorder(Team, -zscore), y = zscore, text = paste(Team, "<br> Reg. Season Miles: ", Distance, '<br> Z Score: ', zscore)), color = "#898f9c", size = 3) +
    geom_point(data = bcity, aes(x = Team, zscore, text = paste(Team, "<br> Reg. Season Miles: ", Distance, '<br> Z Score: ', zscore)), color = "#29487d", size = 4) +
    geom_segment(data = acity, aes(x = Team, y = 0, xend = Team,  yend = zscore), color = "#898f9c") + 
    geom_segment(data = bcity, aes(x = Team, y = 0, xend = Team,  yend = zscore), color = "#29487d") + 
    xlab("") + ylab("") +
    ylim(-2.5,2.5) +
    theme(plot.background = element_rect(fill = "#e9ebee", color = "#e9ebee"),
          panel.background = element_rect(fill = "#e9ebee", color = "#e9ebee"),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size = 8, color = "#898f9c"))
  
    ggplotly(a2, tooltip= c("text")) %>% config(displayModeBar = FALSE) %>%
    
    layout(title = list(x = 1, y = 1,
                                      text = paste0('<br>',
                                      input$scr,
                                      '<br>',
                                      '<sup>',
                                      bcity$Distance, " miles travelled in ", input$season, "\n", 
                                      '</sup>'),
                                       font=list(size=24, color="#29487d")),
           annotations = list(x = 1, y = -0.1, 
                              text = "Z Score: Number of Standard Deviations From League's Average.", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=12, color="#898f9c")))
  
  })
  
  
  
  #reading material tab
  #################################################
  
  #scrolling functionality
  output$scr <- renderScrollytell({scrollytell()})
  observe({cat("section:", input$scr, "\n")})
  
}

# Run the application####
shinyApp(ui = ui, server = server)