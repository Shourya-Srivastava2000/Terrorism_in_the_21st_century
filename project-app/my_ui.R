library(shiny)
source("analysis.R")

page_one <- tabPanel("Introduction",
                     titlePanel("Terrorism in the 21st Century"),
                     sidebarLayout(
                        sidebarPanel(p("by Matthew De La Roca, Arnon Bunyatipanon, Jonathan Thomas & Shourya Srivastava")),
                        mainPanel("As reported by",a("SIPRI",href="https://www.sipri.org/"),"(Stockholm International Peace Research Institute) 
                        global military spending has continued to ",a("increase",href="https://www.sipri.org/sites/default/files/2019-04/fs_1904_milex_2018_0.pdf"),
                                  ". Since the war on terror began at the outset of the 21st century 
          several questions remain. Given the increase in military spending,
          has global terror been decreasing in its intensity or getting more severe? 
          Have arms deals with non-state groups increased, and have these deals affected
          the rate of terror attacks? How has the nature of terror changed, the targets,
          and nations involved? Additionally, recent arms buildup can give us information 
          on areas of rising tensions. This data is significant due its ability to further
          elucidate problems regarding the state of global security today, and possibly point
          us to trends into the future. It is possible that the nature of military spending today
          is in response to other threats on the horizon and the war on terror is waning, but the
          opposite may also be true. Before we proceed it may be useful to familirize yourself with
          some fo the major terrorist groups that have been active in the past few years:",br(),br(),
            a("- The Communist Party of India",div(img(src="CPI.png",height= 100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/Communist_Party_of_India_(Marxist)"),br(),
            a("- The Taliban",div(img(src="Taliban.png",height= 100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/Taliban"),br(),
            a("- Boko Haram",div(img(src="BokoHaram.png",height=100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/Boko_Haram"),br(),
            a("- ISIS",div(img(src="ISIS.png",height=100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/Islamic_State_of_Iraq_and_the_Levant"),br(),
            a("- The PKK",div(img(src="PKK.png",height=100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/Kurdistan_Workers%27_Party"),br(),
            a("- The New People's Army",div(img(src="NPA.png",height=100,width=100),style="text-align: center;"),
               href="https://en.wikipedia.org/wiki/New_People's_Army")
            )
          )
)

page_two <- tabPanel("Data Introduction",titlePanel("Data Introduction"),sidebarLayout(sidebarPanel(p("For More information See our", a("Report",
                                                                                                                                        href="https://info201a-wi20.github.io/project-report-arnonbunya/"))),
                                                                                       mainPanel(h4("Global Terrorism Dataset"),p("Our first main data set is a database 
                                 file detailing all terrorist acts that have occured since 1970,
                                 for the purpose of this project we have selected only terrorist attacks 
                                 occuring after 2000. This data set contains a rich variety of information 
                                 on these attacks, such as deatailing the weapon used, target, and location."),br(),
                                                                                                 p( "This data set originates from the",a("Global Terrorism Database",
                                                                                                                                          href="https://www.start.umd.edu/gtd/"),"compiled by a research consortium called",
                                                                                                    a("START",href="https://en.wikipedia.org/wiki/National_Consortium_for_the_Study_of_Terrorism_and_Responses_to_Terrorism"),
                                                                                                    "START is located at the University of Maryland and is funded by the Department of Homeland Security. 
                                  Their methodology includes using AI as well as Human researchers to compile this data.",br(),
                                                                                                    "Here is a sample of some of column names that will be useuful for our project:", br(),br(),
                                                                                                    
                                                                                                    "- country_txt = the country that the attack took place in",br(),br(),
                                                                                                    "- region = the region that the attack occured in",br(),br(),
                                                                                                    "- attacktype1_txt = the nature of how the attack occured",br(),br(),
                                                                                                    "- targtype1_txt = the building or people targeted.",br(),br(),
                                                                                                    "- gname = the name of the terrorist organization that commited the act.",br(),br(),
                                                                                                    "- weaptype1_txt = the type of weapon used during the attack",br(),br(),
                                                                                                    
                                                                                                    p("Overall there were over 47 variables present in this data set, 
                                    we did filter it down to less, but these are the main columns that will be used in our project."),br(), 
                                                                                                    br(),
                                                                                                    h4("Global Arms Trade Dataset"),p("This dataset details the value of weapons both imported
                                                                 and exported from all countries in the world since 1950, 
                                                                 but for the sake of our project, we will only be using data 
                                                                 since 2000. The data set additionally lists all arms transfers 
                                                                 to non-state groups, which includes terrorist groups."),
                                                                                                    a("SIPRI",href="SIPRI](https://www.sipri.org/"),"Stockholm International Peace Research Institute),
                               which has been collected data on the arms trade and conflict for 50 years. The SIPRI institute is 
                               funded by the Swedish Government and counts around 60 academic researchers among its ranks. 
                               The World Bank also uses this data to track arms shipments."),br(),
                                                                                                 p("Here are some of the columns/ data fields that will be used:",br(),br(),
                                                                                                   
                                                                                                   "- country = the country of origin in the case of exports or the destination in the case of imports.",br(),br(),
                                                                                                   
                                                                                                   "- year = the year the arms shipments were made."))))

page_three <- tabPanel("Analysis #1", 
                       titlePanel('Investigating the impact of arms sold to non-state entities'),
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(inputId = "duration", label = "Year", min = 2000, max=2018, step = 1, value = 2018),
                           radioButtons(inputId = "numArms", "Choose one: ", 
                                        choiceNames = list(
                                          "Casualities vs imports",
                                          "Number of Casualties",
                                          "Value of Arms Sold"
                                        ), 
                                        choiceValues = list(
                                          "all", "Num_Casualties_Terrorism", "sum_of_arms_imports_to_insurgents"
                                        )
                            ),
                           
                         ), mainPanel (
                           p("Introduction: This question addresses the current arms trade to non-state entities.", 
                             "This includes paramilitary forces, rebel groups and insurgencies. Often states will often give", 
                             "money or money to non-state groups in order to advance their national interests, one",
                             "example of this being the U.S supplying the", a("Mujahadeen", href="https://en.wikipedia.org/wiki/Mujahideen"), 
                             "with arms during the", a("Soviet Afghan War(1979-1989)", href="https://en.wikipedia.org/wiki/Soviet%E2%80%93Afghan_War"), 
                            ".Oftentimes these weapons become unaccounted for, and are even used against the very power that supplied them.", 
                             "Do these arms transfers have a noticable negative effect down stream or are they negligable in the bigger picture?"), 
                           plotOutput("casVal"),
                           p("Results: the max number of arms sold to non state entities during a given year between 2000-2018 was in 2002 with" ,
                             "208 million dollars in arms transfered to non state entities, with the largest of these being a major transfer to the", 
                             a("Northern Alliance", href = "https://en.wikipedia.org/wiki/Northern_Alliance"), "a major insurgent group" ,
                            "that was seen as the Taliban's main rival during the early 2000s. Overall the worst year for Terror Casualties" ,
                            "was 2013 with 44,492 deaths from terror attacks, which makes sense given the rise of ISIS, the ongoing" ,
                            "Syrian Civil war and the various intense attacks undertaken by Boko Haram in Nigeria during this period."), 
                           p("Interpretation: It seems as though arms sales to non-state and paramilitary forces spiked heavily in 2002 , and" ,
                             "enjoyed an increase in 2014-2015 as well as in 2018. Although it is not-conclusive it seems as though there" ,
                             "is an increase in arms sales to non-state forces, some of which are terrorist groups in between periods of" ,
                             "intense terrorism. Whether this data could be used to predict increases in terrorist attacks in the future" ,
                             "is not totally clear, but it may be a signal that rebel groups and insurgents will often turn to buy arms" ,
                             "on the open market when they are in dire need to stock up for offensives. Much of the arms data regarding" ,
                             "non-traditional armies is unclear, as many of them are under arms embargos and either trade illegally, or" ,
                             "simply capture their weapons during conflict.")
                         )
                       )
    
                     )

page_four <- tabPanel("Analysis #2")

page_five <- tabPanel("Analysis #3")

page_six <- tabPanel("Analysis #4",
                     titlePanel(title = "Is Terrorism a Practical Concern for ALL Countries around the Globe?"),
                     sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "num_attacks", "Range", c("All", "< 10", "10 to 50", "50 to 100", "100 to 300", "300 to 500", "500 to 1000", "1000 to 5000", "5000 to 10000", "> 10000"))
                        ),
                        mainPanel(
                           p("While its a fact that terrorism is a global humanitarian crisis, there are still many countries
                             who are", strong("not"), "that focused on combating these organizations. This question aims to understand why certain countries
                             do not give fighting terrorism that high of a priority?"),
                           plotOutput("map_plot"),
                           p("Results: The distribution of the colors on the map shows a large disparity in the number of attack that countries face.
                             `r median(counts_country$n)` is the median number of attacks faced by all countries, which is very low when
                             compared to the highest values. This tells us that most countries do not face the threat of terrorist attacks,
                             it is just prevalent in a few countries."),
                           br(),
                           p("Evaluation: When we compare this to the average number of attacks per country `r sum(counts_country$n)/nrow(counts_country)`
                             attacks. This mean, however cannot be used as a reliable estimation as we have alot of significant outliers.
                             The map depicts a majority of the countries either at the end of the spectrum or with a very low numbr of attacks
                             with not very many in the middle. Therefore, we have some countries who are drastically affected by terrorism,
                             such as `r top_country_attacks$country_txt`, which has the highest number of attacks faced at `r top_country_attacks$n`
                             and then a country like `r bottom_country_attacks$country_txt` which faced the lowest number of attacks at 
                             `r bottom_country_attacks$n` in the given time period. Therefore, it is difficult to unite countries who face such
                             varying levels of threats in fighting terrorism on a global scale.")
                        )
                     )
)

ui <- fluidPage( navbarPage("Terrorism Data Exploration",
                             page_one,
                             page_two,
                             page_three,
                             page_four,
                             page_five,
                             page_six
                             )

)
