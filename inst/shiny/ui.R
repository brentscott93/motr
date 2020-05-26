

header <-  dashboardHeader(title = 'motr')

sidebar <- dashboardSidebar(
             useShinyjs(),
              sidebarMenu(
                menuItem('Home', tabName = 'home',  icon = icon('home')),
                menuItem('Analyze', tabName = 'analyze',  icon = icon('microscope'),
                          menuSubItem( 'Summarize ImageJ', 'imagej'),
                          menuSubItem('Fit Curves', 'shinydrc')
                          ),
                menuItem('Sketch', tabName = 'esquisse', icon = icon('pencil'))
                )
)


body <- dashboardBody(#shinyDashboardThemes(theme = 'poor_mans_flatly'),
                      tabItems(
                        tabItem('home',
                                column(2),
                                column(4,
                                img(src = 'motr-logo.gif')
                                )
                        ),
                        tabItem('imagej',
                                imagej_ui(id = 'imagej_ui')
                        ),
                        tabItem('shinydrc',
                                curves_ui(id = 'curves_ui')
                        ),
                        tabItem('esquisse',
                                fluidPage(
                                fileInput('esquisse_data',
                                          'Upload data to sketch',
                                          multiple = TRUE,
                                          buttonLabel = 'Browse...'),

                                esquisserUI(
                                  id = "esquisse_ui",
                                  header = FALSE,
                                  choose_data = FALSE,
                                  container = esquisseContainer(height = "800px")
                                )
                                )
                       )
                      )
)


ui <- dashboardPage(header, sidebar, body)

