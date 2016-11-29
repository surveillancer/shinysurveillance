
library(shiny)
ears_parameters <- function() {
  tagList(
    numericInput("ears_alpha",
                 "Alpha:",
                 min = .Machine$double.eps,
                 max = 1 - .Machine$double.eps,
                 0.001),
    selectInput("ears_method",
                label = "EARS Method",
                choices = c("C1", "C2", "C3"),
                multiple = FALSE,
                selected = "C1")
  )
}

farringtonFlexible_parameters <- function() {
  tagList(
    shiny::fluidRow(
      column(6, tagList(
        numericInput("farringtonflexible_alpha",
                             "Alpha:",
                             min = .Machine$double.eps,
                             max = 1 - .Machine$double.eps,
                             0.001),
        selectInput("farringtonflexible_powertrans",
                    label = "powertrans: Power transformation",
                    selected = "2/3",
                    choices = c("2/3: skewness correction (Default)" = "2/3",
                                "1/2: for variance stabilizing transformation" = "1/2",
                                "none" = "none")),
        numericInput("farringtonflexible_pastWeeksNotIncluded",
                     label = "pastWeeksNotIncluded: Number of past weeks to ignore in the calculation:",
                     min = 1,
                     max = 100,
                     value = 26,
                     step = 1)
      )),
      column(6, tagList(
        numericInput("farringtonflexible_b",
                     "b: How many years back in time to include when forming the base counts:",
                     min = 1,
                     max = 6,
                     value = 3,
                     step = 1),
        numericInput("farringtonflexible_w",
                     "w: Window's half-size, i.e. number of weeks to include before and after the current week in each year:",
                     min = 1,
                     max = 52,
                     value = 5,
                     step = 1)
        )
      ))
    )
}

glrnb_parameters <- function() {
  tagList(
    numericInput("glrnb_c_ARL",
                 "Threshold in the GLR test, i.e. c_gamma:",
                 min = 0, value = 5),
    checkboxInput("glrnb_trend", "Include trend", value = TRUE)
  )
}

dataset_choices <- function() {
  c(
    #"Abattoir Data" = "abattoir",
    #"Cases of Campylobacteriosis and Absolute Humidity in Germany 2002-2011" = "campyDE",
    #"Surgical failures data" = "deleval",
    #"Influenza in Southern Germany" = "fluBYBW",
    #"Toy Data for 'twinSIR'" = "foodata",
    #"Toy Data for 'twinSIR'" = "fooepidata",
    #"Toy Data for 'twinSIR'" = "foofit",
    "'Hepatitis A' in 'Oberbergischer Kreis, Olpe, Rhein-Sieg-kreis' (Germany, Nordrhein-Westfalen) and 'Siegenwittgenstein Altenkirchen' (Germany, Rheinland-Pfalz)" = "h1_nrwrp",
    #"Hepatitis A in Berlin" = "ha",
    #"Hepatitis A in Berlin" = "ha.sts",
    #"1861 Measles Epidemic in the City of Hagelloch, Germany" = "hagelloch",
    "Hepatitis A in Germany" = "hepatitisA",
    #"Hospitalization date for HUS cases of the STEC outbreak in Germany, 2011" = "husO104Hosp",
    #"Occurrence of Invasive Meningococcal Disease in Germany" = "imdepi",
    #"Occurrence of Invasive Meningococcal Disease in Germany" = "imdepifit",
    "Influenza and meningococcal infections in Germany, 2001-2006" = "influMen",
    "k1 'Kryptosporidiose' in Germany, 'Baden-Württemberg'" = "k1",
    "m1 'Masern' in the 'Landkreis Nordfriesland" = "m1",
    "m2 'Masern' in the 'Stadt- und Landkreis Coburg" = "m2",
    "m3 'Masern' in the 'Kreis Leer'" = "m3",
    "m4 'Masern' in the 'Stadt- und Landkreis Aachen' (Germany, Nordrhein-Westfalen)" = "m4",
    "m5 'Masern' in the 'Stadt Verden' (Germany, Niedersachsen)" = "m5",
    #"Measles in the Weser-Ems region of Lower Saxony, Germany, 2001-2002" = "measles.weser",
    #"Measles in the 16 states of Germany" = "measlesDE",
    #"Measles in the Weser-Ems region of Lower Saxony, Germany, 2001-2002" = "measlesWeserEms",
    "Meningococcal infections in France 1985-1995" = "meningo.age",
    #"Danish 1994-2008 all cause mortality data for six age groups" = "momo",
    "n1 'Norovirus' in 'Stadtkreis Berlin Mitte' (Germany, Berlin)" = "n1",
    "n2 'Norovirus' in 'Torgau-Oschatz' (Germany, Sachsen)" = "n2",
    "q1.nrwh 'Q-Fever' in the 'Hochsauerlandkreis' (Germany, Westfalen) and in the 'Landkreis Waldeck-Frankenberg' (Germany, Hessen)" = "q1_nrwh",
    "q2 'Q-Fieber' in 'München' (Germany, Bayern)" = "q2",
    #"Rotavirus cases in Brandenburg, Germany, during 2002-2013 stratified by 5 age categories" = "rotaBB",
    "s1 'Salmonella Oranienburg' in Germany" = "s1",
    "s2 'Salmonella Agona' in 12 'Bundesländern' of Germany" = "s2",
    "s3 'Salmonella Anatum' in Germany" = "s3",
    "Salmonella cases in Germany 2001-2014 by data of symptoms onset" = "salmAllOnset",
    "Hospitalized Salmonella cases in Germany 2004-2014" = "salmHospitalized",
    "Salmonella Newport cases in Germany 2004-2013" = "salmNewport",
    #"Salmonella Agona cases in the UK 1990-1995" = "salmonella.agona",
    "Salmonella Hadar cases in Germany 2001-2006" = "shadar",
    "Salmonella Newport cases in Germany 2004-2013" = "stsNewport"
  )
}

shinyUI(fluidPage(
  includeHTML("github.html"),
  titlePanel("Surveillance Algorithm Assistant"),
  tags$small("Test surveillance algorithms and tune the parameters."),
    fluidRow(
      column(8, tabsetPanel(
        tabPanel("Algorithms",
                 selectInput("algorithms", label = "Algorithm",
                             selected = c("ears", "farringtonflexible"),
                             choices = c("EARSC" = "ears",
                                         "Farrington Flexible" = "farringtonflexible",
                                         "Negative Binomial CUSUM" = "glrnb"),
                             multiple = TRUE),
                 numericInput("range_min", "Monitor last X weeks:", max = 52 * 4,
                              min = 2, value = 100)

        ),
        tabPanel("EARSC", ears_parameters()),
        tabPanel("Farrington Flexible", farringtonFlexible_parameters()),
        tabPanel("Negative Binomial CUSUM", glrnb_parameters())
      )),
      column(4, selectInput("dataset", "Dataset:", selected = "salmAllOnset",
                            choices = dataset_choices()))
    ),
    hr(),
    plotly::plotlyOutput("mainPlot"),
    hr(),
    textOutput("errors")
  )
)
