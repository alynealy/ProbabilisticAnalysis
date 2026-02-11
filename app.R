library(shiny)
library(bslib)
library(ggplot2)
library(DT)
source("DailyActiveUsers.R")
source("ProcesareCereri.R")
source("EX2.R")
source("EX4.R")
source("EX5.R")
source("EX7.R")
source("EX8.R")
source("EX9.R")
source("EX10.R")
source("EX11.R")
source("EX12.R")
# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----
ui <- page_navbar(
  title = "Analiza Serviciu Online",
  bg = "#2D89C8",
  
  
  nav_panel(
    title="Kd",
    
    layout_sidebar(
      sidebar = sidebar(
        title="Kd",
        selectInput(
          "distrK",
          "Selectati tipul distributiei",
          choices = list("Poisson" = "pois", "Binomiala" = "bin"),
          selected = "pois"
        ),
        numericInput("empK", "Simulări", value = 1e3),
        actionButton("recalcK", "Recalculează")
      ),
      
      div (
        card(
          card_header("Comparatie ani"),
          plotOutput("yearPlot")
        ),
        
        
        navset_card_underline(
          id = "pageK",
          title = "Vizualizare lunar",
          nav_panel("2019", 
                    plotOutput("month2019"),
                    DT::dataTableOutput("empK2019")
          ),
          nav_panel("2020",
                    plotOutput("month2020"),
                    DT::dataTableOutput("empK2020")
          ),
          nav_panel("2021", 
                    plotOutput("month2021"),
                    DT::dataTableOutput("empK2021")
          ),
          nav_panel("2022", 
                    plotOutput("month2022"),
                    DT::dataTableOutput("empK2022")
          ),
          nav_panel("2023", 
                    plotOutput("month2023"),
                    DT::dataTableOutput("empK2023")
          )
        ) 
      )
    )
  ),
  
  nav_panel(
    title="Si",
    
    layout_sidebar(
      sidebar = sidebar(
        title="Si",
        selectInput(
          "distrS",
          "Selectati tipul distributiei",
          choices = list("Exponentiala" = "exp", "Normala" = "norm"),
          selected = "exp"
        ),
        
        numericInput("empS", "Simulari", value = 1000),
        
        card (
          card_header("Parametri Exponentiala"),
          numericInput("rateS", "Rata ( 1 / Medie )", value = 1/30)
        ),
        
        card (
          card_header("Parametri Normala"),
          numericInput("meanS", "Medie", value = 30),
          numericInput("varS", "Varianta", value = 25)
        ),
        actionButton("recalcS", "Recalculează")
      ),
      
      div(
        card(
          card_header("Histograma Empirica peste Teoretica"),
          plotOutput("graphS")
        ),
        navset_card_underline(
          title = "Studiu Empiric",
          nav_panel("Empiric",
            DT::dataTableOutput("empS")
          ),
          nav_panel("Teoretic",
            DT::dataTableOutput("thS")
          )
        )
      )
    ),
  ),
  
  nav_panel(
    title = "N",
    card(
      card_header("N"),
      numericInput("nmax", "NMAX", value = 5),
      numericInput("probSucc", "Probabilitate Succes Cerere", value = 0.4),
      actionButton("recalcN", "Recalculează")
    )
  ),
  
  nav_panel(
    title = "(N, F)",
    card(
      card_header("(N, F)"),
      numericInput("empNF", "Simulari", value = 1000),
      actionButton("recalcNF", "Recalculează"),
      plotOutput("heatmapNF"),
      tags$br(),
      textOutput("distN"),
      textOutput("distF")
    )
  ),
  
  nav_panel(
    title = "(N, T)",
    card(
      card_header("(N, T)"),
      numericInput("empNT", "Simulari", value = 1000),
      actionButton("recalcNT", "Recalculează"),
      plotOutput("graphNT"),
      tags$br(),
      DT::dataTableOutput("tableNT")
      
    )
  ),
  
  nav_panel(
    title = "Si dep.",
    card (
      card_header("Timpi Si independenți vs dependenți (latența creste după eșecuri"),
      numericInput("empSDep", "Simulări", value = 1000),
      actionButton("recalcSDep", "Recalculează"),
      plotOutput("graphSDep")
    )
  ),
  
  nav_panel(
    title = "Aproximare Normală",
    div(
      card(
        card_header("Aproximare normală și agregare"),
        numericInput("empAgreg", "Simulări", value = 100),
        actionButton("recalcAgreg", "Recalculează")
      ),
      card(
        card_header("Agregat pentru o zi"),
        plotOutput("graphAgreg"),
        plotOutput("graphAgregLine")
      )
    )
  ),
  
  nav_panel(
    title = "Inegalități Probabilistice",
    div (
      card(
        card_header("Inegalități probabilistice (garanții worst-case) - T"),
        numericInput("empIneg", "Simulări", value = 1e5),
        actionButton("recalcIneg", "Recalculează")
      ),
      card(
        card_header("a) Verificați numeric inegalitățile Markov și Cebîșev (empiric versus teoretic)."),
        div(
          card(
            card_header("Markov"),
            DT::dataTableOutput("tableMarkov")
          ),
          card(
            card_header("Cebîsnev"),
            DT::dataTableOutput("tableCheb")
            
          )
        )
      ),
      card(
        card_header("b) Pentru variabila număr de eșecuri/încercări verificați o inegalitate de tip Chernoff."),
        DT::dataTableOutput("tableChern")
      ),
      card(
        card_header("c) Interpretați utilitatea acestor limite când distribuțiile exacte sunt necunoscute."),
        p(" Atunci cand distributia exacta a unei variabile aleatoare este necunoscuta, inegalitatile probabilistice precum Markov, Chebyshev si Chernoff sunt utile deoarece permit obtinerea unor limite superioare 
        pentru probabilitati, fara a cunoaste forma distributiei. Markov si Chebyshev folosesc doar informatii simple, precum media si varianta, 
        in timp ce inegalitatea de tip Chernoff ofera limite mai stricte pentru probabilitatea aparitiei unor valori mari. Chiar daca aceste 
        limite pot fi conservatoare, ele furnizeaza garantii worst-case, 
        valabile indiferent de distributia exacta. Astfel, aceste inegalitati 
        sunt utile pentru evaluarea riscului si pentru luarea unor decizii 
        prudente atunci cand informatiile despre distributie sunt incomplete.")
      ),
      card(
        card_header(
            HTML(
            "d)  Pentru o funcție convexă &phi; (ex.: x<sup>2</sup>, e<sup>x</sup>) verificați numeric 
             &phi;(E(T)) &le; E(&phi;(T)) (inegalitatea lui Jensen)"
            )
        ),
        uiOutput("jensen_text")
      ),
      card(
        card_header("e) Interpretați rezultatul de la d) în contextul riscului (penalizarea valorilor extreme)."),
        p("In contextul riscului, valorile mari ale timpului de raspuns au un impact mai mare decat valorile mici in cazul unei functii convexe. 
        O crestere mica a timpului de raspuns pentru valorile mari produce o crestere mai mare a costului, decat aceeasi crestere pentru 
        valorile mai mici. Astfel, valorile extreme sunt penalizate inegal.")
      )
    )
  ),
  
  nav_panel(
    title="Churn",
    layout_sidebar(
      sidebar = sidebar(
        title = "Churn Details",
        selectInput(
          "distrChurn",
          "Selectati tipul distributiei",
          choices = list("Constant" = "geom", "Condiționat" = "emp"),
          selected = "geom"
        ),
        card(
          card_header("Parametrii Distribuție Constantă"),
          numericInput("probChurn", "Probabilitatea de Churn", value = 0.05, min = 0, max = 1)
        ),
        card(
          card_header("Parametrii Distribuție Condiționată"),
          numericInput("empChurn", "Simulări", value = 10000),
          numericInput("mChurn", "Mărime Fereastră", value = 5),
          numericInput("kChurn", "Număr Eșecuri", value = 2)
        ),
        actionButton("recalcChurn", "Recalculează")
      ),
      
      div(
        card(
          card_header("Funcția de Masă"),
          plotOutput("plotMChurn")
        ),
        card(
          card_header("Funcția de Repartiție"),
          plotOutput("plotRChurn")
        ),
        card(
          card_header("Probabilitatea de pierdere a unui utilizator"),
          p("Presupunem că numărul de cereri pe care le face un utilizator într-o zi este distribuit poisson"),
          numericInput("medieCerUser", "Media de cereri ale unui utilizator", value = 30),
          numericInput("empCerUser", "Număr simulări empiric", value = 10000),
          textOutput("probCerUser")
        ),
        card(
          card_header("Observații"),
          p("In primul scenariu, pierderea utilizatorilor este modelata folosind o distributie
            geometrica, dgeom si pgeom. La fiecare cerere, probabilitatea ca utilizatorul sa fie pierdut este constanta si este reprezentata de probChurn Pierderea utilizatorului in cadrul unei cereri este independenta de ce s a intamplat in cadrul cererilor anterioare. 
            Numarul de cereri pe care le poate face un utilizator este generat aleator cu functia rpois, folosind valoarea MedieCereriUtilizatori = 20. Astfel, in model, fiecare utilizator are un numar de cereri care urmeaza o distributie Poisson cu medie 20.
            Functia UserLeaveEmp foloseste numarul de cereri per utilizator si calculeaza probabilitatea ca utilizatorul sa fi fost pierdut pana la ultima cerere."),
  
          p("In al doilea scenariu, pierderea utilizatorilor depinde de esecuri. Daca intr-o fereastra de m=5 cereri apar cel putin k=2 cereri, atunci utilizatorul este pierdut. Deci, nu este suficient un singur esec pentru ca utilizatorul sa plece. 
            Functia .check_fer genereaza o succesiune de cereri de tip succes si esec si verifica fiecare grup de 5 cereri. La gasirea unei ferestre in care sunt cel putin 2 esecuri, functia intoarce momentul in care utilizatorul pleaca. 
            Functia generateHist construieste o distributie empirica a momentului de pierdere al utilizatorului. Functiile dChurn si pChurn folosesc distributia pentru a calcula probabilitatea ca utilizatorul sa se fi pierdut pana la un anumit numar de cereri."),

          p("Diferenta dintre cele 2 scenarii este ca in primul scenariu pierderea depinde de esecuri izolate, iar in al doilea scenariu depinde de mai multe esecuri din aceeasi fereastra de m cereri.
            Cel de-al doilea scenariu este mai realist intrucat un esec izolat nu este suficient pentru o pierdere, probabilitatea de churn fiind mai mica.")
        )
      )
    )
  ),
  
  nav_panel(
    title = "Impact economic",
    layout_sidebar(
      sidebar = sidebar(
        title = "Profitul zilnic",
        numericInput("venitPerSucces", "Venit per succes", value = 0.5, min = 0),
        numericInput("costAchizitie", "Pierdere per churn", value = 50, min = 0),
        card(
          card_header("Penalități SLA"),
          numericInput("tSLA", "Pragul de timp SLA (ms)", value = 50, min = 0),
          numericInput("penSLA", "Penalități SLA", value = 2)
        ),
        actionButton("recalcProfit", "Recalculează")
      ),
      
      div(
        card(
          card_header("ProfitZilnic"),
          div(
            style = "display: flex; flex-direction: column;",
            dateInput(
              "dateProfit",
              "Data:",
              value = "2019-01-01",
              min = "2019-01-01",
              max = "2023-12-31",
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 1
            ),
            numericInput("empProfZ", "Simulări", value = 100, min = 1)
          ),
          
          plotOutput("plotProfitZilnic"),
          textOutput("medieProfitZi")
        ),
        card(
          card_header("Simulare Profit Anul Respectiv"),
          plotOutput("plotProfitAn")
      )
      )
    )
  ),
  
  nav_panel(
    title = "Altele",
    numericInput("empEX12", "Simulări", value = 1000, min = 1),
    div(
      plotOutput("hist12T"),
      plotOutput("cond12T")
    )
  ),
  nav_panel(
    title="Analiză de sinteză",
    div(
      card(
        card_header("a. Rolul probabilității empirice"),
        p("Probabilitatea empirica este utila pentru estimarea directa a 
          indicatorilor de performanta atunci cand distributiile sunt complicate
          sau cand apar mecanisme precum retry si backoff. Prin simulare putem estima 
          valori precum E[T], probabilitatea de intarzieri mari si profitul mediu, 
          fara a avea o formula inchisa pentru toate marimile de interes. In plus, 
          comparatia empiric vs teoretic  valideaza implementarea modelului.")
      ),
      card(
        card_header("b. Ce informații aduc condiționările"),
        p("Conditionarile separa efectele si explica mecanismul: de exemplu E[T | N=n] 
          arata cat creste latenta cand apar mai multe incercari, iar P(SLA | N=n) 
          arata cum scade probabilitatea de incadrare pe masura ce retry-urile cresc. 
          Similar, profitul conditionat de depasirea SLA arata impactul economic al 
          evenimentelor rare (valori mari ale lui T), pe care media globala singura nu le evidentiaza.")
      ), 
      
      card(
        card_header("c. Utilitatea inegalităților probabilistice"),
        p("Inegalitatile Markov si Chebyshev ofera limite worst-case pentru probabilitati de tip 
          tail folosind doar media si varianta, fara a cunoaste distributia exacta. Ele pot fi 
          conservatoare, dar sunt utile ca garantii minimale in scenarii cu informatie incompleta 
          si pentru evaluarea prudenta a riscului de intarzieri mari.")
      ),
      
      card(
        card_header("d. Legătura dintre performanța tehnică și impactul economic"),
        p("Performanta tehnica (latenta T si incadrarea in SLA) influenteaza direct profitul prin 
          costuri proportionale cu timpul si prin penalizari la depasirea pragului SLA. Astfel, o 
          crestere a latentei medii sau o crestere a cozii distributiei (intarzieri extreme) poate 
          reduce profitul mediu si poate creste frecventa profitului negativ, chiar daca venitul per 
          cerere ramane constant.")
      ),
      
      card(
        card_header("e. Ce parametri influențează cel mai mult rezultatele finale și ce ați modifica pentru îmbunătățirea sistemului."),
        p("Parametrii cei mai influenti sunt media/varianta timpilor de raspuns (SDetails$mean), 
          limita de retry (NDetails$NMAX) si pragul/penalizarea SLA. Reducerea lui NMAX poate reduce 
          latenta si riscul de penalizare, dar poate scadea rata de succes; imbunatatirea timpilor de 
          raspuns (scaderea lui SDetails$mean) imbunatateste simultan SLA si profitul. In practica as 
          optimiza NMAX, as regla backoff-ul (mai putin agresiv) si as investi in reducerea latentei de 
          baza pentru a diminua coada distributiei.")
      )
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  ####--- Kd functions ---####
  K <- reactiveValues(
    distribution = KDetails$distribution,
    test_emp     = 1000
  )
  observeEvent(input$recalcK, {
    K$distribution <- input$distrK
    K$test_emp <- input$empK
  })
  
  
  output$yearPlot <- renderPlot({ createYearsPlot(distr = K$distribution) })
  output$month2019 <- renderPlot({ createMonthsPlot(2019, distr = K$distribution) })
  output$month2020 <- renderPlot({ createMonthsPlot(2020, distr = K$distribution) })
  output$month2021 <- renderPlot({ createMonthsPlot(2021, distr = K$distribution) })
  output$month2022 <- renderPlot({ createMonthsPlot(2022, distr = K$distribution) })
  output$month2023 <- renderPlot({ createMonthsPlot(2023, distr = K$distribution) })
  output$empK2019 <- DT::renderDT({ getYearEMP(2019, K$test_emp, K$distribution) })
  output$empK2020 <- DT::renderDT({ getYearEMP(2020, K$test_emp, K$distribution) })
  output$empK2021 <- DT::renderDT({ getYearEMP(2021, K$test_emp, K$distribution) })
  output$empK2022 <- DT::renderDT({ getYearEMP(2022, K$test_emp, K$distribution) })
  output$empK2023 <- DT::renderDT({ getYearEMP(2023, K$test_emp, K$distribution) })
  ####--- Kd functions ---####
  
  
  ####--- Si functions ---####
  S <- reactiveValues(
    test_emp     = 1000,
    change = F
  )
  observeEvent(input$recalcS, {
    SDetails$distribution <- input$distrS
    SDetails$mean <- input$meanS
    SDetails$rate <- input$rateS
    SDetails$variation <- input$varS
    SDetails$std_variation <- sqrt(SDetails$variation)
    
    S$test_emp <- input$empS
    S$change = !S$change
  })
  output$graphS <- renderPlot({
    S$change
    empSTheoreticalGraph(S$test_emp, S$distribution)
  })
  output$empS <- DT::renderDT({
    S$change
    getDTempSStudy(S$test_emp)
  })
  output$thS <- DT::renderDT({
    S$change
    getDTthSStudy()
  })
  ####--- Si functions ---####
  
  ####--- N functions ---####
  N <- reactiveValues(
    change = F
  )
  observeEvent(input$recalcN, {
    NDetails$NMAX <- input$nmax
    probSucc <- input$probSucc
    probEsec <- 1 - probSucc
    N$change = !N$change
  })
  ####--- N functions ---####
  
  ####--- (N, F) functions ---####
  NF <- reactiveValues(
    change = F
  )
  
  observeEvent(input$recalcNF, {
    NFDetails$n_sim = input$empNF
    NF$change = !NF$change
  })
  output$heatmapNF <- renderPlot({
    NF$change
    N$change
    regenerateNF()
    NFDetails$gen_heatmap()
  })
  output$distN <- renderText({
    NF$change
    N$change
    paste("Distributie N:", paste(1:NDetails$NMAX, "- ", NFDetails$dist_N, collapse = ", "))
  })
  output$distF <- renderText({
    NF$change
    N$change
    paste("Distributie F:", paste(0:NDetails$NMAX,"- ", NFDetails$dist_F, collapse = ", "))
  })
  ####--- (N, F) functions ---####
  
  ####--- (N, T) functions ---####
  NT <- reactiveValues(
    change = F
  )
  observeEvent(input$recalcNT, {
    NTDetails$n_sim = input$empNT
    NT$change = !NT$change
  })
  
  output$graphNT <- renderPlot({
    NT$change
    N$change
    regenerateNT()
    NTDetails$gen_graph()
  })
  
  output$tableNT <- DT::renderDT({
    NT$change
    N$change
    
    datatable(
      NTDetails$stats,
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    )
  })
  ####--- (N, T) functions ---####
  
  ####--- S dep functions ---####
  SDep <- reactiveValues(
    test_emp = 1000,
    change = F
  )
  observeEvent(input$recalcSDep, {
    SDep$change = !SDep$change
    SDep$test_emp = input$empSDep
  })
  
  output$graphSDep <- renderPlot({
    SDep$change
    N$change
    S$change
    
    exercise_7(SDep$test_emp)
  })
  
  ####--- S dep functions ---####
  
  
  ####--- Agreg functions ---####
  AG <- reactiveValues(
    change = F
  )
  observeEvent(input$recalcAgreg, {
    AgregDetails$n_sim <- input$empAgreg
    AG$change = !AG$change
  })
  
  output$graphAgreg <- renderPlot({
    AG$change
    N$change
    K$change
    S$change
    
    regenerateAgregValues()
    generateNormalAgreg()
  })
  
  output$graphAgregLine <- renderPlot({
    AG$change
    N$change
    K$change
    S$change
    
    generateNormalAgregLine()
  })
  ####--- Agreg functions ---####
  
  ####--- Ineg functions ---####
  Ineg <- reactiveValues(
    test_emp = 1e5,
    change = F
  )
  
  observeEvent(input$recalcIneg, {
    Ineg$change <- !Ineg$change
    Ineg$test_emp <- input$empIneg
  })
  
  output$tableMarkov <- DT::renderDT({
    N$change
    S$change
    Ineg$change
    
    regenerateMarkovChebValues(Ineg$test_emp)
    datatable(
      InegDetails$markov_study,
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    )
  })
  
  output$tableCheb <- DT::renderDT({
    N$change
    S$change
    Ineg$change
    
    datatable(
      InegDetails$cheb_study,
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    )
  })
  
  output$tableChern <- DT::renderDT({
    N$change
    Ineg$change
    
    regenerateChernoffValues(Ineg$test_emp)

    datatable(
      InegDetails$chernoff_table,
      rownames = F,
      options = list(
        pageLength = NDetails$NMAX + 1
      )
    )
  })
  
  output$jensen_text <- renderUI({
    N$change
    S$change
    Ineg$change
    
    phi_square_E_T <- InegDetails$phi_square_E_T
    E_phi_square_T <- InegDetails$E_phi_square_T
    scale_factor <- InegDetails$scale_factor
    phi_exp_E_T <- InegDetails$phi_exp_E_T
    E_phi_exp_T <- InegDetails$E_phi_exp_T
    
    withMathJax(
      HTML(
        paste0(
          "\\( \\varphi(x) = x^2 \\):<br>",
          "\\[",
          "\\varphi(\\mathbb{E}(T)) = (\\mathbb{E}(T))^2 = ", round(phi_square_E_T,4), ",",
          "\\qquad",
          "\\mathbb{E}(\\varphi(T)) = \\mathbb{E}(T^2) = ", round(E_phi_square_T,4),
          "\\]<br>",
          
          "\\( \\varphi(x) = e^x \\):<br>",
          "\\[",
          "\\varphi(\\mathbb{E}(T)) = ",
          "\\exp\\!\\left(\\frac{\\mathbb{E}(T)}{c}\\right) = ", round(phi_exp_E_T,4), ",",
          "\\qquad",
          "\\mathbb{E}(\\varphi(T)) = \\mathbb{E}\\!\\left(\\exp\\!\\left(\\frac{T}{c}\\right)\\right) = ", round(E_phi_exp_T,4),
          "\\]<br>",
          
          "unde \\[ c = \\max\\{\\mathbb{E}(T), 1\\} = ", round(scale_factor,4), "\\]."
        )
      )
    )
  })
  ####--- Ineg functions ---####
  
  ####--- Churn functions ---####
  Churn <- reactiveValues(
    change = F,
    empCerUser = 10000,
    probUserLeave = 0,
  )
  
  observeEvent(input$recalcChurn, {
    Churn$change = !Churn$change
    Churn$empCerUser = input$empCerUser
    
    ChurnDetails$MedieCereriUtilizatori = input$medieCerUser
    ChurnDetails$distribution = input$distrChurn
    ChurnDetails$emp_try = input$empChurn
    ChurnDetails$probChurn = input$probChurn
    ChurnDetails$m = input$mChurn
    ChurnDetails$k = input$kChurn
    
  })
  
  output$plotMChurn <- renderPlot({
    N$change
    Churn$change
    
    regenerateChurn()
    
    generateChurnDensityPlot()
  })
  output$plotRChurn <- renderPlot({
    N$change
    Churn$change
    
    generateChurnDistributionPlot()
  })
  output$probCerUser <- renderText({
    N$change
    Churn$change
    
    Churn$probUserLeave = mean(UserLeaveEmp(Churn$empCerUser))
    paste("P = ", round(Churn$probUserLeave, 4))
  })
  ####--- Churn functions ---####
  
  ####--- Profit functions ---####
  Profit <- reactiveValues(
    change = F,
    date = as.Date("2019-01-01"),
    simulari = 100
  )
  
  observeEvent(input$recalcProfit, {
    Profit$change = !Profit$change
    Profit$date = input$dateProfit
    Profit$simulari = input$empProfZ
    
    EconDetails$t0_SLA = input$tSLA
    EconDetails$penalitate_SLA = input$penSLA
    EconDetails$cost_achizitie = input$costAchizitie
    EconDetails$venit_per_succes = input$venitPerSucces
  })
  
  output$plotProfitZilnic <- renderPlot({
    Profit$change
    N$change
    S$change
    K$change
    Churn$change
    
    ziua = as.integer(format(Profit$date, "%j"))
    an = as.integer(format(Profit$date, "%Y"))
    
    if (ziua == 366) ziua = 365
    
    regenerateProfitValues(
      tries = Profit$simulari,
      day = ziua, year = an,
      Churn$probUserLeave
    )

    
    hist(main = paste("Profit zilnic pe ", Profit$date),
         EconDetails$t, prob = T, breaks = 15,
         xlab="Profit", ylab="Probabilități")
  })
  
  output$medieProfitZi <- renderText({
    Profit$change
    N$change
    S$change
    K$change
    Churn$change
    
    paste("Medie profit zi = ", mean(EconDetails$t))
  })
  
  output$plotProfitAn <- renderPlot({
    Profit$change
    N$change
    S$change
    K$change
    Churn$change
    
    an = as.integer(format(Profit$date, "%Y"))
    generatePlotAn(year = an, Churn$probUserLeave)
  })
  ####--- Profit functions ---####
  
  ####--- EX 12 functions ---####
  
  simulariEX12 <- reactive({
    req(input$empEX12)
    input$empEX12
  })
    
  output$hist12T <- renderPlot({
    N$change
    S$change
    K$change
    
    Details12$t_sim = simulariEX12()
    regenerateDetails12()
    
    generate12HistT()
  }) 
  
  output$cond12T <- renderPlot({
    N$change
    S$change
    K$change
    
    Details12t_sim = simulariEX12()
    generate12TCond()
  }) 
  ####--- EX 12 functions ---####
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)