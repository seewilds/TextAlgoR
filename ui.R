

shinyUI(fluidPage(theme = shinytheme("readable"),
  
  verticalLayout(
  titlePanel(title = "A Text Prediction Algorithm"),
  tabsetPanel(
    tabPanel(
            "App", 
            br(),
            p("This is a submisison for the capstone project for the the Data Scientist Specialization from John's Hopkins Univeristy,
               offered via Coursera. The objective of the capstone is to build a text prediction algorithm, and show it off using the 
              R programming language and shiny application framework respectively."),
            p("This text prediction app takes a phrase into the text input box below. Once the 'Predict' button is pressed, the algorithm
              will (slowly) generate a 3 predictions for the next word, in order of liklihood, separated by |, using Good-Turing frequency estimation and the Katz back-off model."),
            p("More information on preprocessing and the algorithm are available in the tabs above."),
             wellPanel(textInput("text", label = h4("Text input"), 
                   value = NULL), 
            #br(),
            actionButton("goButton", "Predict", class = "btn btn-primary")
  ),
  
  conditionalPanel(class = "text-danger", "$('#value').hasClass('recalculating')", 
                   tags$div('Generating Prediction ... ')),
  
  wellPanel(verbatimTextOutput("value")

)


),
    tabPanel("Preprocessing",
             br(),
             p("For this capstone project for the Data Scientist Specialization, 
               students were provided with three large .txt files mined from public 
               sources: blogs, news sources, and Twitter. These three collections of text 
               serve as the text database, known as a 'corpus' in 
               lingustics,  to generate the final prediction algorithm. A summary table is below:"),
             tableOutput("metTable"),
             p("The three files are read into R, and a 5% sample subset from each,
               to train the algorithm on a corpus of a manageable size from a 
               computational perspective. From there, the following transformations
               were performed:"),
             p("* Twitter hashtags, ie. #DataScience, and user handles, ie.
               @Coursera, were removed"),
             p("* Removal of web addresses and hyperlinks"),
             p("* All punctuation is removed"),
             p("* All words converted to lower case"),
             p("* Numeric characters are removed"),
             p("* Extra white space created by the above transformations is removed"),
             p("Using the 'quanteda' package, a 'document frequency matrix' (dfm) is created
               for uni, bi, tri and four grams. A dfm describes the observed frequency of wordsin the corpus, in 
               the case of the uni-gram, and the frequency of each order of two, three and four
               words in the bi, tri, and four gram case respectively. In addition, 
               the uni-gram model was used to generate an ID and Words table, for use
               in converting Words to numeric and back, to aid in computational efficiency."),
             p("Further house keeping transformations are performed to set the data into 
               sufficiently 'nice' form for processing by the algorithm."),
             br(),
             p("For more information on document-term matrix (analagous to a dfm), check",
               a("the wikipedia page.  ", href = "http://en.wikipedia.org/wiki/Document-term_matrix")),
             p("For a good introduction on n-grams, visit ", 
               a("the wikipedia page.", href = "http://en.wikipedia.org/wiki/N-gram")),
             br(),
             br(),
             br()
             ),
            
    tabPanel("Algorithm",
             br(),
             p("The algoritm selected for the final text prediction is the Katz Back-off model,
               which itself depends upon Good-Turing smoothing."),
             p("Good-Turing frequency estimation is a process for estimating the probability of 
               an unseen species, in this case next word, give observed other species. This is an 
               attempt to 'smooth out' the noise associated with the observed data."),
             p("Katz Backoff Model estimates the conditional probability of a word given its 
               history in the n-gram. This is done by 'backing off' to lower level n-gram models 
               under certain conditions. The Good-Turing frequency estimations are used in the Katz 
               calculations."),
             br(),
             p("For more information on Good_Turing Frequency Estimation check",
               a("the wikipedia page.  ", href = "http://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation")),
             p("For a good introduction on Katz Backoff Model, visit ", 
               a("the wikipedia page.", href = "http://en.wikipedia.org/wiki/Katz%27s_back-off_model")),
             br(),
             br(),
             br()
             )
)


)
)
)
