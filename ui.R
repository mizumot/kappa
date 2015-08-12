library(shiny)
library(shinyAce)


shinyUI(bootstrapPage(


    headerPanel("Cohen's Kappa and Other Interrater Agreement Measures"),



########## Adding loading message #########

tags$head(tags$style(type="text/css", "
#loadmessage {
position: fixed;
top: 0px;
left: 0px;
width: 100%;
padding: 10px 0px 10px 0px;
text-align: center;
font-weight: bold;
font-size: 100%;
color: #000000;
background-color: #CCFF66;
z-index: 105;
}
")),

conditionalPanel(condition="$('html').hasClass('shiny-busy')",
tags$div("Loading...",id="loadmessage")),

########## Added up untill here ##########



    mainPanel(
        tabsetPanel(position = "left", selected = "2 Raters (Nominal)",

        tabPanel("2 Raters (Nominal)",

            h2("2 Raters (Nominal)"),

            h4("Degree of agreement of the nominal scale by two raters"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text1", value="Rater1\tRater2\nB\tB\nA\tA\nC\tC\nB\tB\nB\tC\nC\tC\nB\tC\nA\tA\nC\tC\nC\tC\nC\tC\nC\tC\nB\tB\nA\tA\nB\tB\nC\tC\nA\tB\nC\tC\nA\tA\nB\tB", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data1.out"),

            br(),

            h3("Cohen's kappa"),
            verbatimTextOutput("test1.out"),
            
            p('If the data has more categories than binary (e.g., yes and no) with an ordinal structure (e.g., A > B > C, low < medium < high), consider reporting a weighted Kappa (Weights: squared).', br()),
            
            p('Criteria (Strength of agreement):', br(),
              '   < 0.20  Poor', br(),
              '0.21–0.40  Fair', br(),
              '0.41–0.60  Moderate', br(),
              '0.61–0.80  Good (Substantial)', br(),
              '0.81–1.00  Very good (Almost perfect)', br()
            ),
            
            br(),
            
            h3("Krippendorff's alpha reliability coefficient"),
            verbatimTextOutput("Krippendorff1.out"),
            
            br(),

            h3("Agreement plot"),

            plotOutput("pPlot1", height = "550px"),
                    
            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info1.out")
            ),









        tabPanel("2 Raters (Ordinal)",

            h2("2 Raters (Ordinal)"),

            h4("Degree of agreement of the nominal scale by two raters"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text2", value="Rater1\tRater2\n1\t1\n1\t1\n1\t1\n1\t1\n1\t1\n2\t2\n2\t2\n2\t2\n2\t2\n2\t2\n3\t3\n3\t3\n3\t3\n3\t3\n3\t3\n1\t2\n1\t3\n1\t3\n1\t2\n1\t2\n2\t1\n2\t3\n2\t3\n2\t1\n2\t1\n3\t1\n3\t2\n3\t1\n3\t2\n3\t2", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data2.out"),
            
            br(),

            h3("Cohen's kappa"),
            verbatimTextOutput("test2.out"),
            
            p('Criteria (Strength of agreement):', br(),
              '   < 0.20  Poor', br(),
              '0.21–0.40  Fair', br(),
              '0.41–0.60  Moderate', br(),
              '0.61–0.80  Good (Substantial)', br(),
              '0.81–1.00  Very good (Almost perfect)', br()
            ),
            
            br(),
            
            h3("Rank correlation coefficient"),
            
            radioButtons("method", "Type of correlation coefficient:",
                         list("Spearman's rank correlation coefficient (Spearman's rho)" = "Spearman",
                              "Kendall's tau rank correlation coefficient (Kendall's tau)" = "Kendall")),
            
            verbatimTextOutput("correl2.out"),    
       
            br(),
            
            h3("Krippendorff's alpha reliability coefficient"),
            verbatimTextOutput("Krippendorff2.out"),
            
            br(),
            
            h3("Plot"),
            plotOutput("pPlot2", height = "550px"),

            br(),
            br(),

            plotOutput("mPlot2"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info2.out")
            ),










        tabPanel("3 or More Raters (Nominal)",

            h2("3 or More Raters (Nominal)"), 
                 
            h4("Degree of agreement of the nominal scale by three or more raters"),                 
                     
            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row and the person's IDs in the first column. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text3", value="rater1\trater2\trater3\trater4\trater5\nA\tB\tB\tB\tB\nA\tA\tC\tC\tC\nC\tC\tC\tC\tC\nA\tA\tA\tA\tC\nA\tA\tA\tC\tC\nA\tB\tB\tB\tB\nA\tA\tA\tA\tA\nB\tB\tB\tB\tC\nA\tC\tC\tC\tC\nA\tA\tA\tC\tC",mode="r", theme="cobalt"),
            
            br(),

            h3("Categories selected by raters"),
            verbatimTextOutput("data3.out"),

            br(),

            h3("Fleiss' Kappa"),
            verbatimTextOutput("test3.out"),
            
            p('Criteria (Strength of agreement):', br(),
              '   < 0.40  Poor', br(),
              '0.40–0.75  Intermediate to Good', br(),
              'Over 0.75  Excellent', br()
            ),
            
            br(),
            
            h3("Krippendorff's alpha reliability coefficient"),
            verbatimTextOutput("Krippendorff3.out"),

            br(),
            
            h3("Plot"),

            plotOutput("pPlot3"),

            br(),
            br(),

            plotOutput("mPlot3"),
            
            br(),
            br(),
            
            
            strong('R session info'),
            verbatimTextOutput("info3.out")
            ),
        
        
        
        
        
        
        
        
        
        
        tabPanel("3 or More Raters (Ordinal)",
                 
                 h2("3 or More Raters (Ordinal)"), 
                 
                 h4("Degree of agreement of the ordinal scale by three or more raters"),                 
                 
                 p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),
                 
                 p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row and the person's IDs in the first column. Missing values should be indicated by a period (.) or NA.</div></b>")),
                 
                 aceEditor("text4", value="rater1\trater2\trater3\trater4\trater5\n1\t2\t2\t2\t2\n1\t1\t3\t3\t3\n3\t3\t3\t3\t3\n1\t1\t1\t1\t3\n1\t1\t1\t3\t3\n1\t2\t2\t2\t2\n1\t1\t1\t1\t1\n2\t2\t2\t2\t3\n1\t3\t3\t3\t3\n1\t1\t1\t3\t3",mode="r", theme="cobalt"),
                 
                 br(),
                 
                 h3("Categories selected by raters"),
                 verbatimTextOutput("data4.out"),
                 
                 br(),
                 
                 h3("Fleiss' Kappa"),
                 verbatimTextOutput("test4.out"),
                 
                 p('Criteria (Strength of agreement):', br(),
                   '   < 0.40  Poor', br(),
                   '0.40–0.75  Intermediate to Good', br(),
                   'Over 0.75  Excellent', br()
                 ),
                 
                 br(),
                 
                 h3("Kendall's coefficient of concordance for ranks (W)"),
                 verbatimTextOutput("W.out"),    
                 
                 br(),
                 
                 h3("Krippendorff's alpha reliability coefficient"),
                 verbatimTextOutput("Krippendorff4.out"),
                 
                 br(),
                 
                 h3("Plot"),
                 
                 plotOutput("pPlot4"),
                 
                 br(),
                 br(),
                 
                 plotOutput("mPlot4"),
                 
                 br(),
                 br(),
                 
                 
                 strong('R session info'),
                 verbatimTextOutput("info4.out")
        ),
        
        



        
        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(irr)'),br(),
            code('library(psych)'),br(),
            code('library(vcd)'),br(),
            code('library(lattice)'),br(),
            code('library(reshape2)'),br(),
            code('library(DescTools)'),br(),
            
            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/rep-chi', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("kappa","mizumot")')
            ),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

)
)
)
))