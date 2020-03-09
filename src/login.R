login <- function(){
  tagList(
    div(id = "login",
          wellPanel(textInput("userName", tags$p(style = 'color: black', "Username")),
                    passwordInput("passwd", tags$p(style = 'color: black', "Password")),
                    br(),
                    actionButton("Login", "Log in"),
                    verbatimTextOutput("dataInfo")
          )
    ),
    tags$style("#login {
                                           font-size:15px; 
                                           text-align: left;
                                           position:absolute;
                                           top: 40%;
                                           left: 50%;
                                           margin-top: -100px;
                                           margin-left: -150px; 
                                           color: black;
                                         }")
  )}