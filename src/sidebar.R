sidebar <- function() {
  tagList(
    dashboardSidebar(width = 270,
                     sidebarUserPanel("User Name",
                                      subtitle = a(href = "#", icon("user", class = "text-success"), "Online")
                     ),
                     sidebarMenu(
                       menuItem(tags$em("Pairwise Textual Comparison", style="font-size:120%"),
                                icon=icon("random"), tabName = "sbm_PTC"),
                       menuItem(tags$em("Stylometric Clustering", style="font-size:120%"),
                                icon=icon("user-edit"), tabName = "sbm_SM")
                     )
    )
  )
}