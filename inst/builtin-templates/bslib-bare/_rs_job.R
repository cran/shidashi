options("crayon.enabled" = TRUE)
options("crayon.colors" = 256)

NULL


app <- shidashi:::register_mcp_route(shiny::shinyAppDir("/Users/dipterix/Dropbox (Personal)/projects/shidashi/inst/builtin-templates/bslib-bare"))
shiny::runApp(appDir = app, launch.browser = TRUE, test.mode = FALSE,
    port = 4515L)
