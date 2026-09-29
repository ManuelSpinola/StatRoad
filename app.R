# Launch the ShinyApp (Do not remove this comment)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
StatRoad::run_app()
