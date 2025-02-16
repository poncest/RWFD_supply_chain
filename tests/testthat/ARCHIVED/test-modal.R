# test-modal.R

test_that("Info modal displays correct content", {
  # Use here package to get correct paths
  source(here("global.R"))
  source(here("app.R"))
  
  testServer(server, {
    session$setInputs(show_info = 1)
    
    # Test modal content
    expect_true(grepl("2013-05-26", output$modal))
    expect_true(grepl("Kalganova", output$modal))
    expect_true(grepl("Steven Ponce", output$modal))
  })
})