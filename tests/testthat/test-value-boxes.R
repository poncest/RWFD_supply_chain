# test-value-boxes.R
test_that("Value boxes handle 'No data' states correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test with empty filter results
    session$setInputs(warehouse = "NONEXISTENT", carrier = "All")

    filtered <- isolate(filtered_warehouse())
    expect_equal(nrow(filtered), 0)
    
    # Test that validation occurs
    expect_error(
      output$total_storage_cost,
      "No data"
    )
    
    expect_error(
      output$avg_cost_unit,
      "No data"
    )
    
    expect_error(
      output$ground_transport_pct,
      "No data"
    )
    
    expect_error(
      output$top_route_cost,
      "No data"
    )
  })
})