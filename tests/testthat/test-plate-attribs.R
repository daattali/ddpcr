context("plate-attribs")

test_that("test basic plate attribute getters/setters", {
  plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
  
  expect_identical(
    plate %>% type,
    "custom_thresholds"
  )
  expect_identical(
    plate %>% type(all = TRUE),
    c("custom_thresholds", "ddpcr_plate")
  )
  
  expect_identical(
    plate %>% plate_data,
    plate[['plate_data']]
  )
  
  expect_identical(
    plate %>% plate_meta,
    plate[['plate_meta']]
  )
  
  expect_identical(
    plate %>% status,
    1L
  )
  
  expect_identical(
    plate %>% next_step %>% status,
    2L
  )
  
  expect_false(plate %>% analysis_complete)
  
  expect_true(plate %>% analyze %>% analysis_complete)

  expect_identical(
    plate %>% name,
    "small"
  )
  
  expect_identical(
    plate %>% `name<-`("newname") %>% name,
    "newname"
  )  
  
  expect_identical(
    plate %>% params,
    plate[['params']]
  )
  
  expect_identical(
    plate %>% clusters,
    plate[['clusters']]
  )  

  expect_identical(
    plate %>% cluster(cluster = 'OUTLIER'),
    2L
  )

  expect_identical(
    plate %>% cluster_name(cluster = 2),
    "OUTLIER"
  )     
  
  expect_true(plate %>% has_step("REMOVE_OUTLIERS"))
  
  expect_false(plate %>% has_step("REMOVE_CRAPPY"))
  
  expect_identical(
    plate %>% x_var,
    "HEX"
  )
  
  expect_identical(
    plate %>% `x_var<-`("newx") %>% x_var,
    "newx"
  )  
  
  expect_identical(
    plate %>% y_var,
    "FAM"
  )
  
  expect_identical(
    plate %>% `y_var<-`("newy") %>% y_var,
    "newy"
  )  
 
})