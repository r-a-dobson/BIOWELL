test_that("Expect list to be returned", {
  results <-   add_biowell_scale(biowell_situations = c("S1","S2"),
                      biowell_questions = list(c("Q1"),c("Q2")),
                      all_sliders = F,
                      language = "english")
  expect_true(inherits(results,"list"))

})


test_that("Expect list to be returned", {
  results <-   sliders(2,  biowell_questions = list(c("Q1"),c("Q2")))
  expect_true(inherits(results,"list"))

})


test_that("Expect text to be returned", {
  results <- custom_slider_text(1,  language = "english")
  expect_true(inherits(results,"character"))

})



test_that("Expect list to be returned", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","likert_five"),
                           drop_down = list(c("a","b","c")),
                           return_names = FALSE,
                           return_screen = FALSE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_true(inherits(results,"list"))
})



test_that("Expect list of length three to be returned", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","likert_five"),
                           drop_down = list(c("a","b","c")),
                           return_names = FALSE,
                           return_screen = FALSE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_equal(length(results), 3)
  })



test_that("If return names TRUE, return vector length three", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","likert_five"),
                           drop_down = list(c("a","b","c")),
                           return_names = TRUE,
                           return_screen = FALSE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_equal(length(results), 3)
})



test_that("If return names TRUE, return character vector", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","likert_five"),
                           drop_down = list(c("a","b","c")),
                           return_names = TRUE,
                           return_screen = FALSE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_true(inherits(results,"character"))
})




test_that("If return screen TRUE, return dataframe", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","likert_five"),
                           drop_down = list(c("a:SCREEN","b","c")),
                           return_names = FALSE,
                           return_screen = TRUE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_true(inherits(results,"data.frame"))
})


test_that("If return screen TRUE, return dataframe with a row for each screened", {
  results <- add_questions(questions = c("Q1","Q2","Q3"),
                           type = c("text","selectbox","selectbox"),
                           drop_down = list(c("a:SCREEN","b","c:SCREEN"),
                                            c("d:SCREEN","a")),
                           return_names = FALSE,
                           return_screen = TRUE,
                           type_q = "start",
                           prior_qs = NULL)
  expect_equal(nrow(results), 3)
})


test_that("Expect FALSE if not all in n1", {
  expect_false(answers(c("text1"), n2 = c("text1","select2","check3")))
})

test_that("Expect TRUE if all in n1", {
  expect_false(answers(c("text1","select2","check3"), n2 = c("text1","select2","check3")))
})


test_that("Expect dataframe ", {
results<-sort_qa(qnames = c("text1","select2","checkbox3"),
        response_df = data.frame(text1 = "a",
                                 select2 = "b",
                                 checkbox31 = "e",
                                 checkbox32 = "f"),
        qs = c("ID1","ID2","ID3"),
        dropdownopt = list(c("b","c","d"),
                           c("e","f","g")))
  expect_true(inherits(results,"matrix"))
})


test_that("Expect equal to answers including all poss answers checkboxes ", {
  results<-sort_qa(qnames = c("text1","select2","checkbox3"),
                   response_df = data.frame(text1 = "a",
                                            select2 = "b",
                                            checkbox31 = "e",
                                            checkbox32 = "f"),
                   qs = c("ID1","ID2","ID3"),
                   dropdownopt = list(c("b","c","d"),
                                      c("e","f","g")))
  expect_equal(ncol(results),5)
})



data("sample_BW_data")

test_that("Expect class list returned ", {
results<-generate_report(results = sample_BW_data[1,],
                running_average = NA )
expect_true(inherits(results,"list"))
})


test_that("Expect list length one returned ", {
  results<-generate_report(results = sample_BW_data[1,],
                           running_average = NA )
  expect_equal(length(results),1)
})

test_that("Expect NA returned if offline TRUE ", {
  results<-sort_running_average(filePath,
                                Dropbox_App_folder,
                                token,
                                results,
                                offline_mode = TRUE)
  expect_equal(results,NA)
})


test_that("Returns dataframe ", {

df <- read.csv(paste0(testthat::test_path("test-files"),"/RAWRES.csv"))
results<-generate_results_data(start1 = Sys.time(),
                      df = df,
                      extracted_data_screen = NULL,
                      extracted_data_end = NULL,
                      extracted_data = NULL,
                      input_client = "6/1/2023, 10:49:39 AM" ,
                      input_tz = "Europe/London",
                      input_tz_ch = "10:49:39 GMT+0100 (British Summer Time)",
                      biowell_situations_ID = paste0("Sit",1) ,
                      biowell_questions_ID = list(paste0("BWQ",1:17) ))
  expect_true(inherits(results,"data.frame"))
})

test_that("Returns dataframe with 180 cols", {

  df <- read.csv(paste0(testthat::test_path("test-files"),"/RAWRES.csv"))

  results<-generate_results_data(start1 = Sys.time(),
                                 df = df,
                                 extracted_data_screen = NULL,
                                 extracted_data_end = NULL,
                                 extracted_data = NULL,
                                 input_client = "6/1/2023, 10:49:39 AM" ,
                                 input_tz = "Europe/London",
                                 input_tz_ch = "10:49:39 GMT+0100 (British Summer Time)",
                                 biowell_situations_ID = paste0("Sit",1) ,
                                 biowell_questions_ID = list(paste0("BWQ",1:17) ))
  expect_equal(ncol(results),180)
})


test_that("Returns dataframe with 180 cols PLUS 3 for extracted", {

  df <- read.csv(paste0(testthat::test_path("test-files"),"/RAWRES.csv"))

  results<-generate_results_data(start1 = Sys.time(),
                                 df = df,
                                 extracted_data_screen = data.frame(SCQ1 = TRUE),
                                 extracted_data_end = data.frame(EQ1 = "end"),
                                 extracted_data = data.frame(SQ1 = "start"),
                                 input_client = "6/1/2023, 10:49:39 AM" ,
                                 input_tz = "Europe/London",
                                 input_tz_ch = "10:49:39 GMT+0100 (British Summer Time)",
                                 biowell_situations_ID = paste0("Sit",1) ,
                                 biowell_questions_ID = list(paste0("BWQ",1:17) ))
  expect_equal(ncol(results),183)
})


test_that("Returns dataframe with one row", {

  df <- read.csv(paste0(testthat::test_path("test-files"),"/RAWRES.csv"))

  results<-generate_results_data(start1 = Sys.time(),
                                 df = df,
                                 extracted_data_screen = data.frame(SCQ1 = TRUE),
                                 extracted_data_end = data.frame(EQ1 = "end"),
                                 extracted_data = data.frame(SQ1 = "start"),
                                 input_client = "6/1/2023, 10:49:39 AM" ,
                                 input_tz = "Europe/London",
                                 input_tz_ch = "10:49:39 GMT+0100 (British Summer Time)",
                                 biowell_situations_ID = paste0("Sit",1) ,
                                 biowell_questions_ID = list(paste0("BWQ",1:17) ))
  expect_equal(nrow(results),1)
})

