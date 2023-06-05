
test_that("Error if token doesn't exist", {
  expect_error(build_survey(BW_app_path = tempdir()))
  })


test_that("Returns UI ", {
results <- build_survey(
  survey_title = "Insert survey title here",
  sidepanel_message = "Insert sidepanel message here.",
  start_message = "Insert start message here",
  screen_message = "Insert screen message here",
  screen_questions = c("Insert participant screening question here"),
  screen_questions_ID = c("Screening_Q1"),
  screen_response_options = list(c(
    "You will be removed:SCREEN",
    "You will not be removed"
  )),
  start_questions = c(
    "Insert start question: text",
    "Insert start question: selectbox",
    "Insert start question: checkbox",
    "Insert start question: likert_five",
    "Insert start question: likert_seven"
  ),
  start_questions_ID = c(
    "text_SQ1",
    "selectbox_SQ2",
    "checkbox_SQ3",
    "likert_five_SQ4",
    "likert_seven_SQ5"
  ),
  start_questions_type = c("text",
                           "selectbox",
                           "checkbox",
                           "likert_five",
                           "likert_seven"),
  start_response_options = list(
    c('Choose',  'one',  'of', 'these'),
    c('Choose',  'multiple',  'of', 'these')
  ),
  biowell_situations = c(
    "Insert description of biowell situation 1.",
    "Insert description of biowell situation 2."
  ),
  biowell_situations_ID = c("situation1", "situation2"),
  biowell_questions = list(
    c("Insert biodiversity stem question here BW1"),
    c(
      "Insert biodiversity stem question here - BW2",
      "Insert biodiversity stem question here - BW3"
    )
  ),
  biowell_questions_ID = list(c("BW.S1.Q1"),
                              c("BW.S2.Q1",
                                "BW.S2.Q2")),
  end_questions = c(
    "Insert end question: text",
    "Insert end question: selectbox",
    "Insert end question: checkbox",
    "Insert end question: likert_five",
    "Insert end question: likert_seven"
  ),
  end_questions_ID = c(
    "text_EQ1",
    "selectbox_EQ2",
    "checkbox_EQ3",
    "likert_five_EQ4",
    "likert_seven_EQ5"
  ),
  end_questions_type = c("text",
                         "selectbox",
                         "checkbox",
                         "likert_five",
                         "likert_seven"),
  end_response_options = list(
    c('Choose',  'one',  'of', 'these'),
    c('Choose',  'multiple',  'of', 'these')
  ),
  end_message = "Insert end message here.",
  Dropbox_App_folder = "insert_Dropbox_App_folder_name",
  BW_app_path = tempdir(),
  organisation = "BIOWELL package example",
  organisation_website = "https://github.com/r-a-dobson/BIOWELL",
  all_questions = T,
  all_sliders = F,
  user_report = TRUE,
  offline_mode = TRUE,
  return = "ui"
)
expect_true(inherits(results,"list"))
})

test_that("Returns server ", {
  results <- build_survey(
    survey_title = "Insert survey title here",
    sidepanel_message = "Insert sidepanel message here.",
    start_message = "Insert start message here",
    screen_message = "Insert screen message here",
    screen_questions = c("Insert participant screening question here"),
    screen_questions_ID = c("Screening_Q1"),
    screen_response_options = list(c(
      "You will be removed:SCREEN",
      "You will not be removed"
    )),
    start_questions = c(
      "Insert start question: text",
      "Insert start question: selectbox",
      "Insert start question: checkbox",
      "Insert start question: likert_five",
      "Insert start question: likert_seven"
    ),
    start_questions_ID = c(
      "text_SQ1",
      "selectbox_SQ2",
      "checkbox_SQ3",
      "likert_five_SQ4",
      "likert_seven_SQ5"
    ),
    start_questions_type = c("text",
                             "selectbox",
                             "checkbox",
                             "likert_five",
                             "likert_seven"),
    start_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    biowell_situations = c(
      "Insert description of biowell situation 1.",
      "Insert description of biowell situation 2."
    ),
    biowell_situations_ID = c("situation1", "situation2"),
    biowell_questions = list(
      c("Insert biodiversity stem question here BW1"),
      c(
        "Insert biodiversity stem question here - BW2",
        "Insert biodiversity stem question here - BW3"
      )
    ),
    biowell_questions_ID = list(c("BW.S1.Q1"),
                                c("BW.S2.Q1",
                                  "BW.S2.Q2")),
    end_questions = c(
      "Insert end question: text",
      "Insert end question: selectbox",
      "Insert end question: checkbox",
      "Insert end question: likert_five",
      "Insert end question: likert_seven"
    ),
    end_questions_ID = c(
      "text_EQ1",
      "selectbox_EQ2",
      "checkbox_EQ3",
      "likert_five_EQ4",
      "likert_seven_EQ5"
    ),
    end_questions_type = c("text",
                           "selectbox",
                           "checkbox",
                           "likert_five",
                           "likert_seven"),
    end_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    end_message = "Insert end message here.",
    Dropbox_App_folder = "insert_Dropbox_App_folder_name",
    BW_app_path = tempdir(),
    organisation = "BIOWELL package example",
    organisation_website = "https://github.com/r-a-dobson/BIOWELL",
    all_questions = T,
    all_sliders = F,
    user_report = TRUE,
    offline_mode = TRUE,
    return = "server"
  )
  expect_true(inherits(results,"function"))
})


# Test without questions

test_that("Works without questions ", {
  results <- build_survey(
    survey_title = "Insert survey title here",
    sidepanel_message = "Insert sidepanel message here.",
    biowell_situations = c(
      "Insert description of biowell situation 1.",
      "Insert description of biowell situation 2."
    ),
    biowell_situations_ID = c("situation1", "situation2"),
    biowell_questions = list(
      c("Insert biodiversity stem question here BW1"),
      c(
        "Insert biodiversity stem question here - BW2",
        "Insert biodiversity stem question here - BW3"
      )
    ),
    biowell_questions_ID = list(c("BW.S1.Q1"),
                                c("BW.S2.Q1",
                                  "BW.S2.Q2")),
    Dropbox_App_folder = "insert_Dropbox_App_folder_name",
    BW_app_path = tempdir(),
    organisation = "BIOWELL package example",
    organisation_website = "https://github.com/r-a-dobson/BIOWELL",
    all_questions = T,
    all_sliders = F,
    user_report = TRUE,
    offline_mode = TRUE,
    return = "ui"
  )
  expect_true(inherits(results,"list"))
})













test_that("Works in no IDs ", {
  results <- build_survey(
    survey_title = "Insert survey title here",
    sidepanel_message = "Insert sidepanel message here.",
    start_message = "Insert start message here",
    screen_message = "Insert screen message here",
    screen_questions = c("Insert participant screening question here"),
    screen_response_options = list(c(
      "You will be removed:SCREEN",
      "You will not be removed"
    )),
    start_questions = c(
      "Insert start question: text",
      "Insert start question: selectbox",
      "Insert start question: checkbox",
      "Insert start question: likert_five",
      "Insert start question: likert_seven"
    ),
    start_questions_type = c("text",
                             "selectbox",
                             "checkbox",
                             "likert_five",
                             "likert_seven"),
    start_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    biowell_situations = c(
      "Insert description of biowell situation 1.",
      "Insert description of biowell situation 2."
    ),
    biowell_questions = list(
      c("Insert biodiversity stem question here BW1"),
      c(
        "Insert biodiversity stem question here - BW2",
        "Insert biodiversity stem question here - BW3"
      )
    ),
    biowell_questions_ID = list(c("BW.S1.Q1"),
                                c("BW.S2.Q1",
                                  "BW.S2.Q2")),
    end_questions = c(
      "Insert end question: text",
      "Insert end question: selectbox",
      "Insert end question: checkbox",
      "Insert end question: likert_five",
      "Insert end question: likert_seven"
    ),
    end_questions_type = c("text",
                           "selectbox",
                           "checkbox",
                           "likert_five",
                           "likert_seven"),
    end_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    end_message = "Insert end message here.",
    Dropbox_App_folder = "insert_Dropbox_App_folder_name",
    BW_app_path = tempdir(),
    organisation = "BIOWELL package example",
    organisation_website = "https://github.com/r-a-dobson/BIOWELL",
    all_questions = T,
    all_sliders = F,
    user_report = TRUE,
    offline_mode = TRUE,
    return = "server"
  )
  expect_true(inherits(results,"function"))
})




test_that("Works if error when doing list for questions ", {
  results <- build_survey(
    survey_title = "Insert survey title here",
    sidepanel_message = "Insert sidepanel message here.",
    start_message = "Insert start message here",
    screen_message = "Insert screen message here",
    screen_questions = c("Insert participant screening question here"),
    screen_questions_ID = c("Screening_Q1"),
    screen_response_options = list(c(
      "You will be removed:SCREEN",
      "You will not be removed"
    )),
    start_questions = c(
      "Insert start question: text",
      "Insert start question: selectbox",
      "Insert start question: checkbox",
      "Insert start question: likert_five",
      "Insert start question: likert_seven"
    ),
    start_questions_ID = c(
      "text_SQ1",
      "selectbox_SQ2",
      "checkbox_SQ3",
      "likert_five_SQ4",
      "likert_seven_SQ5"
    ),
    start_questions_type = c("text",
                             "selectbox",
                             "checkbox",
                             "likert_five",
                             "likert_seven"),
    start_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    biowell_situations = c(
      "Insert description of biowell situation 1.",
      "Insert description of biowell situation 2."
    ),
    biowell_situations_ID = c("situation1"),
    biowell_questions = c("Insert biodiversity stem question here BW1"),
    biowell_questions_ID = list(c("BW.S1.Q1"),
                                c("BW.S2.Q1",
                                  "BW.S2.Q2")),
    end_questions = c(
      "Insert end question: text",
      "Insert end question: selectbox",
      "Insert end question: checkbox",
      "Insert end question: likert_five",
      "Insert end question: likert_seven"
    ),
    end_questions_ID = c(
      "text_EQ1",
      "selectbox_EQ2",
      "checkbox_EQ3",
      "likert_five_EQ4",
      "likert_seven_EQ5"
    ),
    end_questions_type = c("text",
                           "selectbox",
                           "checkbox",
                           "likert_five",
                           "likert_seven"),
    end_response_options = list(
      c('Choose',  'one',  'of', 'these'),
      c('Choose',  'multiple',  'of', 'these')
    ),
    end_message = "Insert end message here.",
    Dropbox_App_folder = "insert_Dropbox_App_folder_name",
    BW_app_path = tempdir(),
    organisation = "BIOWELL package example",
    organisation_website = "https://github.com/r-a-dobson/BIOWELL",
    all_questions = T,
    all_sliders = F,
    user_report = TRUE,
    offline_mode = TRUE,
    return = "ui"
  )
  expect_true(inherits(results,"list"))
})



