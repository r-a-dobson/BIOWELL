
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **BIOWELL**

<!-- badges: start -->

[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Codecov test
coverage](https://codecov.io/gh/r-a-dobson/BIOWELL/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-a-dobson/BIOWELL?branch=main)
[![R-CMD-check](https://github.com/r-a-dobson/BIOWELL/workflows/R-CMD-check/badge.svg)](https://github.com/r-a-dobson/BIOWELL/actions)
[![DOI](https://img.shields.io/badge/DOI-10.1016/j.jenvp.2022.101921-green.svg)](https://doi.org/10.1016/j.jenvp.2022.101921)

<!-- badges: end -->

**Introducing `BIOWELL`: An R package for effortlessly creating and
sharing customised surveys that include the BIO-WELL scale**.

The BIO-WELL scale provides an important tool for assessing the
**contribution of biodiversity to human wellbeing**. The scale was
developed to address the limited understanding of how interactions with
nature and biodiversity impact our health and wellbeing.

For more information on the BIO-WELL project and to explore our latest research and resources, please visit the official [BIO-WELL website](https://research.kent.ac.uk/bio-well/).

With the `BIOWELL` package, you have the flexibility to **design and
analyse surveys tailored to your specific research needs**.

This **user-friendly** package provides a range of functions that
**streamline the survey development process and analysis**.

# **What is the BIO-WELL scale?**

BIO-WELL is a reliable and validated self-report scale that links
biodiversity to human wellbeing. It incorporates ecological metrics
(e.g. species diversity) and multi-sensory attributes of biodiversity
(e.g. smells, shapes). Five studies, involving 2962 participants, detail
its development, validation, and psychometric properties (Irvine et al.,
2023).

BIO-WELL provides a **valuable tool for researchers and
decision-makers** to understand the biodiversity-health/wellbeing
relationship and evaluate interventions (Fig 1).

<a href='https://github.com/r-a-dobson/BIOWELL'><img src="https://raw.githubusercontent.com/r-a-dobson/BIOWELL/main/man/Vignette2/Vignette2_0.png" align="centre" height="650" width = "100%"/></a>

Fig. 1. The biodiversity-wellbeing scale, BIO-WELL. Through an
introductory statement, participants are invited to figuratively place
themselves in a nearby woodland and to think about the biodiversity.
Stem questions asked about biodiversity metrics (e.g. abundance, species
diversity), as well as biodiversity attributes (e.g. smells, colours,
shapes). Wellbeing response items reflected either physical, emotional,
cognitive, social or spiritual wellbeing, following a
biopsychosocial-spiritual model of health. They are in Visual Analogue
Scale format, with a positive and negative linguistic anchor either side
of a slider ranging from 0 to 100, allowing participants to indicate
their wellbeing along the line. Two of the seventeen biodiversity stem
questions are included here for illustrative purposes.

**Read more about the development and application of BIO-WELL in:**

[Irvine, K.N., Fisher, J.C., Bentley, P.R., Nawrath, M., Dallimer, M.,
Austen, G.E., Fish, R. and Davies, Z.G., 2023. BIO-WELL: The development
and validation of a human wellbeing scale that measures responses to
biodiversity. Journal of Environmental Psychology, 85, p.101921.
doi:10.1016/j.jenvp.2022.101921.](https://doi.org/10.1016/j.jenvp.2022.101921)

## Benefits of BIO-WELL

- Combines measures of the environment and wellbeing into a **single
  scale**.

- Includes stem questions that specifically ask participants to consider
  their wellbeing in relation to **different elements of biodiversity**,
  such as species diversity, ecological processes, and attributes like
  sounds.

- Captures the **holistic experience of biodiversity** using a
  biopsychosocial-spiritual model of wellbeing, including **physical,
  emotional, cognitive, social, and spiritual domains of wellbeing** and
  a response format that can capture the wellbeing continuum from
  negative to positive.

- Applicable to **diverse research settings**, including scenario-based
  studies, in situ evaluations, and studies that incorporate measures of
  actual and perceived biodiversity.

- The scale’s qualitative underpinning and strong psychometric
  properties make it a **reliable and valid instrument**.

- It has been developed with input from the target population and
  **minimizes participant burden** by using a small number of response
  items.

# Package features

1)  **Customisation**: Adapt the scale to diverse research settings and
    environments. Whether you’re studying woodlands, wetlands, urban
    parks, or other ecological contexts, the `BIOWELL` package allows
    for easy modification of stem questions and attributes to suit the
    specific environment under investigation.

2)  **Validity and Reliability**: Leverage the strength of the BIO-WELL
    scale, which has undergone rigorous validation and psychometric
    evaluation in previous studies. Benefit from a robust measurement
    tool that ensures the quality of data collected through your custom
    surveys.

3)  **Seamless Integration**: The `BIOWELL` package integrates the R
    statistical programming environment, offering a familiar platform
    for survey design and analysis.

    - Upload your BIO-WELL surveys to the free **Shiny Server** for
      sharing surveys via URL.

    - Utilises free cloud storage on **Dropbox** for survey response
      data.

    - Take advantage of **R’s extensive packages** to analyse your
      survey response data.

4)  **Documentation**: The package comes with comprehensive
    documentation, including Vignette tutorials and function examples,
    to assist you in utilising the `BIOWELL` package to its full
    potential.

# Get started!

Install the `BIOWELL` package by running the line below!

``` r
devtools::install_github("r-a-dobson/BIOWELL")
```

There are five stages in the BIOWELL package framework (Fig 2). The
functions available for each stages are described in the package’s five
vignettes.

<a href='https://github.com/r-a-dobson/BIOWELL'><img src="https://raw.githubusercontent.com/r-a-dobson/BIOWELL/main/man/Vignette2/bw_framework2.png" align="centre" height="700" width = "100%"/></a>

Fig. 2. The BIOWELL package framework.

1)  **Activate Dropbox** - set-up cloud storage for survey response data
    with function `activate_dropbox()`.

<!-- -->

2)  **Build survey** - develop your custom BIO-WELL survey from start to
    finish with `build_survey()`.

3)  **Create URL** - upload your survey to the Shiny Servers and obtain
    survey access link to share with participants with `create_URL()`.

4)  **Download data** - download and combine responses from survey
    participants into R with `download_data()`.

5)  **Evaluate survey and explore patterns** - calculate key statistics
    including Cronbach’s Alpha and Item-Total with `evaluate_survey()`
    and then generate plots of BIO-WELL scores across environments and
    stem questions with `explore_data()`.

## Need help learning the BIOWELL package functions?

For more details, see the package manual
[here](https://github.com/r-a-dobson/BIOWELL/blob/main/man/manual/BIOWELL_1.0.pdf)
for more details on each function.

Try our package vignettes that guide you through the stages from
building custom BIO-WELL surveys to analysing survey response data!

If you encounter an error or bug when installing and using BIOWELL,
please post a comment
[here](https://github.com/r-a-dobson/BIOWELL/issues) for guidance and
support from us.
