
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Research Article

This repository contains the code and data used to carry analyses for
this research article:

**Doré et al., 2020 - Relative effects of anthropogenic pressures,
climate, and sampling design on the structure of pollination networks at
the global scale.** [Link to be posted]()

All content is available
[here](https://github.com/MaelDore/Pollination_networks).

## Contents

  - [:file\_folder: **Data**](Data/) directory contains the network
    datasets used in the analyses.

  - [:file\_folder: **Figures**](Figures/) directory contains all the
    intermediate figures generated by the scripts showing effects of
    each explanatory variable on each response variable.

  - [:file\_folder: **Final\_Figures**](Final_Figures/) directory
    contains the final figures displayed in the article.

  - [:file\_folder: **Models\_outputs**](Models_outputs/) directory
    contains the summary of model outputs for the models presented in
    the main text.

  - [:file\_folder:
    **Models\_outputs\_no\_sampling**](Models_outputs_no_sampling/)
    directory contains the summary of model outputs for the models run
    without sampling variables. They are used for analyses and figures
    presented in SM6.

  - [:file\_folder:
    **Models\_outputs\_with\_polar**](Models_outputs_with_polar/)
    directory contains the summary of model outputs for the models run
    with additional three arctic networks. They are used for analyses
    and figures presented in SM5.

  - [:file\_folder: **Scripts**](Scripts/) directory contains the
    scripts used to run the analyses presented in the main text
    
      - [:file\_folder: **Scripts/SM**](Scripts/SM/) sub-directory
        contains the scripts used to run the analyses presented in the
        Supplementary

  - [:file\_folder: **SM\_Figures**](SM_Figures/) directory contains all
    the intermediate figures generated by the scripts for analyses
    presented in Supplementary. These figures display beta-coefficients
    associated with each model.

  - [:file\_folder: **SM\_Final\_figures**](SM_Final_figures/) directory
    contains the final figures displayed in the Supplementary.

  - [:file\_folder: **SM\_Tables**](SM_Tables/) directory contains the
    tables displayed in the Supplementary.

  - [:file\_folder: **SM\_Final\_figures**](SM_Final_figures/) directory
    contains the tables displayed in the Supplementary.

  - [:file\_folder: **Tables**](Tables/) directory contains the tables
    displayed in the main text.

**Note:** The raw interaction network data are available at [Link to be
posted]()

## How to run it

This research has been developed using the statistical programming
language R. To run the analyses, you will need installed on your
computer the [R software](https://cloud.r-project.org/) itself and
optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the entire project as a `.zip` from [this
URL](/archive/master.zip). After unzipping:

  - Open the `Pollination_networks.Rproj` file, found at the root of the
    project, in RStudio

  - Run sequentially the scripts found in the [:file\_folder:
    **Scripts**](Scripts/) folder. It will rebuild the model outputs and
    figures presented in the main text of the article.

  - Run sequentially the scripts found in the [:file\_folder:
    **Scripts/SM**](Scripts/SM/) sub-folder. It will rebuild the model
    outputs and figures presented in the Supplementaries of the article.

## How to cite

Please cite this research article as:

> Doré et al., 2020 - Relative effects of anthropogenic pressures,
> climate, and sampling design on the structure of pollination networks
> at the global scale.
