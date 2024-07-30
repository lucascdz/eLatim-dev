# lexicographR
R code for lexicographR, a Shiny app to create digital dictionaries

This is an **alpha prototype intended as a proof of concept**. It has not been optimized in any way, and we have only tested it on a limited amount of data and use cases. 

If you encounter difficulties when using the app to process your dictionary or corpus data, please contact ligeia.lugli@london.ac.uk; we will be happy to help.

## how to cite

lexicographR app: Lugli, Ligeia (2024). lexicographR app. figshare. Software. https://doi.org/10.6084/m9.figshare.26196914

lexicographR user-guide:  Lugli, Ligeia and Tilak, Balavijayan (2024). lexicographR user guide. figshare. https://doi.org/10.6084/m9.figshare.26196914


## What does lexicographR do?

lexicographR is designed to help with three main tasks involved in the creation of digital dictionaries:

1. Augment pre-existing dictionary data with information extracted from corpora (e.g., frequency/dispersion information, collocations, and examples) and data visualization (representing either information extracted from the corpus, e.g., collocations, or curated information taken from the dictionary, e.g., graphs of word senses).

2. Design an interface for a digital dictionary (to display pre-existing dictionary data, corpus-derived data, or a mixture of both) and automatically produce the code to render the dictionary on a web browser, which can easily be deployed online.

3. Convert pre-existing dictionary data, potentially augmented with corpus data, to the machine-readable and interoperable JSON-LD format.

## Requirements

Windows 10+; macOS 11+ and any other operating system compatible with R 4+, the 2023 version of RStudio, and Quarto 1.4+.

To install R, go to https://cran.r-project.org/
To install RStudio, go to https://posit.co/download/rstudio-desktop/
To install Quarto, go to https://quarto.org/docs/get-started/

On Macs, XQuartz and Xcode also need to be installed.

## How to use

To start the lexicographR app, consult our video guide (LINK) or follow these steps:

### Option 1: From the command line
1. Make sure you have R and RStudio installed.
2. Clone or download this repository to your machine.
3. From the command line, navigate to the app repository on your machine and run `Rscript app.R`.
4. In the interactive app window, go to the documentation tab to read the user guide. The user guide is also available in this repository (UserGuide.html & UserGuide.Rmd).

### Option 2: From RStudio
1. Make sure you have R and RStudio installed.
2. Clone or download this repository to your machine.
3. Unzip the repository
4.In RStuodio create a “new project” with “existing directory” and browse for the unzipped repository (check RStudio documentation to read on how to create projects)
5. In the RStudio 'File' panel, navigate to the file app.R and open it.
6. Select all the text in the file and click 'Run'.
7. In the interactive app window, go to the documentation tab to read the user guide. The user guide is also available in this repository (UserGuide.html & UserGuide.Rmd).

**WARNING:** When running for the first time, the app may take a long time to launch, as various packages will need to be installed. If there are problems with package installation, we recommend installing the required packages one by one (either via the `install.packages()` function or through the RStudio 'Packages' tab) and reading the error messages carefully. A web search for the error message usually reveals how to resolve the issue. The required packages are listed under Imports in the DESCRIPTION file contained in this repo.

## Acknowledgements

This work was funded by the National Endowment for the Humanities (HAA-290402-23).

We thank all the user-testers of our project Democratizing Digital Lexicography (https://doi.org/10.6084/m9.figshare.c.7207656), who tested pre-release versions of this app in their own lexicographic endeavors; special thanks to Luis Quiñones, Lucas Dezotti, Stella Tagnin, Sostenes Rego, and Muborak Madrakhimova.
