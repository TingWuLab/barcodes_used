# Barcode Query App

[Link to online App](https://wulab.connect.hms.harvard.edu/barcodes_used/)

This repository contains an R Shiny application designed to help users query their barcode sequences against a library of available barcodes. The app provides an easy-to-use interface for uploading your query sequences, selecting from a variety of barcode datasets, and retrieving a match vector indicating the presence or absence of each sequence in the selected barcode library.

## Features
Barcode Libraries: Choose from a list of available barcode sets, with options for "with toe" and "toeless" sequences.
File Upload: Supports uploading of query sequences in .txt, .csv, and .tsv formats.
Match Vector: Automatically computes and displays a vector indicating matches (1) and non-matches (0) for your query sequences.
Clipboard Integration: Easily copy the match vector to your clipboard for further processing.
Getting Started
To run the app locally, follow these steps:

## Prerequisites
Ensure that you have R and the following R packages installed:

shiny
shinyjs
dplyr
readr
tibble
clipr
You can install these packages using the following command:

> install.packages(c("shiny", "shinyjs", "dplyr", "readr", "tibble", "clipr"))

## Running the App locally
Clone this repository to your local machine.
Set your working directory to the cloned repository directory.
Open app.R in your R environment (e.g., RStudio).
Run the app by executing the following command in your console:

> shiny::runApp()

## Usage
Select Barcodes: Choose the desired barcode dataset from the dropdown menu.
Upload Query File: Click on "Upload query sequences" to select and upload your sequence file.
View Matches: The app will display a table with your sequences and a match column.
Copy Match Vector: Click the "Copy match vector to clipboard" button to copy the match results for external use.
Contributing
Contributions are welcome! If you have suggestions or run into issues, please create an issue or submit a pull request.

## License
This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgements
This app leverages data hosted by the HMSRSC-Wulab. If using this data, please ensure that appropriate acknowledgments and citations are made where applicable.
