# Hotel Accommodation Price Dashboard for Top 5 Cities in Poland (Shiny App - R)

This repository contains a Shiny App built in R for interactive data visualization and analysis. The app allows users to explore data stored in the provided "data.xlsx" file through the web interface.

## Dataset Description

The data has been scraped from the website of a company offering online accommodation services. This agency acts as an intermediary, presenting users with offers from many hotels, hostels, and guesthouses in one place. The dataset pertains to available accommodations for 2 people in hotels, hostels, and guesthouses during the weekend of February 3-4, 2024, in the five largest cities in Poland.

### Variables

The dataset comprises 3079 records and 4 variables:

- **City**: The city where the accommodation is located.
- **Price**: The price for the accommodation (in PLN).
- **Rating**: Average user rating on the portal (on a scale from 1 to 10).
- **Reviews**: The number of reviews provided by users.

## Files

- `app.R`: This file contains the code for the Shiny App. It defines the user interface layout and functionality, as well as the server-side logic for data visualization.
- `data.xlsx`: This Excel file contains the data used by the Shiny App for visualization and analysis. The data was obtained from the website of one of the companies offering accommodations.

## Getting Started

To run the Shiny App locally, follow these steps:

1. Clone this repository to your local machine.
2. Ensure you have R and the necessary packages installed.
3. Navigate to the directory where the repository is cloned.
4. Open `app.R` in RStudio.
5. Run the app by clicking on the "Run App" button within RStudio.
6. The Shiny App should open in your default web browser, allowing you to interact with the data.

## Usage

At the top of the page, there are tabs that allow you to switch between charts. On the left side of each page, there is a panel that allows you to specify the range of the dataset and the variables to be plotted on the chart. In the central part of the page, there is the main chart.

## License

This project is licensed under the [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/) license.

- **Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)**

For details, please see the [license file](LICENSE.md).




