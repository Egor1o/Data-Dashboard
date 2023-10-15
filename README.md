# Data-Dashboard


Project is a data visualization tool for analyzing public health data from the Finnish Institute for Health and Welfare (THL).

## Overview

Data-dashboard is a data visualization tool designed to fetch and analyze public health data from the Finnish Institute for Health and Welfare (THL). 
It provides a graphical user interface for creating and analyzing charts based on various health metrics, such as COVID-19 cases, in different 
geographical regions and time frames. The tool supports the creation of different types of charts, including bar charts, line charts, and pie charts, and allows 
users to customize colors and comments for data points.

## How to Clone

Make sure you have Git installed on your system.

To clone this repository to your local machine, you can use the following command:

```bash
git clone https://github.com/Egor1o/Data-Dashboard.git
```

## Running the Scala Application

1. Open your terminal and navigate to the 'Data-dashboard' directory


2. Launch the sbt (Scala Build Tool) console by running the following command:

   ```shell
   sbt
   ```

3. In the sbt console, enter the following command to run your application:

   ```shell
   ~run
   ```

   - The '~' character is optional but recommended, as it enables sbt to automatically re-run the application whenever you save changes,
   - facilitating a fast edit/run/debug cycle.
   - sbt will create a 'target' directory during the build process; you can typically ignore this directory.

Your Scala application should now be up and running.


## Usage
Use the graphical user interface to select the region, time frame, and health metrics for your chart.
Customize the colors and add comments to data points as needed.
Save and load your charts for later analysis.
Use the tool's features for comparing and analyzing data points.
