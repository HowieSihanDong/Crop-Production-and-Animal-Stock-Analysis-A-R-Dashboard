# Crop Production and Animal Stock Analysis Dashboard

This project provides an interactive dashboard for visualizing global crop production, animal stock, and per capita data across multiple years and regions. The dashboard uses various types of visualizations, including PieDonut charts, choropleth maps, line charts with animation, and dynamic layouts, all designed to enhance user interactivity and data analysis.

## Features

### Tab 1: Global Production Overview
- **Visualizations**:  
  - **Map**: Interactive map showing crop production and animal stock across different regions.
  - **PieDonut Chart**: Provides a visual breakdown of global production distribution.
  - **Pie Chart**: Shows regional proportions with text summaries.
  
- **Interactivity**:
  - **Slider**: Use the slider to select a specific time period. All visualizations update in real time.
  - **Interactive Maps**:  
    - Hover over regions to highlight them, displaying updated information.
    - Values are normalized and the color saturation represents quantitative variables for better data understanding.
  - **Filters**: Select target items (like crops or animals) to update all visualizations at once, enhancing user experience.

### Tab 2: Production vs Population Trends
- **Three Modes**:  
  - **Global Analysis**: Displays data for all regions globally.
  - **Crop Analysis**: Focuses on crop production trends.
  - **Live Animals Analysis**: Focuses on live animal stock trends.

- **Mode-Specific Layout**:
  - When switching between "Global Analysis" and "Crop/Live Animals Analysis," the layout changes dynamically.
  - **Default Behavior**: Afghanistan is selected as the default target country when switching to Crop or Live Animals modes.
  
- **Interactivity**:
  - Clicking on regions in the map updates the mode to either **Crop Analysis** or **Live Animal Analysis**, with the targeted region reflecting the selected data.
  
- **Visualizations**:
  - **Dual-Y Axis Line Charts**: For Global Analysis, showing the relationship between production and live animal stock, as well as crop production.
  - **Animated Line Chart**: Shows the global per capita production for crops and live animals.
  - **Mode-Specific Charts**:  
    - For Crop or Live Animal mode, a dual-Y axis chart and animated line chart show data for the selected country. Users can add multiple countries for comparison.

- **Data Presentation**:
  - **Normalized vs. Overall Values**: Line charts show normalized (per capita) and overall values to provide multi-perspective data.
  - **Color Hue**: Green represents crops, while orange-red represents animals to easily distinguish between the two categories.
  - **Minimal Decoration**: Grid lines and other distractions are minimized to increase data density and focus on key insights.
  - **Gestalt Law Consideration**: The graphs are designed with symmetry and balance in mind, adhering to Gestalt principles to avoid cognitive overload.

### General Features
- **Responsive Layout**: The layout adjusts based on the selected visualization and mode.
- **Data Integrity**: Visualizations are designed to present data in a coherent and understandable format while avoiding redundancy.
- **Visual and Intellectual Hierarchy**: Background contrast and color choices are optimized for readability and analysis.
- **Data Normalization**: The use of per capita and overall data allows for comprehensive insights into both regional and global trends.

