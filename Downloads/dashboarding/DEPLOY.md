# Publish AeroFleet IQ to shinyapps.io

Use these steps from the Positron Terminal or R Console.

## 1. Open the project folder

In Positron, open:

```text
/Users/jebinlarosh/Downloads/dashboarding
```

## 2. Connect your shinyapps.io account

Go to <https://www.shinyapps.io>, sign in, then open:

```text
Account > Tokens > Show
```

Copy the generated `rsconnect::setAccountInfo(...)` command and run it in the Positron R Console.

It will look like this:

```r
rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT_NAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

Do not commit or share your token or secret.

## 3. Deploy the app

Run this from the Positron R Console:

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "aerofleet-iq-wind-dashboard",
  appTitle = "AeroFleet IQ Wind Farm Dashboard",
  launch.browser = TRUE
)
```

After deployment, shinyapps.io will print and open the public app URL.

## 4. Update the public app later

After editing `app.R`, redeploy with:

```r
rsconnect::deployApp()
```

## Required packages

The deployed app uses:

```r
shiny
bslib
dplyr
ggplot2
plotly
DT
tidyr
scales
sass
```
