from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.by import By
import pandas as pd
import time
import os
from bs4 import BeautifulSoup

def fetch_metal_prices_firefox(date: str):
    """
    Fetch metal prices using Firefox WebDriver for the specified date.
    """
    # Set up Firefox options (e.g., run in headless mode)
    firefox_options = Options()
    firefox_options.add_argument("--headless")  # Run without opening a browser window (optional)

    # Specify the path to GeckoDriver (if not using Homebrew)
    firefox_service = Service('/opt/homebrew/bin/geckodriver')  # Update with your GeckoDriver path

    try:
        # Initialize Firefox WebDriver
        driver = webdriver.Firefox(service=firefox_service, options=firefox_options)

        # Navigate to the target URL
        url = f'https://www.dailymetalprice.com/metaltables.php?d={date}'
        driver.get(url)
        
        # Wait for the table to load (adjust time.sleep if needed for slower page loads)
        time.sleep(0.5)

        # Export the page source to a file for debugging
        # with open('page_source2.html', 'w') as f:
        #     f.write(driver.page_source)

        html = driver.page_source
        soup = BeautifulSoup(html, 'html.parser')
        table = soup.find('table', {'class': 'table table-striped table-hover table-condensed success'})
        metal_prices = {}
        for row in table.find_all('tr'):
            cells = row.find_all('td')
            if len(cells) >= 3:
                metal = cells[0].text
                price_currency = cells[1].text
                price = price_currency.split(' ')[0]
                unit = cells[2].text
                metal_prices[metal] = (price, unit)
        return metal_prices
    finally:
        driver.quit()

def get_metal_prices(from_date: str, to_date: str, metals: list, save_csv: bool = False):
    """
    Fetch metal prices for a range of dates and save to separate CSV files if requested.
    """
    metal_prices = {metal: [] for metal in metals}

    # Iterate over the date range
    for date in pd.date_range(from_date, to_date):
        if date.weekday() < 5:  # Only fetch prices for weekdays (0=Monday, 4=Friday)
            print(f"Fetching metal prices for {date.strftime('%Y-%m-%d')}...")
            date_str = date.strftime('%Y-%m-%d')
            prices = fetch_metal_prices_firefox(date_str)
            if prices:
                for metal in metals:
                    if metal in prices:
                        price, unit = prices[metal]
                        metal_prices[metal].append({'Date': date_str, 'Price': price, 'Unit': unit})

    if save_csv:
        # Save each metal's data to a separate CSV file in the 'data' folder
        for metal, records in metal_prices.items():
            if records:  # Only save if there are records
                df = pd.DataFrame(records)
                if not os.path.exists('data'):
                    os.makedirs('data')
                filename = f"data/{metal.replace(' ', '_')}_prices.csv"
                df.to_csv(filename, index=False)
                print(f"Saved {metal} data to {filename}")

    return metal_prices

date = '2024-12-24'
metals = ['Nickel', 'Copper', 'Lithium', 'Cobalt']
from_date = '2024-01-01'
to_date = '2024-12-31'
metal_prices = get_metal_prices(from_date, to_date, metals, save_csv=True)
