# import requests
# from bs4 import BeautifulSoup
# from datetime import datetime
# import time
from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.by import By

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
        
        # Wait for the table to load
        # time.sleep(3)  # Adjust if needed for slower page loads

        # Locate the table and extract data
        rows = driver.find_elements(By.CSS_SELECTOR, "#rate-table tr")
        metal_prices = {}
        for row in rows:
            cells = row.find_elements(By.TAG_NAME, "td")
            if len(cells) >= 3:
                metal_name = cells[0].text.strip()
                price = cells[1].text.strip()
                unit = cells[2].text.strip()
                metal_prices[metal_name] = (price, unit)

        return metal_prices

    finally:
        driver.quit()

# Example usage
date = '2024-12-29'
prices = fetch_metal_prices_firefox(date)
if prices:
    for metal, (price, unit) in prices.items():
        print(f'{metal}: {price} per {unit}')
