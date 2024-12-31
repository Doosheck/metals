from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.by import By
import time
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
        time.sleep(1)

        # Export the page source to a file for debugging
        with open('page_source2.html', 'w') as f:
            f.write(driver.page_source)

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

# Example usage
date = '2024-12-24'
prices = fetch_metal_prices_firefox(date)
if prices:
    print(f'Metal prices for {date}:')
    for metal, (price, unit) in prices.items():
        print(f'{metal}: {price} per {unit}')
