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
        # <table class="table table-striped table-hover table-condensed success">
        table = soup.find('table', {'class': 'table table-striped table-hover table-condensed success'})
        headers = table.find_all('thead')
        # Print the table content
        # print(headers)
        # print(f"Table {table}")
        # Print the single td elements of separate tr in new lines. The tbody has id="rate-table"
        for row in table.find_all('tr'):
            for cell in row.find_all('td'):
                print(cell.text)
            print("\n")
    finally:
        pass

    #     # Locate the table where tbody id = rate-table and extract data
    #     rows = driver.find_elements(By.CSS_SELECTOR, "#rate-table tr")

    #     # Locate the table and extract data
    #     rows = driver.find_elements(By.CSS_SELECTOR, "#rate-table tr")
    #     metal_prices = {}
    #     print(f"Found {len(rows)} rows")
    #     print(rows[0].find_elements(By.CSS_SELECTOR, "td"))
    #     # print the content of the table
    #     for cell in rows[0].find_elements(By.CSS_SELECTOR, "td"):
    #         print(cell.text)
    #     for cell in rows[1].find_elements(By.CSS_SELECTOR, "td"):
    #         print(cell.text)


    #     # print(rows[1].find_elements(By.CSS_SELECTOR, "td"))
    #     print(driver.find_element(By.CSS_SELECTOR, "#rate-table").text)



    #     print("\n")
    #     # <tbody id="rate-table"><tr><td>Aluminum</td><td>1.1617<span class="hidden-xs"> USD</span></td><td><span class="hidden-xs">lb (</span>Pound<span class="hidden-xs">)</span></td><td class="hidden-xs">Dec 24, 2024</td></tr><tr><td>Cobalt</td><td>11.022<span class="hidden-xs"> USD</span></td><td><span class="hidden-xs">lb (</span>Pound<span class="hidden-xs">)</span></td>
    #     for row in rows:
    #         cells = row.find_elements(By.CSS_SELECTOR, "td")
    #         if len(cells) >= 3:
    #             metal = cells[0].text
    #             price = cells[1].text
    #             unit = cells[2].text
    #             metal_prices[metal] = (price, unit)
    #             # print(f'{metal}: {price} per {unit}')
    #     return metal_prices
    # except Exception as e:
    #     print(f"An error occurred: {e}")
    # finally:
    #     # Close the browser window
    #     driver.quit()

# Example usage
date = '2024-12-24'
prices = fetch_metal_prices_firefox(date)
if prices:
    for metal, (price, unit) in prices.items():
        print(f'{metal}: {price} per {unit}')
