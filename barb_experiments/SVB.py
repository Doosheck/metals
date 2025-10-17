import requests
import pandas as pd
import re
from bs4 import BeautifulSoup
import time
from typing import List, Dict, Optional
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SVBLoansExtractor:
    def __init__(self):
        self.base_url = "https://www.sec.gov/Archives/edgar/data/719739/"
        self.headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        self.cik = "719739"  # SVB Financial Group CIK
        
        # Known 10-K filings for SVB (you can extend this list)
        self.filing_urls = [
            "000071973923000021/sivb-20221231.htm",  # 2022
            "000071973922000015/sivb-20211231.htm",  # 2021
            "000071973921000016/sivb-20201231.htm",  # 2020
            "000071973920000018/sivb-20191231.htm",  # 2019
            "000071973919000018/sivb-12312018x10k.htm",  # 2018
            "000119312518066016/d509574d10k.htm",     # 2017
            "000119312517065515/d270131d10k.htm",     # 2016
            "000119312516494214/d56550d10k.htm",      # 2015
        ]

    def fetch_filing(self, filing_path: str) -> Optional[str]:
        """Fetch a specific 10-K filing from SEC EDGAR."""
        url = self.base_url + filing_path
        try:
            logger.info(f"Fetching: {url}")
            response = requests.get(url, headers=self.headers)
            response.raise_for_status()
            time.sleep(1)  # Be respectful to SEC servers
            return response.text
        except requests.RequestException as e:
            logger.error(f"Error fetching {url}: {e}")
            return None

    def extract_loans_from_text(self, html_content: str, year: int) -> Dict[str, Optional[float]]:
        """Extract loan information from 10-K HTML content."""
        soup = BeautifulSoup(html_content, 'html.parser')
        text = soup.get_text()
        
        loans_data = {
            'year': year,
            'total_loans_gross': None,
            'total_loans_net': None,
            'allowance_for_losses': None
        }
        
        # Patterns to find loan amounts (in millions or thousands)
        patterns = [
            # Look for total loans patterns
            r'total\s+loans[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
            r'loans[^$]*?net[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
            r'loans.*?gross[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
            # Look for specific balance sheet items
            r'loans\s+and\s+leases[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
            r'total\s+loans\s+outstanding[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
        ]
        
        # Look for allowance for loan losses
        allowance_patterns = [
            r'allowance\s+for\s+(?:loan\s+)?losses[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
            r'provision\s+for\s+(?:loan\s+)?losses[^$]*?\$[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)',
        ]
        
        # Extract loan amounts
        text_lower = text.lower()
        
        for pattern in patterns:
            matches = re.findall(pattern, text_lower, re.IGNORECASE | re.MULTILINE)
            if matches:
                try:
                    # Take the largest reasonable amount (likely total loans)
                    amounts = [float(match.replace(',', '')) for match in matches]
                    amounts = [amt for amt in amounts if 1000 <= amt <= 100000]  # Filter reasonable amounts
                    if amounts:
                        loans_data['total_loans_gross'] = max(amounts)
                        break
                except ValueError:
                    continue
        
        # Extract allowance for losses
        for pattern in allowance_patterns:
            matches = re.findall(pattern, text_lower, re.IGNORECASE)
            if matches:
                try:
                    amounts = [float(match.replace(',', '')) for match in matches]
                    amounts = [amt for amt in amounts if 10 <= amt <= 1000]  # Smaller amounts for allowances
                    if amounts:
                        loans_data['allowance_for_losses'] = max(amounts)
                        break
                except ValueError:
                    continue
        
        # Calculate net loans if we have both gross and allowance
        if loans_data['total_loans_gross'] and loans_data['allowance_for_losses']:
            loans_data['total_loans_net'] = loans_data['total_loans_gross'] - loans_data['allowance_for_losses']
        
        return loans_data

    def extract_loans_from_tables(self, html_content: str, year: int) -> Dict[str, Optional[float]]:
        """Try to extract loan data from HTML tables (more precise method)."""
        soup = BeautifulSoup(html_content, 'html.parser')
        tables = soup.find_all('table')
        
        loans_data = {
            'year': year,
            'total_loans_gross': None,
            'total_loans_net': None,
            'allowance_for_losses': None
        }
        
        for table in tables:
            table_text = table.get_text().lower()
            
            # Look for balance sheet tables
            if 'balance sheet' in table_text or 'assets' in table_text:
                rows = table.find_all('tr')
                
                for row in rows:
                    row_text = row.get_text().lower()
                    cells = row.find_all(['td', 'th'])
                    
                    if len(cells) >= 2 and 'loan' in row_text:
                        # Try to extract numeric values from the row
                        for cell in cells[1:]:  # Skip first cell (description)
                            cell_text = cell.get_text().strip()
                            # Look for dollar amounts
                            amount_match = re.search(r'\$?[\s,]*(\d{1,3}(?:,\d{3})*(?:\.\d+)?)', cell_text)
                            if amount_match:
                                try:
                                    amount = float(amount_match.group(1).replace(',', ''))
                                    if 1000 <= amount <= 100000:  # Reasonable range for loans in millions
                                        if 'total' in row_text and 'loan' in row_text:
                                            loans_data['total_loans_gross'] = amount
                                        elif 'net' in row_text and 'loan' in row_text:
                                            loans_data['total_loans_net'] = amount
                                        elif 'allowance' in row_text:
                                            loans_data['allowance_for_losses'] = amount
                                except ValueError:
                                    continue
        
        return loans_data

    def extract_all_loans_data(self) -> pd.DataFrame:
        """Extract loans data from all available 10-K filings."""
        all_data = []
        
        for i, filing_path in enumerate(self.filing_urls):
            year = 2022 - i  # Starting from 2022 going backwards
            logger.info(f"Processing year {year}")
            
            html_content = self.fetch_filing(filing_path)
            if not html_content:
                logger.warning(f"Could not fetch filing for year {year}")
                continue
            
            # Try table extraction first (more precise)
            loans_data = self.extract_loans_from_tables(html_content, year)
            
            # If table extraction didn't work, try text extraction
            if not any([loans_data['total_loans_gross'], loans_data['total_loans_net']]):
                loans_data = self.extract_loans_from_text(html_content, year)
            
            if any([loans_data['total_loans_gross'], loans_data['total_loans_net']]):
                all_data.append(loans_data)
                logger.info(f"Extracted data for {year}: {loans_data}")
            else:
                logger.warning(f"No loan data found for year {year}")
        
        return pd.DataFrame(all_data)

    def save_to_csv(self, df: pd.DataFrame, filename: str = "svb_loans_data.csv"):
        """Save the extracted data to CSV."""
        df.to_csv(filename, index=False)
        logger.info(f"Data saved to {filename}")

def main():
    """Main function to run the extraction."""
    extractor = SVBLoansExtractor()
    
    # Extract loans data
    loans_df = extractor.extract_all_loans_data()
    
    if not loans_df.empty:
        # Sort by year
        loans_df = loans_df.sort_values('year').reset_index(drop=True)
        
        # Display results
        print("\nSilicon Valley Bank Loans Data (in millions USD):")
        print("=" * 60)
        print(loans_df.to_string(index=False))
        
        # Save to CSV
        extractor.save_to_csv(loans_df)
        
        # Create a simple plot if matplotlib is available
        try:
            import matplotlib.pyplot as plt
            
            plt.figure(figsize=(12, 6))
            if 'total_loans_gross' in loans_df.columns:
                plt.plot(loans_df['year'], loans_df['total_loans_gross'], 
                        marker='o', label='Total Loans (Gross)', linewidth=2)
            if 'total_loans_net' in loans_df.columns:
                plt.plot(loans_df['year'], loans_df['total_loans_net'], 
                        marker='s', label='Total Loans (Net)', linewidth=2)
            
            plt.title('Silicon Valley Bank - Loans Over Time')
            plt.xlabel('Year')
            plt.ylabel('Amount (Millions USD)')
            plt.legend()
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig('svb_loans_chart.png', dpi=300, bbox_inches='tight')
            plt.show()
            
        except ImportError:
            print("Install matplotlib to generate charts: pip install matplotlib")
    
    else:
        print("No loans data could be extracted.")

if __name__ == "__main__":
    main()