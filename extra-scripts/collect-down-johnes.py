import yfinance as yf
import pandas as pd
from itertools import combinations

tickers = [
    "AAPL", "AMGN", "AXP", "BA", "CAT", "CSCO", "CVX", "DIS", "DOW", "GS",
    "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK",
    "MSFT", "NKE", "PG", "CRM", "TRV", "UNH", "V", "VZ", "WBA", "WMT"
]

start_date = "2024-07-01"
end_date = "2024-12-31"

data = yf.download(tickers, start=start_date, end=end_date)["Close"]

data.to_csv("./data/dow_jones_2024_S2_closing_prices.csv",  index=False, encoding='utf-8')

def save_combinations(path: str, k: int):
    combs = combinations(tickers, k)
    with open(path, "w") as file:
        for comb in combs:
            file.write(",".join(comb) + "\n")

save_combinations("data/stock_combinations.txt", 25)

print("Dados baixados com sucesso.")
print(data.head())
