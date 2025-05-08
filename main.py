import pandas as pd
import numpy as np
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from functools import partial
from typing import List, Tuple

# Constants
FILE_PATH = "data/dow_jones_2024_S2_closing_prices.csv"
RESULTS_DIR = "results"
RESULTS_FILE = "results/best_wallets_summary.txt"

def load_data(file_path: str) -> pd.DataFrame:
    """Load and preprocess the data."""
    df = pd.read_csv(file_path)
    return df.assign(Date=lambda x: pd.to_datetime(x["Date"])).set_index("Date")

def load_stock_combinations(file_path: str = "data/stock_combinations.txt") -> List[List[str]]:
    """Load stock combinations from file."""
    with open(file_path, "r") as f:
        return [line.strip().split(",") for line in f.readlines()]

def generate_weights(num_stocks: int = 25, max_weight: float = 0.2, total_weight: float = 1.0) -> np.ndarray:
    """Generate random weights with constraints."""
    weights = np.random.rand(num_stocks)
    weights = weights / np.sum(weights) * total_weight
    weights = np.where(weights > max_weight, max_weight, weights)
    return weights / np.sum(weights) * total_weight

def calculate_portfolio_metrics(weights: np.ndarray, 
                              df_wallet_values: np.ndarray, 
                              df_wallet_index: pd.Index, 
                              cov_matrix: np.ndarray) -> Tuple[float, float, float]:
    """Calculate portfolio performance metrics."""
    returns = np.dot(df_wallet_values, weights)
    portfolio_returns = pd.Series(returns, index=df_wallet_index)
    annual_return = portfolio_returns.mean() * 252
    volatility = np.sqrt(weights.T @ cov_matrix @ weights) * np.sqrt(252)
    sharpe = annual_return / volatility if volatility != 0 else 0
    return sharpe, annual_return, volatility

def process_single_weight(args: Tuple) -> Tuple:
    """Wrapper for single weight evaluation."""
    weights, df_wallet_values, df_wallet_index, cov_matrix = args
    sharpe, _, _ = calculate_portfolio_metrics(weights, df_wallet_values, df_wallet_index, cov_matrix)
    return (sharpe, weights.tolist())

def prepare_wallet_data(wallet: List[str], df: pd.DataFrame) -> Tuple[np.ndarray, pd.Index, np.ndarray]:
    """Prepare data for wallet evaluation."""
    wallet = [stock.strip() for stock in wallet]
    df_wallet = df[wallet].pct_change().dropna()
    return df_wallet.values, df_wallet.index, np.cov(df_wallet.T)

def evaluate_wallet(wallet: List[str], 
                   wallet_index: int, 
                   total_wallets: int, 
                   df: pd.DataFrame,
                   num_iterations: int = 1000) -> Tuple[List[str], List[float], float]:
    """Evaluate a single wallet configuration."""
    print(f"wallet {wallet_index + 1}/{total_wallets} iter {wallet_index + 1}")
    
    df_wallet_values, df_wallet_index, cov_matrix = prepare_wallet_data(wallet, df)
    
    create_args = partial(generate_args, 
                         df_wallet_values=df_wallet_values,
                         df_wallet_index=df_wallet_index,
                         cov_matrix=cov_matrix,
                         num_stocks=len(wallet))
    
    args = [create_args() for _ in range(num_iterations)]
    
    with ProcessPoolExecutor() as executor:
        results = list(executor.map(process_single_weight, args))
    
    best_sharpe, best_weights = max(results, key=lambda x: x[0])
    return wallet, best_weights, best_sharpe

def generate_args(df_wallet_values: np.ndarray,
                 df_wallet_index: pd.Index,
                 cov_matrix: np.ndarray,
                 num_stocks: int) -> Tuple:
    """Generate arguments for weight evaluation."""
    return (generate_weights(num_stocks), df_wallet_values, df_wallet_index, cov_matrix)

def write_results(results: List[Tuple], file_path: str) -> None:
    """Write results to file."""
    with open(file_path, "w") as f:
        for wallet, weights, sharpe in results:
            f.write(f"Wallet: {wallet}\nWeights: {weights}\nSharpe Ratio: {sharpe}\n\n")

def process_all_wallets(stock_combinations: List[List[str]], df: pd.DataFrame) -> List[Tuple]:
    """Process all wallet combinations."""
    total = len(stock_combinations)
    evaluate_fn = partial(evaluate_wallet, total_wallets=total, df=df)
    return [evaluate_fn(wallet, i) for i, wallet in enumerate(stock_combinations)]

def main() -> None:
    """Main execution function."""
    Path(RESULTS_DIR).mkdir(exist_ok=True)
    df = load_data(FILE_PATH)
    stock_combinations = load_stock_combinations()
    best_results = process_all_wallets(stock_combinations, df)
    write_results(best_results, RESULTS_FILE)

if __name__ == "__main__":
    main()