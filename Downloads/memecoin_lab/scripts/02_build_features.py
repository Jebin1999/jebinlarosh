import pandas as pd
import numpy as np
from pathlib import Path

# =============================
# CONFIG
# =============================
RAW_DIR = Path("data/raw")
OUT_DIR = Path("data/features")

LOOKBACK_1M = 4
LOOKBACK_5M = 20
FUTURE_WINDOW = 20

OUT_DIR.mkdir(parents=True, exist_ok=True)

# =============================
# LOAD DATA
# =============================
print("📥 Loading raw parquet files...")
df = pd.read_parquet(RAW_DIR)
print(f"Loaded rows: {len(df):,}")

# =============================
# COLUMN NORMALIZATION
# =============================
PRICE_COL_CANDIDATES = ["price", "priceUsd", "price_usd", "priceNative"]

price_col = next((c for c in PRICE_COL_CANDIDATES if c in df.columns), None)
if price_col is None:
    raise ValueError(f"No price column found. Columns: {df.columns.tolist()}")

df = df.rename(columns={price_col: "price"})

# =============================
# BASIC CLEANING
# =============================
required = ["pair_address", "timestamp", "price"]
df = df.dropna(subset=required)

df["timestamp"] = pd.to_datetime(df["timestamp"], utc=True)
df = df.sort_values(["pair_address", "timestamp"]).reset_index(drop=True)

g = df.groupby("pair_address", group_keys=False)

# =============================
# RETURNS
# =============================
df["price_return_1m"] = g["price"].pct_change(LOOKBACK_1M)
df["price_return_5m"] = g["price"].pct_change(LOOKBACK_5M)

# =============================
# VOLATILITY (COMPRESSION CORE)
# =============================
df["volatility_5m"] = (
    g["price"]
    .rolling(LOOKBACK_5M, min_periods=5)
    .std()
    .reset_index(drop=True)
)

# =============================
# LIQUIDITY & VOLUME
# =============================
df["liq_change_5m"] = g["liquidity"].pct_change(LOOKBACK_5M)

df["volume_accel"] = (
    g["volume_5m"]
    .pct_change()
    .replace([np.inf, -np.inf], np.nan)
)

# =============================
# BUY PRESSURE
# =============================
df["buy_ratio"] = (
    df["buys_5m"] /
    (df["buys_5m"] + df["sells_5m"])
).replace([np.inf, -np.inf], np.nan)

df["buy_ratio"] = df["buy_ratio"].fillna(0.5)

# =============================
# COMPRESSION SCORE
# =============================
df["compression_score"] = (
    -df["volatility_5m"].rank(pct=True) +
     df["liq_change_5m"].rank(pct=True) +
     df["buy_ratio"]
)

# =============================
# FUTURE RETURNS (LABEL)
# =============================
df["future_max_price"] = (
    g["price"]
    .shift(-1)
    .rolling(FUTURE_WINDOW)
    .max()
)

df["future_return_5m"] = df["future_max_price"] / df["price"] - 1
df["label_pump"] = (df["future_return_5m"] >= 1.0).astype(int)

# =============================
# FINAL DATASET
# =============================
cols = [
    "timestamp",
    "pair_address",
    "pair_name",
    "price",
    "liquidity",
    "volume_5m",
    "buys_5m",
    "sells_5m",
    "price_return_1m",
    "price_return_5m",
    "volatility_5m",
    "liq_change_5m",
    "volume_accel",
    "buy_ratio",
    "compression_score",
    "future_return_5m",
    "label_pump",
]

df = df[[c for c in cols if c in df.columns]]
df = df.dropna(subset=["future_return_5m"])

# =============================
# SAVE
# =============================
out = OUT_DIR / "features.parquet"
df.to_parquet(out, index=False)

print("✅ Features built successfully")
print(f"📦 Saved to: {out}")
print(f"Final rows: {len(df):,}")
