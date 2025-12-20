import requests
import time
import pandas as pd
from datetime import datetime, timedelta, timezone
from pathlib import Path

# =========================
# CONFIG
# =========================
OUT_DIR = Path("data/raw/dexscreener")
OUT_DIR.mkdir(parents=True, exist_ok=True)

DEXSCREENER_URL = "https://api.dexscreener.com/token-profiles/latest/v1"

COLLECT_DAYS = 2                      # <-- exactly 2 days
POLL_INTERVAL = 15                    # seconds
SAVE_EVERY_SECONDS = 60               # write parquet every minute
MAX_RETRIES = 3

PARQUET_COMPRESSION = "zstd"
ZSTD_LEVEL = 9

# =========================
# HELPERS
# =========================
def fetch_profiles():
    for attempt in range(MAX_RETRIES):
        try:
            r = requests.get(DEXSCREENER_URL, timeout=10)
            r.raise_for_status()
            return r.json()
        except Exception as e:
            if attempt == MAX_RETRIES - 1:
                raise
            time.sleep(2)

def utc_now():
    return datetime.now(timezone.utc)

# =========================
# MAIN LOOP
# =========================
print("🧠 DexScreener Solana Snapshot Collector")
print(f"⏱ Collecting for {COLLECT_DAYS} days")
print(f"💾 Saving every {SAVE_EVERY_SECONDS}s | ZSTD lvl {ZSTD_LEVEL}")
print("-" * 60)

start_time = utc_now()
end_time = start_time + timedelta(days=COLLECT_DAYS)
last_save = time.time()

rows = []
file_counter = 0

try:
    while utc_now() < end_time:
        ts = utc_now().isoformat()

        profiles = fetch_profiles()

        for p in profiles:
            if p.get("chainId") != "solana":
                continue

            rows.append({
                "timestamp": ts,
                "token_address": p.get("tokenAddress"),
                "pair_address": p.get("pairAddress"),
                "symbol": p.get("symbol"),
                "dex_id": p.get("dexId"),
                "price_usd": p.get("priceUsd"),
                "liquidity_usd": p.get("liquidityUsd"),
                "volume_24h": p.get("volume24h"),
                "fdv": p.get("fdv"),
            })

        # Save snapshot every minute
        if time.time() - last_save >= SAVE_EVERY_SECONDS:
            if rows:
                df = pd.DataFrame(rows)

                fname = OUT_DIR / f"dexscreener_snapshot_{file_counter:05d}.parquet"
                df.to_parquet(
                    fname,
                    compression=PARQUET_COMPRESSION,
                    compression_level=ZSTD_LEVEL,
                    index=False
                )

                print(
                    f"✅ Saved {len(df):5d} rows → {fname.name} | "
                    f"{utc_now().strftime('%Y-%m-%d %H:%M:%S')} UTC"
                )

                rows.clear()
                file_counter += 1

            last_save = time.time()

        time.sleep(POLL_INTERVAL)

except KeyboardInterrupt:
    print("\n🛑 Interrupted by user")

# =========================
# FINAL FLUSH
# =========================
if rows:
    df = pd.DataFrame(rows)
    fname = OUT_DIR / f"dexscreener_snapshot_{file_counter:05d}.parquet"
    df.to_parquet(
        fname,
        compression=PARQUET_COMPRESSION,
        compression_level=ZSTD_LEVEL,
        index=False
    )
    print(f"📦 Final save: {len(df)} rows → {fname.name}")

print("🎯 Collection complete")
