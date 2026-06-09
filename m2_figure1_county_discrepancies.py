"""
M2 diagnostic: Figure 1 county-level discrepancies.

This script does not edit replication.qmd. It reproduces the Figure 1 data
construction used there from updated_lightcast_county.csv, builds the same
Panel A and Panel B quantities from AKCLM's published data.csv, and reports
county-level differences.

Run:
  python m2_figure1_county_discrepancies.py

Inputs:
  - updated_lightcast_county.csv in the repo root
  - 228344-V1.zip in the repo root, or AKCLM_ZIP=/path/to/228344-V1.zip
  - if the zip is absent, local data.csv is used as the AKCLM fallback
"""

from __future__ import annotations

import os
import json
import shutil
import urllib.request
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.collections import PatchCollection
from matplotlib.patches import Polygon


ROOT = Path(__file__).resolve().parent
CACHE = ROOT / "replication_cache" / "akclm_aea_archive"
EXTRACT_DIR = CACHE / "extracted"
ZIP_NAME = "228344-V1.zip"

UPDATED_LIGHTCAST = ROOT / "updated_lightcast_county.csv"
LOCAL_AKCLM_DATA = ROOT / "data.csv"
OUT_DIR = ROOT / "replication_cache" / "m2_figure1"
GEOJSON_URL = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
GEOJSON_PATH = OUT_DIR / "geojson-counties-fips.json"


PANEL_A_BINS = [
    ("0-0.06", 0.00, 0.06, "left"),
    ("0.06-0.14", 0.06, 0.14, "open"),
    ("0.14-0.23", 0.14, 0.23, "open"),
    ("0.23-0.37", 0.23, 0.37, "open"),
    ("0.37-0.70", 0.37, 0.70, "open"),
    ("0.70+", 0.70, np.inf, "open_lower"),
]

PANEL_B_BINS = [
    ("<-0.12", -np.inf, -0.12, "upper_open"),
    ("-0.12-0", -0.12, 0.00, "left_open_right_closed"),
    ("0-0.09", 0.00, 0.09, "open"),
    ("0.09-0.24", 0.09, 0.24, "open"),
    ("0.24-0.57", 0.24, 0.57, "open"),
    ("0.57+", 0.57, np.inf, "open_lower"),
]


def banner(title: str, width: int = 92) -> None:
    print("\n" + "=" * width)
    print(title)
    print("=" * width)


def env_zip() -> Path | None:
    value = os.environ.get("AKCLM_ZIP", "").strip()
    if not value:
        return None
    path = Path(value).expanduser().resolve()
    return path if path.exists() else None


def locate_akclm_data() -> Path:
    candidates = [
        env_zip(),
        ROOT / ZIP_NAME,
        CACHE / ZIP_NAME,
    ]
    zip_path = next((p for p in candidates if p and p.exists() and zipfile.is_zipfile(p)), None)

    if zip_path:
        if EXTRACT_DIR.exists():
            shutil.rmtree(EXTRACT_DIR)
        EXTRACT_DIR.mkdir(parents=True)
        with zipfile.ZipFile(zip_path) as zf:
            zf.extractall(EXTRACT_DIR)
        data_files = sorted(p for p in EXTRACT_DIR.rglob("*") if p.name.lower() == "data.csv")
        if data_files:
            print(f"AKCLM source: extracted data.csv from {zip_path}")
            return data_files[0]

    print(f"AKCLM source: local fallback {LOCAL_AKCLM_DATA}")
    return LOCAL_AKCLM_DATA


def bin_value(value: float, bins: list[tuple[str, float, float, str]]) -> str:
    if pd.isna(value):
        return "No data"
    for label, lo, hi, kind in bins:
        if kind == "left" and value >= lo and value < hi:
            return label
        if kind == "open" and value > lo and value < hi:
            return label
        if kind == "open_lower" and value > lo:
            return label
        if kind == "upper_open" and value < hi:
            return label
        if kind == "left_open_right_closed" and value > lo and value <= hi:
            return label
    return "No data"


def build_akclm_metrics(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    df["fips_year"] = df["COUNTY_FIPS"].astype(str) + "-" + df["Year"].astype(str)
    levels = (
        df.assign(ai_intensity=lambda x: np.where(x["nads"] > 0, x["ai"] / x["nads"], np.nan))
        .groupby("COUNTY_FIPS", as_index=False)
        .agg(
            panel_a_akclm=("ai_intensity", "mean"),
            akclm_total_postings=("nads", "sum"),
            akclm_ai_postings=("ai", "sum"),
            akclm_pop_2023=("pop", lambda s: s[df.loc[s.index, "Year"].eq(2023)].iloc[0] if df.loc[s.index, "Year"].eq(2023).any() else np.nan),
        )
    )

    subset = df[df["Year"].isin([2017, 2018, 2022, 2023])].copy()
    subset["period"] = np.where(subset["Year"].isin([2017, 2018]), "2017-2018", "2022-2023")
    pooled = (
        subset.groupby(["COUNTY_FIPS", "period"], as_index=False)
        .agg(ai_sum=("ai", "sum"), nads_sum=("nads", "sum"))
        .assign(ai_intensity=lambda x: np.where(x["nads_sum"] > 0, x["ai_sum"] / x["nads_sum"], np.nan))
    )
    change = (
        pooled.pivot(index="COUNTY_FIPS", columns="period", values="ai_intensity")
        .reset_index()
        .assign(panel_b_akclm=lambda x: x["2022-2023"] - x["2017-2018"])
        [["COUNTY_FIPS", "panel_b_akclm"]]
    )

    return levels.merge(change, on="COUNTY_FIPS", how="outer")


def build_updated_metrics(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path).rename(
        columns={
            "YEAR_POSTED": "Year",
            "AI Postings": "ai",
            "Total Postings": "nads",
        }
    )
    df = df[(df["COUNTY_FIPS"] != 9999) & ~df["COUNTY_FIPS"].between(9110, 9190)].copy()

    levels = (
        df.assign(ai_intensity=lambda x: np.where(x["nads"] > 0, x["ai"] / x["nads"], np.nan))
        .groupby("COUNTY_FIPS", as_index=False)
        .agg(
            panel_a_updated=("ai_intensity", "mean"),
            updated_total_postings=("nads", "sum"),
            updated_ai_postings=("ai", "sum"),
            county_name=("COUNTY_NAME", "first"),
            state_name=("STATE", "first"),
        )
    )

    subset = df[df["Year"].isin([2017, 2018, 2022, 2023])].copy()
    subset["period"] = np.where(subset["Year"].isin([2017, 2018]), "2017-2018", "2022-2023")
    pooled = (
        subset.groupby(["COUNTY_FIPS", "period"], as_index=False)
        .agg(ai_sum=("ai", "sum"), nads_sum=("nads", "sum"))
        .assign(ai_intensity=lambda x: np.where(x["nads_sum"] > 0, x["ai_sum"] / x["nads_sum"], np.nan))
    )
    change = (
        pooled.pivot(index="COUNTY_FIPS", columns="period", values="ai_intensity")
        .reset_index()
        .assign(panel_b_updated=lambda x: x["2022-2023"] - x["2017-2018"])
        [["COUNTY_FIPS", "panel_b_updated"]]
    )

    return levels.merge(change, on="COUNTY_FIPS", how="outer")


def raw_posting_match_summary(akclm_path: Path, updated_path: Path) -> None:
    banner("Raw Lightcast Posting Match")
    akclm = pd.read_csv(akclm_path)[["COUNTY_FIPS", "Year", "ai", "nads"]]
    updated = pd.read_csv(updated_path).rename(
        columns={"YEAR_POSTED": "Year", "AI Postings": "ai", "Total Postings": "nads"}
    )
    updated = updated[(updated["COUNTY_FIPS"] != 9999) & ~updated["COUNTY_FIPS"].between(9110, 9190)]
    updated = updated[["COUNTY_FIPS", "Year", "ai", "nads"]]
    merged = akclm.merge(updated, on=["COUNTY_FIPS", "Year"], how="outer", suffixes=("_akclm", "_updated"), indicator=True)
    both = merged[merged["_merge"].eq("both")].copy()
    ai_match = both["ai_akclm"].eq(both["ai_updated"])
    nads_match = both["nads_akclm"].eq(both["nads_updated"])
    print(f"AKCLM county-year rows: {len(akclm):,}")
    print(f"Updated county-year rows after Figure 1 filters: {len(updated):,}")
    print(merged["_merge"].value_counts().to_string())
    print(f"Matched rows with identical AI postings: {ai_match.sum():,} of {len(both):,}")
    print(f"Matched rows with identical total postings: {nads_match.sum():,} of {len(both):,}")


def add_comparisons(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["panel_a_akclm_pp"] = out["panel_a_akclm"] * 100
    out["panel_a_updated_pp"] = out["panel_a_updated"] * 100
    out["panel_b_akclm_pp"] = out["panel_b_akclm"] * 100
    out["panel_b_updated_pp"] = out["panel_b_updated"] * 100
    out["panel_a_diff_pp"] = out["panel_a_updated_pp"] - out["panel_a_akclm_pp"]
    out["panel_b_diff_pp"] = out["panel_b_updated_pp"] - out["panel_b_akclm_pp"]
    out["panel_a_abs_diff_pp"] = out["panel_a_diff_pp"].abs()
    out["panel_b_abs_diff_pp"] = out["panel_b_diff_pp"].abs()
    out["panel_a_akclm_bin"] = out["panel_a_akclm_pp"].map(lambda x: bin_value(x, PANEL_A_BINS))
    out["panel_a_updated_bin"] = out["panel_a_updated_pp"].map(lambda x: bin_value(x, PANEL_A_BINS))
    out["panel_b_akclm_bin"] = out["panel_b_akclm_pp"].map(lambda x: bin_value(x, PANEL_B_BINS))
    out["panel_b_updated_bin"] = out["panel_b_updated_pp"].map(lambda x: bin_value(x, PANEL_B_BINS))
    out["panel_a_bin_match"] = out["panel_a_akclm_bin"].eq(out["panel_a_updated_bin"])
    out["panel_b_bin_match"] = out["panel_b_akclm_bin"].eq(out["panel_b_updated_bin"])
    out["posting_diff"] = out["updated_total_postings"] - out["akclm_total_postings"]
    out["posting_abs_diff"] = out["posting_diff"].abs()
    return out


def print_diff_summary(df: pd.DataFrame, panel: str, value_col: str, bin_col: str) -> None:
    valid = df[value_col].dropna()
    matched = df[df["_merge"].eq("both")]
    print(f"{panel}: counties compared = {len(valid):,}")
    print(f"  median absolute difference = {valid.median():.6f} percentage points")
    print(f"  90th percentile absolute difference = {valid.quantile(0.90):.6f} percentage points")
    print(f"  max absolute difference = {valid.max():.6f} percentage points")
    print(f"  bin matches among matched counties = {int(matched[bin_col].sum()):,} of {len(matched):,}")


def print_pattern(df: pd.DataFrame) -> None:
    banner("Discrepancy Pattern")
    work = df.dropna(subset=["panel_b_abs_diff_pp"]).copy()
    work["posting_decile"] = pd.qcut(
        work["akclm_total_postings"].rank(method="first"), 10, labels=False, duplicates="drop"
    ) + 1
    work["population_decile"] = pd.qcut(
        work["akclm_pop_2023"].rank(method="first"), 10, labels=False, duplicates="drop"
    ) + 1

    by_postings = (
        work.groupby("posting_decile", as_index=False)
        .agg(
            counties=("COUNTY_FIPS", "size"),
            median_abs_panel_b=("panel_b_abs_diff_pp", "median"),
            p90_abs_panel_b=("panel_b_abs_diff_pp", lambda x: x.quantile(0.90)),
            median_abs_posting_diff=("posting_abs_diff", "median"),
        )
    )
    by_population = (
        work.groupby("population_decile", as_index=False)
        .agg(
            counties=("COUNTY_FIPS", "size"),
            median_abs_panel_b=("panel_b_abs_diff_pp", "median"),
            p90_abs_panel_b=("panel_b_abs_diff_pp", lambda x: x.quantile(0.90)),
        )
    )

    print("By AKCLM total-posting decile, Panel B absolute difference in percentage points:")
    print(by_postings.to_string(index=False, float_format=lambda x: f"{x:.6f}"))
    print("\nBy AKCLM 2023 population decile, Panel B absolute difference in percentage points:")
    print(by_population.to_string(index=False, float_format=lambda x: f"{x:.6f}"))

    corr_cols = [
        "panel_a_abs_diff_pp",
        "panel_b_abs_diff_pp",
        "posting_abs_diff",
        "akclm_total_postings",
        "akclm_pop_2023",
    ]
    corr = work[corr_cols].corr(numeric_only=True)
    print("\nCorrelations with Panel B absolute discrepancy:")
    for col in ["posting_abs_diff", "akclm_total_postings", "akclm_pop_2023"]:
        value = corr.loc["panel_b_abs_diff_pp", col]
        if pd.isna(value):
            print(f"  {col}: undefined because matched-county discrepancies are all zero")
        else:
            print(f"  {col}: {value:.4f}")


def print_top_differences(df: pd.DataFrame) -> None:
    banner("Largest County Differences")
    if df[["panel_a_abs_diff_pp", "panel_b_abs_diff_pp"]].max().max() == 0:
        print("No nonzero Panel A or Panel B differences among counties present in both sources.")
        extras = df[df["_merge"].eq("right_only")].copy()
        if not extras.empty:
            print("\nCounties present in updated_lightcast_county.csv but absent from AKCLM data.csv:")
            print(
                extras[["COUNTY_FIPS", "county_name", "state_name", "updated_total_postings", "updated_ai_postings"]]
                .sort_values(["state_name", "county_name"])
                .to_string(index=False)
            )
        return

    cols = [
        "COUNTY_FIPS",
        "county_name",
        "state_name",
        "panel_a_akclm_pp",
        "panel_a_updated_pp",
        "panel_a_diff_pp",
        "panel_b_akclm_pp",
        "panel_b_updated_pp",
        "panel_b_diff_pp",
        "akclm_total_postings",
        "updated_total_postings",
    ]
    print("Top 15 by Panel A absolute difference:")
    print(
        df.sort_values("panel_a_abs_diff_pp", ascending=False)[cols]
        .head(15)
        .to_string(index=False, float_format=lambda x: f"{x:.6f}")
    )
    print("\nTop 15 by Panel B absolute difference:")
    print(
        df.sort_values("panel_b_abs_diff_pp", ascending=False)[cols]
        .head(15)
        .to_string(index=False, float_format=lambda x: f"{x:.6f}")
    )


def get_county_geojson() -> dict | None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    if not GEOJSON_PATH.exists():
        try:
            print(f"Downloading county GeoJSON: {GEOJSON_URL}")
            urllib.request.urlretrieve(GEOJSON_URL, GEOJSON_PATH)
        except Exception as exc:
            print(f"Map generation skipped: could not download county GeoJSON ({exc}).")
            return None
    try:
        return json.loads(GEOJSON_PATH.read_text(encoding="utf-8"))
    except Exception as exc:
        print(f"Map generation skipped: could not read county GeoJSON ({exc}).")
        return None


def geometry_to_patches(geometry: dict) -> list[Polygon]:
    patches: list[Polygon] = []
    geom_type = geometry.get("type")
    coords = geometry.get("coordinates", [])
    polygons = [coords] if geom_type == "Polygon" else coords if geom_type == "MultiPolygon" else []
    for polygon in polygons:
        if not polygon:
            continue
        exterior = polygon[0]
        patches.append(Polygon(exterior, closed=True))
    return patches


def draw_county_map(ax, features: list[dict], values: dict[str, float], title: str, cmap: str, vmin: float, vmax: float) -> None:
    patches: list[Polygon] = []
    colors: list[float] = []
    for feature in features:
        fips = str(feature.get("id", "")).zfill(5)
        # Unshifted latitude/longitude maps make AK/HI visually dominate; the
        # numeric comparison still includes all counties.
        if fips.startswith(("02", "15", "72")):
            continue
        value = values.get(fips)
        if value is None or pd.isna(value):
            continue
        geom_patches = geometry_to_patches(feature.get("geometry", {}))
        patches.extend(geom_patches)
        colors.extend([value] * len(geom_patches))

    collection = PatchCollection(patches, cmap=cmap, linewidths=0.05, edgecolor="white")
    collection.set_array(np.array(colors))
    collection.set_clim(vmin, vmax)
    ax.add_collection(collection)
    ax.set_xlim(-125, -66)
    ax.set_ylim(24, 50)
    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_title(title, fontsize=10)
    return collection


def generate_maps(comp: pd.DataFrame) -> None:
    banner("Map Output")
    geojson = get_county_geojson()
    if geojson is None:
        return

    features = geojson.get("features", [])
    map_df = comp.copy()
    map_df["fips"] = map_df["COUNTY_FIPS"].astype("Int64").astype(str).str.zfill(5)

    outputs = [
        (
            "figure1_panel_a_side_by_side_difference.png",
            "Panel A: Average AI Intensity, 2014-2023",
            "panel_a_akclm_pp",
            "panel_a_updated_pp",
            "panel_a_diff_pp",
            "YlOrRd",
            "coolwarm",
        ),
        (
            "figure1_panel_b_side_by_side_difference.png",
            "Panel B: Change in AI Intensity, 2022-2023 minus 2017-2018",
            "panel_b_akclm_pp",
            "panel_b_updated_pp",
            "panel_b_diff_pp",
            "YlOrRd",
            "coolwarm",
        ),
    ]

    for filename, title, ak_col, up_col, diff_col, value_cmap, diff_cmap in outputs:
        values = pd.concat([map_df[ak_col], map_df[up_col]]).dropna()
        vmin = float(values.quantile(0.01)) if not values.empty else 0.0
        vmax = float(values.quantile(0.99)) if not values.empty else 1.0
        max_abs_diff = float(map_df[diff_col].abs().max(skipna=True) or 0.0)
        diff_lim = max(max_abs_diff, 1e-9)

        fig, axes = plt.subplots(1, 3, figsize=(15, 5), constrained_layout=True)
        ak_values = dict(zip(map_df["fips"], map_df[ak_col]))
        up_values = dict(zip(map_df["fips"], map_df[up_col]))
        diff_values = dict(zip(map_df["fips"], map_df[diff_col]))

        c0 = draw_county_map(axes[0], features, ak_values, "AKCLM data.csv", value_cmap, vmin, vmax)
        c1 = draw_county_map(axes[1], features, up_values, "Updated Lightcast", value_cmap, vmin, vmax)
        c2 = draw_county_map(axes[2], features, diff_values, "Difference: updated - AKCLM", diff_cmap, -diff_lim, diff_lim)
        fig.suptitle(title, fontsize=13)
        fig.colorbar(c0, ax=axes[:2], orientation="horizontal", fraction=0.045, pad=0.03, label="percentage points")
        fig.colorbar(c2, ax=axes[2], orientation="horizontal", fraction=0.045, pad=0.03, label="percentage-point difference")
        out_path = OUT_DIR / filename
        fig.savefig(out_path, dpi=200)
        plt.close(fig)
        print(f"Wrote map: {out_path}")


def main() -> None:
    banner("M2 Figure 1 County Discrepancy Diagnostic")
    print("AKCLM benchmark: published archive data.csv.")
    print("Your Figure 1 source: updated_lightcast_county.csv, a separate Lightcast extract.")
    print("Panel A: average annual AI intensity, 2014-2023.")
    print("Panel B: pooled AI intensity in 2022-2023 minus pooled AI intensity in 2017-2018.")
    print("All differences below are updated Lightcast minus AKCLM, in percentage points.")

    akclm_path = locate_akclm_data()
    raw_posting_match_summary(akclm_path, UPDATED_LIGHTCAST)
    akclm = build_akclm_metrics(akclm_path)
    updated = build_updated_metrics(UPDATED_LIGHTCAST)
    comp = add_comparisons(akclm.merge(updated, on="COUNTY_FIPS", how="outer", indicator=True))

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out_csv = OUT_DIR / "figure1_county_comparison.csv"
    comp.to_csv(out_csv, index=False)

    banner("Coverage")
    print(f"AKCLM counties: {akclm['COUNTY_FIPS'].nunique():,}")
    print(f"Updated Lightcast counties after Figure 1 filters: {updated['COUNTY_FIPS'].nunique():,}")
    print(comp["_merge"].value_counts().to_string())

    banner("County Difference Summary")
    print_diff_summary(comp, "Panel A", "panel_a_abs_diff_pp", "panel_a_bin_match")
    print_diff_summary(comp, "Panel B", "panel_b_abs_diff_pp", "panel_b_bin_match")

    print_pattern(comp)
    print_top_differences(comp)
    generate_maps(comp)

    banner("Output")
    print(f"Wrote county-level comparison file: {out_csv}")


if __name__ == "__main__":
    main()
