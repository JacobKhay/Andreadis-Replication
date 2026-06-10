"""
M2 diagnostic: Figure 1 county-level discrepancies.

This script does not edit replication.qmd. It compares two Figure 1 code paths
on the same AKCLM data.csv:
  1. AKCLM map code path: aicounty-main/maps_ai.R. Panel A is average annual
     ai/nads. Panel B filters Year > 2017 and computes last(ai/nads) -
     first(ai/nads), which is 2023 minus 2018 when rows are year-sorted.
  2. Our Figure 1 code path: the map construction in replication.qmd, applied
     to the same AKCLM data.csv. Panel B pools 2017-2018 and 2022-2023 before
     differencing.

It also optionally reports whether updated_lightcast_county.csv differs from
AKCLM data.csv, but that is not the main comparison.

Run:
  python m2_figure1_county_discrepancies.py

Inputs:
  - 228344-V1.zip in the repo root, or AKCLM_ZIP=/path/to/228344-V1.zip
  - if the zip is absent, local data.csv is used as the AKCLM fallback
  - updated_lightcast_county.csv is optional for the extra data-source check
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
from matplotlib.patches import Patch


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
    ("0 - 0.06", 0.00, 0.06, "left"),
    ("0.06 - 0.14", 0.06, 0.14, "open"),
    ("0.14 - 0.23", 0.14, 0.23, "open"),
    ("0.23 - 0.37", 0.23, 0.37, "open"),
    ("0.37 - 0.71", 0.37, 0.71, "open"),
    ("0.71 - 10", 0.71, 10.00, "open"),
]

PANEL_B_BINS = [
    ("-5.56 - -0.12", -5.56, -0.12, "left_open_right_closed"),
    ("-0.12 - 0", -0.12, 0.00, "left_open_right_closed"),
    ("0 - 0.09", 0.00, 0.09, "open"),
    ("0.09 - 0.24", 0.09, 0.24, "open"),
    ("0.24 - 0.57", 0.24, 0.57, "open"),
    ("0.57 - 12.35", 0.57, 12.35, "open"),
]

PANEL_A_DISPLAY_ORDER = ["0.71 - 10", "0.37 - 0.71", "0.23 - 0.37", "0.14 - 0.23", "0.06 - 0.14", "0 - 0.06", "No data"]
PANEL_B_DISPLAY_ORDER = ["0.57 - 12.35", "0.24 - 0.57", "0.09 - 0.24", "0 - 0.09", "-0.12 - 0", "-5.56 - -0.12", "No data"]
PANEL_A_COLORS = ["#800026", "#e31a1c", "#fd8d3c", "#fed976", "#ffeda0", "#ffffcc", "#e5e5e5"]
PANEL_B_COLORS = ["#800026", "#fc4e2a", "#fd8d3c", "#fed976", "#ffeda0", "#ffffcc", "#e5e5e5"]


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
        if kind == "closed" and value >= lo and value <= hi:
            return label
    return "No data"


def build_akclm_code_metrics(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    levels = (
        df.assign(ai_intensity=lambda x: np.where(x["nads"] > 0, x["ai"] / x["nads"], np.nan))
        .groupby("COUNTY_FIPS", as_index=False)
        .agg(
            panel_a_akclm_code=("ai_intensity", "mean"),
            akclm_total_postings=("nads", "sum"),
            akclm_ai_postings=("ai", "sum"),
            akclm_pop_2023=("pop", lambda s: s[df.loc[s.index, "Year"].eq(2023)].iloc[0] if df.loc[s.index, "Year"].eq(2023).any() else np.nan),
        )
    )

    # Mirrors aicounty-main/maps_ai.R:
    # data %>% filter(Year > 2017) %>% mutate(aiInt = ai/nads) %>%
    #   group_by(COUNTY_FIPS) %>% summarise(aiIntch = last(aiInt) - first(aiInt))
    # With the supplied data sorted by year within county, first=2018 and last=2023.
    annual = df[df["Year"] > 2017].sort_values(["COUNTY_FIPS", "Year"]).copy()
    annual["ai_intensity"] = np.where(annual["nads"] > 0, annual["ai"] / annual["nads"], np.nan)
    change = (
        annual.groupby("COUNTY_FIPS", as_index=False)
        .agg(
            first_ai_intensity=("ai_intensity", "first"),
            last_ai_intensity=("ai_intensity", "last"),
            first_year=("Year", "first"),
            last_year=("Year", "last"),
        )
        .assign(panel_b_akclm_code=lambda x: x["last_ai_intensity"] - x["first_ai_intensity"])
    )
    change = (
        change[["COUNTY_FIPS", "panel_b_akclm_code", "first_year", "last_year"]]
        .reset_index(drop=True)
    )

    return levels.merge(change, on="COUNTY_FIPS", how="outer")


def build_our_figure1_metrics_from_akclm(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path).copy()
    df = df[(df["COUNTY_FIPS"] != 9999) & ~df["COUNTY_FIPS"].between(9110, 9190)].copy()

    # Mirrors replication.qmd Figure 1: mutate annual ai/nads, then average
    # annual intensities by county.
    levels = (
        df.assign(ai_intensity=lambda x: np.where(x["nads"] > 0, x["ai"] / x["nads"], np.nan))
        .groupby("COUNTY_FIPS", as_index=False)
        .agg(
            panel_a_our_code=("ai_intensity", "mean"),
            our_total_postings=("nads", "sum"),
            our_ai_postings=("ai", "sum"),
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
        .assign(panel_b_our_code=lambda x: x["2022-2023"] - x["2017-2018"])
        [["COUNTY_FIPS", "panel_b_our_code"]]
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
            panel_a_updated_extract=("ai_intensity", "mean"),
            updated_extract_total_postings=("nads", "sum"),
            updated_extract_ai_postings=("ai", "sum"),
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
        .assign(panel_b_updated_extract=lambda x: x["2022-2023"] - x["2017-2018"])
        [["COUNTY_FIPS", "panel_b_updated_extract"]]
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
    out["panel_a_akclm_pp"] = out["panel_a_akclm_code"] * 100
    out["panel_a_our_pp"] = out["panel_a_our_code"] * 100
    out["panel_b_akclm_pp"] = out["panel_b_akclm_code"] * 100
    out["panel_b_our_pp"] = out["panel_b_our_code"] * 100
    out["panel_a_diff_pp"] = out["panel_a_our_pp"] - out["panel_a_akclm_pp"]
    out["panel_b_diff_pp"] = out["panel_b_our_pp"] - out["panel_b_akclm_pp"]
    out["panel_a_abs_diff_pp"] = out["panel_a_diff_pp"].abs()
    out["panel_b_abs_diff_pp"] = out["panel_b_diff_pp"].abs()
    out["panel_a_akclm_bin"] = out["panel_a_akclm_pp"].map(lambda x: bin_value(x, PANEL_A_BINS))
    out["panel_a_our_bin"] = out["panel_a_our_pp"].map(lambda x: bin_value(x, PANEL_A_BINS))
    out["panel_b_akclm_bin"] = out["panel_b_akclm_pp"].map(lambda x: bin_value(x, PANEL_B_BINS))
    out["panel_b_our_bin"] = out["panel_b_our_pp"].map(lambda x: bin_value(x, PANEL_B_BINS))
    out["panel_a_bin_match"] = out["panel_a_akclm_bin"].eq(out["panel_a_our_bin"])
    out["panel_b_bin_match"] = out["panel_b_akclm_bin"].eq(out["panel_b_our_bin"])
    out["posting_diff"] = out["our_total_postings"] - out["akclm_total_postings"]
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


def count_state_counties(df: pd.DataFrame, state_prefix: str) -> int:
    return df["COUNTY_FIPS"].astype("Int64").astype(str).str.zfill(5).str.startswith(state_prefix).sum()


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
        print("No nonzero Panel A or Panel B differences among counties present in both code paths.")
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
        "panel_a_akclm_pp",
        "panel_a_our_pp",
        "panel_a_diff_pp",
        "panel_b_akclm_pp",
        "panel_b_our_pp",
        "panel_b_diff_pp",
        "akclm_total_postings",
        "our_total_postings",
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


def transform_lon_lat(lon: float, lat: float, fips: str) -> tuple[float, float]:
    state_fips = fips[:2]
    if state_fips == "02":
        return (lon + 170) * 0.35 - 124, (lat - 50) * 0.35 + 24
    if state_fips == "15":
        return lon + 52, lat + 5
    return lon, lat


def geometry_to_patches(geometry: dict, fips: str) -> list[Polygon]:
    patches: list[Polygon] = []
    geom_type = geometry.get("type")
    coords = geometry.get("coordinates", [])
    polygons = [coords] if geom_type == "Polygon" else coords if geom_type == "MultiPolygon" else []
    for polygon in polygons:
        if not polygon:
            continue
        exterior = [transform_lon_lat(lon, lat, fips) for lon, lat in polygon[0]]
        patches.append(Polygon(exterior, closed=True))
    return patches


def draw_county_map(ax, features: list[dict], values: dict[str, float], title: str, cmap: str, vmin: float, vmax: float) -> None:
    patches: list[Polygon] = []
    colors: list[float] = []
    missing_patches: list[Polygon] = []
    for feature in features:
        fips = str(feature.get("id", "")).zfill(5)
        if fips.startswith("72"):
            continue
        geom_patches = geometry_to_patches(feature.get("geometry", {}), fips)
        value = values.get(fips)
        if value is None or pd.isna(value):
            missing_patches.extend(geom_patches)
            continue
        patches.extend(geom_patches)
        colors.extend([value] * len(geom_patches))

    if missing_patches:
        missing = PatchCollection(missing_patches, facecolor="#e5e5e5", linewidths=0.05, edgecolor="white")
        ax.add_collection(missing)
    collection = PatchCollection(patches, cmap=cmap, linewidths=0.05, edgecolor="white")
    collection.set_array(np.array(colors))
    collection.set_clim(vmin, vmax)
    ax.add_collection(collection)
    ax.set_xlim(-125, -66)
    ax.set_ylim(23.5, 50)
    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_title(title, fontsize=10)
    return collection


def draw_county_map_categorical(
    ax,
    features: list[dict],
    bins_by_fips: dict[str, str],
    title: str,
    display_order: list[str],
    colors: list[str],
) -> None:
    color_by_bin = dict(zip(display_order, colors))
    patches_by_bin: dict[str, list[Polygon]] = {label: [] for label in display_order}

    for feature in features:
        fips = str(feature.get("id", "")).zfill(5)
        if fips.startswith("72"):
            continue
        label = bins_by_fips.get(fips, "No data")
        if label not in patches_by_bin:
            label = "No data"
        patches_by_bin[label].extend(geometry_to_patches(feature.get("geometry", {}), fips))

    for label in reversed(display_order):
        patches = patches_by_bin[label]
        if not patches:
            continue
        collection = PatchCollection(
            patches,
            facecolor=color_by_bin[label],
            linewidths=0.05,
            edgecolor="white",
        )
        ax.add_collection(collection)

    ax.set_xlim(-125, -66)
    ax.set_ylim(23.5, 50)
    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_title(title, fontsize=10)


def legend_handles_for_two_rows(display_order: list[str], colors: list[str]) -> list[Patch]:
    # Matplotlib legends fill columns first. Reorder handles so the displayed
    # rows still read high-to-low, matching the paper's legend layout.
    top_row = display_order[:3] + display_order[6:]
    bottom_row = display_order[3:6]
    column_major = []
    for col in range(max(len(top_row), len(bottom_row))):
        if col < len(top_row):
            column_major.append(top_row[col])
        if col < len(bottom_row):
            column_major.append(bottom_row[col])

    color_by_bin = dict(zip(display_order, colors))
    return [
        Patch(facecolor=color_by_bin[label], edgecolor="black", linewidth=0.4, label=label)
        for label in column_major
    ]


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
            "panel_a_our_pp",
            "panel_a_diff_pp",
            "panel_a_akclm_bin",
            "panel_a_our_bin",
            PANEL_A_DISPLAY_ORDER,
            PANEL_A_COLORS,
            "coolwarm",
        ),
        (
            "figure1_panel_b_side_by_side_difference.png",
            "Panel B: AI Intensity Change",
            "panel_b_akclm_pp",
            "panel_b_our_pp",
            "panel_b_diff_pp",
            "panel_b_akclm_bin",
            "panel_b_our_bin",
            PANEL_B_DISPLAY_ORDER,
            PANEL_B_COLORS,
            "coolwarm",
        ),
    ]

    for filename, title, ak_col, up_col, diff_col, ak_bin_col, up_bin_col, display_order, colors, diff_cmap in outputs:
        max_abs_diff = float(map_df[diff_col].abs().max(skipna=True) or 0.0)
        diff_lim = max(max_abs_diff, 1e-9)

        fig, axes = plt.subplots(1, 3, figsize=(15, 5), constrained_layout=True)
        ak_bins = dict(zip(map_df["fips"], map_df[ak_bin_col]))
        up_bins = dict(zip(map_df["fips"], map_df[up_bin_col]))
        diff_values = dict(zip(map_df["fips"], map_df[diff_col]))

        draw_county_map_categorical(axes[0], features, ak_bins, "AKCLM code path", display_order, colors)
        draw_county_map_categorical(axes[1], features, up_bins, "Our Figure 1 code path", display_order, colors)
        c2 = draw_county_map(axes[2], features, diff_values, "Difference: our code - AKCLM code", diff_cmap, -diff_lim, diff_lim)
        fig.suptitle(title, fontsize=13)
        legend_handles = legend_handles_for_two_rows(display_order, colors)
        fig.legend(
            handles=legend_handles,
            loc="lower left",
            bbox_to_anchor=(0.045, -0.01),
            ncol=4,
            frameon=False,
            fontsize=8,
        )
        fig.colorbar(c2, ax=axes[2], orientation="horizontal", fraction=0.045, pad=0.03, label="percentage-point difference")
        out_path = OUT_DIR / filename
        fig.savefig(out_path, dpi=200)
        plt.close(fig)
        print(f"Wrote map: {out_path}")


def main() -> None:
    banner("M2 Figure 1 County Discrepancy Diagnostic")
    print("Main comparison: AKCLM maps_ai.R code path vs. our Figure 1 code path on the same AKCLM data.csv.")
    print("AKCLM map code path comes from aicounty-main/maps_ai.R.")
    print("Panel A: average annual AI intensity, 2014-2023.")
    print("AKCLM Panel B: annual 2023 AI intensity minus annual 2018 AI intensity.")
    print("Our Panel B: pooled 2022-2023 AI intensity minus pooled 2017-2018 AI intensity.")
    print("All differences below are our code path minus AKCLM code path, in percentage points.")

    akclm_path = locate_akclm_data()
    akclm = build_akclm_code_metrics(akclm_path)
    ours = build_our_figure1_metrics_from_akclm(akclm_path)
    comp = add_comparisons(akclm.merge(ours, on="COUNTY_FIPS", how="outer", indicator=True))

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out_csv = OUT_DIR / "figure1_county_comparison.csv"
    comp.to_csv(out_csv, index=False)

    banner("Coverage")
    print(f"AKCLM code-path counties: {akclm['COUNTY_FIPS'].nunique():,}")
    print(f"Our code-path counties on same data: {ours['COUNTY_FIPS'].nunique():,}")
    print(comp["_merge"].value_counts().to_string())
    print("State coverage in comparison rows:")
    print(f"  Alaska counties: {count_state_counties(comp, '02'):,}")
    print(f"  Hawaii counties: {count_state_counties(comp, '15'):,}")
    print(f"  Connecticut counties: {count_state_counties(comp, '09'):,} (drawn as No data on maps if absent from data)")

    banner("County Difference Summary")
    print_diff_summary(comp, "Panel A", "panel_a_abs_diff_pp", "panel_a_bin_match")
    print_diff_summary(comp, "Panel B", "panel_b_abs_diff_pp", "panel_b_bin_match")

    print_pattern(comp)
    print_top_differences(comp)
    generate_maps(comp)

    if UPDATED_LIGHTCAST.exists():
        banner("Optional Data-Source Check")
        print("This is separate from the code-path comparison above.")
        raw_posting_match_summary(akclm_path, UPDATED_LIGHTCAST)

    banner("Output")
    print(f"Wrote county-level comparison file: {out_csv}")


if __name__ == "__main__":
    main()
