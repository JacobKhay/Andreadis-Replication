"""
M1 diagnostic: AKCLM Table 2 bachelor's-share coefficient.

This script is intentionally terminal-first:
  1. locate or download AKCLM's published replication archive;
  2. extract data.csv and replication_code.R when available;
  3. run replication_code.R with Rscript when R is installed;
  4. independently recompute the Table 2 bachelor's-share rows in Python;
  5. report source, standardization statistics, and coefficient gaps.

No manuscript response text is generated here; the printed transcript is the
evidence trail.
"""

from __future__ import annotations

import re
import shutil
import subprocess
import textwrap
import urllib.request
import warnings
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

warnings.filterwarnings("ignore")


ROOT = Path(__file__).resolve().parent
CACHE = ROOT / "replication_cache" / "akclm_aea_archive"
ZIP_NAME = "228344-V1.zip"
LOCAL_ZIP = ROOT / ZIP_NAME
EXTRACT_DIR = CACHE / "extracted"

AEA_ARTICLE_URL = "https://www.aeaweb.org/articles?id=10.1257/pandp.20251001"
OPENICPSR_URL = "https://doi.org/10.3886/E228344V1"

# openICPSR changes download routes occasionally. The script tries these first,
# then falls back to a manually downloaded 228344-V1.zip in the repo root.
DOWNLOAD_CANDIDATES = [
    "https://www.openicpsr.org/openicpsr/project/228344/version/V1/download",
    "https://www.openicpsr.org/openicpsr/project/228344/version/V1/download?fileFormat=zip",
    "https://www.openicpsr.org/openicpsr/project/228344/version/V1/download?path=/openicpsr/228344/fcr:versions/V1",
]

AKCLM_RERUN = {
    "Col 1": (0.0196, 0.0250),
    "Col 4": (-0.0322, 0.0292),
    "Col 5": (-0.0401, 0.0263),
}

# Values printed in AKCLM's published Table 2.
AKCLM_PUBLISHED = {
    "Col 1": (0.0022, 0.0027),
    "Col 4": (-0.0035, 0.0032),
    "Col 5": (-0.0044, 0.0029),
}


def banner(title: str, width: int = 88) -> None:
    print("\n" + "=" * width)
    print(title)
    print("=" * width)


def try_download_archive() -> Path | None:
    CACHE.mkdir(parents=True, exist_ok=True)
    cached_zip = CACHE / ZIP_NAME
    env_zip = os_path("AKCLM_ZIP")

    for candidate in (env_zip, LOCAL_ZIP, cached_zip):
        if candidate.exists() and zipfile.is_zipfile(candidate):
            print(f"Using existing archive: {candidate}")
            return candidate

    print(f"AEA article: {AEA_ARTICLE_URL}")
    print(f"openICPSR DOI: {OPENICPSR_URL}")
    print("Trying programmatic download routes...")

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125 Safari/537.36"
        )
    }
    for url in DOWNLOAD_CANDIDATES:
        try:
            print(f"  GET {url}")
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=60) as response:
                payload = response.read()
            cached_zip.write_bytes(payload)
            if zipfile.is_zipfile(cached_zip):
                print(f"Downloaded archive: {cached_zip}")
                return cached_zip
            print("    response was not a zip file")
            cached_zip.unlink(missing_ok=True)
        except Exception as exc:
            print(f"    download failed: {type(exc).__name__}: {exc}")

    print(
        textwrap.dedent(
            f"""
            Archive was not downloaded automatically.
            Place AKCLM's published archive at:
              {LOCAL_ZIP}
            Then rerun:
              python {Path(__file__).name}
            """
        ).strip()
    )
    return None


def os_path(env_name: str) -> Path:
    import os

    value = os.environ.get(env_name, "").strip()
    return Path(value).expanduser().resolve() if value else Path("__missing__")


def extract_archive(zip_path: Path | None) -> tuple[Path | None, Path | None]:
    if zip_path is None:
        return None, None

    if EXTRACT_DIR.exists():
        shutil.rmtree(EXTRACT_DIR)
    EXTRACT_DIR.mkdir(parents=True)

    with zipfile.ZipFile(zip_path) as zf:
        zf.extractall(EXTRACT_DIR)

    files = sorted(p for p in EXTRACT_DIR.rglob("*") if p.is_file())
    data_files = [p for p in files if p.name.lower() == "data.csv"]
    r_files = [p for p in files if p.name.lower() == "replication_code.r"]

    print(f"Extracted files: {len(files)}")
    for p in files[:20]:
        print(f"  {p.relative_to(EXTRACT_DIR)}")
    if len(files) > 20:
        print(f"  ... {len(files) - 20} more")

    data_path = data_files[0] if data_files else None
    r_path = r_files[0] if r_files else None
    print(f"AKCLM data.csv: {data_path if data_path else 'NOT FOUND'}")
    print(f"AKCLM replication_code.R: {r_path if r_path else 'NOT FOUND'}")
    return data_path, r_path


def find_rscript() -> str | None:
    path_rscript = shutil.which("Rscript")
    if path_rscript:
        return path_rscript

    # Windows fallback for machines where R is installed but not on PATH.
    candidates: list[Path] = []
    for base in [Path(r"C:\Program Files\R"), Path(r"C:\Program Files (x86)\R")]:
        if base.exists():
            candidates.extend(base.glob(r"R-*\bin\Rscript.exe"))
            candidates.extend(base.glob(r"R-*\bin\x64\Rscript.exe"))

    if not candidates:
        return None

    return str(sorted(candidates, reverse=True)[0])


def patched_r_script(original_r: Path, data_path: Path) -> Path:
    text = original_r.read_text(encoding="utf-8", errors="replace")
    workdir = str(data_path.parent).replace("\\", "/")
    data_file = str(data_path).replace("\\", "/")

    # Redirect common absolute setwd() lines while leaving estimation logic alone.
    text = re.sub(
        r"setwd\s*\((['\"]).*?\1\s*\)",
        f"setwd({workdir!r})",
        text,
        flags=re.IGNORECASE,
    )

    if "setwd(" not in text[:2000]:
        text = f"setwd({workdir!r})\n" + text

    # Redirect hard-coded absolute data.csv reads, preserving all downstream logic.
    text = re.sub(
        r"(['\"])(?:~|[A-Za-z]:)?[^'\"]*data\.csv\1",
        repr(data_file),
        text,
    )

    # The published data.csv has no immigration column, while the published R
    # script creates unused logim/gim intermediates from it. Supplying NA keeps
    # that dead branch runnable without changing any Table 2 estimation inputs.
    text = text.replace(
        "data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logincome=log(medhhincome),loghpi=log(hpi),logemp=log(pop_above18),logim=log(immigration))",
        (
            'if (!"immigration" %in% names(data_ai_2017_2022)) '
            "data_ai_2017_2022$immigration <- NA_real_\n"
            "data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logincome=log(medhhincome),loghpi=log(hpi),logemp=log(pop_above18),logim=log(immigration))"
        ),
    )

    text += textwrap.dedent(
        """

        cat("\\nAKCLM Table 2 bachelor's-share rows from replication_code.R model objects\\n")
        table2_bac <- data.frame(
          model = c("Col 1", "Col 4", "Col 5"),
          coef = c(coef(est_demog_no)["share_bac14"],
                   coef(est_all)["share_bac14"],
                   coef(est_all_state)["share_bac14"]),
          se = c(se(est_demog_no)["share_bac14"],
                 se(est_all)["share_bac14"],
                 se(est_all_state)["share_bac14"])
        )
        print(table2_bac, row.names = FALSE)
        cat("\\nAKCLM Table 2 sample diagnostics from replication_code.R\\n")
        data_t2_diag <- data_ai_l_z %>%
          filter(dai_intensity9 > -5, dai_intensity9 < 10) %>%
          drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14,
                  logincome14, tightness14, pat_intensity14, patai_intensity14,
                  degshare14, stemshare14, large_firms14, information_intensity14,
                  manuf_intensity14, TurnOvrS14)
        print(data.frame(
          n = nrow(data_t2_diag),
          share_bac14_mean_z = mean(as.numeric(data_t2_diag$share_bac14)),
          share_bac14_sd_z = sd(as.numeric(data_t2_diag$share_bac14))
        ))
        """
    )

    patched = CACHE / "replication_code_patched_path_only.R"
    patched.write_text(text, encoding="utf-8")
    return patched


def run_akclm_r_code(r_path: Path | None, data_path: Path | None) -> None:
    banner("AKCLM replication_code.R terminal run")
    rscript = find_rscript()
    print(f"Rscript: {rscript if rscript else 'NOT FOUND'}")

    if r_path is None or data_path is None:
        print("Skipped: archive did not expose both replication_code.R and data.csv.")
        return
    if rscript is None:
        print("Skipped: Rscript is not installed or is not on PATH.")
        return

    patched = patched_r_script(r_path, data_path)
    print(f"Running patched path-only script: {patched}")
    print("Only path-like setwd(...) calls were redirected; estimation lines were not changed.")
    print("-" * 88)
    proc = subprocess.run(
        [rscript, str(patched)],
        cwd=str(data_path.parent),
        text=True,
        capture_output=True,
        timeout=20 * 60,
    )
    if proc.stdout:
        print(proc.stdout.rstrip())
    if proc.stderr:
        print("\n[stderr]")
        print(proc.stderr.rstrip())
    print("-" * 88)
    print(f"Rscript exit code: {proc.returncode}")


def r_scale(series: pd.Series) -> pd.Series:
    return (series - series.mean()) / series.std(ddof=1)


def build_table2_sample(data_raw: pd.DataFrame) -> tuple[pd.DataFrame, pd.Series]:
    df = data_raw[data_raw["Year"].isin([2017, 2018, 2022, 2023])].copy()
    df["new"] = (df["Year"] > 2020).astype(int)

    df = df.sort_values(["COUNTY_FIPS", "new", "Year"])
    df["ai_sum"] = df.groupby(["new", "COUNTY_FIPS"])["ai"].transform("sum")
    df["nads_sum"] = df.groupby(["new", "COUNTY_FIPS"])["nads"].transform("sum")
    df["ai_intensity"] = df["ai_sum"] / df["nads_sum"]
    df = df[df["Year"].isin([2017, 2022])].copy()

    df["logincome"] = np.log(df["medhhincome"])
    df["logpop"] = np.log(df["pop"])
    df["lads"] = np.log(1 + df["nads_sum"])
    df["pat_intensity"] = df["n_inventors"] / df["Employed"]
    df["patai_intensity"] = (df["ai_patents"] / df["n_patents"]).fillna(0)
    df["large_firms"] = 1 - (df["small"] + df["medium"]) / df["est"]
    df["information_intensity"] = df["information_emp"] / df["emp"]
    df["manuf_intensity"] = df["manuf_emp"] / df["emp"]
    df["degshare"] = (df["udeg"] + df["mdeg"]) / df["Employed"]
    df["stemshare"] = ((df["ustemdeg"] + df["mstemdeg"]) / (df["udeg"] + df["mdeg"])).fillna(0)
    df["tightness"] = df["nads_sum"] / df["Unemployed"]
    df["hpi_ch"] = df["hpi_ch"] / 100

    df = df.sort_values(["COUNTY_FIPS", "Year"]).reset_index(drop=True)
    lag_cols = [
        "share_bac",
        "share_black",
        "share_poverty",
        "logpop",
        "hpi_ch",
        "logincome",
        "tightness",
        "pat_intensity",
        "patai_intensity",
        "degshare",
        "stemshare",
        "large_firms",
        "information_intensity",
        "manuf_intensity",
        "TurnOvrS",
        "lads",
        "unrate",
    ]
    for col in lag_cols:
        df[f"{col}14"] = df.groupby("COUNTY_FIPS")[col].shift(1)

    df["ai_int_lag1"] = df.groupby("COUNTY_FIPS")["ai_intensity"].shift(1)
    df["logpop_lag1"] = df.groupby("COUNTY_FIPS")["logpop"].shift(1)
    df["dai_intensity9"] = df["ai_intensity"] - df["ai_int_lag1"]
    df["gpop"] = (df["logpop"] - df["logpop_lag1"]) / 5

    df2 = df[(df["emp"] != 0) & (df["Year"] == 2022)].dropna(subset=["share_bac14"]).copy()
    share_bac_raw = df2["share_bac14"].copy()

    predictors = [
        "share_bac14",
        "share_black14",
        "share_poverty14",
        "logpop14",
        "hpi_ch14",
        "logincome14",
        "tightness14",
        "pat_intensity14",
        "patai_intensity14",
        "degshare14",
        "stemshare14",
        "large_firms14",
        "information_intensity14",
        "manuf_intensity14",
        "TurnOvrS14",
        "unrate14",
    ]
    for col in predictors:
        df2[col] = r_scale(df2[col])

    df2["dai_intensity9"] = df2["dai_intensity9"] * 100
    df_t2 = df2[(df2["dai_intensity9"] > -5) & (df2["dai_intensity9"] < 10)].dropna(
        subset=[
            "share_bac14",
            "share_black14",
            "share_poverty14",
            "logpop14",
            "hpi_ch14",
            "logincome14",
            "tightness14",
            "pat_intensity14",
            "patai_intensity14",
            "degshare14",
            "stemshare14",
            "large_firms14",
            "information_intensity14",
            "manuf_intensity14",
            "TurnOvrS14",
        ]
    ).copy()

    return df_t2, share_bac_raw


def run_wls(formula: str, data: pd.DataFrame):
    return smf.wls(formula, data=data, weights=data["lads14"]).fit()


def python_table2_check(data_path: Path, source_label: str) -> None:
    banner(f"Python Table 2 check using {source_label}")
    data_raw = pd.read_csv(data_path)
    print(f"Data path: {data_path}")
    print(f"Rows: {len(data_raw):,}; columns: {data_raw.shape[1]}")
    print(f"Years: {sorted(data_raw['Year'].dropna().unique().astype(int).tolist())}")
    print("share_bac source: read directly from AKCLM data.csv; no ACS rebuild is performed.")
    print(
        "Published AKCLM Table 2 bachelor's-share values: "
        "Col 1 = 0.0022 (0.0027), Col 4 = -0.0035 (0.0032), "
        "Col 5 = -0.0044 (0.0029)."
    )
    print(
        "AKCLM replication-code rerun values: "
        "Col 1 = 0.0196 (0.0250), Col 4 = -0.0322 (0.0292), "
        "Col 5 = -0.0401 (0.0263)."
    )

    raw_all = data_raw["share_bac"].dropna()
    raw_2017 = data_raw.loc[data_raw["Year"] == 2017, "share_bac"].dropna()
    print("\nRaw share_bac summary")
    print(f"  all years N={len(raw_all):,}, mean={raw_all.mean():.6f}, sd={raw_all.std(ddof=1):.6f}")
    print(f"  2017      N={len(raw_2017):,}, mean={raw_2017.mean():.6f}, sd={raw_2017.std(ddof=1):.6f}")

    df_t2, share_bac_raw = build_table2_sample(data_raw)
    print("\nTable 2 share_bac14 standardization")
    print(
        f"  pre-scale  N={len(share_bac_raw):,}, "
        f"mean={share_bac_raw.mean():.6f}, sd={share_bac_raw.std(ddof=1):.6f}, "
        f"min={share_bac_raw.min():.6f}, max={share_bac_raw.max():.6f}"
    )
    print(
        f"  reg sample N={len(df_t2):,}, "
        f"mean(z)={df_t2['share_bac14'].mean():.6f}, "
        f"sd(z)={df_t2['share_bac14'].std(ddof=1):.6f}"
    )

    formulas = {
        "Col 1": (
            "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + "
            "hpi_ch14 + logincome14 + tightness14"
        ),
        "Col 4": (
            "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + "
            "hpi_ch14 + logincome14 + tightness14 + pat_intensity14 + patai_intensity14 + "
            "degshare14 + stemshare14 + large_firms14 + information_intensity14 + "
            "manuf_intensity14 + TurnOvrS14"
        ),
        "Col 5": (
            "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + "
            "hpi_ch14 + logincome14 + tightness14 + pat_intensity14 + patai_intensity14 + "
            "degshare14 + stemshare14 + large_firms14 + information_intensity14 + "
            "manuf_intensity14 + TurnOvrS14 + C(state)"
        ),
    }

    print("\nBachelor's-share coefficient comparison")
    print(
        f"{'Model':<8} {'Python coef':>12} {'R rerun':>12} {'R SE':>10} "
        f"{'AKCLM pub':>12} {'pub SE':>10} {'gap vs rerun':>13} {'gap vs pub':>12}"
    )
    print("-" * 88)
    for label, formula in formulas.items():
        fit = run_wls(formula, df_t2)
        coef = fit.params["share_bac14"]
        code_coef, code_se = AKCLM_RERUN[label]
        pub_coef, pub_se = AKCLM_PUBLISHED[label]
        gap_code = coef - code_coef
        gap_pub = coef - pub_coef
        print(
            f"{label:<8} {coef:>12.4f} {code_coef:>12.4f} {code_se:>10.4f} "
            f"{pub_coef:>12.4f} {pub_se:>10.4f} {gap_code:>12.4f} {gap_pub:>12.4f}"
        )


def compare_local_to_archive(local_path: Path, archive_path: Path | None) -> None:
    if archive_path is None or not local_path.exists():
        return
    banner("Local data.csv vs extracted AKCLM data.csv")
    local = pd.read_csv(local_path)
    akclm = pd.read_csv(archive_path)
    keys = ["state", "Year", "COUNTY_FIPS"]
    merged = local[keys + ["share_bac"]].merge(
        akclm[keys + ["share_bac"]],
        on=keys,
        how="outer",
        suffixes=("_local", "_akclm"),
        indicator=True,
    )
    both = merged[merged["_merge"] == "both"].copy()
    diff = (both["share_bac_local"] - both["share_bac_akclm"]).abs()
    print(f"Local rows: {len(local):,}; AKCLM rows: {len(akclm):,}; matched rows: {len(both):,}")
    print(f"share_bac exact matches: {(diff == 0).sum():,} of {len(diff):,}")
    print(f"share_bac abs diff median={diff.median():.8f}; p90={diff.quantile(0.90):.8f}; max={diff.max():.8f}")


def main() -> None:
    banner("Archive acquisition")
    zip_path = try_download_archive()
    data_path, r_path = extract_archive(zip_path)

    run_akclm_r_code(r_path, data_path)

    if data_path is not None:
        python_table2_check(data_path, "extracted AKCLM archive data.csv")
    else:
        python_table2_check(ROOT / "data.csv", "local data.csv fallback")

    compare_local_to_archive(ROOT / "data.csv", data_path)


if __name__ == "__main__":
    main()
