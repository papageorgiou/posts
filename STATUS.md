# Status

Last updated: 2026-10-02 by cloud session (first version, built from git history and the post folders)
State: active (inferred, last post work 2026-08-19)
Category: content (LinkedIn posts and their analysis code)
Client: none

## Goal

One public folder per LinkedIn post, holding the data, the R code and the exported charts, so each post can link to its code and data.

## Needs Alex

- [ ] Confirm this status is right (first version, written from the repo alone).
- [ ] Mark which posts are published. The repo records no publish dates, so every state below is inferred.
- [ ] Decide whether to fix the moneymaxxing chart caption, which links to `tree/main` while the branch is `master` (see Loose ends).

## Blocked by

Nothing

## Next

1. Record publish dates per post (a column here, or a line in each folder README).
2. Fix the moneymaxxing caption link and re-export the charts, if Alex agrees.

## Where we stand

| Folder | Topic | Last commit | State |
|---|---|---|---|
| `moneymaxxing-search-demand/` | US search demand for "moneymaxxing" | 2026-08-19 | drafted (inferred, four chart variants, no pick recorded) |
| `ai-job-titles-search-demand/` | US search demand across 1,052 AI job titles | 2026-07-29 | drafted (inferred, three post charts) |
| `experiential/` | Rise of "experiential X" searches, nine niches | 2026-06-25 | drafted (inferred, post text was drafted then removed 2026-06-23) |
| `experiential-travel/` | Experiential travel brand search interest | 2026-06-25 | published (inferred, first committed 2026-01, retitled since) |
| `youtube-trends-seo/` | YouTube SEO search trends | 2026-06-25 | published (inferred, first committed 2025-06) |
| `gdp-employment-5countries/` | GDP vs employment, five countries, synthetic data | 2026-06-25 | unknown (looks like a chart style exercise) |
| `branded-search-vs-stock-sofi-ttd-upst/` | Branded search vs share price, SoFi, Trade Desk, Upstart | 2026-06-17 | drafted (inferred) |
| `startups-2022/` | Startup search universe 2022 | 2026-01-23 | published (inferred, README reads as the post) |
| `kith-search-data-analysis/` | Search data for the Thessaloniki history center | 2024-03-06 | published (inferred) |
| `big-pizza-sales-vs-searches/` | Big pizza chains, sales vs searches | 2024-03-05 | published (inferred) |

## Delivered

Nothing recorded. No folder notes a publish date or a post URL.

## Outside this folder

- The AI job titles data pipeline lives in a separate repo, `github.com/papageorgiou/ai-jobs` (per its README).
- The full startups-2022 time series is a CSV on Google Drive, linked from `startups-2022/README.md`.
- `youtube-trends-seo/ytseo-data_n_viz.Rmd` loads an R package `kw` that is not in this repo (inferred to be Alex's own).
- The published posts themselves are on LinkedIn. Nothing here links to them.

## Key outputs

- Moneymaxxing charts: `moneymaxxing-search-demand/charts/`
- AI job titles charts: `ai-job-titles-search-demand/post-*.png`
- Experiential: `experiential/experiential_base_3x3_xl.png`, more layouts in `Plots-Base/` and `Plots-Extra/`
- Search vs stock: `branded-search-vs-stock-sofi-ttd-upst/outputs/`
- Method notes: the README in each of `ai-job-titles-search-demand/`, `branded-search-vs-stock-sofi-ttd-upst/`, `experiential/` and `startups-2022/`

## Loose ends

- `moneymaxxing-search-demand/moneymaxxing_chart.R` captions point to `github.com/papageorgiou/posts/tree/main/...`. The default branch is `master`, so that link is broken in the exported charts.
- `experiential/` and `experiential-travel/` overlap in topic. They may be one post series or two posts (inferred).
- The experiential LinkedIn post draft, `experiential/experiential_linkedin_post.md`, was deleted on 2026-06-23. It is still in git history.
- Several folders have no README: `big-pizza-sales-vs-searches/`, `experiential-travel/`, `gdp-employment-5countries/`, `moneymaxxing-search-demand/`, `youtube-trends-seo/`.
- Large data (startups CSV and Parquet) and some plot folders are gitignored, so they exist only on the laptop.
