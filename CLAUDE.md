# EM34AHL – instructions for Claude

Replication of Pawar & Munuswamy (2024) for Empirical Methods, UZH HS 2026.
PS3: `PS3_Solution.R`. PS4: website in `docs/`, data from `PS4_Export.R`. Details: `README.md`.

## Rules

| Rule | What it means |
|---|---|
| Numbers only from `docs/data/results.js` | Every number on the site or in text about results comes from `results.js`. Never type, round by hand or guess a number. If one is missing, add it to `PS4_Export.R` and regenerate. |
| No names or student IDs in the repo | Not in files, commit messages or comments. Report files (`*.tex`, `*.pdf`, …) stay out of the repo (`.gitignore`). |
| Ask before every push | Show the diff first; commit and push only after explicit approval. |

## Running R

`Rscript` is not on the PATH. Use R 4.4.1 (version in `README.md`), from the repo root:

```
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" PS4_Export.R
```

`results.js` contains a creation timestamp and the R version in `meta`, so it changes on every run.
Compare the data without `meta` before committing.
