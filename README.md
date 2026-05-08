# ⛳ PGA Tour 2025: Hardest vs. Easiest Course Analysis

## About Me
I hold an **MA (Hons) in International Business Management** and an **MSc in Sports Data Analytics**. My background includes creating and implementing a Shiny App (linked to Google Sheets) for the elite golf team at the University of Stirling, video analysis at The Glasgow Academy, and I specialise in bridging the gap between raw sports data and actionable tactical insights.

## Project Overview
This project analyses player performance data from the **2025 PGA Tour** to construct two extreme hypothetical 18-hole courses. By using the `golfastr` package in **R**, I clustered over 800 holes to find the ultimate tests of skill. This analysis can be leveraged by broadcasters to predict 'moving day' volatility or by sponsors to identify holes with the highest 'brand exposure time' based on player difficulty and time spent on the green.

### Key Findings
* **The Hardest Course (Par 69):** Average score of +7. Contains zero Par 5s, highlighting how difficult it is to score when "recovery" opportunities are removed.
* **The Easiest Course (Par 89):** Average score of -12.3. Composed of 17 Par 5s, proving that modern scoring relies almost entirely on long-game efficiency.
* **The "Green Mile":** Data confirms that holes 16, 17, and 18 at Quail Hollow remain the most punishing finishing stretch on tour.

## Methodology
* **Language:** R
* **Library:** `golfastr`
* **Technique:** Clustering based on field-relative scoring, volatility, and birdie/bogey rates.

