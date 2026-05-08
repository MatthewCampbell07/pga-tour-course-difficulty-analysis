# pga-tour-course-difficulty-analysis
A comparative study of hole-by-hole difficulty on the 2025 PGA Tour using clustering and field-relative scoring in R.

**Project Overview**

Using performance data from every player across the 2025 PGA Tour Season, I constructed two hypothetical 18-hole courses. By selecting the most extreme versions of every hole number (1-18), the analysis explores how course architecture dictates the specific skills required to succeed.

**Key Insights**

The Hardest Course (Par 69, average +7 score)
- Composed entirely of Par 3s and Par 4s. Insights show that scoring volatility is highest when recovery options are limited, particularly on the "Green Mile" at Quail Hollow.

The Easiest Course (Par 89, average -12.3 score)
- Dominated by 17 Par 5s, highlighting that modern professional golf relies on Par 5 scoring for "safe" birdie opportunities.

Total Score
- Despite a 20-shot difference in Par, the expected field average for both courses is nearly identical (76.0 vs 76.7), shifting the focus from "beating the course" to "skill acquisition".

**Tech Stack & Methodology**

- Language: R
- Library: golfastr
- Process: Clustering 800+ holes based on field-relative scoring, volatility, and birdie, bogey rates.
