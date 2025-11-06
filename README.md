# 🌐 Web Scraping & Data Visualization using R

---

## 🧭 Project Summary
This project demonstrates **web scraping, data cleaning, and visualization** using the **R programming language**.  
The objective was to extract academic journal metadata on the topic **“Immunity and Ageing”**, clean and analyze the data, and visualize trends in keyword distributions and publication patterns.

---

## ⚙️ Tools & Packages
| Purpose | Packages Used |
|----------|----------------|
| Web Scraping | `rvest`, `httr`, `xml2` |
| Data Cleaning | `dplyr`, `tidyr` |
| Visualization | `ggplot2` |
| Data Export | `writexl` |

---

## 🔍 Workflow
1. **Setup Environment:** Installed core scraping and data libraries.  
2. **Data Extraction:**  
   - Used `rvest` to read HTML structure via CSS selectors & XPath.  
   - Employed `SelectorGadget` to identify tags for titles, keywords, and abstracts.  
   - Implemented a `for` loop to scrape ~12 pages, each containing ~50 journal entries.  
3. **Data Cleaning & Preprocessing:**  
   - Handled missing and duplicate entries.  
   - Applied validation and encoding for structured fields.  
4. **Data Analysis & Visualization:**  
   - Created bar plots and histograms using `ggplot2`.  
   - Analyzed **keyword frequency distribution** across all collected articles.  
5. **Data Export:**  
   - Final dataset exported using `writexl` into `.xlsx` format for reporting.

---

## 📊 Key Insights
- Automated scraping reduced manual data collection by >90%.  
- Articles with higher keyword density showed strong correlation to aging research.  
- Visualization highlighted the most frequent research terms in **Immunity & Aging**.  

---

## 🧩 Challenges
- Handling **multi-page scraping** across dynamically generated HTML content.  
- Extracting nested **keyword tags** from complex DOM structures.  
- Synchronizing data extraction speed with server request limits.  

---

## 🚀 Outcome
Successfully developed a reproducible **web scraping and visualization workflow in R**, enabling data analysts to collect, clean, and visualize scholarly article metadata efficiently.

---


