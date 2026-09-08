# Cash Consortium of Yemen (CCY) — Deduplication Platform
### نظام مطابقة ومراجعة بيانات المستفيدين — اتحاد النقد لليمن

[![R Test Suite](https://img.shields.io/badge/testthat-526%20passed%20%7C%200%20failed-1B5E20?style=flat-square&logo=R)](tests/testthat)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.2.0-0F766E?style=flat-square&logo=r)](https://www.r-project.org/)
[![Shiny Framework](https://img.shields.io/badge/Shiny-bslib%20Bootstrap%205-2E7D32?style=flat-square&logo=rstudio)](https://shiny.posit.co/)
[![Data Protection](https://img.shields.io/badge/PII%20Protection-Masked%20Audit%20Ready-green?style=flat-square)](#security-privacy--pii-protection)
[![License](https://img.shields.io/badge/License-Humanitarian%20Non--Commercial-blue?style=flat-square)](LICENSE)

An enterprise-grade **Humanitarian Beneficiary Verification & Deduplication Platform** built with **R Shiny**, purpose-engineered for the **Cash Consortium of Yemen (CCY)** and partner humanitarian agencies (DRC, ACTED, NRC, SCI, IOM). 

The platform identifies and reconciles duplicate beneficiary registrations across emergency Multi-Purpose Cash Assistance (MPCA) programs, mitigating aid diversion, ensuring fair coverage for crisis-affected households, and safeguarding sensitive Personally Identifiable Information (PII).

---

## 📑 Table of Contents

- [Core Mission & Capabilities](#-core-mission--capabilities)
- [End-to-End Workflow (5 Steps)](#-end-to-end-workflow-5-steps)
- [Data Health & Hygiene Audit Suite](#-data-health--hygiene-audit-suite)
- [Dual Matching Engine & Algorithms](#-dual-matching-engine--algorithms)
- [ActivityInfo Integration & Snapshot Architecture](#-activityinfo-integration--snapshot-architecture)
- [Security, Privacy & PII Protection](#-security-privacy--pii-protection)
- [System Architecture & Tech Stack](#-system-architecture--tech-stack)
- [Project Directory Structure](#-project-directory-structure)
- [Installation & Quick Start](#-installation--quick-start)
- [Configuration & Environment Variables](#-configuration--environment-variables)
- [Automated Testing & Quality Assurance](#-automated-testing--quality-assurance)
- [UI/UX Design System (Hallmark Integration)](#-uiux-design-system-hallmark-integration)
- [Consortium Partners & Support](#-consortium-partners--support)

---

## 🎯 Core Mission & Capabilities

In emergency humanitarian cash responses in Yemen, multiple partners operate across overlapping geographic areas. Incomplete registries, informal name variants, Arabic typographical discrepancies, and shared SIM cards create duplicate enrollments. 

This platform provides:
- **Intra-List Deduplication (Same-List Matching)**: Detects duplicate individuals, families, or phone numbers submitted within a single partner's file.
- **Inter-Agency Deduplication (ActivityInfo Master Matching)**: Cross-references partner submissions against the central ActivityInfo consortium database of hundreds of thousands of historical and active beneficiaries.
- **MPCA Assistance Recency Filtering**: Allows deduplicating exclusively against households assisted within a customizable active window (e.g. `< 6 months / 180 days` using `Dist_Date_Calc_New`), enabling dignified re-targeting after assistance cycles conclude.
- **Air-Gapped / Offline Reliability**: Snapshots the central database locally (`tmp/*.rds`) so teams can execute deduplications in field offices without stable satellite connectivity.
- **High-Trust Reporting**: Generates a standardized, auditable 6-sheet Excel workbook ready for distribution and partner verification.

---

## 🔄 End-to-End Workflow (5 Steps)

```
[1. Upload & Health] ➔ [2. Mapping Workbench] ➔ [3. Strategy & MPCA Window] ➔ [4. Async Engine] ➔ [5. Review & Export]
```

### 1. Upload & Data Health
- Supports `.xlsx`, `.xls`, and `.csv` files up to 50 MB.
- Provides a standard downloadable CCY template with pre-formatted column headers.
- Automatically initiates an instant 12-vector **Data Health & Hygiene Audit** before any matching begins.

### 2. Column Mapping Workbench
- Matches raw partner column headers against the 23 standard consortium fields using string-distance heuristics (Jaro-Winkler).
- Interactive drag-and-drop / dropdown alignment workbench with match confidence badges (`Confirmed`, `Suggested`, `Unmapped`).
- Required vs. optional field validation guards the workflow from advancing with incomplete data.

### 3. Strategy Configuration & Parameter Tuning
- **Confidence Thresholds**: Interactive sliders for High Confidence (default `≥ 90%`) and Medium Review (default `75% - 89%`).
- **Candidate Block Limits**: Configurable blocking cap (50 to 2,000 candidate pairs per block) to optimize speed on large datasets.
- **MPCA Distribution Recency Window**: Checkbox filter to target only beneficiaries whose last assistance date (`Dist_Date_Calc_New`) falls within the active recency window (1 to 12 months, default 6 months).

### 4. Asynchronous Matching Execution
- Backed by R `future` and `promises` using multi-process background workers (`plan(multisession)`).
- Keeps the Shiny UI fully interactive and snappy during execution.
- Real-time progress bar, estimated time remaining, processed candidate counts, and safe cancellation ("Stop & Start Over").

### 5. Results Dossier & Review
- Comprehensive executive KPI cards: Total records, exact duplicates, fuzzy candidates, unique beneficiaries.
- Interactive data tables with quick-filters and side-by-side comparison of candidate pairs.
- One-click export of an auditable, stylized `.xlsx` report.

---

## 🩺 Data Health & Hygiene Audit Suite

The platform includes a dedicated pre-processing audit engine that analyzes uploads before matching:

| Diagnostic Check | Problem Detected | Automatic Correction / Flagging |
| :--- | :--- | :--- |
| **Scientific Notation** | Excel converts large IDs (`1.23E+11`) | Extracts integer representation from raw XML/ZIP and restores full digit string |
| **Leading Zero Truncation** | Dropped leading zeros in IDs and phones (`010...` -> `10...`) | Restores 9-digit Yemeni phone numbers and standardizes National ID strings |
| **Arabic Text Discrepancies** | Inconsistent Alefs (`أ`, `إ`, `آ`, `ا`), Ya (`ي`, `ى`), Ta Marbuta (`ة`, `ه`) | Comprehensive Arabic normalization preserving canonical root characters |
| **Phone Number Formats** | International prefixes (`+967`, `00967`), spaces, hyphens | Strips non-digits, extracts 9-digit national subscriber numbers (`77`, `73`, `71`, `70`, `78`) |
| **Field Inversion** | Swapped fields (e.g. Phone placed in National ID or Name in Phone) | Evaluates pattern heuristics to flag displaced columns before matching |
| **Displacement Anomaly** | IDP (نزوح) marked without origin governorate or contradictory status | Cross-references beneficiary status against geographic indicators |
| **Geographic Hierarchy** | Typographical errors in Governorate / District / Sub-district | Normalizes whitespace, casing, and standardizes transliteration |
| **Age Outliers** | Typo ages (`< 15` or `> 110`) | Flags suspicious head-of-household age values for field verification |

---

## ⚙️ Dual Matching Engine & Algorithms

The matching engine uses a two-phase architecture designed for maximum sensitivity and low false-positive rates:

```
                          ┌──────────────────────┐
                          │  Uploaded Dataset    │
                          └──────────┬───────────┘
                                     │
                        ┌────────────┴────────────┐
                        ▼                         ▼
            [ Phase 1: Exact Rules ]    [ Phase 2: Candidate Blocking ]
            - Exact National ID         - Gov + Name Prefix
            - Exact Primary Phone       - Gov + Phone Prefix
            - Exact Secondary Phone     - Phone Suffix (Last 4)
            - Exact Name + Gov          - Gov + Spouse Prefix
                        │                         │
                        ▼                         ▼
             [ Exact Duplicates ]       [ Composite Fuzzy Scoring ]
                                        - HoH Arabic Name (0.40)
                                        - Phone Number    (0.25)
                                        - Spouse Name     (0.20)
                                        - Geography       (0.15)
                                                  │
                                                  ▼
                                       ┌──────────────────────┐
                                       │ Score >= High (90%)  │ ➔ High Confidence
                                       │ Score >= Med  (75%)  │ ➔ Medium Review
                                       │ Score <  Med  (75%)  │ ➔ Discarded
                                       └──────────────────────┘
```

### Scoring Components
- **Arabic Name Similarity**: Combined Jaro-Winkler distance, token-sort ratio, and Levenshtein distance across 4-part Arabic names.
- **Spouse Name Similarity**: Accounts for secondary household contact consistency.
- **Phone Proximity**: Exact match or partial suffix/prefix consistency.
- **Geographic Hierarchy**: Full match on Governorate + District, with partial credit for shared Sub-district/Village.
- **Age Tolerance**: Proximity function penalizing large age disparities while tolerating small self-reporting variances.

---

## ☁️ ActivityInfo Integration & Snapshot Architecture

The platform connects to the consortium's central **ActivityInfo** data store:
- **Chunked API Fetching**: Retrieves large datasets in memory-efficient batches (configurable via `DEDUP_ACTIVITYINFO_BATCH_SIZE`, default 2,000 records).
- **Snapshot Caching**: Saves raw and preprocessed records as `.rds` snapshots under `tmp/`.
- **Freshness Indicator**: The top navigation bar displays a live indicator pill showing the exact age of the local master snapshot (e.g. `Master DB Synced: 2h ago (Offline Ready)`).
- **Graceful Degradation**: If ActivityInfo is temporarily unreachable or internet fails, the platform functions seamlessly using the latest verified local snapshot.

---

## 🔒 Security, Privacy & PII Protection

Adhering to humanitarian **"Do No Harm"** digital principles and IASC Data Responsibility guidelines:

- **Role-Based Access Control (RBAC)**:
  - `ccy_master`: Full administrative access, token configuration, user management, unfiltered dossier access.
  - `partner_admin`: Partner user management, master data refresh, deduplication execution.
  - `partner_deduplicator`: Operational role restricted to deduplication workflows; cannot alter configuration or view other agencies' raw PII.
- **Two-Factor Authentication (MFA)**:
  - Secure 6-digit email-verified One-Time Passwords (OTP) expiring in 5 minutes.
  - Optional MFA enforcement per account managed via User Settings.
- **Master PII Masking on Export**:
  - In all partner export workbooks, sensitive master database identifiers are automatically masked (e.g., National ID: `XXXXXXXX1234`, Phone: `77XXXXX89`).
  - Partners confirm whether a duplicate exists without exposing beneficiary identities from other consortium agencies.
- **Session Timeout (TTL)**:
  - Automatic session expiration after inactivity, flushing temporary reactive values and memory.
- **Compliance Audit Logging**:
  - All critical actions (login, data fetch, deduplication start, report export, token update) are written to an append-only audit trail (`tmp/audit_log.json`).

---

## 📊 Export & Reporting Dossier

The platform exports a professional, multi-tab Excel workbook formatted with bold headers, frozen header rows, auto-filters, and conditional confidence styling:

1. **`Info`**: Metadata, user email, consortium partner, run timestamp, thresholds used, master snapshot version.
2. **`Summary`**: High-level metrics: Total uploaded records, unique cases, exact matches, fuzzy matches, overall duplicate rate.
3. **`Same List Exact Matching`**: Internal records matching on National ID, phone, or name + location.
4. **`Same List Fuzzy Matching`**: Internal records with high/medium similarity scores and match explanations.
5. **`List Vs ActivityInfo Exact`**: Partner records matched identically to previous consortium beneficiaries.
6. **`List Vs ActivityInfo Fuzzy`**: Probabilistic matches against the central master database with masked identifiers.

---

## 🏗 System Architecture & Tech Stack

- **Frontend Framework**: R Shiny (`1.8+`), `bslib` (Bootstrap 5), `DT` (DataTables).
- **Core Processing**: `dplyr`, `data.table`, `tidyr`, `purrr`, `stringi`.
- **Fuzzy Matching**: `stringdist`, `fuzzyjoin`.
- **Async Execution**: `future`, `promises`.
- **Excel Ingestion & Generation**: `readxl`, `openxlsx`.
- **Authentication & Security**: `bcrypt`, `jsonlite`.
- **Design System**: Custom CSS following the **Hallmark** design standard (WCAG 2.1 SC 2.3.3 compliant).

---

## 📁 Project Directory Structure

```text
06_deduplication_app_R/
├── app.R                       # Application entry point (UI, reactive graph, server logic)
├── ccy.ico                     # High-resolution application favicon
├── CCY logo.png                # Official Cash Consortium of Yemen logo
├── install.R                   # Dependency installer script
├── users.json                  # Local credential store (bcrypt password hashes & RBAC roles)
├── AGENTS.md                   # AI Assistant coding guidelines and repository rules
├── GEMINI.md                   # Architecture and technical overview
├── README.md                   # Operational & technical documentation
├── R/                          # Application modules & business logic
│   ├── activityinfo.R          # ActivityInfo API connector, batching, and chunked download
│   ├── audit.R                 # Compliance logging and audit trail recorder
│   ├── auth.R                  # Authentication, password verification, and RBAC helpers
│   ├── config.R                # System configuration, weights, thresholds, and required schema
│   ├── diagnostics.R           # Pre-flight data health, scientific format, and hygiene checks
│   ├── export.R                # openxlsx workbook generator with multi-sheet formatting
│   ├── jobs.R                  # Background job manager (future/promises async execution)
│   ├── mapping.R               # Column auto-mapping algorithms and string similarity
│   ├── matching.R              # Core deduplication algorithms, blocking keys, fuzzy scoring
│   ├── mfa.R                   # Two-factor authentication (email OTP generator & verifier)
│   ├── preprocess.R            # Arabic name, phone, ID, and geographic normalizers
│   ├── ttl.R                   # Inactive session timeout and memory scavenger
│   └── ui_helpers.R            # Hallmark design components, icons, modals, and stepper
├── tests/                      # Automated test suite
│   ├── testthat.R              # Test runner
│   └── testthat/               # Unit and integration test specifications
│       ├── test_inversion_displacement_audit.R  # Diagnostic and hygiene tests
│       ├── test_mapping_workbench.R             # Column mapping tests
│       ├── test_matching.R                      # Core blocking and scoring tests
│       └── test_pii_masking_and_export_cleanup.R # Security, masking, and export tests
├── docs/                       # Detailed subsystem documentation
│   ├── ARCHITECTURE.md         # Technical architecture & pipeline data flows
│   ├── MATCHING.md             # Matching algorithms, weights, and scoring breakdown
│   ├── SECURITY.md             # Data protection, PII masking, and credentials policy
│   ├── PARTNER_GUIDE.md        # User manual for partner data officers
│   ├── API.md                  # ActivityInfo API specification
│   └── DEPLOYMENT.md           # Production deployment & server configuration guide
├── www/                        # Static assets served by Shiny
│   ├── ccy.ico                 # Web favicon
│   ├── ccy_logo.png            # Web-optimized CCY logo
│   └── custom.css              # Hallmark CSS design system (tokens, components, responsive)
└── tmp/                        # Local ephemeral cache (snapshots, job state, logs - gitignored)
```

---

## 🚀 Installation & Quick Start

### Prerequisites
- **R**: Version `4.2.0` or higher (tested on R 4.4.x and R 4.6.x).
- **Rtools** (Windows only): Required for compiling C-based string distance libraries.
- **RAM**: Minimum 4 GB (8 GB+ recommended when handling master databases > 250,000 rows).

### 1. Clone the Repository
```bash
git clone https://github.com/your-org/06_deduplication_app_R.git
cd 06_deduplication_app_R
```

### 2. Install Required R Packages
Run the automated installation script:
```bash
Rscript install.R
```
*Or install manually inside an R session:*
```r
install.packages(c(
  "shiny", "bslib", "DT", "readxl", "openxlsx", "dplyr", "tidyr",
  "purrr", "stringi", "stringdist", "fuzzyjoin", "data.table",
  "httr2", "jsonlite", "promises", "future", "progressr", "bcrypt", "testthat"
))
```

### 3. Launch the Application
Start the Shiny server:
```bash
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```
The application will launch in your default web browser at `http://127.0.0.1:port/`.

---

## ⚙️ Configuration & Environment Variables

The platform can be configured via environment variables or directly in `R/config.R`. For production deployments, define the following in your `.Renviron` or system environment:

| Variable | Description | Default |
| :--- | :--- | :--- |
| `DEDUP_ACTIVITYINFO_TOKEN` | API Token with read access to the CCY ActivityInfo database | `NULL` |
| `DEDUP_ACTIVITYINFO_BASE_URL` | ActivityInfo API endpoint URL | `https://www.activityinfo.org` |
| `DEDUP_ACTIVITYINFO_BATCH_SIZE` | Number of records per chunked API request | `2000` |
| `DEDUP_ADMIN_SETTINGS_PATH` | Path to persistent admin configuration file | `tmp/admin_settings.json` |
| `SMTP_SERVER` | SMTP host for sending MFA login codes | `NULL` |
| `SMTP_USER` | SMTP username / sender email | `NULL` |
| `SMTP_PASS` | SMTP application password | `NULL` |

---

## 🧪 Automated Testing & Quality Assurance

The codebase includes a comprehensive **526-assertion test suite** covering data hygiene, column mapping, blocking efficiency, fuzzy scoring thresholds, PII masking, and Excel generation.

Execute the test suite from PowerShell or Terminal:
```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat")'
```

### Test Suite Coverage:
- `test_matching.R`: Validates deterministic exact matching rules, blocking candidate generation, Jaro-Winkler/Levenshtein weights, and confidence categorization.
- `test_mapping_workbench.R`: Tests heuristic auto-mapping against messy partner schemas, alias dictionary lookups, and mandatory field validation.
- `test_inversion_displacement_audit.R`: Verifies scientific notation restoration, phone formatting, displacement anomalies, and column inversion detectors.
- `test_pii_masking_and_export_cleanup.R`: Guarantees that exported Excel files mask sensitive master data, retain formatting, and conform to consortium reporting standards.

---

## 🎨 UI/UX Design System (Hallmark Integration)

The user interface follows the **Hallmark Design System** — an anti-AI-slop design methodology prioritizing structural variety, tactile authenticity, and corporate humanitarian trust:

- **Humanitarian Color Palette**:
  - **Forest Green** (`#1B5E20`): High-trust primary brand color for navigation, titles, and positive actions.
  - **Ink Green** (`#2E7D32`): Active interactive states, tabs, and primary buttons.
  - **Sea Teal** (`#0F766E`): Secondary data points, download links, and informational accents.
  - **Sand / Mist** (`#F8FAFC`, `#F1F5F9`): Soft, glare-reducing background layers for long working sessions.
- **Bilingual Typographic Harmony**:
  - English display: **Plus Jakarta Sans** & **Inter**.
  - Arabic display: **IBM Plex Sans Arabic** for clear, legible rendering of complex Yemeni names and place names.
- **Snappy, Hardware-Accelerated Micro-Interactions**:
  - Removed clunky full-page animation delays to guarantee instant DOM mounting and rapid DataTables column alignment.
  - Button presses and state toggles execute in a snappy **100–120ms** micro-interaction budget.
- **Accessibility (A11y)**:
  - Strict compliance with **WCAG 2.1 SC 2.3.3 (Reduced Motion)**.
  - Full keyboard navigability (Enter-to-submit login, tab-aware DataTables recalculation).
  - High-contrast text tokens ensuring readability in field conditions.

---

## 🤝 Consortium Partners & Support

The CCY Deduplication Platform is deployed in support of the **Cash Consortium of Yemen (CCY)**:
- **Danish Refugee Council (DRC)** *(Lead Agency)*
- **Agency for Technical Cooperation and Development (ACTED)**
- **Norwegian Refugee Council (NRC)**
- **Save the Children International (SCI)**
- **International Organization for Migration (IOM)**

For technical support, feature requests, or deployment inquiries:
- **Technical Lead / M&E Development**: [hamzaabdullahmoh@gmail.com](mailto:hamzaabdullahmoh@gmail.com)
- **Documentation & Architecture**: See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) and [`docs/MATCHING.md`](docs/MATCHING.md)

---
*Built with care for humanitarian integrity, dignity, and accuracy in Yemen.*
