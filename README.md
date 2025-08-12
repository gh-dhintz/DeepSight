# DeepSight

DeepSight is a Shiny app that allows you to generate dynamic reports from a YAML configuration file.

the file structure is as follows:

```
DeepSight/
├── app.R                     # Main Shiny app (rename from app43v2.R)
├── DeepSight.Rproj      # Keep project file
├── README.md                 # Add project documentation
│
├── R/                        # R code modules
│   ├── plot_functions.R      # Plot generation functions
│   ├── ui_helpers.R          # UI generation functions
│   ├── data_functions.R      # Data creation/manipulation
│   ├── config.R              # Configuration objects (PLOT_CONFIG, etc.)
│   └── utils.R               # Utility functions
│
├── www/                      # Web assets (Shiny auto-serves this)
│   ├── styles.css            # Your CSS file
│   ├── app.js                # Your JavaScript
│   └── logo.svg              # Move from images/Blood_Drop_Full_Color.svg
│
├── templates/                # Quarto templates
│   ├── pdf.qmd              # Keep only pdf_15.qmd, rename to pdf.qmd
│   ├── html.qmd             
│   └── word.qmd             
│
├── assets/                   # Static assets for reports
│   ├── images/              # Images used in reports
│   │   ├── cover.png
│   │   └── company_logos/   # Organize logos
│   ├── fonts/               # Rename from Helvetica_Neue_Fonts
│   │   └── helvetica/       # Font files
│   └── styles/              # Report styles (not web styles)
│       ├── _coverpage.tex
│       ├── _titlepage.tex
│       ├── before-body.tex
│       └── in-header.tex
│
├── data/                     # Keep as is
│   └── mtcars.csv
│
├── output/                   # Generated files (add to .gitignore)
│   ├── reports/             # Generated reports
│   └── logs/                # Log files
│       └── report_log.tsv   # Move here
│
├── archive/                  # Old versions (add to .gitignore)
│   ├── app_versions/        # Move app38-43 here
│   └── legacy/              # Move "legacy app versions" here
│
├── config/                   # Configuration files
│   ├── default_report_vars.yaml
│   └── report_params.yaml
│
├── references/              # Bibliography files
│   ├── packages.bib
│   ├── references.bib
│   └── sage-harvard.csl
│
└── scripts/                 # Utility scripts
    ├── install_deps.R
    └── setup.R              # Initial setup script
```