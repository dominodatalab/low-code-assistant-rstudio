# Unreleased version

- Transformations: fix bug where missing values in numeric columns were not treated as missing values
- Transformations: allow wizard to be initialized with an existing data frame
- Transformations: allow wizard to work with reactive data
 
# assistDomino 0.2.0 (2022-11-26)

- Add Snippets wizard for inserting code snippets
- Data module: add a 'Demo Data' tab to allow loading demo datasets
- Data module: support reading many more file types beyond CSV (.csv, .txt, .tsv, .sas7bdat, .xpt, .sav, .zsav, .dta, .por, .xls, .xlsx)
- Data module: show user-friendly error message when a data source fails to get read correctly 
- Data module: make preview and code always reflect the currently selected tab
- Data module: allow user to automatically generate output variable name based on the selected data
- Transformations module: allow using either {tidyverse} or base R for the transformations code
- Code chunk insertion: Don't insert any `library(...)` calls into a script that already contains this line 
- Code chunk insertion: Insert an empty line after every code chunk that gets inserted into a script
- File browser: allow clicking on any parent path in the breadcrumbs to quickly navigate there
- File browser: allow de-selecting items by clicking on them again
- Improve the UI for all file browsers and list selectors
- Mixpanel: use server-side tracking when the client is blocked
- Add support for {data.table} dataframes in Transformations wizard
- Fix bug where modals would close unexpectedly on Firefox
- Make the package work with older shiny versions (previously version >= 1.7 was needed)
- Add all package dependencies that were previously missing

# assistDomino 0.1.0 (2022-10-13)

- Initial release
