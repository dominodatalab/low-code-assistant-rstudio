# assistDomino 0.3.0 (2022-12-22)

- LCA now works even when there is no internet connection (e.g. in an air-gapped environment)
- Visualizations wizard is now fully functional and powerful. When invoked from the Addins menu, if a variable holding a data frame is selected then that data frame will be used.
- Snippets: support adding snippets, editing snippets, deleting snippets. "Edit mode" for snippets is disabled by default, but enabling it remains "sticky" until the R session is restarted.
- Snippets: when inserting code snippets, `library()` calls should not be removed (in other LCA wizards, duplicate `library()` calls are removed)  
- Data module: add penguins demo dataset
- Data module: column names of loaded dataset are automatically fixed if they're invalid
- Data module: add different options when loading data of different formats, such as selecting the delimeter in a CSV file or selecting the sheet number in an Excel file
- Transformations: When input data has over 5000 rows, show a warning and only use first 10k rows for performance reasons
- Transformations: Previously, the transformations code was getting executed in the global environment after each transformation was made, which meant that new variables were getting created in the global env even without clicking the "Insert Code" button. Now the code only runs after choosing to "Insert Code".
- Transformations: fix bug where filtering a column that's an ordered factor (for example, "cut" or "color" in the `ggplot2::diamonds` dataset) was resulting in an error
- Transformations: fix bug where missing values in numeric columns were not treated as missing values
- Transformations: when invoking the addin in RStudio, if a variable holding a data frame is selected then that data frame will be used
- Transformations: allow wizard to be initialized with an existing data frame
- Transformations: allow wizard to work with reactive data
- All LCA wizards: when creating a variable name, automatically fix the name if it's invalid. For file names, ensure the name is not empty and contains no slashes.
 
# assistDomino 0.2.0 (2022-11-26)

- Add Snippets wizard for inserting code snippets
- Data module: add a 'Demo Data' tab to allow loading demo datasets
- Data module: support reading many more file types beyond CSV (.csv, .txt, .sas7bdat, .xpt, .sav, .zsav, .dta, .por, .xls, .xlsx)
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
