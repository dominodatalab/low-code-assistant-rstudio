# title_bar shiny ui

    Code
      title_bar_ui("test")
    Output
      <html class="lca-page"></html>
      <div class="header-bar flex flex-gap1">
        <div id="test-page_title" class="shiny-text-output"></div>
        <div class="flex-push">Domino R Assistant</div>
        <img height="100%" src="lca-assets/lca/img/domino-logo.svg"/>
      </div>

---

    Code
      title_bar_ui("test", "page title")
    Output
      <html class="lca-page"></html>
      <div class="header-bar flex flex-gap1">
        page title
        <div class="flex-push">Domino R Assistant</div>
        <img height="100%" src="lca-assets/lca/img/domino-logo.svg"/>
      </div>

