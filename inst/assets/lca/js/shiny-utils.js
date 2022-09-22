$(function() {
  Shiny.addCustomMessageHandler("lca-close-window", function(message) {
    window.close();
  });
});
