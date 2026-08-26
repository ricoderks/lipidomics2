// Warn the user when leaving the app (closing the tab/browser or reloading the
// page), because all data and results only live in the R session and are lost.
// The warning is only armed once data has been loaded; the server toggles it
// with the "lipidomics2-unsaved-work" custom message.
(function() {
  "use strict";

  var hasUnsavedWork = false;

  window.addEventListener("beforeunload", function(event) {
    if (!hasUnsavedWork) {
      return undefined;
    }

    // Browsers show their own generic message, a custom one is not possible.
    event.preventDefault();
    event.returnValue = "";
    return "";
  });

  var registerHandler = function() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {
      return;
    }

    Shiny.addCustomMessageHandler("lipidomics2-unsaved-work", function(message) {
      hasUnsavedWork = !!(message && message.unsaved);
    });
  };

  if (window.Shiny && window.Shiny.addCustomMessageHandler) {
    registerHandler();
  } else {
    document.addEventListener("DOMContentLoaded", registerHandler);
  }
})();
