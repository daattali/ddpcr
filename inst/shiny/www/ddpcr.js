var ddpcr = {
  init : function() {
    $("[data-toggle='popover']").popover();
  }
};

document.addEventListener('DOMContentLoaded', ddpcr.init);