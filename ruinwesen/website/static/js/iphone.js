addEventListener("load", hideURLBar, false);
function hideURLBar() {
  setTimeout(function() { window.scrollTo(0, 1); }, 100);
}
