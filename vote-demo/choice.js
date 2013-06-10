
function useSelection(menu) {
  var i = menu.selectedIndex;
  if (i > 0) {
    menu.form.food.value=menu.options[i].value;
  }
}
