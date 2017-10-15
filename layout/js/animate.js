function setEmphasis(id) {
  $('[id$=' + id + ']').addClass('emphasis');
}

function clearEmphasis(){
  $(".emphasis").removeClass("emphasis");
  hovertext(" ");
}
