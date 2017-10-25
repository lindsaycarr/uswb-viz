function clicklink(url, event){
 if( /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
   event.stopPropagation();
  }else{
    ga('send', 'event', 'outbound', 'click', url);
    window.open(url, '_blank');
  }
}

 
function setEmphasis(id) {
  $(".emphasis").removeClass("emphasis");
  hovertext(" ");
  $('[id$=' + id + ']').addClass('emphasis');
}

function setShow(id) {
  $(".showIt").addClass("nill");
  $(".showIt").removeClass("showIt");
  $('[id$=' + id + ']').removeClass("nill");
  $('[id$=' + id + ']').addClass('showIt');
}