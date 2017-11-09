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
  $(".rightSideContent").removeClass("hide_span");
  $("#characteristics").removeClass("hide_span");
  $(".showIt").addClass("nill");
  $(".showIt").removeClass("showIt");
  $(".show_span").addClass("hide_span");
  $(".show_span").removeClass("show_span");
  $('[id$=' + id + ']').removeClass("nill");
  $('[id$=' + id + ']').addClass('showIt');
  $('[id$=' + id + ']').removeClass("hide_span");
  $('[id$=' + id + ']').addClass('show_span');
}