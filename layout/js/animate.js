function setEmphasis(id) {
  $('[id$=' + id + ']').addClass('emphasis');
}

function clearEmphasis(){
  $(".emphasis").removeClass("emphasis");
  hovertext(" ");
}

function clicklink(url, event){
 if( /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
   event.stopPropagation();
  }else{
    ga('send', 'event', 'outbound', 'click', url);
    window.open(url, '_blank');
  }
}