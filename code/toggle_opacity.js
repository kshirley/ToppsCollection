$(document).ready(function(){
  
  $('.allbutton').click(function(){
    $('.dont-own').fadeTo(100, 1.0);
  });
  
  $('.ownbutton').click(function(){
    $('.dont-own').fadeTo(100, 0.2);
  });
});
