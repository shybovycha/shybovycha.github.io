$(function () {
  $("#next-post-countdown").countdown("2015/09/20 22:00:00", function(event) {
    $(this).text(event.strftime('%D days %H:%M:%S'));
  });

  $('.carousel').carousel({
    interval: 3000
  });
});
