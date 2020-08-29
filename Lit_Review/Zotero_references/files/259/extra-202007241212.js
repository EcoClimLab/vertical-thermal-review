function myModifyElement(mySelector, myFunction, myInterval) {
	var interval = setInterval(function() {
		if ($(mySelector).length > 0) {
			myFunction();
			clearInterval(interval);
		}
	}, myInterval);
}
myModifyElement(".trendmd-widget-header__heading", function() {
	//$(".trendmd-widget-header__heading").text("FROM AROUND THE WEB");
	$(".trendmd-widget-settings__link").removeAttr("title");
	$(".trendmd-widget-cookie-notification__info a").remove();
}, 50);
myModifyElement(".spx-sd-adlabel-wrapper", function() {
	$('.spx-sd-adlabel').attr('style', 'color: #595959 !important');
}, 50);

