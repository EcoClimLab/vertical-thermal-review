(function(n){Drupal.behaviors.whatsNewIn={attach:function(t,e){n('#mini-panel-whats_new_in .wni-dropdown',t).each(function(){n(this).addClass('js-processed');var e=n(this).children('.pane-title').first(),t=n('<button />').addClass('pane-title wni-button button-alt');t.text(e.text());e.replaceWith(t);n(this).children('.pane-content').hide()});n('.wni-button',t).click(function(){n(this).parent().siblings('.wni-dropdown').each(function(){n(this).children('.pane-title').removeClass('open');n(this).children('.pane-content').hide()});n(this).siblings('.pane-content').toggle();n(this).toggleClass('open')});window.onclick=function(t){if(!t.target.matches('.wni-button')){n('.wni-button').removeClass('open').siblings('.pane-content').hide()}}}}})(jQuery);