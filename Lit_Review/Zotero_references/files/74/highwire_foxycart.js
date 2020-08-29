// $Id$

(function ($) {
  // Check to see if an add-to-cart cookie exists and if it does add the item to the cart
  Drupal.behaviors.HighWireFoxycart = {
    attach: function(context, settings) {
      $(document, context).one('ready', function () {
        if (Drupal.settings.user_uid !== 0) {
          FC.onLoad = function () {
            FC.client.on('ready.done', function () {
              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;

              // We need to login the user to savesession.
              var user_savesession = getCookie('highwire-foxycart-savesession');
              var savesession_logged_in = getCookie('highwire-foxycart-logged-in');

              if (user_savesession && !savesession_logged_in) {
                $.ajax({
                  url: "/highwire/foxycart/savesession",
                  type: "POST",
                  dataType: 'json',
                })
                .done(function(data) {
                  // Delete the cookie so we don't log the user to savesession
                  // more than once.
                  delCookie('highwire-foxycart-savesession');

                  // If the user navigates to a site that shares the same cookie
                  // domain or top level domain, the user will trigger the Drupal
                  // login process to fire again.  This will result in SaveSession
                  // being called whenever the user does this. See: JCORE-2932
                  var fc_cookie_domain = Drupal.settings.hw_fc_cookie_domain;
                  var cookie_string = "highwire-foxycart-logged-in=true";
                  if (fc_cookie_domain !== '') {
                    cookie_string += ";domain=" + fc_cookie_domain + ";path=/";
                  }
                  document.cookie = cookie_string;
                });
              }

              // Get the cookie values
              var reloadCart = getCookie('highwire-foxycart-reload');
              var addToCart = getCookie('highwire-foxycart-add');

              // Delete the cookies
              delCookie('highwire-foxycart-add');
              delCookie('highwire-foxycart-reload');

              // Remove duplicate cart items.
              // When SSO users are logged into multiple sites, multiple calls
              // are made to SaveSession.  This adds the cart to itself, and
              // items are duplicated.  See: JCORE-2932
              deduplicateItemsInCart(fcsid);

              if (addToCart == "") {
                return
              }

              // Keep track of the apaths in the cart.
              var apathsInCart = [];
              FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&output=json&fcsid=' + fcsid).done(function(cartItems) {
                if (cartItems.items.length > 0) {
                  cartItems.items.forEach(function(cartItem) {
                    itemApath = HighwireFoxycartGetOption(cartItem.options, 'apath');
                    apathsInCart.push(itemApath);
                  });
                }
              });


              // Get the parts
              var parts = decodeURIComponent(addToCart).split('|||');

              // Get the data for the item we are adding to the cart.
              $.get('/highwire/foxycart/getattr/' + parts[1], function(data) {
                var query = [];

                for (var nid in data) {
                  // Check to see if the apath of the item being added is already
                  // in the cart.
                  if (($.inArray(data[nid].attributes.apath, apathsInCart)) == -1) {
                    query.push($.param(data[nid].hmac));
                  }
                }

                // Add the item to the cart
                FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?' + query.join('&') + '&fcsid=' + fcsid).done(function(dataJSON) {

                  // Update Add To Cart links.
                  updateAddToCartLinks(fcsid);

                  // update the MY Cart link.
                  updateCartLink(fcsid);

                  // Show the side cart when adding item.
                  FC.sidecart.show();
                });
              });
            });

            // Run link update function on page load.
            FC.client.on('ready.done', function (params, next) {
              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;
              updateAddToCartLinks(fcsid);

              // update the MY Cart link.
              updateCartLink(fcsid);
  
              next();
            });

            // Removing items from the cart/sidecart.
            FC.client.on('cart-item-remove', function(params) {
              
              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;
              // Update the Add To Cart links.
              updateAddToCartLinks(fcsid);
              // update any items on the page that were zero'd out in the cart
              updateRemovedItems(fcsid, params);
            });

            FC.client.on('cart-item-remove.done', function(params, next) {
              // Let Foxycart do it's processessing.
              next();

              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;
              // update the My Cart link.
              updateCartLink(fcsid);
              // Update the Add To Cart links.
              updateAddToCartLinks(fcsid);
              // update any items on the page that were zero'd out in the cart
              updateRemovedItems(fcsid, params);
            });

            // Update "Add To Cart" links when the cart changes.
            FC.client.on('cart-update.done', function(params, next) {
              // Let Foxycart process first.
              next();

              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;
              // Update the Add To Cart links.
              updateAddToCartLinks(fcsid);
              // update the My Cart link.
              updateCartLink(fcsid);
              // update any items on the page that were zero'd out in the cart
              updateRemovedItems(fcsid, params);
            });

            // Update links when quantity in the cart has changed to 0 as in zero.
            FC.client.on('cart-item-quantity-update', function(params) {

              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;

              // update any items on the page that were zero'd out in the cart
              // to get the page to live-update seems we need this func called 
              // both here and on .done (below)
              updateRemovedItems(fcsid, params);

            });

            // Update links when quantity in the cart has changed to 0 as in zero.
            FC.client.on('cart-item-quantity-update.done', function(params, next) {

              // Let Foxycart process first.
              next();

              // Get the FoxyCart user session id.
              var fcsid = FC.json.session_id;
              // Update the Add To Cart links.
              updateAddToCartLinks(fcsid);
              // update the My Cart link.
              updateCartLink(fcsid);
              // update any items on the page that were zero'd out in the cart
              updateRemovedItems(fcsid, params);
            });
          }
        }

        // JCORE-3156 Change "item in cart" links back to "add to cart"
        function updateRemovedItems(fcsid, params) {
          // removeId is the foxycart item id.
          var removeId = params.id;

          FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&output=json&fcsid=' + fcsid).done(function(removeCartJSON) {
            var cartItems = removeCartJSON.items;

            // Get the nid for the TOC.
            // Note this is for the page we are on
            var tocId = $('form#highwire-foxycart-add-to-cart-toc-form input[name="tocnid"]').val();

            cartItems.forEach(function(cartItem) {
              // Find the item in the cart that was returned from Foxycart event.
              if (cartItem.id == removeId) {

                // We need the apath and nid of the cart item to update the link.
                var removeApath = HighwireFoxycartGetOption(cartItem.options, 'Apath');
                var removeNid = HighwireFoxycartGetOption(cartItem.options, 'Nid');

                // The TOC was removed from the cart
                if (tocId == removeNid) {
                  // just refresh the page
                  location.reload();
                }

                // Change link from "Item in cart", to "Add to Cart ($price)"
                if (removeApath !== null && removeNid !== null) {

                  // Add to cart ctools content type.
                  if (Drupal.settings.highwire_foxycart_add_to_cart_link) {
                    var item = {
                      text : Drupal.settings.highwire_foxycart_add_to_cart_link.text,
                      type : Drupal.settings.highwire_foxycart_add_to_cart_link.type,
                      apath : Drupal.settings.highwire_foxycart_add_to_cart_link.apath,
                    };
                  }

                  // Toc/Citation link.
                  if (Drupal.settings.HighWireFoxycart) {
                    var item = {
                      text : Drupal.settings.HighWireFoxycart.link_text,
                      icon : Drupal.settings.HighWireFoxycart.link_icon,
                      apath : removeApath,
                      nid : removeNid,
                    };
                  }

                  $.ajax({
                    url: "/highwire/foxycart/add_to_cart_ajax_callback",
                    type: "POST",
                    dataType: 'json',
                    data: {'item' : item},
                  })
                  .done(function(data) {
                    $('.highwire-foxycart-in-cart[data-apath="' + removeApath + '"]', context).replaceWith(data);
                  });
                }
              }
            });
          });  
        }

        // Check to see if any items in the cart have an "add to cart" link on the
        // page and modify the add-to-cart link if needed
        function updateAddToCartLinks(fcsid) {
          var numitems = 0;

          // Get items in the cart, JSON.
          FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&output=json&fcsid=' + fcsid).done(function(dataJSON) {

            // Get the number of items in the cart
            numitems = dataJSON.items.length;

            var nids = []
            if (numitems > 0) {
              // Add nids of the items in the cart to the array.
              dataJSON.items.forEach(function(product) {
                nids.push(HighwireFoxycartGetOption(product.options, 'Nid'));
              });

              // Check if any links have the parent-id of an item (issue) in the cart.
              // The article also needs to be marked as "in cart".
              // If the article is free, do not add "Item in Cart" link.
              articleNids = '';
              for (var i = 0; i < nids.length; i++) {
                $('.highwire-foxycart-add-to-cart-ahah[data-parent-id="' + nids[i] + '"]', context).each(function() {
                  var articleNid = $(this, context).parents('.highwire-article-citation').attr('data-node-nid');
                  if (articleNid != null) {
                    if (articleNids == '') {
                      articleNids = articleNid;
                    }
                    else {
                      articleNids += ',' + articleNid;
                    }
                  }
                });

                if (articleNids != '') {
                  // We need to make a request to get the pricing of the articles
                  $.get('/highwire/foxycart/getattr/' + articleNids, function(data) {
                    for (var nid in data) {
                      var attributes = data[nid].attributes;
                      if (attributes.price != 0) {
                        if (attributes.apath != null) {
                          $(".highwire-foxycart-add-to-cart-ahah[data-apath='" + attributes.apath + "'] span.highwire-foxycart-add-to-cart", context)
                          .replaceWith("<a href='https://" + Drupal.settings.foxycart_subdomain + "/cart?cart=view&fcsid=" + fcsid + "' class='highwire-foxycart-in-cart' data-apath='" + attributes.apath + "'>Item in cart</a>");
                        }
                      }
                    }
                  });
                }
              }

              // Change links for single items/articles by apath.
              dataJSON.items.forEach(function(product) {
                var apath = null;
                apath = HighwireFoxycartGetOption(product.options, 'Apath');
                // this correctly returns what is in the cart
                // Swap "Add to cart" with "Item in cart".
                if (apath !== null) {
                  $(".highwire-foxycart-add-to-cart-ahah[data-apath='" + apath + "'] span.highwire-foxycart-add-to-cart", context)
                    .replaceWith("<a href='https://"+Drupal.settings.foxycart_subdomain + "/cart?cart=view&fcsid=" + fcsid + "' class='highwire-foxycart-in-cart' data-apath='" + apath + "'>Item in cart</a>");
                }
              });

              // Add To Cart Toc
              $('form#highwire-foxycart-add-to-cart-toc-form').each(function() {
                var $form = $(this);
                var tocnid = $form.find("input[name='tocnid']").val();

                  dataJSON.items.forEach(function(product) {
                  var nid = null;
                  nid = HighwireFoxycartGetOption(product.options, 'Nid');
                  if (nid !== null) {
                    // Hide the form if the TOC is already in the cart.
                    if (nid == tocnid) {
                      $form.find('div#edit-metaselection.form-radios').hide();
                      $form.find('.highwire-foxycart-add-to-cart-toc-components').hide();
                      $form.find('input[type=submit]').hide();
                      $form.find('#edit-metaselection-toc').hide();
                      $form.find('button[type=submit]').hide();

                      // Check if the link has been added to the page to
                      // prevent duplicates.
                      if (!$("a.highwire-foxycart-in-cart[data-tocnid='" + tocnid + "']").length) {
                        $form.append("<a href='https://" + Drupal.settings.foxycart_subdomain + "/cart?cart=view&fcsid=" + fcsid + "' class='highwire-foxycart-in-cart' data-tocnid='" + tocnid + "'>Item in cart</a>");
                      }
                    }
                    // Disable form components for items in the cart.
                    $form.find("input[type='checkbox'][value='" + nid + "']").attr("disabled", true);
                  }
                });
              });
            }
          });
        }

        function updateCartLink(fcsid) {
          // Change the url of the cart, and add class to identify links.
          $('.menu a[href="/cart"], .menu-path-cart a[href="/cart"]', context).each(function() {
            $(this, context).addClass('highwire-foxycart-menu-link');
            $(this, context).attr('href', 'https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&fcsid=' + fcsid);
            // We need to initiate the side-cart and override conflicts
            // with menu modules (nice-menus, superfish).
            $(this, context).click(function() {
              FC.sidecart.show();
              return false;
            });
          });

          // Get the cart - JSON
          FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&output=json&fcsid=' + fcsid).done(function(dataJSON) {
            var numitems = 0

            // Get the number of items in the cart
            if (dataJSON.items.length !== 'undefined') {
              numitems = dataJSON.items.length;
            }

            $('.highwire-foxycart-menu-link', context).each(function() {
              $(this, context).children('.highwire-foxycart-num-items').remove();
              if (numitems > 0) {
                if (numitems === 1) {
                  $(this, context).append('<span class="highwire-foxycart-num-items"> (' + numitems + ' item)</span>');
                }
                else {
                  $(this, context).append('<span class="highwire-foxycart-num-items"> (' + numitems + ' items)</span>');
                }
                $(this).parent().show();
              }
              else if (Drupal.settings.foxycart_always_show_cart_link == 0) {
                $(this).parent().hide();
              }
            });
          });
        }

        function deduplicateItemsInCart(fcsid) {
          // When logging the user in, get the items in the users cart.
          FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=view&output=json&fcsid=' + fcsid).done(function(cartItems) {

            // Get the number of items in the cart
            var numitems = cartItems.items.length;
            // Keep track of the apaths in the cart.
            var apathsInCart = [];
            // Parameters to send in the foxycart request.
            var fcQueryParams = '';
            // Unique id's to identify items in fcQueryParams.
            // See: https://docs.foxycart.com/v/2.0/json
            var fcItemIndex = 1;

            if (numitems > 0) {
              // Add nids of the items in the cart to the array.
              cartItems.items.forEach(function(cartItem) {

                itemApath = HighwireFoxycartGetOption(cartItem.options, 'Apath');
                itemId = cartItem.id;
                if (($.inArray(itemApath, apathsInCart)) !== -1) {

                  // Build the parameters to send to FC to remove
                  // duplicate items from the cart.
                   fcQueryParams += '&' + fcItemIndex + ':id=' + itemId + '&' + fcItemIndex + ':quantity=0';

                  // Increment the item index.
                  fcItemIndex++;
                }
                else {
                  // Keep track of apaths of items in the cart.
                  apathsInCart.push(itemApath);
                }
              });

              // Send request to remove items from the cart.
              FC.client.request('https://' + Drupal.settings.foxycart_subdomain + '/cart?cart=update' + fcQueryParams).done(function(removedItems) {
                // After removing the items from the cart, update the
                // links on the page.
                updateAddToCartLinks(fcsid);
                updateCartLink(fcsid);
              });
            }
          });
        }
      });
    }
  };

  // Construct an add-to-cart link with AHAH
  Drupal.behaviors.HighWireFoxycartAddToCartLink = {
    attach: function(context, settings) {
      var foxyId = 1;
      var items = [];
      $('.highwire-foxycart-add-to-cart-ahah', context).each(function() {
        // We need to add data-foxy-id here to avoid citation caching.  If there
        // are duplicate data-foxy-id values on the same page, there is potential
        // to add the wrong item to the cart. (See: DRQUEST-735).
        $(this).attr('data-foxy-id', foxyId);
        foxyId++;

        var itemdata = {
          id: $(this).data('foxy-id'),
          apath: $(this).data('apath'),
          text: $(this).data('text'),
          type: $(this).data('type'),
          font_icon: $(this).data('font-icon')
        };
        items.push(itemdata);
      });
      if (items.length > 0) {
        $.ajax({
          url: "/highwire/foxycart/add_to_cart_ahah",
          type: "POST",
          dataType: 'json',
          data: {'items' : items},
        })
        .done(function(data) {
          for(var id in data) {
            $(".highwire-foxycart-add-to-cart-ahah[data-foxy-id='" + id + "']").html(data[id]);
          }
        });
      }
    }
  };

  // Keep the add_to_cart_toc form in sync and sane when selecting different options
  Drupal.behaviors.HighWireFoxycartAddToCartTOC = {
    attach: function(context, settings) {
      $('.highwire-foxycart-add-to-cart-toc-components[data-always-on=1]', context).each(function() {
        self = this;
        // If the user selects a component, change the metaselector accordingly
        $(self).find('input').click(function() {
          if ($(self).find('input:checked').length == 0) {
            $(self).closest('form').find('.form-item-metaselection input[value=toc]').prop('checked',true);
          }
          else {
            $(self).closest('form').find('.form-item-metaselection input[value=components]').prop('checked',true);
          }
        });
        // If the user selects the full issue TOC in the metaselector, clear the component selections
        $(self).closest('form').find('.form-item-metaselection input[value=toc]').click(function() {
          $(self).find('input').prop('checked',false);
        })
      });

      // Only allow 5 items to be submitted at a time
      $('form#highwire-foxycart-add-to-cart-toc-form', context).submit(function(e) {
        if ($(this).find('.highwire-foxycart-add-to-cart-toc-components input:checked').length > 5) {
          alert('Cannot add more than 5 items to your cart at once. Please select a smaller number of items.');
          e.preventDefault();
          return false;
        }
      });

      // Remove the checkbox for items that are free
      $('form#highwire-foxycart-add-to-cart-toc-form #edit-components-select .form-type-checkbox').each(function() {
        if ($(this).find('span.catalog-price-free').length) {
          $(this).find('input[type=checkbox]').attr('disabled', true)
        }
      });
    }
  };

  var getCookie = function(cname) {
      var name = cname + "=";
      var ca = document.cookie.split(';');
      for (var i=0; i < ca.length; i++) {
          var c = ca[i].trim();
          if (c.indexOf(name) == 0) return c.substring(name.length, c.length);
      }
      return "";
  }

  var delCookie = function(cname) {
    document.cookie = cname + "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/;";
  }

  function HighwireFoxycartGetOption(options, name) {
    var value = null
    options.forEach(function(option) {
      if (option.name === name) {
        value = option.value;
      }

      // Fix mismatch in capitilization of option names from Catalog service.
      if (value === null) {
        name = name.toLowerCase();
        if (option.name === name) {
          value = option.value;
        }
      }
    });

    return value;
  }

})(jQuery);
