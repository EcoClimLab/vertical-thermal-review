(function() {

    var __hs_cta_json = {"css":"a#cta_button_5981609_new {\n    -webkit-font-smoothing: antialiased !important;\ncursor: pointer !important;\n-moz-user-select: none !important;\n-webkit-user-select: none !important;\n-o-user-select: none !important;\nuser-select: none !important;\ndisplay: inline-block !important;\nfont-weight: normal !important;\ntext-align: center !important;\ntext-decoration: none !important;\n-moz-transition: all .4s ease !important;\n-webkit-transition: all .4s ease !important;\n-o-transition: all .4s ease !important;\nbackground: rgb(53,116,227) !important;\nborder-radius: 6px !important;\nborder-width: 0px !important;\ncolor: rgb(255,255,255) !important;\nfont-family: sans-serif !important;\nheight: auto !important;\ntransition: all .4s ease !important;\npadding: 6px 18px !important;\ntext-shadow: none !important;\nwidth: auto !important;\nfont-size: 24px !important;\nline-height: 1.5em !important;\n\n    \n  }\n\na#cta_button_5981609_new:hover {\nbackground: rgb(58,127,249) !important;\ncolor: rgb(255,255,255) !important;\n}\n\na#cta_button_5981609_new:active, #cta_button_5981609_new:active:hover {\nbackground: rgb(42,92,181) !important;\ncolor: rgb(244,244,244) !important;\n}\n\n","css_links":[],"image_html":"<a id=\"cta_button_5981609_c3b67c76-5664-432c-abf2-0900ffcd0cae\" class=\"cta_button\" href=\"https://donate.mongabay.org/cs/c/?cta_guid=c3b67c76-5664-432c-abf2-0900ffcd0cae&placement_guid=fb0aab00-e110-4bff-ad96-0e49b955b758&portal_id=5981609&canon=https%3A%2F%2Fnews.mongabay.com%2F2019%2F04%2Fshade-or-sun-forest-structure-affects-tree-responses-to-amazon-drought%2F&redirect_url=APefjpENJptfupzmLxORxGCLmlMctndX-VDCeZRl_cDSamnRlajHAJKVLaF8gJMYAO8nNdI1ZV0kuSt0AkuMXDfCHdb9_4D5RYjlMDsLFsV93TUqsZW3mTorodxzS3jPn1gDWMoM6zG4&click=f9fc0848-0aa6-4e2f-b64d-b4b10ac4038e&hsutk=c7a0000017e81ac41f12017403b484de&signature=AAH58kH-oFbQqADsvUy2KtnGySvN3Z6gXg\"  target=\"_blank\"  cta_dest_link=\"https://form.jotform.com/202117087301140\"><img id=\"hs-cta-img-fb0aab00-e110-4bff-ad96-0e49b955b758\" class=\"hs-cta-img \" style=\"border-width: 0px; /*hs-extra-styles*/\" mce_noresize=\"1\" alt=\"New call-to-action\" height=\"600\" width=\"300\" src=\"https://f.hubspotusercontent00.net/hubfs/5981609/hub_generated/resized/b005ba1f-6127-4700-bf76-7235a36eb01c.jpeg\" /></a>","is_image":true,"placement_element_class":"hs-cta-fb0aab00-e110-4bff-ad96-0e49b955b758","raw_html":"<a id=\"cta_button_5981609_c3b67c76-5664-432c-abf2-0900ffcd0cae\" class=\"cta_button \" href=\"https://donate.mongabay.org/cs/c/?cta_guid=c3b67c76-5664-432c-abf2-0900ffcd0cae&placement_guid=fb0aab00-e110-4bff-ad96-0e49b955b758&portal_id=5981609&canon=https%3A%2F%2Fnews.mongabay.com%2F2019%2F04%2Fshade-or-sun-forest-structure-affects-tree-responses-to-amazon-drought%2F&redirect_url=APefjpENJptfupzmLxORxGCLmlMctndX-VDCeZRl_cDSamnRlajHAJKVLaF8gJMYAO8nNdI1ZV0kuSt0AkuMXDfCHdb9_4D5RYjlMDsLFsV93TUqsZW3mTorodxzS3jPn1gDWMoM6zG4&click=f9fc0848-0aa6-4e2f-b64d-b4b10ac4038e&hsutk=c7a0000017e81ac41f12017403b484de&signature=AAH58kH-oFbQqADsvUy2KtnGySvN3Z6gXg\" target=\"_blank\" style=\"/*hs-extra-styles*/\" cta_dest_link=\"https://form.jotform.com/202117087301140\" title=\"New call-to-action\">New call-to-action</a>"};
    var __hs_cta = {};

    __hs_cta.drop = function() {
        var is_legacy = document.getElementById('hs-cta-ie-element') && true || false,
            html = __hs_cta_json.image_html,
            tags = __hs_cta.getTags(),
            is_image = __hs_cta_json.is_image,
            parent, _style;

        if (!is_legacy && !is_image) {
            parent = (document.getElementsByTagName("head")[0]||document.getElementsByTagName("body")[0]);

            _style = document.createElement('style');
            parent.insertBefore(_style, parent.childNodes[0]);
            try {
                default_css = ".hs-cta-wrapper p, .hs-cta-wrapper div { margin: 0; padding: 0; }";
                cta_css = default_css + " " + __hs_cta_json.css;
                // http://blog.coderlab.us/2005/09/22/using-the-innertext-property-with-firefox/
                _style[document.all ? 'innerText' : 'textContent'] = cta_css;
            } catch (e) {
                // addressing an ie9 issue
                _style.styleSheet.cssText = cta_css;
            }
            
            html = __hs_cta_json.raw_html;

        }
         

        for (var i = 0; i < tags.length; ++i) {

            var tag = tags[i];
            var images = tag.getElementsByTagName('img');
            var cssText = "";
            var align = "";
            for (var j = 0; j < images.length; j++) {
                align = images[j].align;
                images[j].style.border = '';
                images[j].style.display = '';
                cssText = images[j].style.cssText;
            }

            if (align == "right") {
                tag.style.display = "block";
                tag.style.textAlign = "right";
            } else if (align == "middle") {
                tag.style.display = "block";
                tag.style.textAlign = "center";
            }

            tag.innerHTML = html.replace('/*hs-extra-styles*/', cssText);
            tag.style.visibility = 'visible';
            tag.setAttribute('data-hs-drop', 'true');
            window.hbspt && hbspt.cta && hbspt.cta.afterLoad && hbspt.cta.afterLoad('fb0aab00-e110-4bff-ad96-0e49b955b758');
        }

        return tags;
    };

    __hs_cta.getTags = function() {
        var allTags, check, i, divTags, spanTags,
            tags = [];
            if (document.getElementsByClassName) {
                allTags = document.getElementsByClassName(__hs_cta_json.placement_element_class);
                check = function(ele) {
                    return (ele.nodeName == 'DIV' || ele.nodeName == 'SPAN') && (!ele.getAttribute('data-hs-drop'));
                };
            } else {
                allTags = [];
                divTags = document.getElementsByTagName("div");
                spanTags = document.getElementsByTagName("span");
                for (i = 0; i < spanTags.length; i++) {
                    allTags.push(spanTags[i]);
                }
                for (i = 0; i < divTags.length; i++) {
                    allTags.push(divTags[i]);
                }

                check = function(ele) {
                    return (ele.className.indexOf(__hs_cta_json.placement_element_class) > -1) && (!ele.getAttribute('data-hs-drop'));
                };
            }
            for (i = 0; i < allTags.length; ++i) {
                if (check(allTags[i])) {
                    tags.push(allTags[i]);
                }
            }
        return tags;
    };

    // need to do a slight timeout so IE has time to react
    setTimeout(__hs_cta.drop, 10);
    
    window._hsq = window._hsq || [];
    window._hsq.push(['trackCtaView', 'fb0aab00-e110-4bff-ad96-0e49b955b758', 'c3b67c76-5664-432c-abf2-0900ffcd0cae']);
    

}());
