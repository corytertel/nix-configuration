{ changeColor, config, invertedColor, secondaryColor, windowColor }:

(if changeColor == false then "" else ''
:root {

    /*---+---+---+---+---+---+---+
     | C | O | L | O | U | R | S |
     +---+---+---+---+---+---+---*/


    /* Theme Colour */
     --window-colour:               ${windowColor};
     --secondary-colour:            ${secondaryColor};
     --inverted-colour:             ${invertedColor};


    /* Containter Tab Colours */
    --uc-identity-color-blue:      #7ED6DF;
    --uc-identity-color-turquoise: #55E6C1;
    --uc-identity-color-green:     #B8E994;
    --uc-identity-color-yellow:    #F7D794;
    --uc-identity-color-orange:    #F19066;
    --uc-identity-color-red:       #FC5C65;
    --uc-identity-color-pink:      #F78FB3;
    --uc-identity-color-purple:    #786FA6;


    /* URL colour in URL bar suggestions */
    --urlbar-popup-url-color: var(--uc-identity-color-purple) !important;



    /*---+---+---+---+---+---+---+
     | V | I | S | U | A | L | S |
     +---+---+---+---+---+---+---*/

    /* global border radius */
    --uc-border-radius: 10;

    /* dynamic url bar width settings */
    --uc-urlbar-width: clamp(400px, 40vw, 1000px);

    /* dynamic tab width settings */
    --uc-active-tab-width:   clamp(200px, 20vw, 600px);
    --uc-inactive-tab-width: clamp(100px, 15vw, 400px);

    /* if active always shows the tab close button */
    --show-tab-close-button: none; /* DEFAULT: -moz-inline-box; */

    /* if active only shows the tab close button on hover*/
    --show-tab-close-button-hover: none; /* DEFAULT: -moz-inline-box; */

    /* adds left and right margin to the container-tabs indicator */
    --container-tabs-indicator-margin: 20px;

}


:root {

    --uc-theme-colour:                          var(--window-colour,    var(--toolbar-bgcolor));
    --uc-hover-colour:                          var(--secondary-colour, rgba(0, 0, 0, 0.2));
    --uc-inverted-colour:                       var(--inverted-colour,  var(--toolbar-color));

    --button-bgcolor:                           var(--uc-theme-colour)    !important;
    --button-hover-bgcolor:                     var(--uc-hover-colour)    !important;
    --button-active-bgcolor:                    var(--uc-hover-colour)    !important;

    --toolbarbutton-border-radius:              var(--uc-border-radius)   !important;

    --tab-border-radius:                        var(--uc-border-radius)   !important;
    --lwt-text-color:                           var(--uc-inverted-colour) !important;
    --lwt-tab-text:                             var(--uc-inverted-colour) !important;

    --arrowpanel-border-radius:                 var(--uc-border-radius)   !important;

    --tab-block-margin: 2px !important;

}



window,
#main-window,
#toolbar-menubar,
#TabsToolbar,
#PersonalToolbar,
#navigator-toolbox,
#sidebar-box,
#nav-bar {

    -moz-appearance: none !important;

    border: none !important;
    box-shadow: none !important;
    background: var(--uc-theme-colour) !important;

}




/* tab background */
.tabbrowser-tab
    > .tab-stack
    > .tab-background { background: var(--uc-theme-colour) !important; }


/* active tab background */
.tabbrowser-tab[selected]
    > .tab-stack
    > .tab-background { background: var(--uc-hover-colour) !important; }


/* adaptive tab width */
.tabbrowser-tab[selected][fadein]:not([pinned]) { max-width: var(--uc-active-tab-width) !important; }
.tabbrowser-tab[fadein]:not([selected]):not([pinned]) { max-width: var(--uc-inactive-tab-width) !important; }


/* remove border below whole nav */
#navigator-toolbox { border-bottom: none !important; }


#urlbar,
#urlbar * {

    outline: none !important;
    box-shadow: none !important;

}


#urlbar-background { border: var(--uc-hover-colour) !important; }


#urlbar[focused="true"]
    > #urlbar-background,
#urlbar:not([open])
    > #urlbar-background { background: transparent !important; }


#urlbar[open]
    > #urlbar-background { background: var(--uc-theme-colour) !important; }


.urlbarView-row:hover
    > .urlbarView-row-inner,
.urlbarView-row[selected]
    > .urlbarView-row-inner { background: var(--uc-hover-colour) !important; }


/* Container Tabs */
.identity-color-blue      { --identity-tab-color: var(--uc-identity-color-blue)      !important; --identity-icon-color: var(--uc-identity-color-blue)      !important; }
.identity-color-turquoise { --identity-tab-color: var(--uc-identity-color-turquoise) !important; --identity-icon-color: var(--uc-identity-color-turquoise) !important; }
.identity-color-green     { --identity-tab-color: var(--uc-identity-color-green)     !important; --identity-icon-color: var(--uc-identity-color-green)     !important; }
.identity-color-yellow    { --identity-tab-color: var(--uc-identity-color-yellow)    !important; --identity-icon-color: var(--uc-identity-color-yellow)    !important; }
.identity-color-orange    { --identity-tab-color: var(--uc-identity-color-orange)    !important; --identity-icon-color: var(--uc-identity-color-orange)    !important; }
.identity-color-red       { --identity-tab-color: var(--uc-identity-color-red)       !important; --identity-icon-color: var(--uc-identity-color-red)       !important; }
.identity-color-pink      { --identity-tab-color: var(--uc-identity-color-pink)      !important; --identity-icon-color: var(--uc-identity-color-pink)      !important; }
.identity-color-purple    { --identity-tab-color: var(--uc-identity-color-purple)    !important; --identity-icon-color: var(--uc-identity-color-purple)    !important; }
'')
+
(builtins.readFile ./userChrome.css)
+
''
/* replaces buttons with icon theme icons */

#back-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-previous.png) !important;
}

#forward-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-next.png) !important;
}

#reload-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/view-refresh.png) !important;
}

#stop-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/process-stop.png) !important;
}

#home-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-home.png) !important;
}

.tab-close-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/tab-close.png) !important;
  width: 22px !important;
  height: 22px !important;
  padding: 2px !important;
  margin: 0px !important;
}

#downloads-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/places/32x32/folder-downloads.png) !important;
}

#scrollbutton-up{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-previous.png) !important;
}

#scrollbutton-down{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-previous.png) !important;
}

#new-tab-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/list-add.png) !important;
}

#tabs-newtab-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/list-add.png) !important;
}

#alltabs-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/go-down.png) !important;
}

#PanelUI-menu-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/application-menu.png) !important;
}

#star-button{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/bookmark-new.png) !important;
}

#reader-mode-button-icon{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/actions/32x32/format-justify-left.png) !important;
}

#identity-icon{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/apps/32x32/gpg.png) !important;
}

#tracking-protection-icon{
  list-style-image: url(file://${config.theme.icons.package}/share/icons/${config.theme.icons.name}/status/32x32/security-high.png) !important;
}
''
