{ changeColor, config }:
with config.theme.color;

(if changeColor == false then "" else ''
:root {

    /*---+---+---+---+---+---+---+
     | C | O | L | O | U | R | S |
     +---+---+---+---+---+---+---*/


    /* Theme Colour */
     --window-colour:               ${background};
     --secondary-colour:            ${background-alt1};
     --inverted-colour:             ${foreground};


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
    --uc-border-radius: 0;

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
''
/* Source file https://github.com/MrOtherGuy/firefox-csshacks/tree/master/chrome/navbar_tabs_oneliner.css made available under Mozilla Public License v. 2.0
See the above repository for updates as well as full license text. */

/* Make tabs and navbar appear side-by-side tabs on right */

/* Use page_action_buttons_on_hover.css to hide page-action-buttons to save more space for the address */

/*
urlbar_popup_full_width.css is VERY MUCH recommended for Firefox 71+ because of new urlbar popup
*/

:root[uidensity="compact"]{
  --tab-block-margin: 2px !important;
}

/* Modify these to change relative widths or default height */
#navigator-toolbox{
  --uc-navigationbar-width: 40vw;
  --uc-toolbar-height: 40px;
  --uc-urlbar-min-width: 50vw; /* minimum width for opened urlbar */
}

#scrollbutton-up,
#scrollbutton-down{ border-block-width: 2px !important; }

/* Override for other densities */
:root[uidensity="compact"] #navigator-toolbox{ --uc-toolbar-height: 34px; }
:root[uidensity="touch"] #navigator-toolbox{ --uc-toolbar-height: 44px; }

/* prevent urlbar overflow on narrow windows */
/* Dependent on how many items are in navigation toolbar ADJUST AS NEEDED */
@media screen and (max-width: 1500px){
  #urlbar-container{ min-width:unset !important }
}

#TabsToolbar{
  margin-left: var(--uc-navigationbar-width);
  --tabs-navbar-shadow-size: 0px;
}
#tabbrowser-tabs{
  --tab-min-height: calc(var(--uc-toolbar-height) - 2 * var(--tab-block-margin,0px)) !important;
}

/* This isn't useful when tabs start in the middle of the window */

.titlebar-spacer[type="pre-tabs"]{ display: none }

#navigator-toolbox > #nav-bar{
  margin-right:calc(100vw - var(--uc-navigationbar-width));
  margin-top: calc(0px - var(--uc-toolbar-height));
}

/* Window drag space  */
:root[tabsintitlebar="true"] #nav-bar{ padding-left: 24px !important }

/* Rules specific to window controls on right layout */
@supports -moz-bool-pref("layout.css.osx-font-smoothing.enabled"){
  .titlebar-buttonbox-container{ position: fixed; display: block; left: 0px; z-index: 3; }
  :root[tabsintitlebar="true"] #nav-bar{ padding-left: 96px !important; padding-right: 0px !important; }
}

/* 1px margin on touch density causes tabs to be too high */
.tab-close-button{ margin-top: 0 !important }

/* Make opened urlbar overlay the toolbar */
#urlbar[open]:focus-within{ min-width: var(--uc-urlbar-min-width,none) !important; }

/* Hide dropdown placeholder */
#urlbar-container:not(:hover) .urlbar-history-dropmarker{ margin-inline-start: -28px; }

/* Fix customization view */
#customization-panelWrapper > .panel-arrowbox > .panel-arrow{ margin-inline-end: initial !important; }




/* Source file https://github.com/MrOtherGuy/firefox-csshacks/tree/master/chrome/page_action_buttons_on_hover.css made available under Mozilla Public License v. 2.0
See the above repository for updates as well as full license text. */

/* Page action-buttons "slide in" when cursor is on top of them and don't reserve space when not used */

#page-action-buttons{ padding-inline-start: 8px }

.urlbar-page-action{
  margin-inline-end: calc(-16px - 2 * var(--urlbar-icon-padding) );
  opacity: 0;
  transition: margin-inline-end 100ms linear, opacity 200ms linear;
}

#page-action-buttons:hover > .urlbar-page-action,
.urlbar-page-action[open],
.urlbar-page-action[open] ~ .urlbar-page-action{
  opacity: 1;
  margin-inline-end: 0px !important;
}


/* Source file https://github.com/MrOtherGuy/firefox-csshacks/tree/master/chrome/urlbar_popup_full_width.css made available under Mozilla Public License v. 2.0
See the above repository for updates as well as full license text. */

/* Make urlbar popup appear full size like it did before urlbar re-design */

/* Temporary dummy variables, can probably be removed when Fx92 releases */
:root{
  --toolbar-field-background-color: var(--toolbar-field-non-lwt-bgcolor);
  --toolbar-field-focus-background-color: var(--lwt-toolbar-field-focus,Field);
}
:root:-moz-lwtheme{
  --toolbar-field-background-color: var(--lwt-toolbar-field-background-color);
}

.urlbarView-row-inner{
   /* This sets how far the dropdown-items are from window edge */
  padding-inline-start: 6px !important;
}

#urlbar-container,
#urlbar{
  position: static !important;
  display: -moz-box !important;
}
#urlbar{
  height: auto !important;
  width: auto !important;
  box-shadow: inset 0 0 0 1px var(--toolbar-field-border-color, hsla(240,5%,5%,.25));
  background-color: var(--toolbar-field-background-color, hsla(0,0%,100%,.8));
  border-radius: var(--toolbarbutton-border-radius);
  --uc-urlbar-min-width: none; /* navbar_tabs_oneliner.css compatibility */
}

#urlbar[focused]{ box-shadow: inset 0 0 0 1px var(--toolbar-field-focus-border-color, highlight); }

.urlbarView{
  position: absolute !important;
  margin: 0 !important;
  left: 0 !important;
  width: 100vw !important;
  border-width: 1px 0;
  top: var(--urlbar-toolbar-height);
  background-color: var(--toolbar-field-focus-background-color, inherit);
  z-index: 4;
  box-shadow: 0 1px 4px rgba(0,0,0,.05);
}

#urlbar > #urlbar-input-container{ padding: 0px !important; width: auto !important; height: auto !important; }
#urlbar > #urlbar-background{ display: none !important; }

/* This may seem pretty weird, but it gets around an issue where the height of urlbar may suddenly change when one starts typing into it */
/* If you are otherwise modifying the urlbar height then you might need to modify the height of this too */
#urlbar > #urlbar-input-container::before{ content: ""; display: -moz-box; height: 24px; }
''
