{ changeColor, config, invertedColor, secondaryColor, windowColor }:

''
/* Always show tab close button on hover */

.tabbrowser-tab:not([pinned]):hover .tab-close-button{
    display:-moz-box !important;
}

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
  width: 20px !important;
  height: 20px !important;
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
