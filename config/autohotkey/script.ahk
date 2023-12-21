SetTitleMatchMode RegEx

cursorToEdge() {
  wingetpos,,,W,H, A
  h := h * 0.9
  w -= 2
  mousemove,w,h
}

nextWindow() {
  win := []
  WinGet, wins, List
  Loop, %wins% {
   WinGetTitle, ttitle, % winTitle := "ahk_id " wins%A_Index%
   WinGet, proc, ProcessName, %winTitle%
   WinGet, state, MinMax, %winTitle%
   SplitPath, proc,,,, proc
   WinGetClass, class, %winTitle%
   If (ttitle > "" && (ttitle != "Program Manager" || proc != "Explorer") && class != "#32770" && state != -1)
    If proc not in Rainmeter
     win.Push(wins%A_Index%)
  }
  WinActivate, % "ahk_id " win[win.Count()]
  cursorToEdge()
}

lastWindow() {
  Send !{Tab}
  cursorToEdge()
}

maximize() {
  WinGet, WinState, MinMax, A
  if (WinState = 1) {
    WinRestore, A
  }
  else {
    WinMaximize, A
  }
}

raiseEmacs() {
  WinActivateBottom ahk_class RAIL_WINDOW
  cursorToEdge()
}

raiseFirefox() {
  WinActivateBottom ahk_class MozillaWindowClass
  cursorToEdge()
}

raiseTerminal() {
  WinActivateBottom ahk_class CASCADIA_HOSTING_WINDOW_CLASS
  cursorToEdge()
}

F2::nextWindow()
F6::lastWindow()
F7::maximize()
F8::WinClose A
F9::raiseEmacs()
F10::raiseFirefox()
F11::raiseTerminal()
