import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map as M

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt"). You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["web","fplane","dev","qt","media"] ++ map show [6..9]

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

{-
myManageHook = composeAll
    [ className =? "Firefox" --> doShift "web"
    , className =? "Google-chrome" --> doShift "web"
    , className =? "Sublime_text" --> doShift "dev"
    , className =? "sublime_text" --> doShift "dev"
    , className =? "QtCreator" --> doShift "qt"
    , title     =? "QtCreator" --> doIgnore
    , title     =? "Google - Bookmarks - Mozilla Firefox" --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator" --> doFloat
    , className =? "Steam" --> doFloat
    -- , className =? "Gimp" --> doFloat
    , resource  =? "gpicview" --> doFloat
    , className =? "MPlayer" --> doFloat
    -- , className =? "VirtualBox" --> doShift "4:vm"
    , className =? "Xchat" --> doShift "5:media"
    , className =? "stalonetray" --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]
-}

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
-- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
-- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q. Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    setWMName "LG3D"
    -- spawn "/home/sandeepd/bin/startup"
    {-
    spawn "firefox"
    spawn "sublime_text"
    spawn "gnome-terminal"
    spawn "freeplane"
    -}


main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.rc"
    xmonad $ defaultConfig
        { manageHook    = manageDocks <+> manageHook defaultConfig
        , layoutHook    = avoidStruts  $  layoutHook defaultConfig
        , startupHook   = myStartupHook
        , logHook       = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor "green" "" . shorten 50
                            }
        , modMask       = myModMask
        , workspaces    = myWorkspaces
        , terminal      = "gnome-terminal"
        } `additionalKeys`
        [ --((myModMask, xK_l),        spawn "xscreensaver-command -lock")
          ((myModMask, xK_Print),    spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print),            spawn "scrot")
        , ((myModMask, xK_p),        spawn "exec $(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')")
        , ((myModMask, xK_c),        kill) -- Close focused window
        , ((0 ,                      xF86XK_AudioLowerVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 2-")
        , ((0 ,                      xF86XK_AudioRaiseVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 2+")
        , ((0 ,                      xF86XK_AudioMute), spawn "amixer set Master toggle && amixer set Headphone toggle")
        ]
