-- Renjith Rajan's xmonad config
-- Based on:
--    David Beckingsale's xmonad config
--    which started out as avandael's xmonad.hs 
--    Also uses stuff from https://github.com/pbrisbin
--
 
--{{{ Imports 
import Data.List
 
import Graphics.X11.Xlib
 
import System.IO
 
import XMonad
 
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
 
import XMonad.Core
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Circle
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
 
import XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
 
import qualified Data.Map as M
--}}}
 
--{{{ Helper Functions
stripIM s = if ("IM " `isPrefixOf` s) then drop (length "IM ") s else s
 
wrapIcon icon = "^p(5)^i(" ++ icons ++ icon ++ ")^p(5)"
--}}}
 
--{{{ Path variables
icons = "/home/renjith/.xmonad/"
--}}}
 
main = do
   myStatusBarPipe <- spawnPipe myStatusBar
   conkyBar <- spawnPipe myConkyBar
   xmonad $ myUrgencyHook $ defaultConfig
      { terminal = "urxvt"
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , borderWidth = myBorderWidth
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = mod4Mask
      , keys = myKeys
      , XMonad.Core.workspaces = myWorkspaces
      , startupHook = setWMName "LG3D"
     }   
 
--{{{ Theme 
 
--Font
myFont = "Terminus-8"
 
-- Colors
 
--- Main Colours
myFgColor = "#EDEDED"
myBgColor = "#292421"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#8B8386"
 
--- Borders
myActiveBorderColor = myCurrentWsBgColor
myInactiveBorderColor = "#262626"
myBorderWidth = 1

--- Ws Stuff
myCurrentWsFgColor = myHighlightedFgColor
myCurrentWsBgColor = myHighlightedBgColor
myVisibleWsFgColor = myBgColor
myVisibleWsBgColor = "#CCDC90"
myHiddenWsFgColor = myHighlightedFgColor
myHiddenEmptyWsFgColor = "#8F8F8F"
myUrgentWsBgColor = "#DCA3A3"
myTitleFgColor = myFgColor
 
 
--- Urgency
myUrgencyHintFgColor = "#8DEEEE"
myUrgencyHintBgColor = "#A8A8A8"
 
-- }}}
 
-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -h '15'" ++ " -e 'onstart=lower' -fn '" ++ myFont ++ "'"
 
-- Status Bar
myStatusBar = "dzen2 -w 700 -ta l " ++ myDzenGenOpts
 
-- Conky Bar
myConkyBar = "conky -c .xmonad/conky.conf | dzen2 -x 700 -ta r " ++ myDzenGenOpts
 
-- Layouts
myLayoutHook = avoidStruts $ onWorkspace " 3 im " imLayout $ standardLayouts
               where standardLayouts = tiled ||| centerMaster Grid ||| Mirror tiled ||| Full ||| simpleTabbed
                     imLayout = withIM (2/10) (Role "MainWindow#2") (standardLayouts)
                     tiled = ResizableTall nmaster delta ratio []
                     nmaster = 1 
                     delta = 0.03
                     ratio = 0.5
-- Workspaces
myWorkspaces =
   [
      " 1 sh ",
      " 2 www ",
      " 3 im ",
      " 4 mail ",
      " 5 ",
      " 6 ",
      " 7 ",
      " 8 ",
      " 9 "
   ]
 
-- Urgency hint configuration
myUrgencyHook = withUrgencyHook NoUrgencyHook
 
--{{{ Hook for managing windows
myManageHook = composeAll
   [ resource  =? "Do"               --> doIgnore,              -- Ignore GnomeDo
     className =? "Kopete"           --> doShift " 3 im ",      -- Shift Pidgin to im desktop 
     className =? "Opera"            --> doShift " 9 mail ",     -- Shift opera to 9
     className =? "Firefox"          --> doShift " 2 www ",     -- Shift Firefox to www
     className =? "Orage"            --> doFloat,                -- Float Wicd window 
     className =? "xpad"            --> doFloat,                -- Float Wicd window 
     isFullscreen            --> (doF W.focusDown <+> doFullFloat)
   ]
--}}}
 
-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
 
--{{{ Keybindings 
--    Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  ((modm, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'"),  --Uses a colourscheme with dmenu
  ((modm, xK_b), spawn "firefox"),
  ((0, xK_Print), spawn "scrot"),
  ((modm .|. shiftMask, xK_l), spawn "alock -auth md5:file=/home/renjith/.unlock -bg shade:shade=70,color=#3B6AA0 -cursor image:file=/home/renjith/.1x1.jpg"),
  ((modm, xK_z), goToSelected myGSConfig),
  ((modm, xK_y), sendMessage ToggleStruts),
  ((modm, xK_u), sendMessage MirrorShrink),
  ((modm, xK_i), sendMessage MirrorExpand),
  ((modm, xK_Tab), rotSlavesUp),
  ((modm, xK_Left), prevWS),
  ((modm, xK_Right), nextWS),
  ((modm, xK_c), spawn "orage --toggle"),
  ((modm, xK_n), spawn "xpad --toggle"),
  ((modm, xK_s), spawn "surf"),
  ((modm, xK_q), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
  ((modm, xK_BackSpace), focusUrgent)
   ]
--}}}
 
---{{{ Dzen Config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = (wrapFg myHighlightedBgColor "|"),
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> "  " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor "".
                (\x -> " " ++ x ++ case x of
                    "ResizableTall" -> wrapIcon "dzen_bitmaps/tall.xbm"
                    "Mirror ResizableTall" -> wrapIcon "dzen_bitmaps/mtall.xbm"
                    "Full" -> wrapIcon "dzen_bitmaps/full.xbm"
                    _ -> wrapIcon "dzen_bitmaps/alert.xbm"
                ) . stripIM
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
--}}}
 
--{{{ GridSelect
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_colorizer = ""
    , gs_font = "" ++ myFont ++ ""
    --, gs_navigate = ""
    --, gs_originFractX = ""
    --, gs_originFractY = ""
    }
--}}}
