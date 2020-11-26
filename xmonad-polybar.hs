-- IMPORT                                                                    {{{
--------------------------------------------------------------------------------
import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run

import XMonad.Layout.FixedColumn
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Prompt

import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.Layout.BoringWindows as B

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import System.IO (hClose)

import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------------------------------------------------------}}}
-- MAIN                                                                      {{{
--------------------------------------------------------------------------------
--TODO: move some programs automatically to workspaces
main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    $ dynamicProjects projects
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    -- $ docks ???
    $ myAddDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
    -- $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myAdditionalKeys
    $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-----------------------------------------------------------------------------}}}
-- GLOBAL VARIABLES                                                          {{{
--------------------------------------------------------------------------------
-- General config
myTerminal     = "tilix"
myModMask      = mod4Mask
myBorderWidth  = 1
myBrowser      = "firefox"
mySpacing :: Int
mySpacing      = 0
myLargeSpacing :: Int
myLargeSpacing = 30
noSpacing :: Int
noSpacing      = 0
prompt         = 20

-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Font
myFont = "xft:SpaceMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-----------------------------------------------------------------------------}}}
-- LAYOUT                                                                    {{{
--------------------------------------------------------------------------------
myLayouts = avoidStruts
   (smartBorders $ myTile ||| Mirror myTile ||| noBorders Full ||| noBorders myTabbed )

-- myLayouts = renamed [CutWordsLeft 1] . avoidStruts . minimize . B.boringWindows $ perWS

-- layout per workspace
perWS = onWorkspace wsWEB my3FT $
        onWorkspace wsWRK myAll $
        onWorkspace wsSYS myFTM $
        onWorkspace wsMED my3FT $
        onWorkspace wsTMP myFTM $
        onWorkspace wsGAM myFT  $
        myAll -- all layouts for all other workspaces


myFT  = myTile ||| myFull
myFTM = myTile ||| myFull ||| myMagn
my3FT = myTile ||| myFull ||| my3cmi
myAll = myTile ||| myFull ||| my3cmi ||| myMagn


myFull = renamed [Replace "Full"] $ spacing 0 $ noBorders Full
myTile = renamed [Replace "Main"] $ spacing mySpacing $ Tall nmaster delta ratio
    where 
       nmaster = 1   -- The default number of windows in the master pane
       ratio   = 1/2 -- Default proportion of screen occupied by master pane
       delta   = 3/100 -- Percent of screen to increment by when resizing panes
   
myTabbed  = named "Tabbed" $ simpleTabbed

my3cmi = renamed [Replace "3Col"] $ spacing mySpacing $ ThreeColMid 1 (3/100) (1/2)
myMagn = renamed [Replace "Mag"]  $ noBorders $ limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10

-----------------------------------------------------------------------------}}}
-- THEMES                                                                    {{{
--------------------------------------------------------------------------------
-- Prompt themes
myPromptTheme = def
  { font              = myFont
  , bgColor           = darkgreen
  , fgColor           = white
  , fgHLight          = white
  , bgHLight          = pur2
  , borderColor       = pur2
  , promptBorderWidth = 0
  , height            = prompt
  , position          = Top
  }

warmPromptTheme = myPromptTheme
  { bgColor           = yellow
  , fgColor           = darkred
  , position          = Top
  }

coldPromptTheme = myPromptTheme
  { bgColor           = aqua
  , fgColor           = darkgreen
  , position          = Top
  }

-----------------------------------------------------------------------------}}}
-- WORKSPACES                                                                {{{
--------------------------------------------------------------------------------
wsWEB = "\xf269" -- firefox logo
wsWRK = "\xf02d" -- book
wsSYS = "\xf300"
wsMED = "\xf001" -- music note 
wsTMP = "\xf2db" -- cpu
wsGAM = "\xf11b" -- game pad

myWorkspaces :: [String]
myWorkspaces = ["1", wsWRK, wsWEB, wsMED, wsTMP, wsGAM, "7", "8", "9"]

-----------------------------------------------------------------------------}}}
-- PROJECTS                                                                  {{{
--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "study"
            , projectDirectory = "~/Documents/studie/master"
            , projectStartHook = Just $ do spawn "tilix -e tmux"
                                           spawn myTerminal
            }
  , Project { projectName      = "term"
            , projectDirectory = "~/Documents/"
            , projectStartHook = Just $ do spawn myBrowser
                                           spawn myTerminal
            }
  , Project { projectName      = "program"
            , projectDirectory = "~/Documents/program"
            , projectStartHook = Just $ do spawn myBrowser
                                           spawn "tilix -e tmux"
            }
  , Project { projectName      = "system"
            , projectDirectory = "~/Documents/"
            , projectStartHook = Just $ do spawn "tilix -e ncmpcpp"
                                           spawn "tilix -e ncmpcpp"
                                           spawn "tilix -e htop"
            }

  ]

-----------------------------------------------------------------------------}}}
-- KEYBINDINGS                                                               {{{
--------------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=adobe courier"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

-- this is a copy of XMonad.Util.NamedActions.addDesrcKeys but with a custom default binding
myAddDescrKeys :: (HasName b1, HasName b) =>
    ((KeyMask, KeySym),[((KeyMask, KeySym), NamedAction)] -> b)
    -> (XConfig Layout -> [((KeyMask, KeySym), b1)])
    -> XConfig l
    -> XConfig l
myAddDescrKeys k ks = addDescrKeys' k (\l -> myInitialKeys l ^++^ ks l)

myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  myProgramKeys ++ myWindowManagerKeys ++ myMediaKeys

myProgramKeys :: [(String, NamedAction)]
myProgramKeys = 
  [ ("M-p"        , addName "Launcher" $ spawn "eval \"zsh -ci 'dmenu_run'\"" )
  , ("M-S-q"      , addName "Exit" $ io $ exitWith ExitSuccess )
 -- , ("M-S-q"      , addName "Exit" $ spawn "gnome-session-quit --logout --no-prompt" )
 -- , ("M-z"        , addName "Open calendar & todo list" $ spawn "tilix -e vim ~/Dropbox/my_wiki/index.wiki")
 -- , ("M-s"        , addName "Open Steam" $ spawn "steam")
 -- , ("M-S-s"      , addName "Sleep" $ spawn "systemctl suspend")
 -- , ("M-f"        , addName "Open firefox" $ spawn myBrowser)
 -- , ("M-S-g"      , addName "Open notes for final assignment" $ spawn "tilix -e vim ~/Documents/studie/master/afstudeeropdracht/notes/general.md")
  , ("M-Print"    , addName "Screenshot" $ spawn "gnome-screenshot" )
  ]


myWindowManagerKeys :: [(String, NamedAction)]
myWindowManagerKeys = 
  [ ("M-f"        , addName "Do (not) respect polybar" $ sendMessage ToggleStruts)
--  , ("M-S-d"      , addName "Increase spacing between windows" $ incSpacing mySpacing)
--  , ("M-v"        , addName "Set default spacing between windows" $ setSpacing mySpacing)
--  , ("M-S-v"      , addName "Decrease spacing between windows" $ incSpacing (-mySpacing))
--  , ("M-c"        , addName "Set to default large spacing between windows" $ setSpacing myLargeSpacing)
  , ("M-u"        , addName "Switch view to project" $ switchProjectPrompt warmPromptTheme)
  , ("M-S-u"      , addName "Send current window to project" $ shiftToProjectPrompt coldPromptTheme)
  , ("M-S-h"      , addName "Move to previous non empty workspace" $ moveTo Prev NonEmptyWS)
  , ("M-S-l"      , addName "Move to next non empty workspace" $ moveTo Next NonEmptyWS)
  ] 


myMediaKeys :: [(String, NamedAction)]
myMediaKeys = 
  [ ("<XF86MonBrightnessUp>"   , addName "Increase backlight" $ spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>" , addName "Decrease backlight" $ spawn "xbacklight -dec 10")
  -- mpc
  , ("<XF86AudioPrev>"         , addName "Previous track" $ spawn "mpc prev")
  , ("<XF86AudioNext>"         , addName "Next track" $ spawn "mpc next")
  , ("<XF86AudioPlay>"         , addName "Toggle play/pause" $ spawn "mpc toggle")
  , ("<XF86AudioRaiseVolume>"  , addName "Raise volume" $ spawn "pactl set-sink-volume 1 +5%")
  , ("<XF86AudioLowerVolume>"  , addName "Lower volume" $ spawn "pactl set-sink-volume 1 -5%")
  , ("<XF86AudioMute>"         , addName "Toggle mute" $ spawn "pactl set-sink-mute 1 toggle")
  , ("<XF86Sleep>"             , addName "Lock screen" $ spawn "xscreensaver-command --lock")
  -- volume: for if meta keys are not available
  , ("C-S-="                   , addName "Raise volume" $ spawn "pactl set-sink-volume 1 +5%")
  , ("C-S--"                   , addName "Lower volume" $ spawn "pactl set-sink-volume 1 -5%")
  -- media keys if meta keys are not available
  , ("C-S-,"                   , addName "Previous track" $ spawn "mpc prev")
  , ("C-S-."                   , addName "Next track" $ spawn "mpc next")
  , ("C-S-/"                   , addName "Toggle play/pause" $ spawn "mpc toggle")
  ]

--
-- xbindkeys -k to find a particular key's keycode
--
myInitialKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myInitialKeys conf@(XConfig {XMonad.modMask = modm}) = 
    [ subtitle "launching and killing programs"
    , ((modm .|. shiftMask, xK_Return), addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm,               xK_l     ), addName "Launch dmenu" $ spawn "eval \"zsh -ci 'dmenu_run'\"") -- %! Launch dmenu
    , ((modm .|. shiftMask, xK_c     ), addName "Close the focused window" kill) -- %! Close the focused window

    , subtitle "changing layouts"
    , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , separator
    , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

    , subtitle "move focus up or down the window stack"
    , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    -- , ((modm,               xK_j     ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    -- , ((modm,               xK_k     ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_m     ), addName "Focus the master" $ windows W.focusMaster  ) -- %! Move focus to the master window

    , subtitle "modifying the window order"
    , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_s     ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_t     ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

    , subtitle "resizing the master/slave ratio"
    , ((modm,               xK_h     ), sendMessage' Shrink) -- %! Shrink the master area
    , ((modm,               xK_r     ), sendMessage' Expand) -- %! Expand the master area

    , subtitle "floating layer support"
    , ((modm,               xK_i     ), addName "Push floating to tiled" $ withFocused $ windows . W.sink) -- %! Push window back into tiling
    
    -- comma = virgule, period = point, semicolon= point-virgule, colon = deux-points
    , subtitle "change the number of windows in the master area"
    , ((modm              , xK_comma ), sendMessage' (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage' (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
-- , ((modm              , xK_f     ),  sendMessage ToggleLayout)
    -- , ((modm              , xK_a     ),  goToSelected defaultGSConfig)
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)


    , subtitle "quit, or restart"
    , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modm              , xK_q     ), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
    ]

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    subtitle "switching workspaces":
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) workspaceKeys]
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
   ++
   subtitle "switching screens" :
   [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip screenKeys [1,0,2]]
   where
        -- (workspaceKeys, screenKeys) = ([xK_1 .. xK_9] ++ [xK_0], [xK_w, xK_e, xK_r]) -- QWERTY
        (workspaceKeys, screenKeys) = ([0x22,0x3c,0x3e,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a], [xK_b, xK_eacute, xK_p])     -- BÉPO 

----------------------------------------------------------------------------}}}
-- MANAGEHOOK                                                                {{{
--------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "Firefox"          --> viewShift wsWEB 
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "feh"              --> doFloat
    , className =? "Gpick"            --> doFloat
    , role      =? "pop-up"           --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

-----------------------------------------------------------------------------}}}
-- LOGHOOK                                                                   {{{
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-----------------------------------------------------------------------------}}}
-- STARTUPHOOK                                                               {{{
--------------------------------------------------------------------------------
myStartupHook = do
  -- gnomeRegister
  --setWMName "LG3D"
  spawn "$HOME/.config/polybar/launch.sh"

-----------------------------------------------------------------------------}}}
-- CONFIG                                                                    {{{
--------------------------------------------------------------------------------
myConfig = def
  { terminal            = myTerminal
  , layoutHook          = myLayouts
  , manageHook          = placeHook(smart(0.5, 0.5))
      <+> manageDocks
      <+> myManageHook
      <+> myManageHook'
      <+> manageHook def
  , handleEventHook     = docksEventHook
      <+> minimizeEventHook
      <+> fullscreenEventHook
  , startupHook         = myStartupHook
  , focusFollowsMouse   = True
  , clickJustFocuses    = False
  , borderWidth         = myBorderWidth
  , normalBorderColor   = bg
  , focusedBorderColor  = pur2
  , workspaces          = myWorkspaces
  , modMask             = myModMask
  }
-----------------------------------------------------------------------------}}}
