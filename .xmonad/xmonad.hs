-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
 
import System.IO
import Data.List
import System.Exit
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.DragPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.Dishes
import XMonad.Layout.Cross
import XMonad.Layout.Accordion
import XMonad.Layout.Magnifier
import XMonad.Layout.Circle
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Layout.PerWorkspace
import XMonad.Config.Gnome
import XMonad.Util.Replace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/gnome-terminal"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:code","2:web","3:term","4:im","5:sql","6:files","7:media"] ++ map show [8..9]
 
myTopics :: [Topic]
myTopics =
    [  "eclipse", "web", "terminal", "skype", "sql", "files"
        , "mail", "music", "dashboard", "talk", "text", "tools", "persloadm", "perforce", "facebook", "xmonad", "prod", "irc"
    ]
myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
        [("eclipse","proj/branches/VODCms")]
        , defaultTopicAction = const $ spawnShell >*> 3
        , defaultTopic = "dashboard"
        , topicActions = M.fromList $
        [ ("eclipse", spawn "eclipse")
          ,("web", spawn "firefox")
          ,("skype", spawn "skype")
          ,("facebook", spawn "empathy")
          ,("mail", spawn "evolution")
          ,("sql", spawn "oracle-sqldeveloper")
          ,("perforce", spawn "p4v")
          ,("irc" , spawn "xchat")
          ,("persloterm", spawn "persloterm")
          ,("files", spawn "marlin")]
    }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "gnome-terminal --working-directory="++dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myPromptConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myPromptConfig $ windows . W.shift 

mailAction :: X ()
mailAction = spawn $ "evolution"
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
--myManageHook = composeAll . concat $
--    [ [ className   =? c --> doFloat           | c <- myFloats]
--    , [ title       =? t --> doFloat           | t <- myOtherFloats]
--    , [ className   =? c --> doF (W.shift "2:web") | c <- webApps]
--    , [ className   =? c --> doF (W.shift "3:term") | c <- ircApps]
--    ]
--    where   myFloats      = ["MPlayer", "Gimp"]
--            myOtherFloats = ["alsamixer"]
--            webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
--            imApps        = ["Ksirc"]                -- open on desktop 3
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "web"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Google-chrome"  --> doShift "web"
    , className =? "Firefox"  --> doShift "web"
    , resource  =? "gpicview"       --> doFloat
    , resource  =? "kdesktop"       --> doIgnore
    , className  =? "Do"       --> doIgnore
    , className  =? "nautilus"       --> doShift "files"
    , className  =? "gnome-terminal"       --> doShift "terminal"
    , className =? "MPlayer"        --> doFloat
    , resource =? "xfce4-notifyd" --> doIgnore
    , resource  =? "skype"          --> doShift "skype"
--    , className =? "Skype" <&&> role =? "" --> doCenterFloat'
    , resource  =? "amarok"          --> doShift "music"
    , className =? "Xchat"          --> doShift "irc"
    , isDialog --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        doMaster = doF W.shiftMaster
        doCenterFloat' = doCenterFloat <+> doMaster
        doFullFloat' = doFullFloat <+> doMaster


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    onWorkspace "facebook" empathyIM $
    onWorkspace "skype" skypeIM $
    (
        Tall 1 (3/100) (1/2) |||
        Mirror (Tall 1 (3/100) (1/2)) |||
        tabbed shrinkText tabConfig |||
--        magnifier (Grid) |||
        Grid |||
        Circle 
--    noBorders (fullscreenFull Full)
        )
    )
    where
        skypeIM = withIM (1%7) (Title "martin.harvan1 - Skype™ (Beta)") Grid 
        empathyIM = withIM (1%7) (Title "Contact List") Grid 

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#B3FF8C"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 4


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod1Mask

myPromptConfig = defaultXPConfig { 
         searchPredicate = isInfixOf
        ,autoComplete = Just 500000 
        ,font = myFont
        ,fgColor = colorWhiteAlt
        ,bgColor = colorBlack
        }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Launch dmenu via yeganesh.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn "gnome-do")
  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_p),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn "screenshot")

    , ((0, xF86XK_AudioMute), spawn "sh /home/kane/configs/bin/voldzen.sh t -d")                     --Mute/unmute volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "sh /home/kane/configs/bin/voldzen.sh + -d")              --Raise volume
    , ((0, xF86XK_AudioLowerVolume), spawn "sh /home/kane/configs/bin/voldzen.sh - -d")              --Lower volume
  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  , ((modMask .|. controlMask .|. shiftMask, xK_plus ), sendMessage MagnifyMore)
  , ((modMask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
  , ((modMask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
  , ((modMask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
  , ((modMask .|. controlMask              , xK_m    ), sendMessage Toggle     )
  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
          setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )
  , ((modMask              , xK_a     ), currentTopicAction myTopicConfig)
  , ((modMask              , xK_g     ), promptedGoto)
  , ((modMask .|. shiftMask, xK_g     ), promptedShift)
  , ((modMask , xK_b     ),  windowPromptBring myPromptConfig)
--  , ((modMask .|. shiftMask, xK_m    V ),
--     workspacePrompt myPromptConfig . )
  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++
 
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
 

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
 
myFont               = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
dzenFont             = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#3955c4"
colorRed             = "#d74b73"
colorGreen           = "#99cc66"
myArrow              = "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"
--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }

-- StatusBars
myWorkspaceBar, myBottomStatusBar, myTopStatusBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '1034' -h '16' -w '1000' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e ''"
myBottomStatusBar = "/home/kane/configs/bin/bottomstatusbar.sh"
myTopStatusBar    = "/home/kane/configs/bin/topstatusbar.sh"

-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
	, ppSort            = (ppSort defaultPP) -- hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = "^fg(" ++ colorGray ++ ")|"
	, ppWsSep           = ""
	, ppCurrent         = dzenColor colorBlue     colorBlack . pad
	, ppUrgent          = dzenColor colorGreen    colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppVisible         = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppHidden          = dzenColor colorWhiteAlt colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppHiddenNoWindows = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppLayout          = dzenColor colorBlue     colorBlack . pad . wrapClickLayout . layoutText
	, ppTitle           = dzenColor colorWhiteAlt colorBlack . pad . wrapClickTitle . titleText . dzenEscape
	}
	where
		--display config
		orderText (ws:l:t:_) = [ws,l,t]
		titleText [] = "Desktop " ++ myArrow
		titleText x = (shorten 82 x) ++ " " ++ myArrow
		layoutText "Minimize T"  = "ReTall"
		layoutText "Minimize O"  = "OneBig"
		layoutText "Minimize TS" = "Tabbed"
		layoutText "Minimize TM" = "Master"
		layoutText "Minimize M"  = "Mosaic"
		layoutText "Minimize MT" = "Mirror"
		layoutText "Minimize G"  = "Mosaic"
		layoutText "Minimize C"  = "Mirror"
		layoutText "Minimize ReflectX T"  = "^fg(" ++ colorGreen ++ ")ReTall X^fg()"
		layoutText "Minimize ReflectX O"  = "^fg(" ++ colorGreen ++ ")OneBig X^fg()"
		layoutText "Minimize ReflectX TS" = "^fg(" ++ colorGreen ++ ")Tabbed X^fg()"
		layoutText "Minimize ReflectX TM" = "^fg(" ++ colorGreen ++ ")Master X^fg()"
		layoutText "Minimize ReflectX M"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		layoutText "Minimize ReflectX MT" = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		layoutText "Minimize ReflectX G"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		layoutText "Minimize ReflectX C"  = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		layoutText "Minimize ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall Y^fg()"
		layoutText "Minimize ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig Y^fg()"
		layoutText "Minimize ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed Y^fg()"
		layoutText "Minimize ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master Y^fg()"
		layoutText "Minimize ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		layoutText "Minimize ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		layoutText "Minimize ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		layoutText "Minimize ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		layoutText "Minimize ReflectX ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall XY^fg()"
		layoutText "Minimize ReflectX ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig XY^fg()"
		layoutText "Minimize ReflectX ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed XY^fg()"
		layoutText "Minimize ReflectX ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master XY^fg()"
		layoutText "Minimize ReflectX ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		layoutText "Minimize ReflectX ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		layoutText "Minimize ReflectX ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		layoutText "Minimize ReflectX ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		layoutText x = "^fg(" ++ colorGreen ++ ")" ++ x ++ "^fg()"
		--clickable config
		wrapClickLayout content = "^ca(1,xdotool key alt+space)" ++ content ++ "^ca()"                                                           --clickable layout
		wrapClickTitle content = "^ca(1,xdotool key alt+j)" ++ content ++ "^ca()"                                                                --clickable title
		wrapClickWorkSpace (idx,str) = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "e;" ++ xdo index ++ ")" ++ str ++ "^ca()^ca()" --clickable workspaces
			where
				wsIdxToString Nothing = "1"
				wsIdxToString (Just n) = show (n+1)
				index = wsIdxToString (elemIndex idx myWorkspaces)
				xdo key = "xdotool key alt+" ++ key

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main :: IO ()
main = do
    workspaceBar            <- spawnPipe myWorkspaceBar
    bottomStatusBar         <- spawnPipe myBottomStatusBar
    topStatusBar            <- spawnPipe myTopStatusBar
    replace
    xmonad $ myUrgencyHook $ ewmh defaults {
        logHook            = (myLogHook workspaceBar) <+> ewmhDesktopsLogHook >> setWMName "LG3D" --ewmh needed so that chromium gain focus
        , manageHook = manageDocks <+> myManageHook
        , startupHook = setWMName "LG3D"
    }
 
------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myTopics,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
 
    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
 
    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook
}
