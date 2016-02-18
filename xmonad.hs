import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import Data.Monoid
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.ManageDocks
import XMonad.Layout.EqualSpacing
import XMonad.Util.EZConfig(additionalKeysP)
-- Status bar
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
-- End Status bar

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myTerminal = "urxvt"
myBorderWidth = 2
myModMask = mod4Mask
myWorkspaces = ["1:main","2","3","4","5","6","7:news","8:org","9:IRC/prtg"]

-- Theme
colorButter1 = "#FCE94F"
colorButter2 = "#EDD400"
colorButter3 = "#C4A000"

colorOrange1 = "#FCAF3E"
colorOrange2 = "#F57900"
colorOrange3 = "#CE5C00"

colorChocolate1 = "#E0B96E"
colorChocolate2 = "#C17D11"
colorChocolate3 = "#8F5902"

colorChameleon1 = "#8AE234"
colorChameleon2 = "#73D212"
colorChameleon3 = "#4E9A06"

colorSkyBlue1 = "#729FCF"
colorSkyBlue2 = "#3465A4"
colorSkyBlue3 = "#204A87"

colorPlum1 = "#AD7FA8"
colorPlum2 = "#75507B"
colorPlum3 = "#5C3566"

colorScarletRed1 = "#EF2929"
colorScarletRed2 = "#CC0000"
colorScarletRed3 = "#A40000"

colorAluminiumLight1 = "#EEEEEC"
colorAluminiumLight2 = "#D3D7CF"
colorAluminiumLight3 = "#BABDB6"

colorAluminiumDark1 = "#888A85"
colorAluminiumDark2 = "#555753"
colorAluminiumDark3 = "#2E3436"

myNormalBorderColor  = colorAluminiumLight1
myFocusedBorderColor = colorScarletRed3

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_p), spawn $ "dmenu_run -o 0.8 -nb \""++ colorOrange2 ++"\" -nf \""++colorAluminiumDark3++"\" -sb \""++colorOrange1++"\" -sf \""++colorAluminiumDark2++"\" -fn Inconsolata-13 -y 445")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2dB-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2dB+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((modm .|. shiftMask, xK_c), kill)
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_n), refresh)
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm, xK_t), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_h), sendMessage LessSpacing)
    , ((modm .|. shiftMask, xK_l), sendMessage MoreSpacing)
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

equalSpaceLayout = equalSpacing 50 5 0 1 $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

newsLayout = equalSpacing 50 5 0 1 $ Full

myLayoutHook = avoidStruts $ onWorkspaces ["7:main"] newsLayout $
               equalSpaceLayout

myManageHook = composeAll
    [ className =? "mpv"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myEventHook = mempty



myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor colorOrange1 colorAluminiumDark3 . pad 
    , ppHidden          = dzenColor colorAluminiumLight1 colorAluminiumDark3 . pad 
    , ppHiddenNoWindows = dzenColor colorAluminiumDark1 colorAluminiumDark3 . pad 
    , ppLayout          = dzenColor colorButter1 colorAluminiumDark3 . pad 
    , ppUrgent          = dzenColor colorScarletRed2 colorAluminiumDark3 . pad . dzenStrip
    , ppTitle           = shorten 100
    , ppWsSep           = ""
    , ppSep             = ""
    , ppOutput          = hPutStrLn h
    }

myStartupHook = setWMName "LG3D"

-- StatusBar
myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '1000' -ta 'l' -fg '" ++ colorAluminiumLight2 ++"' -bg '" ++ colorAluminiumDark3 ++"'"
myStatusBar = "dzen2 -x '1000' -y '0' -h '24' -w '600' -ta 'r' -fg '" ++ colorAluminiumLight2 ++"' -bg '" ++ colorAluminiumDark3 ++"'"

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    spawn $ "conky -c /home/glsubri/.config/conky/conky.config | " ++ myStatusBar
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook dzenLeftBar,
        startupHook        = myStartupHook
    }
