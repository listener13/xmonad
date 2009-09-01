import XMonad hiding (Tall)
import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
<<<<<<< HEAD
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
=======
>>>>>>> 797b637b4e411fb7d1cfbffa1967972d96d096cc
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
<<<<<<< HEAD
import XMonad.Layout.IM
import System.Exit
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.StackSet as W
=======
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
>>>>>>> 797b637b4e411fb7d1cfbffa1967972d96d096cc
import Data.Ratio
import System.IO (hPutStrLn)
import GHC.IOBase (Handle)

<<<<<<< HEAD
wsMain    = "1:main "
wsWeb     = "2:web "
wsIm      = "3:im "
wsEmacs   = "4:emacs "
wsTorrent = "5:torrent "

wslist    = [wsMain, wsWeb, wsIm, wsEmacs, wsTorrent, "6:etc ", "7:etc ", "8:etc "]

main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar"
    sp <- mkSpawner
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { normalBorderColor = backgroundColor
        , focusedBorderColor = focusColor
        , workspaces = wslist
        , terminal = "terminator"
        , layoutHook = myLayouts
        , modMask = mod4Mask
        , manageHook = manageSpawn sp <+> myManageHook <+> manageDocks
        , borderWidth = 2
        , logHook = dynamicLogWithPP (myPP xmobar)
                 >> updatePointer (Relative 1 1)
        }  
        
        `additionalKeysP`
	        [ ("M-S-<Return>", spawn "terminator")
		    , ("M-S-p", spawn "gmrun")
		    , ("M-S-c", kill)
		    , ("M-<Space>", sendMessage NextLayout)
		    , ("M-n", refresh)
		    , ("M-<Tab>", windows W.focusDown)
		    , ("M-j", windows W.focusDown)
		    , ("M-k", windows W.focusUp  )
		    , ("M-m", windows W.focusMaster  )
		    , ("M-<Return>", windows W.swapMaster)
		    , ("M-S-j", windows W.swapDown  )
		    , ("M-S-k", windows W.swapUp    )
		    , ("M-h", sendMessage Shrink)
		    , ("M-l", sendMessage Expand)
		    -- Push window back into tiling
		    , ("M-t", withFocused $ windows . W.sink)
		    , ("M-,", sendMessage (IncMasterN 1))
		    , ("M-.", sendMessage (IncMasterN (-1)))
		    , ("M-S-q", io (exitWith ExitSuccess))
		    --, ("M-q", spawn "xmonad --recompile; xmonad --restart")
          	, ("M-q", recompile True >> restart "xmonad" True)
			, ("M-w", runOrRaise "firefox" (className =? "Shiretoko"))
			, ("M-e", runOrRaise "emacs" (className =? "Emacs"))
			, ("M-t", runOrRaise "transmission" (className =? "Transmission"))
			, ("<XF86AudioPlay>", spawn "mocp -G")
			, ("<XF86AudioPrev>", spawn "mocp -r")
			, ("<XF86AudioNext>", spawn "mocp -f")
			, ("<XF86AudioLowerVolume>", spawn "amixer set Master 5dB-")
			, ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5dB+")
			]
        
        where
        
            myFont = "xft:DejaVu Sans:size=10"
            focusColor = "#60ff45"
            textColor = "#c0c0a0"
            lightTextColor = "#fffff0"
            backgroundColor = "#304520"
            lightBackgroundColor = "#456030"
            urgentColor = "#ffc000"
            
            myManageHook = composeAll
                 [ className =? "MPlayer"      --> doFloat
                 --OpenOffice popup windows
                 , appName   =? "VCLSalFrame"  --> doIgnore
                 , className =? "Shiretoko"    --> moveTo wsWeb
                 , className =? "Gajim.py"     --> moveTo wsIm
                 , className =? "Emacs"        --> moveTo wsEmacs
                 , className =? "Transmission" --> moveTo wsTorrent
                 , title     =? "Downloads"    --> doFloat
                 ]
                 where moveTo = doF . W.shift
 
            genericLayouts = layoutHints $ avoidStruts $ smartBorders $ hintedTile Tall
																	||| hintedTile Wide
																	||| Full
																	||| spiral (1 % 1)
																		where
																            hintedTile = HintedTile nmaster delta ratio TopLeft
																            nmaster = 2
																            ratio = 1/2
																            delta = 3/100
            
            myLayouts = onWorkspace wsIm myIm$
						genericLayouts
				where
					myIm = avoidStruts $ gridIM (1%7) (ClassName "Gajim.py")
 
=======
--import XMonad hiding ( (|||) )

--import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.ManageHelpers
--import XMonad.Hooks.XPropManage
---- for JAVA:
--import XMonad.Hooks.SetWMName 

--import XMonad.Layout.Combo
--import XMonad.Layout.IM
--import XMonad.Layout.Grid
--import XMonad.Layout.LayoutModifier
--import XMonad.Layout.LayoutCombinators
--import XMonad.Layout.NoBorders
--import XMonad.Layout.PerWorkspace
--import XMonad.Layout.ResizableTile
--import XMonad.Layout.Tabbed
--import XMonad.Layout.ToggleLayouts
--import XMonad.Layout.WindowNavigation

--import XMonad.Util.Run(spawnPipe)
--import XMonad.Util.EZConfig(additionalKeys)
--import XMonad.Util.Dmenu
--import XMonad.Util.WindowProperties

--import System.IO
--import XMonad.Actions.CopyWindow
--import XMonad.Actions.TagWindows
--import XMonad.Prompt
--import XMonad.Prompt.Window
--import qualified XMonad.StackSet as W
--import qualified Data.Map        as M
--import Data.Ratio ((%))
--import Control.Monad
--import Graphics.X11.Xlib.Extras
--import Foreign.C.Types (CLong)

 
main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar"
    xmonad 
	$ withUrgencyHook NoUrgencyHook 
	$ defaultConfig
        { normalBorderColor  = backgroundColor
        , focusedBorderColor = focusColor
        , workspaces = ["term ", "web ", "im ", "torrent ", "filez ", "etc1 ", "etc2 ", "etc3 ", "etc4 "]
        , terminal = "$XTERMCMD"
        , layoutHook = myLayouts
        , manageHook = myManageHook <+> manageDocks
        , modMask = mod4Mask
        , borderWidth = 2
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = dynamicLogWithPP (myPP xmobar)
                 >> updatePointer (Relative 1 1)
        }
        where
              
          
            myManageHook = composeAll
                 [ className =? "MPlayer" --> doFloat
                 ]

            myLayouts = layoutHints $ avoidStruts $ smartBorders $ hintedTile Tall
                                 ||| hintedTile Wide
                                 ||| Full
                                 ||| spiral (1 % 1)
            hintedTile = HintedTile nmaster delta ratio TopLeft
            nmaster = 2
            ratio   = 1/2
            delta   = 3/100

>>>>>>> 797b637b4e411fb7d1cfbffa1967972d96d096cc
            myPP :: Handle -> PP
            myPP din = defaultPP
                { ppCurrent = xmobarColor focusColor ""
                , ppVisible = xmobarColor lightTextColor ""
                , ppHiddenNoWindows = xmobarColor lightBackgroundColor ""
                , ppUrgent = xmobarColor urgentColor ""
                , ppSep = " · "
                , ppWsSep = ""
                , ppTitle = xmobarColor lightTextColor ""
                , ppOutput = hPutStrLn din
                }
<<<<<<< HEAD
=======
 
            myFont = "xft:DejaVu Sans:size=10"
            focusColor = "#60ff45"
            textColor = "#c0c0a0"
            lightTextColor = "#fffff0"
            backgroundColor = "#304520"
            lightBackgroundColor = "#456030"
            urgentColor = "#ffc000"
 
            myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
                [ ((modMask,             xK_t), spawn "terminator")
                , ((modMask .|. shiftMask, xK_Return), spawn "terminator")
                , ((0 , 0x1008ff14), spawn "mocp -G")
                , ((0 , 0x1008ff13), spawn "amixer set Master 5dB+")
                , ((0 , 0x1008ff11), spawn "amixer set Master 5dB-")
                , ((0 , 0x1008ff17), spawn "mocp -f")
                , ((0 , 0x1008ff16), spawn "mocp -r")
                -- Shrink the master area
                , ((modMask,               xK_h     ), sendMessage Shrink)
                -- Expand the master area
                , ((modMask,               xK_l     ), sendMessage Expand)
                , ((modMask,                 xK_Return), promote)
                , ((modMask .|. controlMask, xK_l), spawn "xscreensaver-command -activate")
                , ((modMask,                 xK_z), warpToWindow 1 1)
                , ((modMask,                 xK_q), recompile True >> restart "xmonad" True)
                ]
>>>>>>> 797b637b4e411fb7d1cfbffa1967972d96d096cc
