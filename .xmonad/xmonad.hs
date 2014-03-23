{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when, unless)
import Data.Ratio ((%))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Data.Map as M

import XMonad
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn, spawnAndDo)
import XMonad.Hooks.ManageDocks (ToggleStruts(..), avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.Tabbed (Theme(..), tabbed, shrinkText, defaultTheme)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.WindowNavigation (Navigate(..), Direction2D(..))
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.StackSet (StackSet, view, currentTag)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), namedScratchpadAction)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook, customFloating)
import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad defaultConfig
    -- Simple stuff
    { terminal = myTerminal
    , modMask = mod4Mask
    , workspaces = myWorkspaces
    , focusFollowsMouse = True
    , borderWidth = 2
    -- Key bindings
    , keys = myKeys
    , mouseBindings = myMouseBindings
    -- Hooks, layouts
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , startupHook = myStartupHook
    }
  where
    myLayoutHook =
        avoidStruts $
        smartBorders $
        windowNavigation $
            onWorkspace "1" (task ||| Full) $
            onWorkspace "2" (makeTiled (11%20)) $
            onWorkspace "3" (coding ||| Full) $
            onWorkspace "4" task $
            onWorkspace "5" coding $
            onWorkspace "6" twoPane $
            -- Fallback to default.
            tiled ||| Full
      where
        task = makeTiled (7%10)
        coding = makeTiled codingRatio
        twoPane =
            combineTwo
                (TwoPane defaultRatioIncrement codingRatio)
                myTabbed
                myTabbed
        tiled = makeTiled defaultRatio
        makeTiled = Tall 1 defaultRatioIncrement
        myTabbed = tabbed shrinkText tabbedTheme
        -- Default misc fixed font isn't working.
        -- See <https://code.google.com/p/xmonad/issues/detail?id=361>.
        tabbedTheme = defaultTheme { fontName="xft:DejaVu Sans:size=10" }
        codingRatio = 1074%1920
        defaultRatioIncrement = 1%100
        defaultRatio = 1%2

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { modMask=modm, .. }) = mkKeymap conf
    [ ("M-e", spawn terminal)
    , ("M-p", spawn "dmenu_run_exec")
    , ("M-a", namedScratchpadAction scratchpads scratchpadName)
    , ("<XF86Tools>", spawn "xscreensaver-command -lock")
    , ("<XF86Calculator>", spawn "prepare-torrents")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")

    -- Basic navigation.
    , ("M1-<Tab>", windows W.focusDown)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)

    -- Extended navigation.
    , ("C-M-j", sendMessage $ Move R)
    , ("C-M-k", sendMessage $ Move L)

    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", setLayout layoutHook)
    , ("M-<Return>", windows W.swapMaster)
    , ("M-r", withFocused $ windows . W.sink)
    , ("M-,", sendMessage $ IncMasterN 1)
    , ("M-.", sendMessage $ IncMasterN (-1))
    , ("M-c", kill)
    , ("M-b", sendMessage ToggleStruts)

    , ("M-x r", updateConfig)
    , ("M-<F12>", io exitSuccess)

    , ("M-x s", initShells)
    , ("M-x d 1", initDevEnv devWorkspace1)
    , ("M-x d 2", initDevEnv devWorkspace2)
    -- Toggle fullscreen on web workspace.
    , ("M-x f", sendMessage NextLayout >> sendMessage ToggleStruts)
    ]
    `M.union` (M.fromList $
    [ ((mdf, key), windows $ f wrk)
        | (wrk, key) <- zip workspaces [xK_F1..]
                      -- Switch to workspace
        , (mdf, f) <- [ (0, W.greedyView)
                      -- Move window to workspace
                      , (modm, W.shift)
                      -- Move window to workspace and switch to it
                      , (shiftMask, \ws -> W.greedyView ws . W.shift ws)
                      ]
    ])
  where
    updateConfig :: X ()
    updateConfig = do
        -- Not forced recompile
        isCompiled <- recompile False
        when isCompiled $
            -- Restart with resume
            restart "xmonad" True

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ = M.fromList $
    -- Move
    [ ((mod1Mask, button1), \w -> focus w >> mouseMoveWindow w)
    -- To tile
    , ((mod1Mask, button2), windows . W.sink)
    -- Resize
    , ((mod1Mask, button3), \w -> focus w >> mouseResizeWindow w)
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageDocks
    , manageSpawn
    , namedScratchpadManageHook scratchpads
    , isFullscreen --> doFullFloat
    , className =? "MPlayer" --> doFloat
    , className =? "mplayer2" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Stardict" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "Gajim" <&&> role /=? "roster" --> doFloat
    , className =? "Firefox" <&&> role /=? "browser" --> doFloat
    , className =? "Wine" --> doFloat
    , className =? "java-lang-Thread" --> doFloat
    ]
  where
    q /=? x = fmap (/= x) q

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS scratchpadName
         (myTerminal ++ " --role=" ++ scratchpadName)
         (role =? scratchpadName)
         (customFloating $ W.RationalRect (1%4) (1%5) (1%2) (3%5))
    ]

scratchpadName :: String
scratchpadName = "scratchpad"

myStartupHook :: X ()
myStartupHook = do
    args <- io getArgs
    -- Check for the first start
    unless ("--resume" `elem` args) $ do
        -- X-related things.
        spawn "xscreensaver -no-splash"
        spawn "xset r rate 300 50"
        spawn "feh --no-fehbg --bg-center /media/hdd/images/diff/wallpaper.jpg"
        spawn "xsetroot -cursor_name left_ptr"
        spawn "wmname LG3D"
        spawn "fbpanel"
        -- Spawn all needed apps.
        spawn "stardict"
        spawnOn webWorkspace "firefox-bin"
        spawnOn webWorkspace "deadbeef"
        spawnOn imWorkspace "gajim"
        spawnAndDo
            (doSwapDown imWorkspace <+> doShift imWorkspace)
            "thunderbird-bin"
        -- replicateM_ shellWorkspace $
        --     spawnOn shellWorkspace $
        --     myTerminal ++ " --disable-server"
        spawnOn filesWorkspace "thunar /media/hdd/downloads/"
        spawnAndDo
            (doSwapDown filesWorkspace <+> doShift filesWorkspace)
            "transmission-gtk"

myTerminal :: FilePath
myTerminal = "xfce4-terminal"

myWorkspaces :: [WorkspaceId]
myWorkspaces =
    [ webWorkspace
    , imWorkspace
    , shellWorkspace
    , filesWorkspace
    , devWorkspace1
    , devWorkspace2
    ] ++ extraWorkspaces

webWorkspace :: WorkspaceId
webWorkspace = "1"

imWorkspace :: WorkspaceId
imWorkspace = "2"

shellWorkspace :: WorkspaceId
shellWorkspace = "3"

filesWorkspace :: WorkspaceId
filesWorkspace = "4"

devWorkspace1 :: WorkspaceId
devWorkspace1 = "5"

devWorkspace2 :: WorkspaceId
devWorkspace2 = "6"

extraWorkspaces :: [WorkspaceId]
extraWorkspaces = ["7", "8"]

initShells :: X ()
initShells = do
    windows $ W.greedyView shellWorkspace
    safeSpawn myTerminal ["-e", "true", "--window", "--window"]

initDevEnv :: WorkspaceId -> X ()
initDevEnv workspace = do
    windows $ W.greedyView workspace
    safeSpawn "chromium" []
    safeSpawn myTerminal ["-e", "zsh -is eval 'cd ~/code'"]

---
-- Utils.
---

-- | Do swapDown on the specified workspace.
doSwapDown :: WorkspaceId -> ManageHook
doSwapDown i = doF $ onWS i $ W.swapDown

-- | Taken from XMonad.StackSet.onWorkspace because it for some reason
-- was not exported.
onWS :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
     -> (StackSet i l a s sd -> StackSet i l a s sd)
onWS n f s = view (currentTag s) . f . view n $ s
