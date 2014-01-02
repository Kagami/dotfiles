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
import XMonad.StackSet (StackSet, view, currentTag)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), namedScratchpadAction,
                                    namedScratchpadManageHook, customFloating)
import qualified XMonad.StackSet as W

import XMonad.Extra (myTerminal, myWorkspaces, initDevEnv, initJsDevEnv)

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
        onWorkspace "1" (task ||| Full) $
        onWorkspace "2" (makeTiled (11%20)) $
        onWorkspace "3" coding $
        onWorkspace "4" task $
        onWorkspace "5" coding $
        tiled ||| Full
      where
        task = makeTiled (7%10)
        coding = makeTiled (1074%1920)
        tiled = makeTiled (1%2)
        makeTiled n = Tall 1 (1%100) n

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

    , ("M1-<Tab>", windows W.focusDown)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)

    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", setLayout layoutHook)
    , ("M-<Return>", windows W.swapMaster)
    , ("M-r", withFocused $ windows . W.sink)
    , ("M-,", sendMessage $ IncMasterN 1)
    , ("M-.", sendMessage $ IncMasterN (-1))
    , ("M-c", kill)
    , ("M-b", sendMessage ToggleStruts)

    , ("<F12>", updateConfig)
    , ("M-<F12>", io exitSuccess)

    , ("M-x d", initDevEnv)
    , ("M-x j", initJsDevEnv)
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
        -- X related things
        spawn "xscreensaver -no-splash"
        spawn "xset r rate 300 50"
        spawn "feh --no-fehbg --bg-center /media/hdd/images/diff/wallpaper.jpg"
        spawn "xsetroot -cursor_name left_ptr"
        spawn "wmname LG3D"
        spawn "fbpanel"
        -- Spawn all needed apps
        spawn "stardict"
        spawnOn "1" "firefox-bin"
        spawnOn "1" "deadbeef"
        spawnOn "2" "gajim"
        spawnAndDo (doSwapDown "2" <+> doShift "2") "thunderbird-bin"
        --replicateM_ 3 $ spawnOn "3" $ myTerminal ++ " --disable-server"
        spawnOn "4" "thunar /media/hdd/downloads/"
        spawnAndDo (doSwapDown "4" <+> doShift "4") "transmission-gtk"

-- | Do swapDown on the specified workspace.
doSwapDown :: WorkspaceId -> ManageHook
doSwapDown i = doF $ onWS i $ W.swapDown

-- | Taken from XMonad.StackSet.onWorkspace because it for some reason
-- was not exported.
onWS :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
     -> (StackSet i l a s sd -> StackSet i l a s sd)
onWS n f s = view (currentTag s) . f . view n $ s
