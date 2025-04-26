module Module.Scratchpad
    ( scratchpadsYaziKeyAction
    , scratchpadsTelegramKeyAction
    , scratchpadsNautilusKeyAction
    , scratchpadsYoutubeMusicKeyAction
    , scratchpadsGoogleChatKeyAction
    , scratchpadsMonkeyTypeKeyAction
    , scratchpadsManageHooks
    , scratchpadsLogHooks
    ) where

import qualified Module.Variable as V
import qualified Util.Common as U
import XMonad
import XMonad.Util.NamedScratchpad

-- ######################## PUBLIC ##########################

scratchpadsManageHooks = namedScratchpadManageHook scratchpads
scratchpadsLogHooks = nsSingleScratchpadPerWorkspace scratchpads >> nsHideOnFocusLoss scratchpads

scratchpadsYaziKeyAction = namedScratchpadToggle scratchpadsYaziId "📂 Yazi File Manager"
scratchpadsTelegramKeyAction = namedScratchpadToggle scratchpadsTelegramId "💬 Telegram"
scratchpadsNautilusKeyAction = namedScratchpadToggle scratchpadsNautilusId "💽 Music"
scratchpadsYoutubeMusicKeyAction = namedScratchpadToggle scratchpadsYoutubeMusicId "✉️ Chat"
scratchpadsGoogleChatKeyAction = namedScratchpadToggle scratchpadsGoogleChatId "📂 Nautilus"
scratchpadsMonkeyTypeKeyAction = namedScratchpadToggle scratchpadsMonkeyTypeId "⌨️ Monkey Type"

-- ######################## PRIVATE ##########################

scratchpadsYaziId = "yazi-scratchpad-id-1"
scratchpadsTelegramId = "telegram-scratchpad-id-2"
scratchpadsNautilusId = "nautilus-scratchpad-id-3"
scratchpadsYoutubeMusicId = "youtube_music-scratchpad-id-4"
scratchpadsGoogleChatId = "google_chat-scratchpad-id-5"
scratchpadsMonkeyTypeId = "monkey_type-scratchpad-id-6"

appsTerminalYazi = "ghostty --class=com.scratchpad.yazi -e yazi"
appsTelegram = "telegram-desktop"
appsNautilus = "nautilus"
appsBraveYoutubeMusic = "brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
appsBraveGoogleChat = "brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
appsBraveMonkeyType = "brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"

scratchpads :: [NamedScratchpad]
scratchpads =
    [ scratchToClass scratchpadsYaziId appsTerminalYazi "com.scratchpad.yazi"
    , scratchTo scratchpadsTelegramId appsTelegram "TelegramDesktop" "telegram-desktop"
    , scratchTo scratchpadsNautilusId appsNautilus "org.gnome.Nautilus" "org.gnome.Nautilus"
    , scratchTo scratchpadsYoutubeMusicId appsBraveYoutubeMusic "Brave-browser" "crx_cinhimbnkkaeohfgghhklpknlkffjgod"
    , scratchTo scratchpadsGoogleChatId appsBraveGoogleChat "Brave-browser" "crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"
    , scratchTo scratchpadsMonkeyTypeId appsBraveMonkeyType "Brave-browser" "crx_picebhhlijnlefeleilfbanaghjlkkna"
    ]
    where
        scratchToClass scratchId appRun cname = NS scratchId appRun (className =? cname) (customFloating U.toRationalRect)
        scratchToInstance scratchId appRun iname = NS scratchId appRun (resource =? iname) (customFloating U.toRationalRect)
        scratchTo scratchId appRun cname iname = NS scratchId appRun (className =? cname <&&> resource =? iname) (customFloating U.toRationalRect)

namedScratchpadToggle :: String -> String -> X ()
namedScratchpadToggle scratchpadId notifyMsg = do
    U.notifySend notifyMsg
    namedScratchpadAction scratchpads scratchpadId
