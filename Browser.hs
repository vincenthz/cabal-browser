{-# LANGUAGE ViewPatterns #-}
import Control.Monad.Trans
import Data.Char hiding (Control)
import Data.List (sort,sortBy, groupBy, unfoldr, intercalate)
import Control.Arrow (second)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.ModelView as New
import System.Environment
import System.IO
import System.Process
import System.Directory

import Data.IORef
import Data.Tree

pkgCats = [ "All Packages", "User Packages", "System Packages", "Favorites" ]

type PackageVersion = [Integer]
type PackageDescOne = (String,PackageVersion)
data PackageDesc = PackageDesc
    { pdescName     :: String
    , pdescVersions :: [PackageVersion]
    } deriving (Show,Eq)

listUnmarshall :: String -> [Tree String]
listUnmarshall s = makeForest $ groupByName $ map toDesc $ sortCI $ words s where
    toDesc :: String -> PackageDescOne
    toDesc package = case reverse $ splitOn '-' $ package of
        (ver:r) -> (intercalate "-" $ reverse r, map read $ splitOn '.' ver)
        _       -> (package, [])
    groupByName :: [PackageDescOne] -> [PackageDesc]
    groupByName = map (\l -> PackageDesc (fst $ head l) (reverse $ sort $ map snd l)) . groupBy (\a b -> fst a == fst b)
    splitOn delim = takeWhile (not . null) . unfoldr (Just . (second $ drop 1) . break (==delim))

listAllPackages = do
    (_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--simple-output" ] ""
    return $ listUnmarshall out

listUserPackages = do
    (_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--user", "--simple-output" ] ""
    return $ listUnmarshall out

listSystemPackages = do
    (_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--global", "--simple-output" ] ""
    return $ listUnmarshall out

listField :: String -> String -> IO [String]
listField package field = do
    (_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "field", package, field ] ""
    return $ map (drop 2 . snd . break ((==) ':')) $ lines out

makeForest :: [PackageDesc] -> [Tree String]
makeForest pkgs =
    map (\(n, l) -> Node { rootLabel = [n], subForest = map toPackage l }) $
    groupByAlphabet pkgs
    where
        groupByAlphabet :: [PackageDesc] -> [(Char, [PackageDesc])]
        groupByAlphabet =
            map (\l -> (fst (head l), map snd l)) .
            groupBy (\a b -> fst a == fst b) .
            map (\pdesc -> (toUpper $ head $ pdescName pdesc,pdesc))
        toPackage (PackageDesc name ver) = Node { rootLabel = name, subForest = map (toLeaf . intercalate "." . map show) ver }
        toLeaf x = Node { rootLabel = x, subForest = [] }

sortCI = sortBy (\a b -> compare (map toLower a) (map toLower b))

setupView view model = do
    treeViewSetHeadersVisible view False

    col      <- treeViewColumnNew
    renderer <- cellRendererTextNew
    treeViewColumnPackStart col renderer True
    cellLayoutSetAttributes col renderer model $ \row -> [ cellText := row ]
    treeViewColumnSetTitle col "Package"
    treeViewAppendColumn view col

modelRefresh model l = treeStoreInsertForest model [] 0 l

loadHaddock disconnectedVar wv name ver = do
    let v = name ++ "-" ++ ver
    x <- listField v "haddock-html"
    let filepath = head x ++ "/index.html"

    --exists <- doesDirectoryExist filepath
    disconnected <- readIORef disconnectedVar
    if disconnected
        then webViewLoadUri wv ("file://" ++ filepath)
        else webViewLoadUri wv ("http://hackage.haskell.org/package/" ++ v)

main :: IO ()
main = do
    pkgs <- listAllPackages
    disconnectedVar <- newIORef False

    home <- getEnv "HOME"
    let url = "file://" ++ home ++ "/.cabal/share/doc/index.html"

    _      <- initGUI

    window <- windowNew
    vbox   <- vBoxNew False 2
    vboxl  <- vBoxNew False 5
    swpkg  <- scrolledWindowNew Nothing Nothing
    swwv   <- scrolledWindowNew Nothing Nothing
    wv     <- webViewNew
    mbar   <- toolbarNew
    hpaned <- hPanedNew
    btHome <- toolButtonNewFromStock stockHome
    toolbarInsert mbar btHome 0
    btConnect <- toggleToolButtonNewFromStock stockConnect
    toolbarInsert mbar btConnect 1
    view   <- treeViewNew
    pkgCombo <- comboBoxNew

    comboBoxSetModelText pkgCombo
    mapM_ (comboBoxAppendText pkgCombo) pkgCats
    comboBoxSetActive pkgCombo 0

    toolbarSetStyle mbar ToolbarIcons

    set window
        [ containerChild       := vbox
        , windowDefaultWidth   := 500
        , windowDefaultHeight  := 400
        , containerBorderWidth := 2
        ]
    set swwv [ containerChild := wv ]
    set swpkg [ containerChild := view ]

    boxPackStart vboxl pkgCombo PackNatural 0
    boxPackStart vboxl swpkg PackGrow 0

    panedAdd1 hpaned vboxl
    panedAdd2 hpaned swwv
    panedSetPosition hpaned 300

    boxPackStart vbox mbar PackNatural 0
    boxPackStart vbox hpaned PackGrow 0

    store <- treeStoreNew pkgs
    treeViewSetModel view store
    setupView view store

    webViewLoadUri wv url

    onDestroy window mainQuit
    onToolButtonClicked btHome (webViewLoadUri wv url)
    onToolButtonToggled btConnect $ do
        modifyIORef disconnectedVar not
        disconnected <- readIORef disconnectedVar
        toolButtonSetStockId btConnect (Just $ if disconnected then stockDisconnect else stockConnect)

    onRowActivated view (\treepath _ -> do
        case length treepath of
            2 -> do
                name <- treeStoreGetValue store treepath
                ver <- treeStoreGetValue store (treepath ++ [0])
                loadHaddock disconnectedVar wv name ver
            3 -> do
                name <- treeStoreGetValue store $ init treepath
                ver  <- treeStoreGetValue store treepath
                loadHaddock disconnectedVar wv name ver
            _ -> return ()
        )
    window `on` keyPressEvent $ tryEvent $ do
        [Control] <- eventModifier
        "q"       <- eventKeyName
        liftIO mainQuit

    on pkgCombo changed (do
        i <- comboBoxGetActive pkgCombo
        treeStoreClear store
        case i of
            2 -> listSystemPackages >>= modelRefresh store
            1 -> listUserPackages   >>= modelRefresh store
            _ -> listAllPackages    >>= modelRefresh store
        )

    widgetShowAll window

    mainGUI
