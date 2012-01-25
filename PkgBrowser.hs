import Control.Monad.Trans
import Data.Char hiding (Control)
import Data.List (sortBy, groupBy)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.ModelView as New
import System.Environment
import System.IO
import System.Process
import System.Directory

import Data.Tree

pkgCats = [ "All Packages", "User Packages", "System Packages", "Favorites" ]

listAllPackages :: IO [String]
listAllPackages = do
	(_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--simple-output" ] ""
	return $ words out

listUserPackages :: IO [String]
listUserPackages = do
	(_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--user", "--simple-output" ] ""
	return $ words out

listSystemPackages :: IO [String]
listSystemPackages = do
	(_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--global", "--simple-output" ] ""
	return $ words out

listField :: String -> String -> IO String
listField package field = do
	(_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "field", package, field ] ""
	return $ drop 2 $ snd $ break ((==) ':') out

setupView view model = do
	treeViewSetHeadersVisible view False

	col      <- treeViewColumnNew
	renderer <- cellRendererTextNew
	treeViewColumnPackStart col renderer True
	cellLayoutSetAttributes col renderer model $ \row -> [ cellText := row ]
	treeViewColumnSetTitle col "Package"
	treeViewAppendColumn view col

modelRefresh model l = treeStoreInsertForest model [] 0 l

sortCI = sortBy (\a b -> compare (map toLower a) (map toLower b))

makeForest pkgs =
	map (\(n, l) -> Node { rootLabel = [n], subForest = map toLeaf $ sortCI l }) $
	map (\l -> (fst (head l), map snd l)) $
	groupBy (\a b -> fst a == fst b) $
	map (\x -> (toUpper $ head x,x)) $ sortCI pkgs
	where
		toLeaf x = Node { rootLabel = x, subForest = [] }

main :: IO ()
main = do
	pkgs <- listAllPackages

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

	store <- treeStoreNew $ makeForest pkgs
	treeViewSetModel view store
	setupView view store

	webViewLoadUri wv url

	onDestroy window mainQuit
	onToolButtonClicked btHome (webViewLoadUri wv url)
	onRowActivated view (\treepath _ -> do
		v <- treeStoreGetValue store treepath
		x <- listField v "haddock-html"
		let filepath = x ++ "/index.html"

		exists <- doesDirectoryExist filepath
		exists <- return True
		if exists
			then webViewLoadUri wv ("file://" ++ filepath)
			else webViewLoadUri wv ("http://hackage.haskell.org/package/" ++ v)
		)
	window `on` keyPressEvent $ tryEvent $ do
		[Control] <- eventModifier
		"q"       <- eventKeyName
		liftIO mainQuit

	on pkgCombo changed (do
		i <- comboBoxGetActive pkgCombo
		treeStoreClear store
		case i of
			2 -> listSystemPackages >>= modelRefresh store . makeForest
			1 -> listUserPackages   >>= modelRefresh store . makeForest
			_ -> listAllPackages    >>= modelRefresh store . makeForest
		)

	widgetShowAll window

	mainGUI
