import Control.Monad.Trans
import Data.Char hiding (Control)
import Data.List (sortBy)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import System.Environment
import System.IO
import System.Process
import System.Directory

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

modelRefresh model l = do
	mapM_ (listStoreAppend model) l

sortCI = sortBy (\a b -> compare (map toLower a) (map toLower b))

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
	btHome <- toolbarInsertNewButton mbar 0 stockHome Nothing
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

	boxPackStart vbox mbar PackNatural 0
	boxPackStart vbox hpaned PackGrow 0

	store <- listStoreNew (sortCI pkgs)
	treeViewSetModel view store
	setupView view store

	webViewLoadUri wv url

	onDestroy window mainQuit
	onClicked btHome (webViewLoadUri wv url)
	onRowActivated view (\treepath _ -> do
		let idx = head treepath
		v <- listStoreGetValue store idx
		x <- listField v "haddock-html"
		let filepath = x ++ "/index.html"

		exists <- doesDirectoryExist filepath
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
		listStoreClear store
		case i of
			2 -> listSystemPackages >>= modelRefresh store . sortCI
			1 -> listUserPackages   >>= modelRefresh store . sortCI
			_ -> listAllPackages    >>= modelRefresh store . sortCI
		)

	widgetShowAll window

	mainGUI
