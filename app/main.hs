import Prelude
import Yesod.Default.Config
import Yesod.Default.Main
import Settings             (parseExtra)
import Application          (makeApplication)
import Network.Wai.Handler.CGI (run)

main :: IO ()
--main = defaultMainLog (fromArgs parseExtra) makeApplication
main = do
	config <- loadConfig $ (configSettings Production) {csParseExtra = parseExtra}
	(app, _) <- makeApplication config
	run app
--main = do
--	(app, _) <- fromArgs parseExtra >>= makeApplication
--	run app
