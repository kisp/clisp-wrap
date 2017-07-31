{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DoAndIfThenElse #-}
module Main (main)
where

import Control.Monad
import System.Process
import System.Posix.Process (executeFile)
import System.IO
import System.Exit
import System.Directory
import System.Environment
import Data.List

import Crypto.Hash.MD5 (hash)

import Data.ByteString (pack, unpack)
import Data.Char (ord, toLower)
import Data.Word (Word8)

import Text.Printf

-- EitherT
newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

#if __GLASGOW_HASKELL__ >= 710
instance Monad m => Functor (EitherT a m) where
    fmap  = liftM

instance Monad m => Applicative (EitherT a m) where
    pure  = return
    (<*>) = ap  -- defined in Control.Monad
#endif

instance Monad m => Monad (EitherT a m) where
        return   = EitherT . return . return
        m >>= k  = EitherT $ do
                a <- runEitherT m
                case a of
                        Left  l -> return (Left l)
                        Right r -> runEitherT (k r)

liftEitherT :: Monad m => (x -> Either a b) -> x -> EitherT a m b
liftEitherT f = EitherT . return . f

-- utils
word8hex :: Word8 -> String
word8hex = printf "%02x"

md5 :: String -> String
md5 = concatMap word8hex . unpack . hash . pack . map (fromIntegral . ord)

-- system names to hash
systemsHash :: [String] -> String
systemsHash names = md5 $ concat $ sort (map l names)
    where l = map toLower

-- clisp
clisp :: String
clisp = "clisp"

clispScript :: String -> [String] -> String
clispScript imagePath systems = intercalate "\n" lines
    where lines = [ "(setq *debugger-hook* (lambda (c h) (declare (ignore h)) (format *error-output* \"ERROR of type ~A: ~A~%\" (type-of c) c) (ext:quit 1)))" ] ++
                  map loadSystem systems ++
                  [ "(ensure-directories-exist \"" ++ imagePath ++ "\")"
                  , "(ext:saveinitmem \"" ++ imagePath ++ "\")" ]
          loadSystem name =  "(asdf:load-system \"" ++ name ++ "\")"

-- IO
handleToDevNull :: IO Handle
handleToDevNull = openFile "/dev/null" WriteMode

makeImage :: String -> [String] -> IO (Either (String, Int) String)
makeImage imagePath systems = do
  devNull <- handleToDevNull
  (Just hIn, _, _, p) <- createProcess
                         (proc clisp [])
                         { std_out = UseHandle devNull
                         , std_err = UseHandle devNull
                         , std_in  = CreatePipe
                         , close_fds = False }
  let script = clispScript imagePath systems
  hPutStrLn hIn script
  hClose hIn
  code <- waitForProcess p
  hClose devNull
  case code of
    ExitSuccess -> return $ Right imagePath
    ExitFailure c -> return $ Left ("clisp image builder for `" ++
                                    intercalate ", " systems ++
                                    "' returned " ++ show c,
                                    77)

ensureImage :: [String] -> IOErr String
ensureImage systems = EitherT $ do
  let hash = systemsHash systems
  cacheDirectory <- getCacheDirectory
  let imagePath = cacheDirectory ++ "/" ++ hash ++ ".mem"
  imageExists <- doesFileExist imagePath
  if imageExists then
     return $ Right imagePath
  else
     makeImage imagePath systems

getCacheDirectory :: IO FilePath
getCacheDirectory =  liftM (++ "/.cache/clisp-wrap") getHomeDirectory

ensureAtLeastOne :: [String] -> Either (String, Int) ()
ensureAtLeastOne (_:_) = Right ()
ensureAtLeastOne _     = Left ("no arguments given", 88)

ensureDoubleHyphen :: [String] -> Either (String, Int) ()
ensureDoubleHyphen s | "--" `elem` s = Right ()
                     | otherwise     = Left ("missing separator `--'", 88)

ensureClispScript :: [String] -> Either (String, Int) ()
ensureClispScript (_:_)  = Right ()
ensureClispScript _      = Left ("clispScript argument missing", 88)

splitFirst :: [String] -> [String]
splitFirst (h:t) = words h ++ t

parseArgs :: [String] -> Either (String, Int) SystemsAndClispCall
parseArgs args = do
  ensureAtLeastOne args
  let args' = splitFirst args
  ensureDoubleHyphen args'
  let (systemNames, "--":tail) =
          break (== "--") args'
  ensureClispScript tail
  let (clispScript:clispArgs) = tail
  return (systemNames, clispScript, clispArgs)

parseArgsAndEnsureImage :: [String] -> IOErr ImagePathAndClispCall
parseArgsAndEnsureImage args = do
  (systemNames, clispScript, clispArgs) <- liftEitherT parseArgs args
  imagePath <- ensureImage systemNames
  return (imagePath, clispScript, clispArgs)

putClispWrapMessage :: String -> String -> IO ()
putClispWrapMessage tag message =
  putStrLn $ "[clisp-wrap] " ++ tag ++ ": " ++ message

main :: IO ()
main = do
  args <- getArgs
  res <- runEitherT $ parseArgsAndEnsureImage args
  case res of
    Left (message, code) -> putClispWrapMessage "INFO" ("called with args " ++ show args) >>
                            putClispWrapMessage "ERROR" message >>
                            exitWith (ExitFailure code)
    Right imagePathAndClispCall -> execClisp imagePathAndClispCall

execClisp :: ImagePathAndClispCall -> IO ()
execClisp (imagePath, clispScript, clispArgs) =
    executeFile clisp
              True
              (["-q", "-norc", "-M", imagePath, clispScript] ++ clispArgs)
              Nothing

type IOErr a = EitherT (String, Int) IO a
type ImagePathAndClispCall = (String, String, [String])
type SystemsAndClispCall = ([String], String, [String])
