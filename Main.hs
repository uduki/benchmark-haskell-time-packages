{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import           Control.Applicative
import           Control.DeepSeq       (NFData (..), deepseq, ($!!))
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Text             ()
import qualified Data.Text.Encoding    as TE
import qualified Data.Thyme            as Thyme
import qualified Data.Thyme.Time       as Thyme
import           Data.Time
import           Data.Time.Exts
import qualified Data.UnixTime         as UT
import           Foreign.C.Types       (CTime (..))
import           Network.HTTP.Date
import           System.IO             (IOMode (..), openFile)
import           System.Locale
import           Text.Shakespeare.Text (st)


main :: IO ()
main = do
    putStrLn "[Bench : Parse]"
    benchParse
    putStrLn "[Bench : Print]"
    benchPrint
    putStrLn "[Bench : Compare]"
    benchComp
    return ()

benchParse :: IO ()
benchParse = do
    parseDateTimeString "unix-time" parseTextUnixTime
    parseDateTimeString "http-date" parseTextHttpDate
    parseDateTimeString "time" parseTextTime
    parseDateTimeString "thyme" parseTextThyme
    parseDateTimeString "time-exts" parseTextTimeExts

benchPrint :: IO ()
benchPrint = do
    printDateTimeString "unix-time" printTextUnixTime
    printDateTimeString "http-date" printTextHttpDate
    printDateTimeString "time" printTextTime
    printDateTimeString "thyme" printTextThyme
    printDateTimeString "time-exts" printTextTimeExts

benchComp :: IO ()
benchComp = do
    compareDateTime "unix-time" parseTextUnixTime compUnixTime
    compareDateTime "http-date" parseTextHttpDate' compHttpDate
    compareDateTime "time" parseTextTime compTime
    compareDateTime "thyme" parseTextThyme compThyme
    compareDateTime "time-exts" parseTextTimeExts compTimeExts

---------------------------------------------
-- ベンチマーク対象
---------------------------------------------

-- パース
parseTextUnixTime :: BS.ByteString -> Maybe UT.UnixTime
parseTextUnixTime = Just . UT.parseUnixTimeGMT UT.webDateFormat

parseTextHttpDate :: BS.ByteString -> Maybe (Int, Int, Int, Int, Int, Int, Int)
parseTextHttpDate t = (\d -> (hdYear d, hdMonth d, hdDay d, hdWkday d, hdHour d, hdMinute d, hdSecond d)) <$> parseHTTPDate t

parseTextHttpDate' :: BS.ByteString -> Maybe HTTPDate
parseTextHttpDate' = parseHTTPDate

parseTextTime :: BS.ByteString -> Maybe UTCTime
parseTextTime = parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" . BS.unpack

parseTextThyme :: BS.ByteString -> Maybe Thyme.UTCTime
parseTextThyme = Thyme.parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" . BS.unpack

parseTextTimeExts :: BS.ByteString -> Maybe UnixDateTime
parseTextTimeExts = either (const Nothing) Just . parseUnixDateTime "%a, %d %b %Y %H:%M:%S GMT" . TE.decodeUtf8

-- 文字列生成
printTextUnixTime :: IO BS.ByteString
printTextUnixTime = UT.formatUnixTimeGMT UT.webDateFormat <$> UT.getUnixTime

printTextHttpDate :: IO BS.ByteString
printTextHttpDate = formatHTTPDate . epochTimeToHTTPDate . UT.toEpochTime <$> UT.getUnixTime

printTextTime :: IO BS.ByteString
printTextTime = BS.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

printTextThyme :: IO BS.ByteString
printTextThyme = BS.pack . Thyme.formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> Thyme.getCurrentTime

printTextTimeExts :: IO BS.ByteString
printTextTimeExts = TE.encodeUtf8 . p . toDateTimeStruct <$> getCurrentUnixDateTime
  where
    p (DateTimeStruct y m d w h mi s) = [st|#{show w}, #{show d} #{show m} #{show y} #{show h}:#{show mi}:#{show s} GMT|]

-- 現在時刻生成して比較
compUnixTime :: UT.UnixTime -> IO Bool
compUnixTime t = (t >) <$> UT.getUnixTime

compHttpDate :: HTTPDate -> IO Bool
compHttpDate t = (t >) . epochTimeToHTTPDate . UT.toEpochTime <$> UT.getUnixTime

compTime :: UTCTime -> IO Bool
compTime t = (t >) <$> getCurrentTime

compThyme :: Thyme.UTCTime -> IO Bool
compThyme t = (t >) <$> Thyme.getCurrentTime

compTimeExts :: UnixDateTime -> IO Bool
compTimeExts t = (t >) <$> getCurrentUnixDateTime

---------------------------------------------
-- ベンチマーク
---------------------------------------------

c :: Int
c = 1000000 -- 1,000,000

file :: FilePath
file = "/dev/null"

instance NFData CTime where
    rnf (CTime a) = a `deepseq` ()

instance NFData UT.UnixTime where
    rnf (UT.UnixTime a b) = a `deepseq` b `deepseq` ()

withCount :: IO s -> (s -> IO ()) -> String -> IO ()
withCount initialize f name = do
    state <- initialize
    start <- Thyme.getCurrentTime
    f state
    end <- Thyme.getCurrentTime
    let diff = end `Thyme.diffUTCTime` start
    putStrLn $ concat [name, ": ", show diff]
    putStrLn "------------------------------"

-- 日付文字列のパース処理
-- ここではByteStringからの変換で時間を競って頂く(実践ではタイミングの違いこそあれ、入力文字列はByteStringからの変換である為)。
-- なお、パース対象の文字列は全てunix-timeによって生成する。
parseDateTimeString :: NFData t => String -> (BS.ByteString -> Maybe t) -> IO ()
parseDateTimeString name f = withCount ini go name
  where
    ini = replicateM c (UT.formatUnixTimeGMT UT.webDateFormat <$> UT.getUnixTime)
    go ts = forM_ ts $ \t -> void . return $!! f t

-- 日付文字列の生成処理
-- フォーマット文字列はパースの時と同じです。
-- なお、ここでは自分で勝手に現在時刻から文字列を生成してもらいます。
printDateTimeString :: String -> IO BS.ByteString -> IO ()
printDateTimeString name f = withCount ini go name
  where
    ini = openFile file WriteMode
    go h = forM_ [1..c] $ \_ -> BS.hPutStr h =<< f

-- 日時型の比較処理
compareDateTime :: Ord t => String -> (BS.ByteString -> Maybe t) -> (t -> IO Bool) -> IO ()
compareDateTime name parse comp = withCount ini go name
  where
    ini = fromJust . parse . UT.formatUnixTimeGMT UT.webDateFormat <$> UT.getUnixTime
    go t = forM_ [1..c] $ \_ -> comp t >>= (void . return $!!)

