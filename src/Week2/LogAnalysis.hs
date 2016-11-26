module Week2.LogAnalysis
  (parseMessage
  ,parse
  ,insert
  ,build
  ,inOrder
  ,whatWentWrong)
  where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage str =
    case wordList of
        ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
        ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
        ("E":level:ts:msg) ->
            LogMessage (Error (read level)) (read ts) (unwords msg)
        _ -> Unknown (unwords wordList)
  where
    wordList = words str

parse :: String -> [LogMessage]
parse  = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg1@(LogMessage _ ts1 _) (Node left logMsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left logMsg2 (insert logMsg1 right)
  | otherwise = Node (insert logMsg1 left) logMsg2 right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = (inOrder left) ++ [logMsg] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter isWrong
  where
    getMsg (LogMessage _ _ msg) = msg
    getMsg _ = ""

    isWrong (LogMessage (Error level) _ _)
      | level >= 50 = True
      | otherwise = False
    isWrong _ = False
