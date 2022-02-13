module Parser.Error where

unexpectedInput e a = "Expecting " ++ show e ++ " but got " ++ show a

unexpectedToken e = unexpectedInput ("token " ++ show e)