module Password where

import Text.Regex.PCRE

inc :: String -> String
inc = undefined

requirements :: String -> Bool
requirements inp = all ($ inp) [straight, unambiguous, twoPairs]

straight = undefined

unambiguous = undefined

twoPairs = undefined
