# purescript-parsing tutorial
This tutorial approaches parsing using the purescript-parsing tutorial.

## Hello, World!

When we parse we wish to transform a string into a data structure that is denser in some sense. In order to do this we rely on the data in the string carrying some structure. In the case the string does not conform to this structure we should report a useful error to the user. In the parser combinator approach we use a newtype wrapper around a function from a `String` (or stringlike) to a `Either ParseError a`.

```purescript
newtype Parser a = Parser (String -> Either ParseError a)

```
The newtype allows us to define an instance of `Monad` which makes allows composition of parsers.
    
In the below example we use our first parser, `string`. Our input has only one valid form, the string "Hello, World". This is parsed to the data type `Token`, which for now only has one constructor, `HelloWorld`. 

```purescript
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string, eof)


type SParser a = Parser String a 

data Token = HelloWorld 

instance showToken :: Show Token where
    show = case _ of 
        HelloWorld -> "HELLOWORLD"

helloP :: SParser Token
helloP = do 
    token <- HelloWorld <$ string "Hello, World" 
    eof
    pure token

main :: Effect Unit
main = do
  log $ show $ runParser "Hello, World" helloP 
```
This has the intended behavior mostly. If we change a character in the input,
we will get an error. However change it to "Hello, World, Hi", we don't get an error!
This is because the parser consumes only the "Hello, World" part, the ", Hi" is left unconsumed.
If we want to make sure that all input is consumed we can combine it with another parser as such:

```purescript

helloP :: SParser Token
helloP = do 
    token <- HelloWorld <$ string "Hello, World"
    eof
    pure token

```

The monad instance for parsers will sequence the parsers so they consume the remaining input 
of the last parser. In this case the eof parser returns a `Left ParseError` if its consumed output is non-empty, and otherwise returns `Right Unit`.


