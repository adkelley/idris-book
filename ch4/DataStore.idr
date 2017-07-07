module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore


data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit


size : DataStore -> Nat
size (MkData size' _) = size'


items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem =
  MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items') = item :: addToData items'


parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let storeItems = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just ((index id storeItems ++ "\n"), store)


getId : (store : DataStore) -> (entry: String) -> Nat
getId store entry =
  length $ takeWhile (\e => e /= entry) (toList $ items store)


searchResults : (store : DataStore) -> (inp : String) -> Maybe (String, DataStore)
searchResults store inp =
  let
    results = snd $ Data.Vect.filter (\item => Strings.isInfixOf inp item) (items store)
    resultIds = map (\result => getId store result) results
    resultsStr =
      (concat $ toList $ Data.Vect.intersperse ", " $ zipWith (\a, b => show a ++ ": " ++ show b) resultIds results) ++ "\n"
  in
    Just (resultsStr, store)


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) =>
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just Size => Just ("Size is: " ++ show (size store) ++ "\n", store)
    Just (Search inp)=> searchResults store inp
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
