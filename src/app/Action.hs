module Action (Action) where

--Action is a type synonym used to build an Either monad.
type Action a = Either String a
