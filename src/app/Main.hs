module Main where

import Action
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Random
import System.Random.Shuffle
import User

--readUsers opens users.txt and creates a list of users from it.
readUsers :: IO (Action [User])
readUsers = do
    userHandle <- openFile "users.txt" ReadMode
    contents <- hGetContents userHandle
    return $ parseUserList contents

--getUsers parses the user file, checking for errors.
getUsers :: IO [User]
getUsers = do
    us <- readUsers
    case us of
        Left error -> die ("Error: " ++ error ++ ".")
        Right xs -> return xs

--readPreferences opens preferences.txt and creates a list of preference
--assignments from it.
readPreferences :: IO (Action [Preference])
readPreferences = do
    preferenceHandle <- openFile "preferences.txt" ReadMode
    contents <- hGetContents preferenceHandle
    return $ parsePreferenceList contents

--getPreferences parses the preference files and looks for errors.
getPreferences :: IO [Preference]
getPreferences = do
    ps <- readPreferences
    case ps of
        Left error -> die ("Error: " ++ error ++ ".")
        Right xs -> return xs

--combineUserPreferences joins the users and preferences data sets.
combineUserPreferences :: [User] -> [Preference] -> [User]
combineUserPreferences us [] = us
combineUserPreferences us (p : ps) =
    flip combineUserPreferences ps (mapOverUs p us)
    where
        mapOverUs p [] = []
        mapOverUs (Preference (Email e) pf) (u@(User name (Email e') _) : us) =
            if e == e' then
                (User name (Email e') (Just pf)) : us
            else
                u : (mapOverUs p us)

--verifyPreferenceFilesExist ensures that each user has a preference file name
--set and that this file exists.
verifyPreferenceFilesExist :: [User] -> IO ()
verifyPreferenceFilesExist us =
    mapM_ verifyPreferenceFile us
    where
        verifyPreferenceFile (User _ (Email e) (Just (PreferenceFile f))) = do
            fexist <- doesFileExist f
            if fexist then
                return ()
            else
                die (f ++ " does not exist.")
        verifyPreferenceFile (User _ (Email e) Nothing) =
            die (e ++ " has no preference set.")

--assignUsers does the Secret Santa assignment, by zip shuffling two lists of
--users.
assignUsers :: [User] -> IO [Assignment]
assignUsers us = do
    sg1 <- newStdGen
    sg2 <- newStdGen
    k <- assignOver
                (shuffle' us (length us) sg1)
                (shuffle' us (length us) sg2)
                []
    if isJust k then return (fromJust k)
    else assignUsers us
    where
        assignOver [] [] as = return (Just as)
        assignOver o@(u@(User _ (Email e) _) : [])
                   o'@(u'@(User _ (Email e') _) : []) as =
            if e == e' then return Nothing
            else return $ Just (Assignment u u' : as)
        assignOver o@(u@(User _ (Email e) _) : us)
                   o'@(u'@(User _ (Email e') _) : us') as =
            if e == e' then doShuffle o o' as
            else doShuffle us us' (Assignment u u' : as)
        doShuffle u1 u2 as = do
            g1 <- newStdGen
            g2 <- newStdGen
            assignOver
                (shuffle' u1 (length u1) g1)
                (shuffle' u2 (length u2) g2)
                as

--the setup command
doSetup :: IO ()
doSetup = do
    die "setup is not yet implemented."

--the assign command
doAssign :: [String] -> IO ()
doAssign as = doAssignment

--doAssignment is the meat and potatoes of the assign command.
doAssignment :: IO ()
doAssignment = do
    us <- getUsers
    ps <- getPreferences
    let cs = combineUserPreferences us ps
    verifyPreferenceFilesExist cs
    putStrLn "Found users and preferences:"
    mapM_ (putStrLn.show) cs
    as <- assignUsers cs
    putStrLn ""
    putStrLn "Set assignments:"
    mapM_ (putStrLn.show) as

--the arbitrate command
doArbitration :: [String] -> IO ()
doArbitration as = die "arbitrate is not yet implemented."

--the usage / help command
printUsage :: Bool -> IO ()
printUsage shouldFail = die "help is not yet implemented."

main :: IO ()
main = do
    args <- getArgs
    parse args
    where
        parse ["setup"] = doSetup
        parse ("assign" : as) = doAssign as
        parse ("arbitrate" : as) = doArbitration as
        parse [] = printUsage True
        parse ["help"] = printUsage False
        parse ["-h"] = printUsage False
        parse _ = printUsage True
