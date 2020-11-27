module User (
    Name (Name), Email (Email), User (User), PreferenceFile (PreferenceFile),
    Preference (Preference), Assignment (Assignment),
    userParser, nameParser, emailParser, parseUserList, parsePreferenceList
    ) where

import Action
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.Parsec.Extra

--Name wraps a String, representing a user's name.
data Name = Name String

--Name is trivially displayed.
instance Show Name where
    show (Name name) = name

--Email wraps a String, representing a user's e-mail address.
data Email = Email String

--Email is trivially displayed.
instance Show Email where
    show (Email email) = email

--PreferenceFile wraps a String, representing the filename for a user's prefs.
data PreferenceFile = PreferenceFile String

--PreferenceFile is trivially displayed.
instance Show PreferenceFile where
    show (PreferenceFile file) = file

--User is a simple record of Name, Email, and maybe PreferenceFile.
data User = User Name Email (Maybe PreferenceFile)

--User is structurally displayed.
instance Show User where
    show (User name email (Just file)) =
        show name ++ " <" ++ show email ++ ">" ++ " ("
            ++ show file ++ ")"
    show (User name email Nothing) =
        show name ++ " <" ++ show email ++ ">" ++ " preference not set."

--Preference assigns an Email to a preference file.
data Preference = Preference Email PreferenceFile

--Preference is structurally displayed.
instance Show Preference where
    show (Preference email file) = show email ++ " " ++ show file

--Assignment implies that the first User is shopping for the second User.
data Assignment = Assignment User User

--Assignment is displayed in prose (for debugging only).
instance Show Assignment where
    show (Assignment (User (Name n1) _ _) (User (Name n2) _ _)) =
        "User " ++ n1 ++ " is secret Santa for user " ++ n2

--The userParser builds a User in a successful parse.
userParser :: Parser User
userParser = User <$> nameParser <*> emailParser <*> pure Nothing

--The preferenceParser builds a Preference with a successful parse.
preferenceParser :: Parser Preference
preferenceParser = Preference <$> emailParser <*> filenameParser

--The nameParser parses a Name as the starting sequence in an e-mail address.
nameParser :: Parser Name
nameParser = Name <$> (strip <$> many1 (noneOf "<"))

--The emailParser parses an e-mail address as being between two angle brackets.
emailParser :: Parser Email
emailParser =
    (Email <$> (oneOf "<" *> many1 (noneOf ">") <* oneOf ">")) <* spaces

--The filename parser parses a filename as being anything after the e-mail.
filenameParser :: Parser PreferenceFile
filenameParser =
    PreferenceFile <$> (strip <$> manyTill anyChar (try eol))

--The parseUserList function parses the contents of a user.txt file.
parseUserList :: String -> Action [User]
parseUserList contents =
    case parse (many1 (userParser <* spaces)) "users" contents of
        Left err -> Left $ show err
        Right u -> Right u

--The parsePreferenceList function parses the contents of a preferences.txt
--file.
parsePreferenceList :: String -> Action [Preference]
parsePreferenceList contents =
    case parse (many1 (preferenceParser <* spaces)) "preferences" contents of
        Left err -> Left $ show err
        Right p -> Right p
