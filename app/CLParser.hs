module CLParser where


--import Options.Applicative
--import Options.Applicative
import Data.Semigroup ((<>), Option (Option))
import Options.Applicative


pFilename :: Parser String
pFilename = strOption
  (  long "filename"
  <> short 'f'
  <> metavar "filename"
  <> help "File that contains your programm" )


newtype Options = Options {filename::String}

pOptions :: Parser Options
pOptions = Options <$> pFilename 

optsDesc :: ParserInfo Options
optsDesc = info (pOptions <**> helper)
      ( fullDesc
     <> progDesc "Compute something with primitive recursive Functions"
     <> header "hello - a test for optparse-applicative" )

parseCommandLine :: IO Options
parseCommandLine = execParser optsDesc
