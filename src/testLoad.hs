import qualified LoadAssociation 
import qualified LoadSettings

main = do
--     wordAssociationTableContentObject <- LoadAssociation.makeNewWordAssociation
--     print $ LoadAssociation.getItem "a2" wordAssociationTableContentObject
    settingsObject <- LoadSettings.readSettingsFile
    print $ LoadSettings.getItem "igrac1_ime" settingsObject
    
