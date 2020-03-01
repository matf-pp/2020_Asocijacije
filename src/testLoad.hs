import qualified Load 

main = do
    wordAssociationTableContentObject <- Load.makeNewWordAssociation
    print $ Load.getItem "a2" wordAssociationTableContentObject
