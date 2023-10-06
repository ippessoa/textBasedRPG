module Main (main) where
import Data.List
import Data.Maybe

data GameObject = Player | Key | Knife | Doll | Rosary | Cross | Holywater | Safe
    deriving (Eq, Show) 
data Room = Room Description [GameObject]
    deriving (Show)
type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)

initialState :: GameState
initialState = 
    ([Room "You are in a cold kitchen filled with cobwebs and dust covering the countertops." [Knife, Key]
    , Room "You are at the children's bedroom with a creepy doll on the bed, it sends a chill up your spine!" [Doll, Holywater, Safe]
    , Room "You are in a dark and eerie living room. It echoes with the unsettling remnants of its past." [Player, Cross]
    ]
    , [Rosary])

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Everything is dark..."
    putStrLn ""
    putStrLn "A loud noise wakes you up!" 
    putStrLn "It seems to be that you passed out in a haunted mansion!"
    putStrLn "You don't remember exactly how you ended up here"
    putStrLn ""
    putStrLn "You look around and see a hooded figure! They see that you are holding a rosary. "
    putStrLn "Realizing you are a man of faith, they suddenly turn to you and beg you to help free their family!"
    putStrLn "And then they just vanish right in front of you"
    putStrLn ""
    gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop (rooms, currentInv) = do
    let currentRoom = 
            case findRoomWithPlayer rooms of
            Just r -> r
            Nothing -> error $ "Somehow the player "
                             ++ "ended up outside the map!"
        currentRoomDesc = getRoomDescription currentRoom
        possibleCmds = 
            validCommands currentRoom currentRoomDesc currentInv
    if playerWon (rooms, currentInv)
    then gameOverRestart
    else do
        describeWorld currentRoom currentInv possibleCmds
        takeActionThenLoop
            currentRoom currentInv possibleCmds rooms

getRoomDescription :: Room -> String
getRoomDescription (Room desc _) = desc

findRoomWithPlayer :: [Room] -> Maybe Room
findRoomWithPlayer rooms = find (\(Room _ gameObjs) ->
                                   any (== Player) gameObjs)
                                rooms

validCommands :: Room -> String -> Inventory -> [String]
validCommands (Room desc gameObjs) currentRoom invItems  = 
        moveCommands ++ takeRosaryCommand ++ holdupCrossCommand ++ prayRosaryCommand ++ takeKeyCommand ++ takeDollCommand 
        ++ takeKnifeCommand ++ useKnifeCommand ++ startExorcism ++ openSafeCommand ++ takeCrossCommand ++ useKeyCommand ++ ["quit"]
    where 
        moveCommands =
            ["b" | currentRoom /= "You are at the children's bedroom with a creepy doll on the bed, it sends a chill up your spine!"] ++
            ["k" | currentRoom /= "You are in a cold kitchen filled with cobwebs and dust covering the countertops."] ++
            ["l" | currentRoom /= "You are in a dark and eerie living room. It echoes with the unsettling remnants of its past."]
        takeKeyCommand = 
            if elem Key gameObjs && Safe `notElem` gameObjs
            then ["key"]
            else []
        takeKnifeCommand = 
            if elem Knife gameObjs
            then ["knife"]
            else []
        takeDollCommand = 
            if elem Doll gameObjs
            then ["doll"]
            else []
        openSafeCommand = 
            if elem Key invItems && any (==Safe) gameObjs
            then ["open safe"]
            else []
        useKeyCommand =
            if elem Key invItems && Safe `notElem` gameObjs
            then ["usekey"]
            else []
        takeRosaryCommand = 
            if elem Rosary gameObjs
            then ["rosary"]
            else []
        holdupCrossCommand = 
            if elem Cross invItems
            then ["usecross"]
            else []
        startExorcism =
            if elem Cross invItems && elem Rosary invItems && elem Holywater invItems && any (==Doll) gameObjs
            then ["start exorcism"]
            else []
        takeCrossCommand = 
            if elem Cross gameObjs
            then ["cross"]
            else []
        prayRosaryCommand = 
            if elem Rosary invItems && notElem Holywater invItems && notElem Doll gameObjs
            then ["pray"]
            else []
        useKnifeCommand = 
            if elem Knife invItems
            then ["useknife"]
            else []

somethingToTake :: [GameObject] -> Bool
somethingToTake = any (/=Player)

playerWon :: GameState -> Bool
playerWon (rooms, currentInv) = 
    any hasRosaryAndBedroom rooms
  where hasRosaryAndBedroom (Room description gameObjs) = 
          description == "You are at the children's bedroom with a creepy doll on the bed, it sends a chill up your spine!"
          && any (==Rosary) gameObjs 
          && any (==Holywater) gameObjs
          && any (==Cross) gameObjs

gameOverRestart :: IO()
gameOverRestart = do
    putStrLn ""
    putStrLn "You quickly stand up and identify that the demon is possessing the doll!"
    putStrLn "You point your cross towards it and start praying the rosary again!"
    putStrLn ""
    putStrLn "As soon as you finished the 'Hail Holy Queen' prayer you hear a horrifying screech from the demon!"
    putStrLn "As a final blow, you break the holy water bottle on the doll's head splashing all over it and on the bed!"
    putStrLn "The doll starts levitating and then falls on the bed. The demon has been expelled!"
    putStrLn ""
    putStrLn "The ghost of an old lady appears out of nowhere and thanks you: "
    putStrLn "I'll be eternally grateful! Now I can finally liberate my dear family members! "
    putStrLn ""
    putStrLn "The wardrobe's doors suddenly open revealing the skeletons of her family! "
    putStrLn ""
    putStrLn "Alongside the bones and spiderwebs you find lots of gold and jewelry! "
    putStrLn "As a token of gratitude you open a museum where the house is to honor "
    putStrLn "the ghost's family history and legacy."
    putStrLn ""
    putStrLn "The END"
    putStrLn ""
    putStrLn "Do you want to play again? y = yes"
    playAgain <- getLine
    if playAgain == "y"
    then gameLoop initialState
    else putStrLn "Thanks for playing!"

youDie :: IO()
youDie = do
    putStrLn "Do you want to play again? y = yes"
    playAgain <- getLine
    if playAgain == "y"
    then gameLoop initialState
    else putStrLn "Goodbye!"

getCommand :: IO String
getCommand = do
    putStrLn ""
    putStrLn "What do you want to do? or where do you want to go?"
    putStrLn "b for bedroom"
    putStrLn "k for kitchen"
    putStrLn "l for living room"
    putStrLn "enter name of the object to take or use"
    putStrLn "or quit to give up!"
    putStrLn ""
    getLine

takeActionThenLoop :: Room -> 
                      Inventory ->
                      [String] ->
                      [Room] ->
                      IO()
takeActionThenLoop currentRoom
                   currentInv
                   possibleCmds
                   rooms =
    do
        command <- getCommand
        if any (==command) possibleCmds
        then case command of
            "b" -> 
                do
                    putStrLn "You go upstairs to the bedroom..."
                    gameLoop $ moveToBedroom (rooms, currentInv)
            "k" -> 
                do
                    putStrLn "You go to the kitchen..."
                    gameLoop $ moveToKitchen (rooms, currentInv)
            "l" -> 
                do
                    putStrLn "You go to the living room..."
                    gameLoop $ moveToLivingRoom (rooms, currentInv)
            "doll" -> 
                do
                    putStrLn "You hold the doll in your arms..."
                    putStrLn "The doll's neck turns 180 degrees and its eyes starts glowing red!"
                    putStrLn "Your soul is painfully sucked and your body dries up till you die miserably!"
                    putStrLn "GAME OVER!"
                    youDie
            "pray" -> 
                do
                    putStrLn "Hail Mary, Full of Grace the Lord is with Thee, Blessed art Thou amongst women..."
                    putStrLn "A demon possessing the house becomes enraged and throw you against the wall!"
                    putStrLn "You die a quick death... so sad you fought bravely!"
                    youDie
            "rosary" -> 
                do
                    putStrLn "You pick up the rosary..."
                    gameLoop $ 
                        moveRosaryToInventory (rooms, currentInv)
            "cross" -> 
                do
                    putStrLn "You pick up the cross..."
                    gameLoop $ 
                        moveCrossToInventory (rooms, currentInv)
            "key" -> 
                do
                    putStrLn "You take the key..."
                    gameLoop $ 
                        moveKeyToInventory (rooms, currentInv)
            "open safe" -> 
                do
                    putStrLn "You open the safe to find a bottle of holy water..."
                    gameLoop $
                        moveHolywaterToInventory (moveKeyFromInventory(rooms, currentInv))
            "usekey" -> 
                do
                    putStrLn "You tried to unlock a door with no success..."
                    gameLoop (rooms, currentInv)
            "usecross" -> 
                do
                    putStrLn "You hold up the cross... nothing really happens"
                    gameLoop (rooms, currentInv)
            "start exorcism" -> 
                do
                    putStrLn "You are ready to start the exorcism, you put everything in place and start the ritual!"
                    gameLoop $
                        moveCrossFromInventory(moveRosaryFromInventory(moveHolywaterFromInventory(rooms, currentInv)))
            "knife" ->
                do
                    putStrLn "You take the knife..."
                    gameLoop $ 
                        moveKnifeToInventory (rooms, currentInv)
            "useknife" -> 
                do
                    putStrLn "As you are walking with the knife, you accidentally fall on it and die miserably!"
                    putStrLn "GAME OVER!"
                    youDie
            "quit" -> 
                putStrLn $ "You decide to give up."
                         ++ " The spirit takes revenge and they killed you before you even managed to escape!"
            _ -> 
                do
                    putStrLn "Be careful! Choose wisely!"
                    gameLoop (rooms, currentInv)
        else do
            putStrLn $ "Command not possible here,"
                     ++ " or that is not a command."
            gameLoop (rooms, currentInv)

moveToBedroom :: GameState -> GameState
moveToBedroom (rooms, inv) = 
    let
    (Just playerRoomIndex) = findIndex (\ (Room _ objs) -> any (== Player) objs) rooms
    newRooms = update playerRoomIndex (removeObject Player) rooms
    bedroomIndex = fromJust $ findIndex (\ (Room desc _) -> desc == "You are at the children's bedroom with a creepy doll on the bed, it sends a chill up your spine!") newRooms
    updatedRooms = update bedroomIndex (addObject Player) newRooms
    in (updatedRooms, inv)

moveToKitchen :: GameState -> GameState
moveToKitchen (rooms, inv) = 
    let
    (Just playerRoomIndex) = findIndex (\ (Room _ objs) -> any (== Player) objs) rooms
    newRooms = update playerRoomIndex (removeObject Player) rooms
    kitchenIndex = fromJust $ findIndex (\ (Room desc _) -> desc == "You are in a cold kitchen filled with cobwebs and dust covering the countertops.") newRooms
    updatedRooms = update kitchenIndex (addObject Player) newRooms
    in (updatedRooms, inv)

moveToLivingRoom :: GameState -> GameState
moveToLivingRoom (rooms, inv) = 
    let
    (Just playerRoomIndex) = findIndex (\ (Room _ objs) -> any (== Player) objs) rooms
    newRooms = update playerRoomIndex (removeObject Player) rooms
    livingRoomIndex = fromJust $ findIndex (\ (Room desc _) -> desc == "You are in a dark and eerie living room. It echoes with the unsettling remnants of its past.") newRooms
    updatedRooms = update livingRoomIndex (addObject Player) newRooms
    in (updatedRooms, inv)
    
-- Updates the nth element in a list
update :: Int -> (a -> a) -> [a] -> [a]
update n f xs = take n xs ++ [f (xs !! n)] ++ drop (n+1) xs

-- Removes an object from a room
removeObject :: GameObject -> Room -> Room
removeObject obj (Room desc objs) = Room desc (delete obj objs)

-- Adds an object to a room
addObject :: GameObject -> Room -> Room
addObject obj (Room desc objs) = Room desc (obj:objs)

moveKeyToInventory :: GameState -> GameState
moveKeyToInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            Key : inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            Room description (filter (/=Key) gameObjs)

moveHolywaterFromInventory:: GameState -> GameState
moveHolywaterFromInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            filter (/=Holywater) inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            if any (==Player) gameObjs
            then Room description (Holywater : gameObjs)
            else Room description gameObjs

moveCrossFromInventory :: GameState -> GameState
moveCrossFromInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            filter (/=Cross) inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            if any (==Player) gameObjs
            then Room description (Cross : gameObjs)
            else Room description gameObjs

moveCrossToInventory :: GameState -> GameState
moveCrossToInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            Cross : inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            Room description (filter (/=Cross) gameObjs)

moveKeyFromInventory :: GameState -> GameState -- verificar a necessidade
moveKeyFromInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            filter (/=Key) inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            if any (==Player) gameObjs
            then Room description (Key : gameObjs)
            else Room description gameObjs

moveRosaryToInventory :: GameState -> GameState
moveRosaryToInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            Rosary : inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            Room description (filter (/=Rosary) gameObjs)

moveHolywaterToInventory :: GameState -> GameState
moveHolywaterToInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            Holywater : inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            Room description (filter (/=Holywater) gameObjs)

moveRosaryFromInventory :: GameState -> GameState
moveRosaryFromInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            filter (/=Rosary) inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            if any (==Player) gameObjs
            then Room description (Rosary : gameObjs)
            else Room description gameObjs

moveKnifeToInventory :: GameState -> GameState
moveKnifeToInventory (rooms, inv) = 
        (newRooms, newInv)
    where
        newInv = 
            Knife : inv
        newRooms = 
            map adjustRooms rooms
        adjustRooms (Room description gameObjs) = 
            Room description (filter (/=Knife) gameObjs)

describeWorld :: Room -> 
                 Inventory ->
                 [String] -> 
                 IO ()
describeWorld currentRoom
              currentInv
              possibleCmds = 
    do 
        putStrLn $ describeRoom currentRoom
        putStrLn $ describeInventory currentInv
        putStrLn $ describeCommands possibleCmds

describeRoom :: Room -> String
describeRoom (Room description gameObjs) = 
    description ++ if any (==Key) gameObjs && notElem Doll gameObjs
            then " There is an old copper key lying around."
            else
            if any (==Cross) gameObjs
            then " There is a beautiful silver cross here."
            else
            if any (==Rosary) gameObjs
            then " There is a blessed Saint Benedict rosary here."
            else
            if any (==Holywater) gameObjs
            then " There is a locked safe here."
            else
            if any (==Doll) gameObjs
            then " There is a scary old doll lying on the bed."
            else 
            if any (==Knife) gameObjs
            then " There is an used rusted knife here."
            else " "

describeInventory :: Inventory -> String
describeInventory [] = 
    "You are not carrying anything"
describeInventory inv = 
    "Inventory: " ++ (intercalate ", " (map show inv))

describeCommands :: [String] -> String
describeCommands commands = 
    "Commands: " ++ (intercalate ", " commands)
