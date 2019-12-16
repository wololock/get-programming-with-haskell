

robot (name,attack,hp) = \message -> message (name,attack,hp)

killerRobot = robot ("Kill3r",25,200)
gentleGiant = robot ("Mr. Friendly",10,300)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot n = aRobot (\(_,a,h) -> robot (n,a,h))
setAttack aRobot a = aRobot (\(n,_,h) -> robot (n,a,h))
setHP aRobot h = aRobot (\(n,a,_) -> robot (n,a,h))

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack: " ++ show a ++ " hp: " ++ show h)

damage aRobot dmg = aRobot (\(n,a,h) -> robot (n,a,h-dmg))

fight aRobot defender = damage defender attack
  where 
    attack = if getHP aRobot > 10 then getAttack aRobot else 0

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiantRound1 killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound2 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound3 killerRobotRound2


android = robot ("Android",12,280)

robots = [killerRobot, gentleGiant, android]

ex1 = map getName robots

counter x = (\x -> x + 1)
            ((\x -> x + 1)
             ((\x -> x) x))

singleRound robotA robotB = 
  (\robotA robotB -> 
    (\robotA robotB -> fight robotB robotA) robotA robotB) 
  robotA robotB

