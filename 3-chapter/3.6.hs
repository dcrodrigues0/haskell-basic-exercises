module Ex6 where

-- Create a function to check if a month is over
data Month = January | February | March | April | May |June | July | August | September | October | November | Dezember

checkIfMonthIsFinished day January   = (==) day 31
checkIfMonthIsFinished day February  = (==) day 28
checkIfMonthIsFinished day March     = (==) day 31
checkIfMonthIsFinished day April     = (==) day 30
checkIfMonthIsFinished day May       = (==) day 31
checkIfMonthIsFinished day June      = (==) day 30
checkIfMonthIsFinished day July      = (==) day 31
checkIfMonthIsFinished day August    = (==) day 31
checkIfMonthIsFinished day September = (==) day 30
checkIfMonthIsFinished day October   = (==) day 31
checkIfMonthIsFinished day November  = (==) day 30
checkIfMonthIsFinished day Dezember  = (==) day 31

-- Create a method to get the next month
nextMonth January   = February
nextMonth February  = March
nextMonth March     = April
nextMonth April     = May
nextMonth May       = June
nextMonth June      = July
nextMonth July      = August
nextMonth August    = September
nextMonth September = October
nextMonth October   = November
nextMonth November  = Dezember
nextMonth Dezember  = January

-- Create a function to get the season using the hemisphere and month
data Hemisphere = North | South
data Season     = Summer | Autumn | Winter | Spring

getSeason January North   = Winter
getSeason February North  = Winter
getSeason March North     = Winter
getSeason April North     = Spring
getSeason May North       = Spring
getSeason June North      = Summer
getSeason July North      = Summer
getSeason August North    = Summer
getSeason September North = Summer
getSeason October North   = Autumn
getSeason November North  = Autumn
getSeason Dezember North  = Winter

getSeason January South   = Summer
getSeason February South  = Summer
getSeason March South     = Summer
getSeason April South     = Autumn
getSeason May South       = Autumn
getSeason June South      = Autumn
getSeason July South      = Winter
getSeason August South    = Winter
getSeason September South = Winter
getSeason October South   = Spring
getSeason November South  = Spring
getSeason Dezember South  = Summer