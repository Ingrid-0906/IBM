### _Problem 3: How many records are in the farm prices dataset?_

    print(nrow(CROP_DATA))
    print(nrow(DAILY_DATA))
    print(nrow(FARM_PRICE))
    print(nrow(MONTHLY_DATA))
  
### _Problem 4: Which provinces are included in the farm prices dataset?_

    print(count(FARM_PRICE, "GEO"))
    
### _Problem 5: How many hectares of Rye were harvested in Canada in 1968?_

    CROP <- filter(CROP_DATA, CROP_DATA$cropType == 'Rye' & CROP_DATA$GEO == 'Canada')
    print(sum(CROP$harvestedArea, na.rm=TRUE))
    
### _Problem 6: Query and display the first 6 rows of the farm prices table for Rye._
    
    FARM <- filter(FARM_PRICE, FARM_PRICE$cropType == 'Rye')
    print(head(FARM, n = 6L))
    
### _Problem 7: Which provinces grew Barley?_

    BAR <- filter(FARM_PRICE, FARM_PRICE$cropType == 'Barley')
    print(table(BAR$GEO))

### _Problem 8: Find the first and last dates for the farm prices data._

    firstPrice <- sort(FARM_PRICE$date, decreasing = TRUE)
    firstPrice[[1]]
    
    ***
    
    lastPrice <- sort(FARM_PRICE$date, decreasing = FALSE)
    lastPrice[[1]]
    
### _Problem 9: Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?_

    crops <- filter(FARM_PRICE, 350.00 >= as.numeric(FARM_PRICE$pricePerMT))
    table(crops$cropType)
    
### _Problem 10: Rank the crop types harvested in Saskatchewan in the year 2000 by their average yield. Which crop performed best?_

    CROP_DATA$YEAR <- as.Date(CROP_DATA$YEAR, "%Y-%m-%d")
    CROP_DATA <- filter(CROP_DATA, CROP_DATA$YEAR %in% CROP_DATA$YEAR[grep("2020",CROP_DATA$YEAR)] & CROP_DATA$GEO == 'Saskatchewan')
    CROP_DATA
    
### _Problem 11: Rank the crops and geographies by their average yield (KG per hectare) since the year 2000. Which crop and province had the highest average yield since the year 2000?_

    CROP_DATA$YEAR <- as.Date(CROP_DATA$YEAR, "%Y-%m-%d")
    CROP_CROP <- filter(CROP_DATA, CROP_DATA$YEAR %in% CROP_DATA$YEAR[grep("2000",CROP_DATA$YEAR)] & sort(CROP_DATA$avgYield, decreasing = TRUE))
    cropType <- CROP_CROP$cropType
    GEO <- CROP_CROP$GEO
    avgYield <- CROP_CROP$avgYield
    X <- data.frame(cropType,GEO,avgYield)
    X

### _Problem 12: Use a subquery to determine how much wheat was harvested in Canada in the most recent year of the data._

    CROP_DATA$YEAR <- as.Date(CROP_DATA$YEAR, "%Y-%m-%d")
    CROP_WHEAT <- filter(CROP_DATA, CROP_DATA$YEAR %in% CROP_DATA$YEAR[grep("2020",CROP_DATA$YEAR)] & CROP_DATA$cropType == 'Wheat')
    harvested_SUM <- sum(CROP_WHEAT$harvestedArea)
    Y <- data.frame(harvested_SUM)
    Y
    
### _Problem 13: Use an implicit inner join to calculate the monthly price per metric tonne of Canola grown in Saskatchewan in both Canadian and US dollars._
### _Display the most recent 6 months of the data._

    FARM <- filter(FARM_PRICE, FARM_PRICE$date %in% FARM_PRICE$date[grep("-12-",FARM_PRICE$date)])
    CROPS <- filter(CROP_DATA, CROP_DATA$YEAR %in% CROP_DATA$YEAR[grep("-12-",CROP_DATA$YEAR)])
    FARM$date <- as.Date(FARM$date, "%Y-%m-%d")
    FARM_DATA <- filter(FARM, FARM$cropType == 'Canola' & FARM$GEO == 'Saskatchewan')
    CROPS_DATA <- filter(CROPS, CROPS$cropType == 'Canola' & CROPS$GEO == 'Saskatchewan')

    C <- CROPS_DATA[order(CROPS_DATA$YEAR, decreasing = TRUE),]
    F <- FARM_DATA[order(FARM_DATA$date, decreasing = TRUE),]

    CROP <- slice(C, c(1:6))
    MONTHLY_PRICE <- slice(F, c(1:6))

    CROPS_FARM <- data.frame(CROP$production, MONTHLY_PRICE$pricePerMT)
    CROPS_FARM['Price_Total'] <- CROPS_FARM$CROP.production * CROPS_FARM$MONTHLY_PRICE.pricePerMT
    CROPS_FARM
