
library(shiny)
library(geosphere)
library(tidyverse)
library(lubridate)
library(GA)
library(leaflet)


set.seed(1989)

# Define Initial conditions/defaults::
# 
numLoads <- 12
numTrucks <- 4

# Pick up and Dropoff Locations of any and all loads
locations_df <- tibble(Locations = c("Macon", "Port of Savannah", "Jacksonville", "Savannah Warehouses and Rail"),
                       Latitude = c(32.810280, 32.119867, 30.353136, 32.081157),
                       Longitude = c(-83.564390, -81.139283, -81.627473, -81.145606))

# Date and Time of Load Windows
earliest_Pickup <- ymd_hms("2019/08/01 6:00:00")
latest_Pickup <- ymd_hms("2019/08/01 12:00:00")

earliest_Dropoff <- ymd_hms("2019/08/01 13:00:00")
latest_Dropoff <- ymd_hms("2019/08/01 20:00:00")

CarrierSpeed_MetersPerMinute <- 1609.34 # 60mph --> 1609 m/min.

CurrentTime_CarrierStart <- ymd_hms("2019/08/01 5:00:00")

# Map Icons
drayageIcons <- iconList(
    trucks = makeIcon("/home/sean/pCloudDrive/sfitzgerald Drive/Drayage/Map Icons/logistics-delivery-truck-in-movement.png", 32, 32),
    truckStop = makeIcon("/home/sean/pCloudDrive/sfitzgerald Drive/Drayage/Map Icons/icons8-sleeping-over-the-computer-64.png", 32, 32),
    loads = makeIcon("/home/sean/pCloudDrive/sfitzgerald Drive/Drayage/Map Icons/packages-transportation-on-a-truck.png", 32, 32),
    dropoff = makeIcon("/home/sean/pCloudDrive/sfitzgerald Drive/Drayage/Map Icons/Shipment_DropOff-512.png", 32, 32)
)

# Optimization fitness function
fitFunction <- function(x) {
    x <- as.logical(x)
    
    potentialLoads <- unique(c(trips_df$Load_ID, trips_df$Load_ID_2))
    # filter out the 0 loads
    potentialLoads <- potentialLoads[potentialLoads>0]
    
    loadsCarried <- c(trips_df$Load_ID[x], trips_df$Load_ID_2[x])
    # filter out the 0 loads
    loadsCarried <- loadsCarried[loadsCarried>0]
    
    
    # Define fitness as positive result of carrying a load
    fitness = sum(potentialLoads %in% loadsCarried)*max(trips_df$TotalTripTime)
    
    ## Penalties
    # Total Trip time (we are trying to minimize this)
    penalty_Time <- -1*sum(trips_df$TotalTripTime[x])
    
    # Penalty for leaving loads behind (otherwise solution is to take 1 load)
    penalty_NotCarryingLoads <- -1*sum(length(potentialLoads) - unique(loadsCarried))*max(trips_df$TotalTripTime)     
    
    # Harsh penalties for breaking constraints
    penalty_ReuseTrucks  <- ifelse(any(duplicated(trips_df$Truck_ID[x])),
                                   -1000*sum(duplicated(trips_df$Truck_ID[x]))*max(trips_df$TotalTripTime),
                                   0)
    
    penalty_ReuseLoads  <- ifelse(any(duplicated( c(trips_df$Load_ID[x], trips_df$Load_ID_2[x]) )),
                                  -1000*sum(duplicated( c(trips_df$Load_ID[x], trips_df$Load_ID_2[x]) ))*max(trips_df$TotalTripTime),
                                  0)
    
    fitness = fitness + penalty_Time + penalty_ReuseTrucks + penalty_ReuseLoads
    
    return(fitness)
}

# Define UI for application
ui <- fluidPage(
    leafletOutput("baseMap"),
    p(),
    actionButton("recalc", "Find Routes")
)


# Define server logic required ---------------------------------------------------
server <- function(input, output) {

    # generate Loads ----------------
    distanceMatrix <- locations_df %>% 
        select(-Locations)
    distanceMatrix <- geosphere::distm(distanceMatrix[2:1], fun = distHaversine)
    colnames(distanceMatrix) <- locations_df$Locations
    rownames(distanceMatrix) <- locations_df$Locations
    
    loads_df <- tibble(Load_ID = seq(1, numLoads),
                       Load_Weight = ceiling(rnorm(numLoads, mean = 100, sd = 15)))
    
    trip_Sim_Matrix<- replicate(numLoads,sample(seq(1, nrow(locations_df)),2, replace = F))
    
    loads_df$Load_Origin_Location <- locations_df$Locations[trip_Sim_Matrix[1,]]
    loads_df$Load_Destination_Location <- locations_df$Locations[trip_Sim_Matrix[2,]]
    loads_df$LoadOrigin_Latitude <- locations_df$Latitude[trip_Sim_Matrix[1,]]
    loads_df$LoadDestination_Latitude <- locations_df$Latitude[trip_Sim_Matrix[2,]]
    loads_df$LoadOrigin_Longitude <- locations_df$Longitude[trip_Sim_Matrix[1,]]
    loads_df$LoadDestination_Longitude <- locations_df$Longitude[trip_Sim_Matrix[2,]]
    
    ## Simulate pick up and dropoff windows
    TimeWindowSim <- function(N, st=ymd_hms("2012/01/01 7:00:00"), et=ymd_hms("2012/01/01 9:00:00")) {
        require(lubridate)
        dt <- as.numeric(difftime(et,st,unit="mins"))
        ev <- ceiling(sort(runif(N,30, dt)))
        rt <- st + minutes(ev)
    }
    
    loads_df$PickupStartTime <- TimeWindowSim(numLoads, earliest_Pickup, latest_Pickup)
    loads_df$DropoffStartTime <- TimeWindowSim(numLoads, earliest_Dropoff, latest_Dropoff)
    
    loads_df <- loads_df %>% 
        mutate(PickupEndTime = TimeWindowSim(1, PickupStartTime, latest_Pickup),
               DropoffEndTime = TimeWindowSim(1, DropoffStartTime, latest_Dropoff))
    
    ## Calculate Trip distance. Will use straight line as proxy for trip time.
    loads_df <- loads_df %>% 
        mutate(BundleDistance_meters = geosphere::distHaversine(p1 = cbind(LoadOrigin_Longitude, LoadOrigin_Latitude), p2 = cbind(LoadDestination_Longitude, LoadDestination_Latitude))) %>% 
        mutate(BundleTimeEstimate_minutes = ceiling(BundleDistance_meters/CarrierSpeed_MetersPerMinute))
   
    ## Add truck locations
    trucks_df <- tibble(Truck_ID = seq(1, numTrucks),
                        ELD_TimeRemaining = rpois(numTrucks, 7),
                        CurrentLocation_Longitude = runif(numTrucks, -83.564390, -81.139283),
                        CurrentLocation_Latitude = runif(numTrucks, 32.081157, 32.810280),
                        DesiredEndLocation_Longitude  = runif(numTrucks, -83.564390, -81.139283),
                        DesiredEndLocation_Latitude  = runif(numTrucks, 32.081157, 32.810280))
    
    mapLocations_df <- tibble(ID = rep(x = c(loads_df$Load_ID, trucks_df$Truck_ID), 2),
                              lng = c(loads_df$LoadOrigin_Longitude, trucks_df$CurrentLocation_Longitude, loads_df$LoadDestination_Longitude, trucks_df$DesiredEndLocation_Longitude),
                              lat = c(loads_df$LoadOrigin_Latitude, trucks_df$CurrentLocation_Latitude, loads_df$LoadDestination_Latitude, trucks_df$DesiredEndLocation_Latitude),
                              description =  rep(c(paste0("Weight: ", loads_df$Load_Weight), paste0("ELD Hours Remaining: ", trucks_df$ELD_TimeRemaining)), 2),
                              position = c(rep("Start", times = nrow(loads_df)+nrow(trucks_df)), rep("End", times = nrow(loads_df)+nrow(trucks_df))),
                              group = rep(c(rep(x = "Load", times = nrow(loads_df)), rep(x = "Truck", times = nrow(trucks_df))), times = 2))
    
    # Render Leaflet base map --------------------------------------------------
    output$baseMap <- renderLeaflet({
        leaflet(mapLocations_df) %>% 
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(data = filter(mapLocations_df, group == "Load" & position == "Start"),
                       label = ~LETTERS[ID],
                       popup = ~description,
                       clusterOptions = markerClusterOptions(),
                       icon = ~drayageIcons$loads) %>% 
            addMarkers(data = filter(mapLocations_df, group == "Truck" & position == "Start"),
                       label = ~ID,
                       popup = ~description,
                       clusterOptions = markerClusterOptions(),
                       icon = ~drayageIcons$trucks) %>% 
            addMarkers(data = filter(mapLocations_df, group == "Load" & position == "End"),
                       label = ~LETTERS[ID],
                       popup = ~description,
                       clusterOptions = markerClusterOptions(),
                       icon = ~drayageIcons$dropoff) %>% 
            addMarkers(data = filter(mapLocations_df, group == "Truck" & position == "End"),
                       label = ~ID,
                       popup = ~description,
                       clusterOptions = markerClusterOptions(),
                       icon = ~drayageIcons$truckStop)
    })
    
    # Optimize! -------------
    
    bundles_df <- tibble(Load_ID = seq(0, numLoads),
                         Load_ID_2 = seq(0, numLoads)) %>% 
        complete(Load_ID, Load_ID_2) %>% 
        filter(Load_ID > 0) %>% 
        filter(Load_ID != Load_ID_2) %>%
        rowwise() %>% 
        mutate(DeadHeadDistanceBtwnLoads_meters = ifelse(Load_ID_2 > 0,
                                                         distHaversine(p1=cbind(loads_df$LoadDestination_Longitude[Load_ID],loads_df$LoadDestination_Latitude[Load_ID]),
                                                                       p2=cbind(loads_df$LoadOrigin_Longitude[Load_ID_2], loads_df$LoadOrigin_Latitude[Load_ID_2])),
                                                         0)) %>%
        mutate(BundleDistance_meters = ifelse(Load_ID_2 > 0,
                                              DeadHeadDistanceBtwnLoads_meters + loads_df$BundleDistance_meters[Load_ID] + loads_df$BundleDistance_meters[Load_ID_2],
                                              loads_df$BundleDistance_meters[Load_ID])) %>% 
        mutate(BundleTimeEstimate_minutes = ceiling(BundleDistance_meters/CarrierSpeed_MetersPerMinute))
    
    viableBundles_df <- bundles_df %>% 
        rowwise() %>% 
        mutate(LastCall = ifelse(Load_ID_2 > 0,
                                 loads_df$DropoffEndTime[Load_ID_2],
                                 loads_df$DropoffEndTime[Load_ID])) %>% 
        filter(LastCall > loads_df$PickupStartTime[Load_ID] + minutes(BundleTimeEstimate_minutes))

    possibleTrips_df <- expand.grid(Truck_ID = seq(1, numTrucks),
                                    Load_ID = seq(1, numLoads),
                                    Load_ID_2 = seq(0, numLoads)) 
    
    keys <- c("Load_ID", "Load_ID_2")
    
    trips_df <- merge(possibleTrips_df, viableBundles_df, by=keys) %>% 
        select(Truck_ID, Load_ID, Load_ID_2, everything()) %>% 
        rowwise() %>% 
        mutate(DistanceToFirstLoad_meters = distHaversine(p1 = cbind(trucks_df$CurrentLocation_Longitude[Truck_ID], trucks_df$CurrentLocation_Latitude[Truck_ID]), 
                                                          p2 = cbind(loads_df$LoadOrigin_Longitude[Load_ID], loads_df$LoadOrigin_Latitude[Load_ID]))) %>% 
        mutate(TimeToFirstLoad_minutes = ceiling(DistanceToFirstLoad_meters/CarrierSpeed_MetersPerMinute)) %>% 
        filter(CurrentTime_CarrierStart + minutes(TimeToFirstLoad_minutes) < loads_df$PickupEndTime[Load_ID]) %>% 
        mutate(EstimatedEndOfTripTime = CurrentTime_CarrierStart + minutes(TimeToFirstLoad_minutes) + BundleTimeEstimate_minutes) %>% 
        filter(ifelse(Load_ID_2 > 0,
                      EstimatedEndOfTripTime < loads_df$DropoffEndTime[Load_ID_2],
                      EstimatedEndOfTripTime < loads_df$DropoffEndTime[Load_ID])) %>% 
        mutate(TripHome_meters = ifelse(Load_ID_2 > 0, 
                                        distHaversine(p1 = cbind(loads_df$LoadDestination_Longitude[Load_ID_2],
                                                                 loads_df$LoadDestination_Latitude[Load_ID_2]),
                                                      p2 = cbind(trucks_df$DesiredEndLocation_Longitude[Truck_ID],
                                                                 trucks_df$DesiredEndLocation_Latitude[Truck_ID])),
                                        distHaversine(p1 = cbind(loads_df$LoadDestination_Longitude[Load_ID],
                                                                 loads_df$LoadDestination_Latitude[Load_ID]),
                                                      p2 = cbind(trucks_df$DesiredEndLocation_Longitude[Truck_ID],
                                                                 trucks_df$DesiredEndLocation_Latitude[Truck_ID])))) %>% 
        mutate(TripHome_minutes = ceiling(TripHome_meters / CarrierSpeed_MetersPerMinute)) %>% 
        filter(EstimatedEndOfTripTime + minutes(TripHome_minutes) < CurrentTime_CarrierStart + hours(trucks_df$ELD_TimeRemaining[Truck_ID])) %>% 
        mutate(TotalTripDistance = TripHome_meters + DistanceToFirstLoad_meters + BundleDistance_meters,
               TotalTripTime = ceiling(TripHome_minutes + TimeToFirstLoad_minutes + BundleTimeEstimate_minutes))    
    
    ga_results <- ga(type = "binary",
                     fitness = fitFunction,
                     lower = rep(x = 0, nrow(trips_df)),
                     upper = rep(x = 1, nrow(trips_df)),
                     nBits = nrow(trips_df),
                     maxiter=1000, # Maximum number of generations
                     run=100,     # Stop if the best-so-far fit hasn't improved for 'run' generations 
                     popSize=100, 
                     seed=101)
    
    solution <- (summary(ga_results)$solution)
    
    trips_df$isOptimal <- solution[1:nrow(trips_df)]
    
    optimizedTrips <- trips_df %>% filter(isOptimal == 1)
    
    optimizedTrips_extended_df <- optimizedTrips %>% 
        select(Truck_ID, Load_ID, Load_ID_2) %>% 
        gather(key = "LoadOrder", value = "Load_ID", -Truck_ID) %>% 
        mutate(LoadOrder = ifelse(str_detect(string = LoadOrder, pattern = "_2"), 2, 1)) %>% 
        left_join(., trucks_df, by= "Truck_ID") %>% 
        left_join(., loads_df, by="Load_ID") %>% 
        select(Truck_ID, Load_ID, LoadOrder, CurrentLocation_Longitude, CurrentLocation_Latitude, LoadOrigin_Longitude, LoadOrigin_Latitude, LoadDestination_Longitude,
               LoadDestination_Latitude, DesiredEndLocation_Longitude, DesiredEndLocation_Latitude) %>% 
        gather(key = TripNode, value = Coordinate, -c(ends_with("ID"), LoadOrder)) %>% 
        separate(col = TripNode, sep = "_", into = c("TripNode", "CoordinateAxis")) %>% 
        spread(key = CoordinateAxis, value = Coordinate) %>% 
        mutate(LoadOrder = ifelse(TripNode == "LoadOrigin" & LoadOrder == 2,
                                  3,
                                  ifelse(TripNode == "LoadDestination",
                                         2*LoadOrder,
                                         LoadOrder))) %>% 
        mutate(LoadOrder = ifelse(TripNode == "CurrentLocation", 
                                  0,
                                  ifelse(TripNode == "DesiredEndLocation",
                                         5,
                                         LoadOrder))) %>%  
        filter(!duplicated(.)) %>% 
        group_by(Truck_ID) %>% 
        filter(duplicated(TripNode == "CurrentLocation")) %>% 
        # filter(duplicated(TripNode == "DesiredEndLocation")) %>%
        arrange(Truck_ID, LoadOrder) %>% 
        ungroup() %>% 
        select(Truck_ID, Latitude, Longitude)
    
    # Render Optimized Routes on top of base map
    
    observe({

        for (i in unique(optimizedTrips_extended_df$Truck_ID)) {
            leafletProxy("baseMap", data = optimizedTrips_extended_df()) %>%
                addPolylines(data = subset(optimizedTrips_extended_df(), Truck_ID == i),
                             group = ~Truck_ID,
                             lng = ~Longitude,
                             lat = ~Latitude,
                             color = ~pal(Truck_ID[i]))

        }

    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
