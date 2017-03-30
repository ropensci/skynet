#testground

netImport("~/OneDrive - UGent/Data/BTS/715332541_T_DB1B_COUPON.csv", "~/OneDrive - UGent/Data/BTS/787018770_T_DB1B_TICKET.csv")
# Successful
# Creates netMerged
# Creates nodes
# Creates nodesTr

netMetro(netMerged)
# Successful
# Creates netMet


netDir(netMerged)
# Successful
# Creates netDir_all
# Creates gDir

netDir(netMerged, disp = TRUE)
# Successful
# Creates netDir_disp
# Creates gDir_disp
# Disparity filter is slow

netDir(netMerged, cap = TRUE)
# Successful
# Creates netDir_cap
# Creates gDir_cap


netUnd(netMerged)
# Successful
# Creates netUnd_all
# Creates gUnd

netUnd(netMerged, disp = TRUE)
# Successful
# Creates netUnd_disp
# Creates gUnd_disp

netUnd(netMerged, cap = TRUE)
# Successful
# Creates netUnd_cap
# Creates gUnd_cap



airportsfilter <- select(airports, Latitude, Longitude , OBJECTID,FacilityTy,LocationID,EffectiveD,RegionCode,DistrictCo,StateAbbv,StateName,County,CountyPost,City,FullName,OwnerType,FacilityUs,CoordDet,Elevation,ElevDet,MagneticVa,MagneticDi,MageneticY,TrafficPat,AeroChart,DistanceFr,DirectionT,Acres,ARTCID,ARTCCID,ARTCName,RespARTCID,RespARTCCI,RespARTCNa,FacilityRe,hasNOTAMSe,Activation,Status,ARFFClass,ARFFIndx,ARFFServ,ARFFDate,AAADet,isInternat,hasCustoms,isMilCivJo,hasMilLand,Inspection,Inspecting,Inspecti_1,Informatio,hasAirFram,hasPowerPl,BottledO2,BulkO2,LightingSc,BeaconSche,hasTower,UnicomFreq,CTAF,SegmentedC,OperableBe,hasNonComm,hasMedical,SingleEngC,MultiEngCo,JetEngCoun,HeloCount,GlidersCou,MilitaryCr,Ultralight,Commercial,Commuter,AirTaxi,Local,Itinerant,Military,OpsDate,PositionSo,PositionDa,ElevationS,ElevationD,isContract,hasTransie,OtherServi,WindIndica,ICAOID,TieInOnFac,FSSID,FSSName,FSSLocalPh,FSSTollFre,AltFSSID,AltFSSName,AltFSSToll,Enplanemen,Passengers,Arrivals,Departures,STFIPS)
