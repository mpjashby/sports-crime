# Analysis of relationships between different sports and crime in several US cities


`[daily count of crime] ~ ([TRUE/FALSE for each sporting event type] * [weekday/weekend] * [daytime/evening event] + [daylight length] + [lagged crime count] | city)`

Crime counts will be for homicide and assaults combined, day will be defined as starting at 05:00. Model should be for count data, e.g. Poisson, although counts may be high enough that it matters less. Bayesian modelling may be needed. For now we will ignore location type and count all offences in each area.

Separate models for:

  * area around the sporting event
  * downtown
  * rest of the city excluding the area around the event and downtown

Sports:

  * Baseball (MLB)
  * Basketball (NBA)
  * Football (NFL, NCAA division 1)
  * Ice Hockey (NHL)
  * Soccer (MLS)
