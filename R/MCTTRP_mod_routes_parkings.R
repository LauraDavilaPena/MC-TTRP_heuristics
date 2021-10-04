
route_of_client <- function(i, resultado){

  for(r in 1:length(resultado)){
    if(sum(resultado[[r]]$route==i)>=1){
      route.of.client <- resultado[[r]]$route
      index.of.client.route <- r
      break
    }
  }
  return(list(route=route.of.client, index = index.of.client.route)) 
}



client_is_parking <- function(i, resultado){
  if(sum(route_of_client(i,resultado)$route==i)>1){
    client.is.parking = 1
  }else{client.is.parking = 0}
  return(client.is.parking) 
}



client_in_main_tour <- function(i, resultado){
  client.in.main.tour <- 1
  
  for(r in 1:length(resultado)){
    if(all(resultado[[r]]$route %in% route_of_client(i, resultado)$route) && sum(resultado[[r]]$route)!=0){
      client_type_route <- resultado[[r]]$type
      index_route <- r
    }
  }
  
  if(client_type_route == "CVR"){
    for(rr in 1:length(resultado[[index_route]]$subtour)){
      if(sum(resultado[[index_route]]$subtour[[rr]]$tour[2:(length(resultado[[index_route]]$subtour[[rr]]$tour)-1)] == i) > 0){
        client.in.main.tour <- 0
      }
    }
  }
  return(client.in.main.tour) 
}


