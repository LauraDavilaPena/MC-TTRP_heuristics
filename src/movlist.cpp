#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <string>
#include <R.h>
#include <Rdefines.h>    
#include <Rinternals.h>
#include <iostream>    
#include <algorithm>   

using namespace Rcpp;


struct MovsList  
{  
  int indexr1;
  int indexr2;
  std::string string1; // nome movemento
  std::vector<int>  route1; 
  std::vector<int>  route2;
  std::vector<int>  client1;
  std::vector<int>  client2;
  int root1;  // 
  int root2;  // 
  int total_hoppers_truck1;
  int total_hoppers_trailer1;
  int total_hoppers_truck2;
  int total_hoppers_trailer2;
  int opt_reconf;
};


// GlobalVARs
std::vector<MovsList> mov_list;
std::vector<double>   mov_list_cost;
std::vector<double>   mov_list_cost_pen;
std::vector<double>   mov_list_cost_feas;
std::vector<double>   mov_list_cost_nopen;
std::vector<int>      index_ordered;
int counter=0;

// [[Rcpp::export]]
RcppExport SEXP  createMovsList() {
  
  for (int i=0; i<((int) mov_list.size()); ++i) {
    mov_list[i].route1.clear();
    mov_list[i].route2.clear();
  }
  mov_list.clear();
  mov_list_cost.clear();
  mov_list_cost_pen.clear();
  mov_list_cost_feas.clear();
  mov_list_cost_nopen.clear();
  index_ordered.clear();
  
  counter++;
  
  return(R_NilValue);
};

// return_mov_list_cost
// [[Rcpp::export]]
RcppExport SEXP delete_elements_using_changed_list(SEXP changed_list){
  
  std::vector<int> c_changed_list, c_index_to_remain;
  
  for (int i=0; i<LENGTH(changed_list); ++i)  c_changed_list.push_back(INTEGER(changed_list)[i]-1);
  
  for (int i=0; i< ((int)mov_list.size()); ++i ) {
    std::vector<int>::iterator it1 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr1-1);
    std::vector<int>::iterator it2 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr2-1);
    
    if (((it1 == c_changed_list.end())&&(it2 == c_changed_list.end()))){
      c_index_to_remain.push_back(i);
    }
  }

  std::vector<MovsList> mov_list_aux;
  std::vector<double>   mov_list_cost_aux;
  std::vector<double>   mov_list_cost_pen_aux;
  std::vector<double>   mov_list_cost_feas_aux;
  std::vector<double>   mov_list_cost_nopen_aux;
  
  
  for (int i = 0; i <((int) c_index_to_remain.size()); ++i) {
    mov_list_aux.push_back(mov_list[c_index_to_remain[i]]);
    mov_list_cost_aux.push_back(mov_list_cost[c_index_to_remain[i]]);
    mov_list_cost_pen_aux.push_back(mov_list_cost_pen[c_index_to_remain[i]]);
    mov_list_cost_feas_aux.push_back(mov_list_cost_feas[c_index_to_remain[i]]);
    mov_list_cost_nopen_aux.push_back(mov_list_cost_nopen[c_index_to_remain[i]]);
  }
  
  for (int i=0; i<((int) mov_list.size()); ++i) {
    mov_list[i].route1.clear();
    mov_list[i].route2.clear();
  }
  mov_list.clear();
  mov_list_cost.clear();
  mov_list_cost_pen.clear();
  mov_list_cost_feas.clear();
  mov_list_cost_nopen.clear();
  index_ordered.clear();
  
  mov_list = mov_list_aux;
  mov_list_cost = mov_list_cost_aux;
  mov_list_cost_pen = mov_list_cost_pen_aux;
  mov_list_cost_feas = mov_list_cost_feas_aux;
  mov_list_cost_nopen = mov_list_cost_nopen_aux;
  
  return(R_NilValue);
}

// return_mov_list_cost
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_string(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  StringVector result(1);
  
  result[0] = mov_list[c_id].string1;
  
  return result;
}

// return_mov_list_cost
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_cost(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = mov_list_cost[c_id];
  UNPROTECT(1);
  
  return result;
}

// return_mov_list_cost_pen
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_cost_pen(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = mov_list_cost_pen[c_id];
  UNPROTECT(1);
  
  return result;
}

// return_mov_list_cost_feas
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_cost_feas(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = mov_list_cost_feas[c_id];
  UNPROTECT(1);
  
  return result;
}

// return_mov_list_cost_nopen
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_cost_nopen(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = mov_list_cost_nopen[c_id];
  UNPROTECT(1);
  
  return result;
}

// return_route1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_route1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, mov_list[c_id].route1.size()));
  for (int i=0; i < ((int) mov_list[c_id].route1.size()); ++i) {
    INTEGER(result)[i] = mov_list[c_id].route1[i];
  }
  UNPROTECT(1);
  
  return result;
}

// return_route2
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_route2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, mov_list[c_id].route2.size()));
  for (int i=0; i < ((int) mov_list[c_id].route2.size()); ++i) {
    INTEGER(result)[i] = mov_list[c_id].route2[i];
  }
  UNPROTECT(1);
  
  return result;
}

// return_client1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_client1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, mov_list[c_id].client1.size()));
  for (int i=0; i < ((int) mov_list[c_id].client1.size()); ++i) {
      INTEGER(result)[i] = mov_list[c_id].client1[i];
  }
  UNPROTECT(1);
  
  return result;
}

// return_client1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_client2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, mov_list[c_id].client2.size()));
  for (int i=0; i < ((int) mov_list[c_id].client2.size()); ++i) {
    INTEGER(result)[i] = mov_list[c_id].client2[i];
  }
  UNPROTECT(1);
  
  return result;
}

// return_root1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_root1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].root1;
  UNPROTECT(1);
  
  return result;
}

// return_root2
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_root2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].root2;
  UNPROTECT(1);
  
  return result;
}


// return_root1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_hoppers_truck1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].total_hoppers_truck1;
  UNPROTECT(1);
  
  return result;
}

// return_root2
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_hoppers_trailer1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].total_hoppers_trailer1;
  UNPROTECT(1);
  
  return result;
}
// return_root1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_hoppers_truck2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].total_hoppers_truck2;
  UNPROTECT(1);
  
  return result;
}

// return_mov_list_hoppers_trailer2
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_hoppers_trailer2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].total_hoppers_trailer2;
  UNPROTECT(1);
  
  return result;
}

// return_mov_list_opt_reconf
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_opt_reconf(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].opt_reconf;
  UNPROTECT(1);
  
  return result;
}

// return_indexr1
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_indexr1(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].indexr1;
  UNPROTECT(1);
  
  return result;
}

// return_indexr2
// [[Rcpp::export]]
RcppExport SEXP return_mov_list_indexr2(SEXP id){
  
  int c_id = index_ordered[INTEGER(id)[0]-1]; //index_ordered[INTEGER(id)[0]-1];
  
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = mov_list[c_id].indexr2;
  UNPROTECT(1);
  
  return result;
}

// return_size_mov_list
// [[Rcpp::export]]
RcppExport SEXP return_size_mov_list(){
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = mov_list.size();
  UNPROTECT(1);
  
  return result;
}

// [[Rcpp::export]]
RcppExport SEXP order(SEXP option){
  
  int c_option = INTEGER(option)[0];
  
  // cost
  if (c_option) {
    std::vector<int> index_ordered_aux(mov_list_cost.size());
    std::size_t n(0);
    std::generate(std::begin(index_ordered_aux), std::end(index_ordered_aux), [&]{ return n++; });
    
    std::sort(  std::begin(index_ordered_aux), 
                std::end(index_ordered_aux),
                [&](int i1, int i2) { return mov_list_cost[i1] < mov_list_cost[i2]; } );
    
    index_ordered = index_ordered_aux;
  } 
  // cost pen
  else {
    std::vector<int> index_ordered_aux(mov_list_cost_pen.size());
    std::size_t n(0);
    std::generate(std::begin(index_ordered_aux), std::end(index_ordered_aux), [&]{ return n++; });
    
    std::sort(  std::begin(index_ordered_aux), 
                std::end(index_ordered_aux),
                [&](int i1, int i2) { return mov_list_cost_pen[i1] < mov_list_cost_pen[i2]; } );
    
    index_ordered = index_ordered_aux;
  }
  
  return(R_NilValue);
};

// insertMovsList
// [[Rcpp::export]]
RcppExport SEXP  insertMovsList(SEXP indexr1, SEXP indexr2, SEXP client1, SEXP client2, 
                                SEXP route1, SEXP route2, SEXP string, SEXP origin1, SEXP origin2) {

  IntegerVector test(route1);
  
  MovsList new_mov;
  new_mov.indexr1 = INTEGER(indexr1)[0];
  new_mov.indexr2 = INTEGER(indexr2)[0];
  new_mov.root1 = INTEGER(origin1)[0];
  new_mov.root2 = INTEGER(origin2)[0];
  
  for (int i=0; i<LENGTH(client1); ++i) {
    new_mov.client1.push_back(INTEGER(client1)[i]);
  }
  
  for (int i=0; i<LENGTH(client2); ++i) {
    new_mov.client2.push_back(INTEGER(client2)[i]);
  }
  
  for (int i=0; i<LENGTH(route1); ++i) {
    new_mov.route1.push_back(INTEGER(route1)[i]);
  }
  
  
  for (int i=0; i<LENGTH(route2); ++i) {
    new_mov.route2.push_back(INTEGER(route2)[i]);
  }
  
  new_mov.string1 = CHAR(STRING_ELT(string,0));

  //std::cout << "new_mov.indexr1" << " " << new_mov.indexr1 << std::endl;
  //std::cout << "new_mov.indexr2 " << new_mov.indexr2 << std::endl;
  //std::cout << "new_mov.client1 " << new_mov.client1[0] << std::endl;
  //std::cout << "new_mov.client2 " << new_mov.client2[0] << std::endl;
  //std::cout << "new_mov.route1 " << new_mov.route1[0] << std::endl;
  //std::cout << "new_mov.route2 " << new_mov.route2[0] << std::endl;
  //std::cout << "new_mov.string1 " << new_mov.string1 << std::endl;
  
  mov_list.push_back(new_mov);
  mov_list_cost.push_back(0.0);
  mov_list_cost_pen.push_back(0.0);
  mov_list_cost_feas.push_back(0.0);
  mov_list_cost_nopen.push_back(0.0);

  return(R_NilValue);
};


// insertMovsList
// [[Rcpp::export]]
RcppExport SEXP  insertMovsList_MCTTRP(SEXP indexr1, SEXP indexr2, SEXP client1, SEXP client2, 
                                SEXP route1, SEXP route2, SEXP string, SEXP origin1, SEXP origin2,
                                SEXP total_hoppers_truck1, SEXP total_hoppers_trailer1,
                                SEXP total_hoppers_truck2, SEXP total_hoppers_trailer2, SEXP opt_reconf) {


  IntegerVector test(route1);
  
  MovsList new_mov;
  new_mov.indexr1 = INTEGER(indexr1)[0];
  new_mov.indexr2 = INTEGER(indexr2)[0];
  new_mov.root1 = INTEGER(origin1)[0];
  new_mov.root2 = INTEGER(origin2)[0];
  new_mov.total_hoppers_truck1   = INTEGER(total_hoppers_truck1)[0];
  new_mov.total_hoppers_trailer1 = INTEGER(total_hoppers_trailer1)[0];
  new_mov.total_hoppers_truck2   = INTEGER(total_hoppers_truck2)[0];
  new_mov.total_hoppers_trailer2 = INTEGER(total_hoppers_trailer2)[0];
  new_mov.opt_reconf = INTEGER(opt_reconf)[0];
  
  for (int i=0; i<LENGTH(client1); ++i) {
    new_mov.client1.push_back(INTEGER(client1)[i]);
  }
  
  for (int i=0; i<LENGTH(client2); ++i) {
    new_mov.client2.push_back(INTEGER(client2)[i]);
  }
  
  for (int i=0; i<LENGTH(route1); ++i) {
    new_mov.route1.push_back(INTEGER(route1)[i]);
  }
  
  
  for (int i=0; i<LENGTH(route2); ++i) {
    new_mov.route2.push_back(INTEGER(route2)[i]);
  }
  
  new_mov.string1 = CHAR(STRING_ELT(string,0));
  
  mov_list.push_back(new_mov);
  mov_list_cost.push_back(0.0);
  mov_list_cost_pen.push_back(0.0);
  mov_list_cost_feas.push_back(0.0);
  mov_list_cost_nopen.push_back(0.0);
  
  return(R_NilValue);
};

// local_cost
// [[Rcpp::export]]
double local_cost(std::vector<int> route, NumericMatrix c_dist_matrix) {
  double cost = 0.0;
  
  for (int i=0; i < (((int) route.size())-1); ++i) {
    cost = cost + c_dist_matrix(route[i], route[i+1]);
  }

  //std::cout << "cost " << cost << std::endl;
    
  return cost;
}


// CALC_PENALTY_ROUTE
// [[Rcpp::export]]
double calc_penalty_route(std::vector<int> route, NumericVector c_demandas_vector, 
                          double capacity_max, int cap_truck) {
  
  // calc load
  std::map<int, std::vector<int>> check_ocurrence;
  double load, exc;
  
  exc = 0.0;
  load = 0.0;
  
  for (int i=1; i < (((int)route.size())-1); ++i) {
    if  (check_ocurrence.find(route[i]) == check_ocurrence.end()) {
      std::vector<int> pos_vec;
      pos_vec.push_back(i);
      check_ocurrence.insert( std::make_pair(route[i], pos_vec));
    }
    else {
      std::vector<int> pos_vec = check_ocurrence.at(route[i]);
      pos_vec.push_back(i);
      check_ocurrence.at(route[i]) = pos_vec;
    }
    
    if ((check_ocurrence.at(route[i])).size() == 1) load = load + c_demandas_vector[route[i]];
  }
  
  exc = std::max(0.0 , load - capacity_max);

  for (auto elem: check_ocurrence) {
    if (elem.second.size() > 1) {
      for (int i = 0; i < (((int)elem.second.size())-1); ++i){
        load = 0.0;
        std::vector<int> route_s;
        for (int j = (elem.second[i]+1); j < elem.second[i+1]; ++j) {
          load = load + c_demandas_vector[route[j]];
          route_s.push_back(route[j]);
        }
        
        exc = exc +  std::max(0.0, load - cap_truck);
      }
    }
  }
  
  return exc;
}

// CALC_PENALTY_ROUTE
// [[Rcpp::export]]
double calc_penalty_route_MCTTRP(std::vector<int> route, int total_hoppers_truck, int total_hoppers_trailer,  
                                 int max_h_truck, int max_h_trailer,
                                 NumericMatrix c_demandas_vector, int size_dim,
                                 double capacity_max, double cap_truck, double cap_trailer) {
  
  // calc load
  std::map<int, std::vector<int>> check_ocurrence;
  double load, exc;
  
  exc = 0.0;
  load = 0.0;
// total load  
  for (int i=1; i < (((int)route.size())-1); ++i) {
    if  (check_ocurrence.find(route[i]) == check_ocurrence.end()) {
      std::vector<int> pos_vec;
      pos_vec.push_back(i);
      check_ocurrence.insert( std::make_pair(route[i], pos_vec));
    }
    else {
      std::vector<int> pos_vec = check_ocurrence.at(route[i]);
      pos_vec.push_back(i);
      check_ocurrence.at(route[i]) = pos_vec;
    }
    
    if ((check_ocurrence.at(route[i])).size() == 1) {
      for (int j=0; j<size_dim; ++j) {
        load = load + c_demandas_vector(route[i],j+1);
      }
    }
  }
  
  exc = std::max(0.0 , load - capacity_max);
  
// subroutes
  for (auto elem: check_ocurrence) {
    if (elem.second.size() > 1) {
      load = 0.0;
      for (int i = 0; i < (((int)elem.second.size())-1); ++i){
        for (int j = (elem.second[i]+1); j < elem.second[i+1]; ++j) {
          for (int z = 0; z < size_dim; ++z ) load = load + c_demandas_vector(route[j], z+1);
        }
      }
      exc = exc +  std::max(0.0, load - cap_truck);
    }
  }

// hoppers
  exc = exc + ((double) std::max(0, total_hoppers_truck - max_h_truck));
  exc = exc + ((double) std::max(0, total_hoppers_trailer - max_h_trailer));

  return exc;
}


// C_EVAL_MOVS
// [[Rcpp::export]]
RcppExport SEXP  c_eval_movs(SEXP changed_list, SEXP cap_routes_vector, SEXP dist_matrix, 
                             SEXP demandas_matrix, SEXP input_cap_truck, 
                             SEXP static_feas, SEXP static_cost, SEXP alpha, SEXP option) {
  
  int c_input_cap_truck, c_option;
  double c_alpha, feas, cost, cap;
  std::vector<int> c_changed_list, route_local;
  std::vector<double> c_cap_routes_vector;

  c_input_cap_truck   = REAL(input_cap_truck)[0];
  c_alpha = REAL(alpha)[0];
  c_option = INTEGER(option)[0];

  for (int i=0; i<LENGTH(changed_list); ++i)  c_changed_list.push_back(INTEGER(changed_list)[i]-1);
  
  for (int i=0; i<LENGTH(cap_routes_vector); ++i) c_cap_routes_vector.push_back(REAL(cap_routes_vector)[i]);
  
  NumericMatrix c_dist_matrix(dist_matrix);
//NumericMatrix c_demandas_matrix(demandas_matrix);
  NumericVector c_demandas_vector(demandas_matrix);
  NumericVector c_static_feas(static_feas);
  NumericVector c_static_cost(static_cost);
  
  //std::cout << "c_alpha    " << c_alpha << std::endl; 
  //std::cout << "c_input_n1 " << c_input_n1 << std::endl; 
  //std::cout << "c_input_cap_truck " << c_input_cap_truck << std::endl; 
  //std::cout << "c_changed_list " << c_changed_list[0] << " " << c_changed_list[1] << " " << c_changed_list[2] << std::endl; 
  //std::cout << "c_cap_routes_vector " << c_cap_routes_vector[0] << " " << c_cap_routes_vector[1] << " " << c_cap_routes_vector[2] << std::endl; 
  //std::cout << "c_dist_matrix " << c_dist_matrix(0,0) << " " <<  c_dist_matrix(0,1) << " " <<  c_dist_matrix(0,3) << std::endl; 
  //std::cout << "c_static_feas " << c_static_feas[0] << " " << c_static_feas[1] << " " << c_static_feas[2] 
  //            << " " << c_static_feas[3] << " " << c_static_feas[4] <<  std::endl; 
  //std::cout << "c_static_cost " << c_static_cost[0] << " " << c_static_cost[1] << " " << c_static_cost[2] 
  //          << " " << c_static_cost[3] << " " << c_static_cost[4] <<  std::endl; 
  //std::cout << "mov_list.size()) " << mov_list.size() << std::endl;
  //std::cout << "c_demandas_vector " << c_demandas_vector[0] << " " << c_demandas_vector[1] << " " << c_demandas_vector[2] <<  std::endl; 
  

  for (int i=0; i< ((int)mov_list.size()); ++i) {
    std::vector<int>::iterator it1 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr1-1); // buscar si indexr1 cambiou 
    std::vector<int>::iterator it2 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr2-1); // buscar si indexr2 cambiou 
    
    if ((it1 != c_changed_list.end())||(it2 != c_changed_list.end())){
      feas = 0;
      cost = 0;
      mov_list_cost_feas[i] = 0.0;
      mov_list_cost_nopen[i] = 0.0;
      for (int j=0; j < ((int)c_cap_routes_vector.size()); ++j) {
        cap = c_cap_routes_vector[j];
        if ((j == (mov_list[i].indexr1-1))||(j == (mov_list[i].indexr2-1))) {
            if (j == (mov_list[i].indexr1-1)) route_local = mov_list[i].route1;
            if (j == (mov_list[i].indexr2-1)) route_local = mov_list[i].route2;
// as infactibilidad midense aqui -> calc_penalty_route
            mov_list_cost_feas[i]  = mov_list_cost_feas[i]  + calc_penalty_route(route_local, c_demandas_vector, cap, c_input_cap_truck);
            mov_list_cost_nopen[i] = mov_list_cost_nopen[i] + local_cost(route_local, c_dist_matrix);
        }
        else {
            cost = cost + c_static_cost(j);
            feas = feas + c_static_feas(j);
        }
      }
      mov_list_cost[i] = (mov_list_cost_nopen[i] + cost) + c_alpha * (mov_list_cost_feas[i] + feas);
      mov_list_cost_pen[i] = 0.0;
      
      
    } else {
      feas = 0;
      cost = 0;
      for (int j=0; j < ((int)c_cap_routes_vector.size()); ++j) {
        if ((j != (mov_list[i].indexr1-1))&&(j != (mov_list[i].indexr2-1))) {
          cost = cost + c_static_cost(j);
          feas = feas + c_static_feas(j);
        }
      }
      mov_list_cost[i] = (mov_list_cost_nopen[i] + cost) + c_alpha * (mov_list_cost_feas[i] + feas);
      mov_list_cost_pen[i] = 0.0;

    }
  }
  
//  std::cout << "C <- c( "<< std::endl;
//  for (int i=0; i< ((int)mov_list.size()); ++i) {
//    std::cout << mov_list_cost[i] << ", ";
//  }
//std::cout << ")" << std::endl;
  return(R_NilValue);
}

// C_EVAL_MOVS
// [[Rcpp::export]]
RcppExport SEXP  c_eval_movs_MCTTRP(SEXP changed_list, SEXP cap_routes_vector, SEXP dist_matrix, SEXP size_dem, 
                             SEXP demandas_matrix, SEXP input_cap_truck, SEXP input_cap_trailer,
                             SEXP n_hoppers_truck, SEXP n_hoppers_trailer, 
                             SEXP static_feas, SEXP static_cost, SEXP alpha, SEXP option) {

  double c_input_cap_truck, c_input_cap_trailer, max_cap_h_trucks, max_cap_h_trailers;
  int c_option, c_size_dem;
  double c_alpha, feas, cost, cap;
  std::vector<int> c_changed_list, route_local;
  int total_hoppers_truck, total_hoppers_trailer;
  std::vector<double> c_cap_routes_vector, c_h_trucks, c_h_trailers;
  
  c_input_cap_truck   = REAL(input_cap_truck)[0];
  c_input_cap_trailer = REAL(input_cap_trailer)[0];
  max_cap_h_trucks      = REAL(n_hoppers_truck)[0];
  max_cap_h_trailers    = REAL(n_hoppers_trailer)[0];
  c_size_dem          = INTEGER(size_dem)[0];
  c_alpha = REAL(alpha)[0];
  c_option = INTEGER(option)[0];
  
  for (int i=0; i<LENGTH(changed_list); ++i)  c_changed_list.push_back(INTEGER(changed_list)[i]-1);
  
  for (int i=0; i<LENGTH(cap_routes_vector); ++i) c_cap_routes_vector.push_back(REAL(cap_routes_vector)[i]);
  

  NumericMatrix c_dist_matrix(dist_matrix);
  NumericMatrix c_demandas_matrix(demandas_matrix);
  NumericVector c_static_feas(static_feas);
  NumericVector c_static_cost(static_cost);
  
  
  for (int i=0; i< ((int)mov_list.size()); ++i) {
    std::vector<int>::iterator it1 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr1-1);
    std::vector<int>::iterator it2 = std::find(c_changed_list.begin(), c_changed_list.end(), mov_list[i].indexr2-1);
    
    if ((it1 != c_changed_list.end())||(it2 != c_changed_list.end())){
      feas = 0;
      cost = 0;
      mov_list_cost_feas[i] = 0.0;
      mov_list_cost_nopen[i] = 0.0;
      for (int j=0; j < ((int)c_cap_routes_vector.size()); ++j) {
        cap = c_cap_routes_vector[j];
        if ((j == (mov_list[i].indexr1-1))||(j == (mov_list[i].indexr2-1))) {
          if (j == (mov_list[i].indexr1-1)) {
            total_hoppers_truck = mov_list[i].total_hoppers_truck1;
            total_hoppers_trailer = mov_list[i].total_hoppers_trailer1;
            route_local  = mov_list[i].route1;
          }
          if (j == (mov_list[i].indexr2-1)) {
            total_hoppers_truck = mov_list[i].total_hoppers_truck2;
            total_hoppers_trailer = mov_list[i].total_hoppers_trailer2;
            route_local = mov_list[i].route2;
          }

          mov_list_cost_feas[i]  = mov_list_cost_feas[i]  + calc_penalty_route_MCTTRP(route_local, total_hoppers_truck, total_hoppers_trailer,  
                                                                                      max_cap_h_trucks, max_cap_h_trailers,
                                                                                      c_demandas_matrix, c_size_dem, cap, 
                                                                                      c_input_cap_truck, c_input_cap_trailer);
                                                                                        
          mov_list_cost_nopen[i] = mov_list_cost_nopen[i] + local_cost(route_local, c_dist_matrix);
        }
        else {
          cost = cost + c_static_cost(j);
          feas = feas + c_static_feas(j);
        }
      }
      mov_list_cost[i] = (mov_list_cost_nopen[i] + cost) + c_alpha * (mov_list_cost_feas[i] + feas);
      mov_list_cost_pen[i] = 0.0;
      
      
    } else {
      feas = 0;
      cost = 0;
      for (int j=0; j < ((int)c_cap_routes_vector.size()); ++j) {
        if ((j != (mov_list[i].indexr1-1))&&(j != (mov_list[i].indexr2-1))) {
          cost = cost + c_static_cost(j);
          feas = feas + c_static_feas(j);
        }
      }
      mov_list_cost[i] = (mov_list_cost_nopen[i] + cost) + c_alpha * (mov_list_cost_feas[i] + feas);
      mov_list_cost_pen[i] = 0.0;
      
    }
  }
  
  return(R_NilValue);
}

// C_EVAL_MOVS
// [[Rcpp::export]]
RcppExport SEXP  c_eval_movs_pen(SEXP table_freq, SEXP zeta, SEXP counter_i) {
  
  NumericMatrix c_table_freq(table_freq);
  
  for (int i = 0; i < ((int) mov_list.size()); ++i) {
    if (mov_list[i].route2.size()>1) {
      double penalty_freq = 0.0;
      for (int j = 0; j < ((int) mov_list[i].client1.size()); ++j) {
        penalty_freq = penalty_freq + c_table_freq(mov_list[i].client1[j]-1, mov_list[i].indexr1-1);
      }
      if (mov_list[i].client2.size()  && mov_list[i].client2[0] != 0) {
        for (int j = 0; j < ((int) mov_list[i].client2.size()); ++j) {
          penalty_freq = penalty_freq + c_table_freq(mov_list[i].client2[j]-1, mov_list[i].indexr2-1);
        }
      }
      penalty_freq = 1 + REAL(zeta)[0] * penalty_freq / REAL(counter_i)[0];
      mov_list_cost_pen[i] = mov_list_cost[i] * penalty_freq;
        
    }
    else {
      double penalty_freq = 0.0;
      for (int j = 0; j < ((int) mov_list[i].client1.size()); ++j) {
        penalty_freq = penalty_freq + c_table_freq(mov_list[i].client1[j]-1, mov_list[i].indexr1-1);
      }      
      penalty_freq = 1 + REAL(zeta)[0]  * penalty_freq / REAL(counter_i)[0];
      mov_list_cost_pen[i] = mov_list_cost[i] * penalty_freq;
    }
  }
  
  return(R_NilValue);
  
};






                        

