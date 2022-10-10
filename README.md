# Two-phase heuristics for the MC-TTRP

This repository includes novel two-phase heuristic algorithms to solve both TTRP and MC-TTRP optimization problems. The first phase, comon to all approaches, consists in a modification of the Clarke & Wright savings algorithm; while the second phase consists in a metaheuristics:

* Iterated local search (ITS)
* Adaptive large neighborhood search (ALNS)
* Hybrid ALNS with tabu search (ALNS-TS)
* Penalized ALNS-TS (PALNS-TS)

This code has been implemented using R 4.0.2, and it has been tested in Linux clusters running CentOS 6.7 and in Windows 10 laptops.



## REFERENCES

### Main references: 
Laura Davila-Pena, David R. Penas, and Balbina Casas-Méndez (2023). A new two-phase heuristic for a problem of food distribution with compartmentalized trucks and trailers. *International Transactions in Operational Research*, 30(2), 1031-1064. https://doi.org/10.1111/itor.13071.

Laura Davila-Pena, David R. Penas, Balbina Casas-Méndez, Maria Antónia Carravilla, and José Fernando Oliveira (2022). An adaptive large neighborhood search for the multi-compartment truck and trailer routing problem. *Manuscript under preparation*. 

### TTRP problems distributed with this library:
I-Ming Chao (2002). A tabu search method for the truck and trailer routing problem. *Computers & Operations Research*, 29(1), 33-51.
