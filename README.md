# Clarke and Wright Multi-Compartment Truck and Trailer Routing Problem (CW-MCTTRP)

This is version 0.1 of a novel heuristic based in Clarke and Wright savings algorithm, to solve MC-TTRP optimization problems. This constructive strategy consists in building a high-quality feasible solution, based on the notion of savings in the routing cost. The CW-MCTTRP code has been implemented using R 4.0.2, and it has been tested in Linux clusters running CentOS 6.7 and in Windows 10 laptops.

For the reproducibility of the results presented in the main paper, we recommend using example scripts located in the folder "reproducibility". Moreover, the different route problems used ( MCTTRP and TTRP ) are located in the folder "instances".

## REFERENCES

### Main reference: 
Laura Davila-Pena, David R Penas, and Balbina Casas-Méndez. A classic heuristic for a new problem of feed distribution by means of compartmentalized trucks and trailers.  International Transactions in Operational Research (major revision).

### TTRP problems distributed with this library:
I-Ming Chao. A tabu search method for the truck and trailer routing problem. Computers & Operations Research, Volume 29, Issue 1, 2002, Pages 33-51.
