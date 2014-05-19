This is a library for writing distributed (i.e., multi-dispatcher, multi-domain)
services.

It currently provides:
- startup code - a framework for starting distributed services
- service registration - code to register and lookup services
- barriers - based on the chips name service

This library is in development. We envisage it to contain support for:
- group management
- distributed datastructures
  + replication and consistency for the distributed datastructs
  + partitioning for the distributed datastructs
  + data sharing for the distributed datastructs
- group communication
- consensus 
- fault tolerance
- direct barriers (not based on name-service)?


Todo:
