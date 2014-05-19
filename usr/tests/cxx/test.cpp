/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

extern "C" {
  //#include <barrelfish/barrelfish.h>
// #include <vfs/vfs.h>
}

#include <fstream>
#include <iostream>
// #include <list>

// static std::list<int> mylist;

int main(int argc, char *argv[])
{
  // mylist.push_back(1);
  // mylist.push_back(2);

  // vfs_init();

  std::cout << "Hello world! " << std::endl;

  // for(std::list<int>::iterator i = mylist.begin(); i != mylist.end(); i++) {
  //   std::cout << *i << std::endl;
  // }

  std::ifstream f("bootmodules");
  int temp;

  while(f >> temp) {
    std::cout << temp;
  }

  return 0;
}
