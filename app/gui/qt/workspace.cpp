//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++

#include "workspace.h"

#include <iostream>
#include <vector>

using namespace std;

Workspace::Workspace() {
	instances.push_back(this);
}

Workspace::~Workspace() { 
	for (std::vector<Workspace const *>::iterator it=Workspace::instances.begin(); 
		 it!=Workspace::instances.end(); ++it) {
		if(this == *it) {
			instances.erase(it);
			return;
		}
	}
	cerr << "destructing unknown instance: " << this << endl;
}

void Workspace::enumerate(void) {
	for (std::vector<Workspace const *>::iterator it=Workspace::instances.begin(); 
		 it!=Workspace::instances.end(); ++it) {
		std::cout << *it << endl;
	}
}


std::vector<Workspace const *> Workspace::instances;
