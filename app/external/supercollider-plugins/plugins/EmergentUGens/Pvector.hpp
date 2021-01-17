/*
  This file is part of supercollider-plugins
  Full source at https://github.com/ethancrawford/supercollider-plugins
  License: https://github.com/ethancrawford/supercollider-plugins/LICENSE

  This file was adapted from boids (https://github.com/jyanar/Boids).
*/

#include <iostream>

using namespace std;

#ifndef PVECTOR_H_
#define PVECTOR_H_

// The Pvector class implements Euclidian vectors -- that is, each vector has
// both a magnitude and a direction. We use Pvectors for implementing movement
// and the three Boid rules -- cohesion, separation, and matching velocity
// through the use of acceleration, force, and velocity vectors.

class Pvector {

public:
  float x;
  float y;

  //Constructors
  Pvector() {}

  Pvector(float xComp, float yComp) {
    x = xComp;
    y = yComp;
  }

  //Mutator Functions
  void set(float x, float y);

  //Scalar functions scale a vector by a float
  void addVector(Pvector v);
  void addScalar(float x);

  void subVector(Pvector v);
  Pvector subTwoVector(Pvector v, Pvector v2);
  void subScalar(float x);

  void mulVector(Pvector v);
  void mulScalar(float x);

  void divVector(Pvector v);
  void divScalar(float x);

  void limit(double max);

  //Calculating Functions
  float distance(Pvector v);
  float dotProduct(Pvector v);
  float magnitude();
  void setMagnitude(float x);
  float angleBetween(Pvector v);
  void normalize();

  Pvector copy(Pvector v);
};

#endif
