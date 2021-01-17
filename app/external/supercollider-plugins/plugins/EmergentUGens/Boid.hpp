/*
  This file is part of supercollider-plugins
  Full source at https://github.com/ethancrawford/supercollider-plugins
  License: https://github.com/ethancrawford/supercollider-plugins/LICENSE

  This file was adapted from boids (https://github.com/jyanar/Boids).
*/

#include "Pvector.hpp"
#include <vector>
#include <stdlib.h>
#include <iostream>
#include "RTAllocator.hpp"

#ifndef BOID_H_
#define BOID_H_

// The Boid Class
//
// Attributes
//  bool predator: flag that specifies whether a given boid is a predator.
//  Pvector location: Vector that specifies a boid's location.
//  Pvector velocity: Vector that specifies a boid's current velocity.
//  Pvector acceleration: Vector that specifies a boid's current acceleration.
//  float maxSpeed: Limits magnitude of velocity vector.
//  float maxForce: Limits magnitude of acceleration vector. (F = m*a!)
//
// Methods
//  applyForce(Pvector force): Adds the given vector to acceleration
//
//  Pvector Separation(vector<Boid> Boids): If any other boids are within a
//      given distance, Separation computes a vector that distances the
//      current boid from the boids that are too close.
//
//  Pvector Alignment(vector<Boid> Boids): Computes a vector that causes the
//      velocity of the current boid to match that of boids that are nearby.
//
//  Pvector Cohesion(vector<Boid> Boids): Computes a vector that causes the
//      current boid to seek the center of mass of nearby boids.

class Boid {
public:
  bool predator;
  Pvector location;
  Pvector velocity;
  Pvector acceleration;
  float maxSpeed;
  float maxForce;
  Boid(float x, float y, float vx, float vy, float _max_speed, float _max_force, bool predCheck);
  void applyForce(Pvector force);
  // Three Laws that boids follow
  Pvector Separation(vector<Boid, rt_allocator<Boid>> Boids);
  Pvector Alignment(vector<Boid, rt_allocator<Boid>> Boids);
  Pvector Cohesion(vector<Boid, rt_allocator<Boid>> Boids);

  Pvector seek(Pvector v);
  void run(vector<Boid, rt_allocator<Boid>> v);
  void update();
  void flock(vector<Boid, rt_allocator<Boid>> v);
  void borders();
  float angle(Pvector v);
};

#endif
