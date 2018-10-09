# Revision history for components

## 0.1.0.0  -- 2018-10-07

* First version. Provided way to abstract and compose components, along with toy example.

## 0.1.0.1  -- 2018-10-08

* Removed some dumbness from examples.

## 0.2.0.0  -- 2018-10-09

* `Component.List`(`HasComponent`, `Find`) were replaced by `Contains`

## 0.3.0.0  -- 2018-10-09

* Added `NoComponent` as an empty list of components
* Removed `catchFrom` field from `IsComponent` class
* Added `catchFrom` as a global function
* Added `tryThe` as `Control.Exception.try` analogue
* Added README.md
* Renamed `NoComponent` to `End` \[of the list]
* Separated type family `Find` fron the implementation of component list
