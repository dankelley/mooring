### Line properties

Stored in file `line_data.dat`, with data sources as below.

#### 3/8-inch wire cable

* CD = 1.2 [DeepWater Buoyancy, 2021b].
* radius = 0.0047625
```R
> (1/2) * (3/8) * 0.0254 # radius [m]
[1] 0.0047625
```

### Float properties

#### Mooring Systems SSF-30
* https://www.mooringsystems.com/buoyancy.htm
* radius = 0.381
```R
> (1/2) * (30)*0.0254 # radius [m]
[1] 0.381
```


#### Mooring Systems SSF-37
* https://www.mooringsystems.com/buoyancy.htm
* radius = 0.4699
```R
> (1/2) * (37)*0.0254 # radius [m]
[1] 0.4699
```

#### HydroFloat 20
* https://deepwaterbuoyancy.com/wp-content/uploads/hydro-float-mooring-buoys-deepwater-buoyancy.pdf
* diameter 20 inches
* radius [m]
```r
> (1/2) * (20)*0.0254 # radius [m]
[1] 0.254
```
* CD=0.5 (range perhaps 0.4 to 1.0) guessing based on Figures 3 and 4 of Finke & Siedler (1986)

Stored in file `float_data.dat`.

*Float drag coefficient*

* Drag coefficient (diagrams figures): Fink and Siedler (1986).

**References**

* DeepWater Buoyancy, 2021a. “Syntactic Foam Subsea Buoyancy Products for
  Oceanography / Oil & Gas.” Accessed May 14, 2021.
https://deepwaterbuoyancy.com/products/.

* DeepWater Buoyancy, 2021b. “Mooring Line Cable Fairing.” Accessed May 14,
  2021.  https://deepwaterbuoyancy.com/product/mooring-line-cable-fairing/.

* Finke, M., and G. Siedler, 1986. “Drag Coefficients of Oceanographic Mooring
  Components.” Journal of Atmospheric and Oceanic Technology 3(2):255-264")

