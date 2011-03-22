Some kind of bin packing problem variant where I aim to

* learn Haskell
* perhaps provide material for internal company training

The idea is to create solutions for a variant of the [bin packing
problem](http://en.wikipedia.org/wiki/Bin_packing_problem). Basically
we have

* a list of _Lumps_ that each have weight, volume and id for
  identification
* two types of _Containers_ : _bins_ and _bowls_ that have minimum and
  maximum volume and weight

and we want to pack the given list of Lumps to Containers so that

* we need as few Containers as possible
* in case it is impossible to fit all given Lumps to Containers
  satisfying minimum and maximum (see below), the overflow is as small
  as possible
* preferably, the Containers are as evenly filled as possible.

The weight and volume limits are as follows:

* bin : volume 450 -- 900, weight 160 -- 210
* bowl: volume 500 -- 800, weight 720 -- 900

The limits are interpreted so that the maximums may never be surpassed,
but the Container is full enough when either minimum is satisfied.

Sometimes it is not possible to fit all given Lumps to Containers. For
example, if we have three Lumps with a weight of 400 and volume of 400
each, they will not fit to bins at all, because the maximum weight that
can be put to a bin is 210. So we will need to use bowls, and can fit
two Lumps to a bowl, producing a bowl with a weight of 800. But then we
cannot fit the third Lump to the same bowl, and it alone cannot satisfy
the minimum weight nor volume of the bowl, so it is left as overflow. In
case of overflow, it is desired to be as small as possible.

