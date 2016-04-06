Lambdaya
--------

### Library for doing IO on [RedPitaya](http://redpitaya.com/) using [Haskell](https://www.haskell.org/)

This library provides native binding on
[Fpga](https://github.com/RedPitaya/RedPitaya/blob/master/fpga/doc/RedPitaya_HDL_memory_map.odt?raw=true)
Registry map.

[Notes](doc/build.md) on how to build GHC as crosscompiler


Example from [tutorial](doc/tutorial.md) that turns bunch of LEDs on RedPitaya.


    import System.RedPitaya.Fpga

    main = withOpenFpga (setLed 0x55)
