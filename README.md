Lambdaya-bus
------------

### Fpga bus and serialistion for [RedPitaya](http://redpitaya.com/) using [CλaSH](http://www.clash-lang.org/)

#### Installation

-   Clash

    > Follow installation manuals on
    > [clash-lang.org](http://www.clash-lang.org/#details) Following
    > this you will install both recent Haskell compiler GHC and Clash
    > compiler

-   Install lambdaya-bus and dependencies using cabal

    > `cabal update`\
    > `cabal install lambdaya-bus`

-   Install Xilinx Vivado 2015.4 for example. Tool is free to use and
    software SDK is not requred.

-   Get RedPitaya fpga code.

    > Branch [`clash`](https://github.com/ra1u/RedPitaya/tree/clash) is
    > patched with code that enables automatic inclusion in existing
    > Redpitaya fpga coe

        git clone -b clash https://github.com/ra1u/RedPitaya.git

    > or if you have RedPitaya git repo

        cd <git dir>
        git remote add lambdaya https://github.com/ra1u/RedPitaya.git
        git fetch lambdaya
        git checkout -b clash lambdaya/clash

#### Build first core

Example application is available in [lambdaya-bus
repo](https://github.com/ra1u/lambdaya-bus/tree/master/examples/MatrixMultiply)
and includes code for both core and remote tcp client. It is matrix
multiplication core.

Clone repository

    git clone https://github.com/ra1u/lambdaya-bus.git
    cd lambdaya-bus/examples/MatrixMultiply

compile into verilog code

    clash Core.hs --verilog

This will generate `verilog` folder with with verilog code that can be
included in RedPitaya fpga core.

Copy this folder in RedPitaya folder `RedPitaya/fpga/rtl/clash`

    . /opt/Xilinx/Vivado/2015.4/settings64.sh
     

move to `fpga` folder

    make clean
    make fpga

Once finished send fpga bitstream on RedPitaya

    ssh root@<RP_IP> tee /dev/xdevcfg < ./out/red_pitaya.bit > /dev/null

That is It. Core is uploaded and running. It represents matrix
multiplication of 2 matrices 3x3 with elements being 16 bit Signed
values.Addressees for core input are from 0x40500000. Since each value
is 16 Bit in wide there are first 9 \* 16 bit as first 3x3 matrix and
2nd matrix is from from next bit on. Reading out starts at same address
that is 0x40500000. Functions of original core like oscilloscope and
function generator are included in bit-stream.

### Remote call

Making core and having bus is one part of fun. We can write and read
data from fpga using provided `monitor` tool from address 0x40500000 or
we can run this code remotely using haskell and this libraray.

First we need to start
[`server`](https://github.com/ra1u/Lambdaya/blob/master/bin/server) from
[lambdaya](https://github.com/ra1u/Lambdaya/tree/master/bin) on
RedPitaya.

Then set ip in `lamndaya-bus/examples/MatrixMultiply/Client.hs` for
RedPitaya.

    ghc -O3 Client.hs

And finaly run client

    ./Client

to get output

    <<1,2,3>,<4,5,6>,<7,8,9>>
    <<10,11,12>,<13,14,15>,<16,17,18>>
    <<84,90,96>,<201,216,231>,<318,342,366>>

That is 3rd line is product of previous two. What happens here is that 2
matrices are send over tcp on rp server, serialized on fpga bus and
consumed by fpga core. Then result is read back same way from core and
displayed.
