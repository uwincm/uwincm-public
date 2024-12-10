# w2c

Vortex tracking and relocation.

## Getting started

```
git clone git@gitlab.com:uwincm/w2c.git
cd w2c
make
```

If you are not using Intel compiler, edit `FC` and `FCFLAGS` in `Makefile`.

+++ Path to NetCDF library must be set in `NETCDF` env variable. +++

## Usage

1. Place `in.w2c` and `do.w2c` in a local directory.
2. Edit `in.w2c` to set storm-dependent parameters.
3. Run `do.w2c`. Output will be written in `w2c/`.

## Origin

* Original code by Guo-yuan Lien
* Modifications by Chia-ying Lee
* Bug fixes and clean-up by Milan Curcic
