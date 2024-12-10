# WRF Input Directory

This is a "one stop shop" directory for generating inputs for the WRF component of UWIN-CM. It is built off of the standard WPS directory, and also includes some additional scripts.

+++ WPS needs to be compiled first! +++

Tip: If you will be generating multiple sets of WRF inputs within the same uwincm directory,
feel free to copy this entire directory and run the steps in the copied directory.

The following steps can be followed to generate WRF inputs.

## 1. Copy WPS files 

The **Makefile.MASTER** file is set up to get the necessary files from **../src/components/WPS-3.9.1**.
- `cp Makefile.MASTER Makefile`
- Edit the path to WPS in the Makefile, if needed.
- `make` to copy over the files that you need.
- `make clean` to reset it.


## 2. Download global model data

### ERA-5

The Python scripts **do.download.cdsapi.pres.py** and **do.download.cdsapi.sfc.py** will download ERA5 data from the Climate Data Store (CDS) using their Python based API system. Copy the MASTER scripts then edit the time period.
```
cp do.download.cdsapi.sfc.py.MASTER do.download.cdsapi.sfc.py
cp do.download.cdsapi.pres.py.MASTER do.download.cdsapi.pres.py
```

For instructions on setting up your access to CDS, see [here](https://cds.climate.copernicus.eu/api-how-to). Follow their instructions for "Install the CDS API key." The rest of the steps are repeated below in this section.

Copy the MASTER scripts then edit the time period.

To use the scripts, you need to use a Python environment that has the cdsapi module. If you already have one, activate it. Otherwise, you need to create one. If you are familiar with Anaconda, use those instructions. Or, you can generate a Python virtual environment (venv) like this:
```
python -m venv ./venv
source ./venv/bin/activate
pip install cdsapi
```
If you're coming back to a directory where you already installed cdsapi, just use the "source" line.

_Note that the pressure level data takes **MUCH** longer than the surface data!_ I suggest downloading the surface data, then using wgrib to verify that you have the time period you want, then go ahead and download the pressure level data.

To run the scripts, do:
```
python do.download.cdsapi.sfc.py
python do.download.cdsapi.pres.py
```

This will download the ERA5 grib data to two files: **era5.sfc.grib** and **era5.pres.grib**. Or whatever file names you changed it to in the scripts.

After downloading the data, it is a good idea to deactivate the Python environment using the `deactivate` command to avoid potential Python environment conflicts as you keep using the terminal window.


## 3. Set up WRF model domain and topography

Follow the Geogrid instructions from WRF. See also the [Wiki page]().
- Edit namelist.wps
- Preview the domain using `ncl util/plotgrids_new.ncl` (optional)
- `geogrid.exe` to generate geo_em files.

Note: geogrid usually requires high memory use, so do `ulimit -s unlimited` or source the src/set_env_orca.sh environment before running geogrid.exe.


## 4. Create met_em files for real.exe.

In this step, follow the instructions from WRF.
- link_grib.csh to link the grib files.
- metgrid.exe to generate met_em files.

