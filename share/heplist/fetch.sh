#!/bin/bash

mkdir -p download

wget --quiet -P download http://pdg.lbl.gov/2012/mcdata/mass_width_2012.mcd

wget --quiet -P download http://cepa.fnal.gov/psm/stdhep/pdg/pdg_elem.tex
wget --quiet -P download http://cepa.fnal.gov/psm/stdhep/pdg/pdg_meson.tex
wget --quiet -P download http://cepa.fnal.gov/psm/stdhep/pdg/pdg_baryon.tex
