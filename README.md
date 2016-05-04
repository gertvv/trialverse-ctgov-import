# ctgov-import

Import of ClinicalTrials.gov records into TrialVerse / ADDIS RDF format.

## Usage

Dependencies:

You need to `lein install` the `addis-rdf` module from `addis-core`, in `trialverse/addis-rdf`.

Command line conversion of ClinicalTrials.gov XML:

`lein run $FILE`

Web service:

`lein ring server`

Build an uberjar for the webservice:

`lein ring uberjar`

Build and run in docker:

```
lein ring uberjar
cp target/ctgov-import-*-standalone.jar docker/
cd docker
docker build -d ctgov-import .
docker run -d -p 3000:3000 ctgov-import
```

## License

Copyright (c) 2014-2016 Gert van Valkenhoef

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
