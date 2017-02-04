# Python Coverage Fathead
Pulls data from Coverage API Documentation at https://coverage.readthedocs.io/en/latest/api.html

## Dependencies

Requires Python 3. Created/Tested using Python 3.5.2
### includes requirements.txt

Dependencies are:
* requests
* bs4

## Running

* Install requirements:
```
$ pip3.5 install -r requirements.txt
```

* Download latest Coverage API Documentation via:
```
$ bash fetch.sh
```

* Parse downloaded documentation via:
```
$ bash parse.sh
```
