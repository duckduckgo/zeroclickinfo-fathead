#!/bin/bash

mkdir -p download
cd download
rm -f *.html

wget --quiet https://docs.djangoproject.com/en/1.10/ref/contrib/ -O contrib.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/django-admin/ -O django-admin.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/exceptions/ -O exceptions.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/forms/fields/ -O form_fields.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/forms/widgets/ -O widgets.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/templates/builtins/ -O builtins.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/settings/ -O settings.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/validators/ -O validators.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/views/ -O view.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/urlresolvers/ -O urlresolvers.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/urls/ -O urls.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/models/database-functions/ -O database-functions.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/models/fields/ -O fields.html
wget --quiet https://docs.djangoproject.com/en/1.10/ref/migration-operations/ -O migration-operations.html
