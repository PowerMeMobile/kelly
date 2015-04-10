## Create virtualenv and install deps into it
<pre>
$ virtualenv env --no-site-packages
$ source env/bin/activate
$ pip install -r requirements.txt
</pre>

## Activate virtualenv and run tests
<pre>
$ source env/bin/activate
$ py.test -v *.py
$ py.test -v http_blacklist_test.py -k SOME_TESTS
</pre>
