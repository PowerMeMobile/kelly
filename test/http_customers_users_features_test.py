# setup python's virtualenv as described here
# https://gist.github.com/ten0s/125f708b185daeab184a

# $ . env/bin/activate
# $ py.test http_customers_users_features_test.py
# $ py.test http_customers_users_features_test.py -k test_bad_customer_fail
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest
import requests

HOST = 'localhost'
PORT = '8080'
CUSTOMER_ID = '493b3678-9dc8-11e2-8cce-00269e42f7a5'
USER_ID = 'user'

BAD_CUSTOMER_ID = 'bad_customer'
BAD_USER_ID = 'bad_user'

BASE_URL = 'http://'+HOST+':'+PORT+'/customers/'+CUSTOMER_ID+'/users/'+USER_ID+'/features'
BAD_CUSTOMER_URL = 'http://'+HOST+':'+PORT+'/customers/'+BAD_CUSTOMER_ID+'/users/'+USER_ID+'/features'
BAD_USER_URL = 'http://'+HOST+':'+PORT+'/customers/'+CUSTOMER_ID+'/users/'+BAD_USER_ID+'/features'

@pytest.fixture(scope="function")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        req = http.get(BASE_URL)
        if req.status_code == 200:
            for data in req.json():
                http.delete(BASE_URL+'/'+data['name'])

    request.addfinalizer(fin)
    return http

def test_bad_customer_fail(http):
    req = http.get(BAD_CUSTOMER_URL)
    assert req.status_code == 404

def test_bad_user_fail(http):
    req = http.get(BAD_USER_URL)
    assert req.status_code == 404

def test_get_all_empty_succ(http):
    req = http.get(BASE_URL)
    assert req.status_code == 200
    assert req.json() == []

def test_get_non_existing_fail(http):
    req = http.get(BASE_URL+'/override_originator')
    assert req.status_code == 404

def test_post_new_succ(http):
    req = http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})
    assert req.status_code == 200
    assert len(req.json()) == 1
    assert req.json() == [{'name':'override_originator', 'value':'empty'}]

def test_post_existing_fail(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})
    assert req.status_code == 400

def test_post_another_succ(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.post(BASE_URL, data={'name':'inbox', 'value':'false'})
    assert req.status_code == 200
    assert len(req.json()) == 2
    assert req.json() == [{'name':'override_originator', 'value':'empty'}, {'name':'inbox', 'value':'false'}]

def test_get_all_succ(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_URL, data={'name':'inbox', 'value':'false'})

    req = http.get(BASE_URL)
    assert req.status_code == 200
    assert len(req.json()) == 2
    assert req.json() == [{'name':'override_originator', 'value':'empty'}, {'name':'inbox', 'value':'false'}]

def test_get_existing_succ(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_URL, data={'name':'inbox', 'value':'false'})

    req = http.get(BASE_URL+'/inbox')
    assert req.json() == {'name':'inbox', 'value':'false'}

def test_put_existing_succ(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.put(BASE_URL+'/override_originator?value=any')
    assert req.status_code == 200
    assert req.json() == [{'name':'override_originator', 'value':'any'}]

def test_put_non_existing_fail(http):
    req = http.put(BASE_URL+'/override_originator?value=any')
    assert req.status_code == 404

def test_delete_existing_succ(http):
    http.post(BASE_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_URL, data={'name':'inbox', 'value':'false'})

    req = http.delete(BASE_URL+'/override_originator')
    assert req.status_code == 200
    assert req.json() == [{'name':'inbox', 'value':'false'}]

def test_delete_non_existing_succ(http):
    req = http.delete(BASE_URL+'/unknown')
    assert req.status_code == 200
    assert req.json() == []
