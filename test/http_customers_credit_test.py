# -*- coding: utf-8 -*-

import pytest

import os
import requests

KELLY_HOST = os.getenv('KELLY_HOST')
if KELLY_HOST == None or KELLY_HOST == '':
    KELLY_HOST = '127.0.0.1'

KELLY_PORT = os.getenv('KELLY_PORT')
if KELLY_PORT == None or KELLY_PORT == '':
    KELLY_PORT = '8080'

CUSTOMER_ID = '493b3678-9dc8-11e2-8cce-00269e42f7a5'
BAD_CUSTOMER_ID = 'bad_customer'

CUSTOMER_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers/'+CUSTOMER_ID
CREDIT_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers/'+CUSTOMER_ID+'/credit'
BAD_CREDIT_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers/'+BAD_CUSTOMER_ID+'/credit'

@pytest.fixture(scope="function")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")

    request.addfinalizer(fin)
    return http

def test_get_credit_fail(http):
    req = http.get(CREDIT_URL)
    assert req.status_code == 400

def test_delele_credit_fail(http):
    req = http.delete(CREDIT_URL)
    assert req.status_code == 400

def test_post_credit_fail(http):
    req = http.post(CREDIT_URL)
    assert req.status_code == 400

def test_put_bad_customer_fail(http):
    req = http.put(BAD_CREDIT_URL, data={'amount':0.0})
    assert req.status_code == 404

def test_put_non_float_fail(http):
    req = http.put(CREDIT_URL, data={'amount':'zero'})
    assert req.status_code == 400

def test_put_float_succ(http):
    req = http.put(CREDIT_URL, data={'amount':0.0})
    assert req.status_code == 200

def test_inc_credit_succ(http):
    inc_credit = 1000.0

    req = http.get(CUSTOMER_URL)
    assert req.status_code == 200
    init_credit = req.json()['credit']

    req = http.put(CREDIT_URL, data={'amount':inc_credit})
    assert req.status_code == 200

    req = http.get(CUSTOMER_URL)
    assert req.status_code == 200
    new_credit = req.json()['credit']

    assert init_credit + inc_credit == new_credit

def test_dec_credit_succ(http):
    dec_credit = -1000.0

    req = http.get(CUSTOMER_URL)
    assert req.status_code == 200
    init_credit = req.json()['credit']

    req = http.put(CREDIT_URL, data={'amount':dec_credit})
    assert req.status_code == 200

    req = http.get(CUSTOMER_URL)
    assert req.status_code == 200
    new_credit = req.json()['credit']

    assert init_credit + dec_credit == new_credit
