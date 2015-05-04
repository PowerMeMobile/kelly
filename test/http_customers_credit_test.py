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

CUSTOMER_UUID = '2dca065f-c328-4a19-bc92-b222f48764e1'
BAD_CUSTOMER_UUID = 'bad_customer'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers'
CUSTOMER_URL = BASE_URL+'/'+CUSTOMER_UUID
CREDIT_URL = CUSTOMER_URL+'/credit'
BAD_CREDIT_URL = BASE_URL+'/'+BAD_CUSTOMER_UUID+'/credit'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    data = {'customer_uuid':CUSTOMER_UUID,
            'customer_id':'0',
            'name':'name',
            'priority':1,
            'rps':1000,
            'receipts_allowed':True,
            'no_retry':False,
            'default_validity':'000003000000000R',
            'max_validity':259200,
            'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
            'network_map_id':'befa8b7c-c4a3-11e3-b670-00269e42f7a5',
            'pay_type':'postpaid',
            'credit':10000.0,
            'credit_limit':10000.0,
            'language':'en',
            'state':'active'}
    req = http.post(BASE_URL, data=data)

    def fin():
        print ("finalizing...")
        http.delete(BASE_URL+'/'+CUSTOMER_UUID)

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
