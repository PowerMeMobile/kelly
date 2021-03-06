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
BAD_CUSTOMER_UUID = '00000000-0000-0000-0000-000000000000'

USER_ID = 'user'
BAD_USER_ID = 'bad_user'

ADDR = '999887776655'
TON = '1'
NPI = '1'
MSISDN = ADDR+','+TON+','+NPI

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
MSISDNS_URL = BASE_URL+'/msisdns'
CUSTOMERS_URL = BASE_URL+'/customers'
CUSTOMERS_MSISDNS_URL = CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/msisdns'
CUSTOMERS_USERS_URL = CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users'
CUSTOMERS_USERS_MSISDNS_URL = CUSTOMERS_USERS_URL+'/'+USER_ID+'/msisdns'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    data = {'msisdn':MSISDN}
    req = http.post(MSISDNS_URL, data=data)

    data2 = {'customer_uuid':CUSTOMER_UUID,
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
            'interfaces':'',
            'features':'inbox,true',
            'pay_type':'postpaid',
            'credit':10000.0,
            'credit_limit':10000.0,
            'language':'en',
            'state':'active'}
    req2 = http.post(CUSTOMERS_URL, data=data2)

    data3 = {'user_id':USER_ID,
             'password':'secret',
             'interfaces':'',
             'features':'inbox,true',
             'mobile_phone':'375290000000',
             'first_name':'fn',
             'last_name':'ln',
             'company':'com',
             'occupation':'oc',
             'email':'u@m.c',
             'country':'cou',
             'language':'en',
             'state':'active'}
    req3 = http.post(CUSTOMERS_USERS_URL, data=data3)

    data4 = {'msisdn':MSISDN}
    req4 = http.post(CUSTOMERS_MSISDNS_URL, data=data4)

    def fin():
        print ("finalizing...")
        http.delete(CUSTOMERS_MSISDNS_URL+'/'+MSISDN)
        http.delete(CUSTOMERS_USERS_URL+'/'+USER_ID)
        http.delete(CUSTOMERS_URL+'/'+CUSTOMER_UUID)
        http.delete(MSISDNS_URL+'/'+MSISDN)

    request.addfinalizer(fin)
    return http

def test_read_msisdns_empty_succ(http):
    req = http.get(CUSTOMERS_USERS_MSISDNS_URL)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_create_msisdn_succ(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(CUSTOMERS_USERS_MSISDNS_URL, data=req_data)
    assert req.status_code == 201

def test_create_msisdn_again_fail(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(CUSTOMERS_USERS_MSISDNS_URL, data=req_data)
    assert req.status_code == 400

def test_read_msisdns_non_empty_succ(http):
    req = http.get(CUSTOMERS_USERS_MSISDNS_URL)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == [{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)}]

def test_read_bad_user_fail(http):
    req = http.get(CUSTOMERS_USERS_URL+'/'+BAD_USER_ID+'/msisdns')
    assert req.status_code == 404

def test_delete_msisdn_succ(http):
    req = http.delete(CUSTOMERS_USERS_MSISDNS_URL+'/'+MSISDN)
    assert req.status_code == 204

def test_read_msisdns_2_empty_succ(http):
    req = http.get(CUSTOMERS_USERS_MSISDNS_URL)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_disable_inbox_from_user_should_unassign_msisdn_succ(http):
    test_create_msisdn_succ(http)
    test_read_msisdns_non_empty_succ(http)

    req_data = {'features':'inbox,false'}
    http.put(CUSTOMERS_USERS_URL+'/'+USER_ID, data=req_data)

    try:
        req = http.get(CUSTOMERS_MSISDNS_URL+'/'+MSISDN+'?state=used')
        assert req.json() == []
    finally:
        test_delete_msisdn_succ(http)

def test_delete_user_should_unassign_msisdn_succ(http):
    test_create_msisdn_succ(http)
    test_read_msisdns_non_empty_succ(http)
    http.delete(CUSTOMERS_USERS_URL+'/'+USER_ID)
    req = http.get(CUSTOMERS_MSISDNS_URL+'/'+MSISDN+'?state=used')
    assert req.json() == []
