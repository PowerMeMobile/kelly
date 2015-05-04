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

ADDR = '999887776655'
TON = '1'
NPI = '1'
MSISDN = ADDR+','+TON+','+NPI
BAD_MSISDN = '00000,0,0'
CUSTOMER_UUID = '493b3678-9dc8-11e2-8cce-00269e42f7a5'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
BASE_MSISDNS_URL = BASE_URL+'/msisdns'
BASE_CUSTOMERS_URL = BASE_URL+'/customers'

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
    req = http.post(BASE_CUSTOMERS_URL, data=data)
    print req.json()

    def fin():
        print ("finalizing...")
        http.delete(BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID)
        http.delete(BASE_MSISDNS_URL+'/'+MSISDN)

    request.addfinalizer(fin)
    return http

def test_read_bad_msisdn_succ(http):
    req = http.get(BASE_MSISDNS_URL+'?msisdn='+BAD_MSISDN)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_create_msisdn_succ(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(BASE_MSISDNS_URL, data=req_data)
    assert req.status_code == 201

def test_create_same_msisdn_fail(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(BASE_MSISDNS_URL, data=req_data)
    assert req.status_code == 400
    resp_data = req.json()
    assert resp_data['request_error']['service_exception']['message_id'] == 'SVC0004'

def test_read_all_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':None,
                 'customer_id':None,
                 'customer_name':None}]
    assert resp_data == exp_data

def test_read_free_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=free')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':None,
                 'customer_id':None,
                 'customer_name':None}]
    assert resp_data == exp_data

def test_read_used_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=used')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_update_msisdn_succ(http):
    req_data = {'customer_uuid':CUSTOMER_UUID}
    req = http.put(BASE_MSISDNS_URL+'/'+MSISDN, data=req_data)
    assert req.status_code == 200

def test_read_all_2_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':CUSTOMER_UUID,
                 'customer_id':'0',
                 'customer_name':'name'}]
    assert resp_data == exp_data

def test_read_free_2_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=free')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_read_used_2_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=used')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':CUSTOMER_UUID,
                 'customer_id':'0',
                 'customer_name':'name'}]
    assert resp_data == exp_data

def test_delete_msisdn_succ(http):
    req = http.delete(BASE_MSISDNS_URL+'/'+MSISDN)
    assert req.status_code == 204

def test_read_all_3_succ(http):
    req = http.get(BASE_MSISDNS_URL+'/'+MSISDN+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []
