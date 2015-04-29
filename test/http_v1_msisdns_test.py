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

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1/msisdns'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(BASE_URL+'/'+MSISDN)

    request.addfinalizer(fin)
    return http

def test_read_bad_msisdn_succ(http):
    req = http.get(BASE_URL+'?msisdn='+BAD_MSISDN)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_create_msisdn_succ(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(BASE_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add some fields expected in response
    req_data['msisdn'] = {'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)}
    req_data['customer_uuid'] = None
    assert resp_data == req_data

def test_create_same_msisdn_fail(http):
    req_data = {'msisdn':MSISDN}
    req = http.post(BASE_URL, data=req_data)
    assert req.status_code == 400
    resp_data = req.json()
    assert resp_data['request_error']['service_exception']['message_id'] == 'SVC0004'

def test_read_all_succ(http):
    req = http.get(BASE_URL+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                'customer_uuid':None}]
    assert resp_data == exp_data

def test_read_free_succ(http):
    req = http.get(BASE_URL+'/'+MSISDN+'?state=free')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':None}]
    assert resp_data == exp_data

def test_read_used_succ(http):
    req = http.get(BASE_URL+'/'+MSISDN+'?state=used')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_update_msisdn_succ(http):
    req_data = {'customer_uuid':CUSTOMER_UUID}
    req = http.put(BASE_URL+'/'+MSISDN, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add some fields expected in response
    req_data['msisdn'] = {'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)}
    assert resp_data == req_data

def test_read_all_2_succ(http):
    req = http.get(BASE_URL+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':CUSTOMER_UUID}]
    assert resp_data == exp_data

def test_read_free_2_succ(http):
    req = http.get(BASE_URL+'/'+MSISDN+'?state=free')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []

def test_read_used_2_succ(http):
    req = http.get(BASE_URL+'/'+MSISDN+'?state=used')
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = [{'msisdn':{'addr':ADDR, 'ton':int(TON), 'npi':int(NPI)},
                 'customer_uuid':CUSTOMER_UUID}]
    assert resp_data == exp_data

def test_delete_msisdn_succ(http):
    req = http.delete(BASE_URL+'/'+MSISDN)
    assert req.status_code == 204

def test_read_all_3_succ(http):
    req = http.get(BASE_URL+'/'+MSISDN+'?state=all')
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == []
