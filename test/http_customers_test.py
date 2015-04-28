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

CUSTOMER_UUID = '493b3678-9dc8-11e2-8cce-00269e42f7a5'
BAD_CUSTOMER_UUID = 'bad_customer'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(BASE_URL+'/'+CUSTOMER_UUID)

    request.addfinalizer(fin)
    return http

def test_create_customer_succ(http):
    req_data = {'customer_uuid':CUSTOMER_UUID,
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
    req = http.post(BASE_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add some fields expected in response
    req_data['users'] = []
    req_data['originators'] = []
    assert resp_data == req_data

def test_update_customer_succ(http):
    req_data = {'customer_id':'1',
                'name':'name2',
                'priority':2,
                'rps':500,
                'receipts_allowed':False,
                'no_retry':True,
                'default_validity':'000004000000000R',
                'max_validity':259201,
                'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec74',
                'network_map_id':'cf1563b0-c4a3-11e3-8a61-00269e42f7a5',
                'pay_type':'prepaid',
                'credit':20000.0,
                'credit_limit':0.0,
                'language':'fr',
                'state':'blocked'}
    req = http.put(BASE_URL+'/'+CUSTOMER_UUID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add some fields expected in response
    req_data['customer_uuid'] = CUSTOMER_UUID
    req_data['users'] = []
    req_data['originators'] = []
    assert resp_data == req_data

def test_read_bad_customer_succ(http):
    req = http.get(BASE_URL+'/'+BAD_CUSTOMER_UUID)
    assert req.status_code == 404

def test_read_customer_succ(http):
    req = http.get(BASE_URL+'/'+CUSTOMER_UUID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'customer_uuid':CUSTOMER_UUID,
                'customer_id':'1',
                'name':'name2',
                'priority':2,
                'rps':500,
                'receipts_allowed':False,
                'no_retry':True,
                'default_validity':'000004000000000R',
                'max_validity':259201,
                'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec74',
                'network_map_id':'cf1563b0-c4a3-11e3-8a61-00269e42f7a5',
                'pay_type':'prepaid',
                'credit':20000.0,
                'credit_limit':0.0,
                'language':'fr',
                'state':'blocked',
                'users':[],
                'originators':[]}
    assert resp_data == exp_data

def test_delete_customer_succ(http):
    req = http.delete(BASE_URL+'/'+CUSTOMER_UUID)
    assert req.status_code == 204
