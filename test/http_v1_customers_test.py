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

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
CUSTOMERS_URL = BASE_URL+'/customers'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(CUSTOMERS_URL+'/'+CUSTOMER_UUID)

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
                'interfaces':'transmitter;receiver;transceiver;soap;mm;oneapi;email',
                'features':'inbox,true',
                'pay_type':'postpaid',
                'credit':10000.0,
                'credit_limit':10000.0,
                'language':'en',
                'state':'active'}
    req = http.post(CUSTOMERS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add some fields expected in response
    req_data['users'] = []
    req_data['originators'] = []
    req_data['interfaces'] = ['transmitter', 'receiver', 'transceiver', 'soap', 'mm', 'oneapi', 'email']
    req_data['features'] = [{'name': 'inbox', 'value': 'true'}]
    assert resp_data == req_data

def test_create_customer_w_same_customer_id_fail(http):
    req_data = {'customer_id':'0',
                'name':'name3',
                'priority':1,
                'rps':1000,
                'receipts_allowed':True,
                'no_retry':False,
                'default_validity':'000003000000000R',
                'max_validity':259200,
                'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
                'network_map_id':'befa8b7c-c4a3-11e3-b670-00269e42f7a5',
                'interfaces':'transmitter;receiver;transceiver;soap;mm;oneapi;email',
                'features':'inbox,true',
                'pay_type':'postpaid',
                'credit':10000.0,
                'credit_limit':10000.0,
                'language':'en',
                'state':'active'}
    req = http.post(CUSTOMERS_URL, data=req_data)
    assert req.status_code == 400

def test_update_customer_succ(http):
    req_data = {'name':'name2',
                'priority':2,
                'rps':500,
                'receipts_allowed':False,
                'no_retry':True,
                'default_validity':'000004000000000R',
                'max_validity':259201,
                'default_provider_id':'',
                'network_map_id':'cf1563b0-c4a3-11e3-8a61-00269e42f7a5',
                'interfaces':'transmitter',
                'features':'inbox,false;sms_from_email,true',
                'pay_type':'prepaid',
                'credit_limit':0.0,
                'language':'fr',
                'state':'blocked'}
    req = http.put(CUSTOMERS_URL+'/'+CUSTOMER_UUID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add some fields expected in response
    req_data['customer_uuid'] = CUSTOMER_UUID
    req_data['customer_id'] = '0'
    req_data['credit'] = 10000.0
    req_data['users'] = []
    req_data['originators'] = []
    req_data['interfaces'] = ['transmitter']
    req_data['features'] = [{'name':'inbox', 'value':'false'}, {'name':'sms_from_email', 'value':'true'}]
    assert resp_data == req_data

def test_read_bad_customer_fail(http):
    req = http.get(CUSTOMERS_URL+'/'+BAD_CUSTOMER_UUID)
    assert req.status_code == 404

def test_read_customer_succ(http):
    req = http.get(CUSTOMERS_URL+'/'+CUSTOMER_UUID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'customer_uuid':CUSTOMER_UUID,
                'customer_id':'0',
                'name':'name2',
                'priority':2,
                'rps':500,
                'originators':[],
                'receipts_allowed':False,
                'no_retry':True,
                'default_validity':'000004000000000R',
                'max_validity':259201,
                'default_provider_id':None,
                'network_map_id':'cf1563b0-c4a3-11e3-8a61-00269e42f7a5',
                'users':[],
                'interfaces':['transmitter'],
                'features':[{'name':'inbox', 'value':'false'}, {'name':'sms_from_email', 'value':'true'}],
                'pay_type':'prepaid',
                'credit':10000.0,
                'credit_limit':0.0,
                'language':'fr',
                'state':'blocked'}
    assert resp_data == exp_data

def test_delete_customer_succ(http):
    req = http.delete(CUSTOMERS_URL+'/'+CUSTOMER_UUID)
    assert req.status_code == 204
