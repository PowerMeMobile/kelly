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
CUSTOMER_ID = '0'
CUSTOMER_NAME = 'name'

BAD_CUSTOMER_UUID = '00000000-0000-0000-0000-000000000000'

ORIGINATOR_ID = 'd4000b38-f2d8-11e3-ba01-00269e42f7a5'
BAD_ORIGINATOR_ID = 'bad_originator'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
CUSTOMERS_URL = BASE_URL+'/customers'
ORIGINATORS_URL = CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/originators'

NETWORK_MAP_ID = 'c32d6325-6823-49dc-9b39-0a9451028c65'
DEFAULT_PROVIDER_ID = 'd5be9611-d36a-4821-a1b3-0a9d4db8ea90'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    data = {'customer_uuid':CUSTOMER_UUID,
            'customer_id':CUSTOMER_ID,
            'name':CUSTOMER_NAME,
            'priority':1,
            'rps':1000,
            'receipts_allowed':True,
            'no_retry':False,
            'default_validity':'000003000000000R',
            'max_validity':259200,
            'network_map_id':'befa8b7c-c4a3-11e3-b670-00269e42f7a5',
            'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
            'interfaces':'',
            'features':'',
            'pay_type':'postpaid',
            'credit':10000.0,
            'credit_limit':10000.0,
            'language':'en',
            'state':'active'}
    req = http.post(CUSTOMERS_URL, data=data)

    def fin():
        print ("finalizing...")
        http.delete(CUSTOMERS_URL+'/'+CUSTOMER_UUID)

    request.addfinalizer(fin)
    return http

def test_create_originator_succ(http):
    req_data = {'id':ORIGINATOR_ID,
                'msisdn':'375290000000,1,1',
                'description':'descr',
                'state':'approved',
                'routings':'',
                'is_default':True}
    req = http.post(ORIGINATORS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['customer_uuid'] = CUSTOMER_UUID
    req_data['customer_id'] = CUSTOMER_ID
    req_data['customer_name'] = CUSTOMER_NAME
    req_data['msisdn'] = {'addr':'375290000000', 'ton':1, 'npi':1}
    req_data['routings'] = []
    assert resp_data == req_data

def test_update_originator_succ(http):
    req_data = {'msisdn':'Hello,5,0',
                'description':'descr2',
                'state':'pending',
                'routings':NETWORK_MAP_ID + ',' + DEFAULT_PROVIDER_ID,
                'is_default':False}
    req = http.put(ORIGINATORS_URL+'/'+ORIGINATOR_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['id'] = ORIGINATOR_ID
    req_data['customer_uuid'] = CUSTOMER_UUID
    req_data['customer_id'] = CUSTOMER_ID
    req_data['customer_name'] = CUSTOMER_NAME
    req_data['msisdn'] = {'addr':'Hello', 'ton':5, 'npi':0}
    req_data['routings'] = [{'network_map_id':NETWORK_MAP_ID,'default_provider_id':DEFAULT_PROVIDER_ID}]
    assert resp_data == req_data

def test_read_bad_originator_succ(http):
    req = http.get(ORIGINATORS_URL+'/'+BAD_ORIGINATOR_ID)
    assert req.status_code == 404

def test_read_originator_succ(http):
    req = http.get(ORIGINATORS_URL+'/'+ORIGINATOR_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':ORIGINATOR_ID,
                'customer_uuid':CUSTOMER_UUID,
                'customer_id':CUSTOMER_ID,
                'customer_name':CUSTOMER_NAME,
                'description':'descr2',
                'state':'pending',
                'is_default':False,
                'routings':[{'network_map_id':NETWORK_MAP_ID,'default_provider_id':DEFAULT_PROVIDER_ID}],
                'msisdn':{'addr':'Hello', 'ton':5, 'npi':0}}
    assert resp_data == exp_data

def test_delete_originator_succ(http):
    req = http.delete(ORIGINATORS_URL+'/'+ORIGINATOR_ID)
    assert req.status_code == 204
