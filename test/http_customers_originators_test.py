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

ORIGINATOR_ID = 'd4000b38-f2d8-11e3-ba01-00269e42f7a5'
BAD_ORIGINATOR_ID = 'bad_originator'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers'
BASE_ORIGINATORS_URL = BASE_URL+'/'+CUSTOMER_UUID+'/originators'

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

def test_create_originator_succ(http):
    req_data = {'id':ORIGINATOR_ID,
                'address':'375290000000,1,1',
                'description':'descr',
                'state':'approved',
                'is_default':True}
    req = http.post(BASE_ORIGINATORS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['address'] = {'addr':'375290000000', 'ton':1, 'npi':1, 'ref_num':None}
    assert resp_data == req_data

def test_update_originator_succ(http):
    req_data = {'address':'Hello,5,0',
                'description':'descr2',
                'state':'pending',
                'is_default':False}
    req = http.put(BASE_ORIGINATORS_URL+'/'+ORIGINATOR_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['id'] = ORIGINATOR_ID
    req_data['address'] = {'addr':'Hello', 'ton':5, 'npi':0, 'ref_num':None}
    assert resp_data == req_data

def test_read_bad_originator_succ(http):
    req = http.get(BASE_ORIGINATORS_URL+'/'+BAD_ORIGINATOR_ID)
    assert req.status_code == 404

def test_read_originator_succ(http):
    req = http.get(BASE_ORIGINATORS_URL+'/'+ORIGINATOR_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':ORIGINATOR_ID,
                'address':'Hello,5,0',
                'description':'descr2',
                'state':'pending',
                'is_default':False,
                'address':{'addr':'Hello', 'ton':5, 'npi':0, 'ref_num':None}}
    assert resp_data == exp_data

def test_delete_originator_succ(http):
    req = http.delete(BASE_ORIGINATORS_URL+'/'+ORIGINATOR_ID)
    assert req.status_code == 204
