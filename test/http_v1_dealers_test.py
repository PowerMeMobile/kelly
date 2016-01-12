# -*- coding: utf-8 -*-

import pytest

import os
import requests
import uuid

KELLY_HOST = os.getenv('KELLY_HOST')
if KELLY_HOST == None or KELLY_HOST == '':
    KELLY_HOST = '127.0.0.1'

KELLY_PORT = os.getenv('KELLY_PORT')
if KELLY_PORT == None or KELLY_PORT == '':
    KELLY_PORT = '8080'

DEALER_UUID = uuid.uuid4().__str__()
DEFAULT_PROVIDER_ID = uuid.uuid4().__str__()
NETWORK_MAP_ID = uuid.uuid4().__str__()

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
DEALERS_URL = BASE_URL+'/dealers'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(DEALERS_URL+'/'+DEALER_UUID)

    request.addfinalizer(fin)
    return http

def test_create_dealer_succ(http):
    req_data = {'id':DEALER_UUID,
                'name':'name',
                'default_provider_id':uuid.uuid4().__str__(),
                'network_map_id':uuid.uuid4().__str__(),
                'interfaces':'transmitter;receiver;transceiver;soap;mm;oneapi;email',
                'features':'inbox,true',
                'state':'active'}
    req = http.post(DEALERS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add some fields expected in response
    req_data['interfaces'] = ['transmitter', 'receiver', 'transceiver', 'soap', 'mm', 'oneapi', 'email']
    req_data['features'] = [{'name': 'inbox', 'value': 'true'}]
    req_data['default_provider_name'] = ''
    req_data['network_map_name'] = ''
    assert resp_data == req_data

def test_create_dealer_w_same_dealer_id_fail(http):
    req_data = {'id':DEALER_UUID,
                'name':'name',
                'default_provider_id':uuid.uuid4().__str__(),
                'network_map_id':uuid.uuid4().__str__(),
                'interfaces':'transmitter;receiver;transceiver;soap;mm;oneapi;email',
                'features':'inbox,true',
                'state':'active'}
    req = http.post(DEALERS_URL, data=req_data)
    assert req.status_code == 400

def test_update_dealer_succ(http):
    req_data = {'id':DEALER_UUID,
                'name':'newname',
                'default_provider_id':DEFAULT_PROVIDER_ID,
                'network_map_id':NETWORK_MAP_ID,
                'interfaces':'email',
                'features':'',
                'state':'blocked'}
    req = http.put(DEALERS_URL, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add some fields expected in response
    req_data['interfaces'] = ['email']
    req_data['features'] = []
    req_data['default_provider_name'] = ''
    req_data['network_map_name'] = ''
    assert resp_data == req_data

def test_read_dealer_succ(http):
    req = http.get(DEALERS_URL+'/'+DEALER_UUID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':DEALER_UUID,
                'name':'newname',
                'default_provider_id':DEFAULT_PROVIDER_ID,
                'network_map_id':NETWORK_MAP_ID,
                'interfaces':'email',
                'features':'',
                'state':'blocked',
                'interfaces':['email'],
                'features':[],
                'default_provider_name':'',
                'network_map_name':''}
    assert resp_data == exp_data

def test_delete_dealer_succ(http):
    req = http.delete(DEALERS_URL+'/'+DEALER_UUID)
    assert req.status_code == 204

def test_read_deleted_dealer_fail(http):
    req = http.get(DEALERS_URL+'/'+DEALER_UUID)
    assert req.status_code == 404

def test_create_dealer_w_same_id_ad_delete_one_fail(http):
    req_data = {'id':DEALER_UUID,
                'name':'name',
                'default_provider_id':uuid.uuid4().__str__(),
                'network_map_id':uuid.uuid4().__str__(),
                'interfaces':'transmitter;receiver;transceiver;soap;mm;oneapi;email',
                'features':'inbox,true',
                'state':'active'}
    req = http.post(DEALERS_URL, data=req_data)
    assert req.status_code == 500

def test_read_bad_dealer_fail(http):
    bad_dealer_id = uuid.uuid4().__str__()
    req = http.get(DEALERS_URL+'/'+bad_dealer_id)
    assert req.status_code == 404
