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

GATEWAY_ID = '2a4d2aee-c4b1-11e3-9bc1-00269e42f7a5'
CONNECTION_ID = '0'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT
GATEWAYS_URL = BASE_URL+'/gateways'
GATEWAYS_CONNECTIONS_URL = GATEWAYS_URL+'/'+GATEWAY_ID+'/connections'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    req_data = {'id':GATEWAY_ID,
                'name':'gateway',
                'rps':10000}
    req = http.post(GATEWAYS_URL, data=req_data)

    def fin():
        print ("finalizing...")
        http.delete(GATEWAYS_URL+'/'+GATEWAY_ID)
        http.delete(GATEWAYS_CONNECTIONS_URL+'/'+CONNECTION_ID)

    request.addfinalizer(fin)
    return http

def test_create_connection_succ(http):
    req_data = {'id':int(CONNECTION_ID),
                'host':'127.0.0.1',
                'port':8001,
                'bind_type':'transmitter',
                'system_id':'id',
                'password':'password',
                'system_type':'smpp',
                'addr_ton':1,
                'addr_npi':1,
                'addr_range':''}
    req = http.post(GATEWAYS_CONNECTIONS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    assert resp_data == req_data

def test_update_connection_succ(http):
    req_data = {'id':int(CONNECTION_ID),
                'host':'localhost',
                'port':8002,
                'bind_type':'receiver',
                'system_id':'id2',
                'password':'passworD',
                'system_type':'SMPP',
                'addr_ton':5,
                'addr_npi':0,
                'addr_range':'hello'}
    req = http.put(GATEWAYS_CONNECTIONS_URL+'/'+CONNECTION_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == req_data

def test_read_connection_succ(http):
    req = http.get(GATEWAYS_CONNECTIONS_URL+'/'+CONNECTION_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':int(CONNECTION_ID),
                'host':'localhost',
                'port':8002,
                'bind_type':'receiver',
                'system_id':'id2',
                'password':'passworD',
                'system_type':'SMPP',
                'addr_ton':5,
                'addr_npi':0,
                'addr_range':'hello'}
    assert resp_data == exp_data

def test_delete_connection_succ(http):
    req = http.delete(GATEWAYS_CONNECTIONS_URL+'/'+CONNECTION_ID)
    assert req.status_code == 204
