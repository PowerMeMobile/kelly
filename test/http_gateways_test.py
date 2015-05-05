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

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT
GATEWAYS_URL = BASE_URL+'/gateways'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(GATEWAYS_URL+'/'+GATEWAY_ID)

    request.addfinalizer(fin)
    return http

def test_create_gateway_succ(http):
    req_data = {'id':GATEWAY_ID,
                'name':'gateway',
                'rps':10000}
    req = http.post(GATEWAYS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add some fields expected in response
    req_data['connections'] = []
    req_data['settings'] = []
    assert resp_data == req_data

def test_update_gateway_succ(http):
    req_data = {'id':GATEWAY_ID,
                'name':'new gateway',
                'rps':100}
    req = http.put(GATEWAYS_URL+'/'+GATEWAY_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add some fields expected in response
    req_data['connections'] = []
    req_data['settings'] = []
    assert resp_data == req_data

def test_read_gateway_succ(http):
    req = http.get(GATEWAYS_URL+'/'+GATEWAY_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':GATEWAY_ID,
                'name':'new gateway',
                'rps':100,
                'connections':[],
                'settings':[]}
    assert resp_data == exp_data

def test_delete_gateway_succ(http):
    req = http.delete(GATEWAYS_URL+'/'+GATEWAY_ID)
    assert req.status_code == 204
