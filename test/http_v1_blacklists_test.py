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

ENTRY_ID = '2efa6712-cc3d-480f-8e31-bf95730a9ce9'
BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
BLACKLISTS_URL = BASE_URL+'/blacklists'

@pytest.fixture(scope="function")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(BLACKLISTS_URL+'/'+ENTRY_ID)

    request.addfinalizer(fin)
    return http

def test_create_entry_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1',
                                    'src_addr':'Hola,5,0'})
    assert req.status_code == 201
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['dst_addr']['addr'] == '375296000001'
    assert json['dst_addr']['ton'] == 1
    assert json['dst_addr']['npi'] == 1
    assert json['src_addr']['addr'] == 'Hola'
    assert json['src_addr']['ton'] == 5
    assert json['src_addr']['npi'] == 0

def test_create_entry_wo_src_addr_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1'})
    assert req.status_code == 201
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['dst_addr']['addr'] == '375296000001'
    assert json['dst_addr']['ton'] == 1
    assert json['dst_addr']['npi'] == 1

def test_read_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1',
                                    'src_addr':'Hola,5,0'})
    assert req.status_code == 201

    req = http.get(BLACKLISTS_URL+'/'+ENTRY_ID)
    assert req.status_code == 200
    json = req.json()
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['dst_addr']['addr'] == '375296000001'
    assert json['dst_addr']['ton'] == 1
    assert json['dst_addr']['npi'] == 1
    assert json['src_addr']['addr'] == 'Hola'
    assert json['src_addr']['ton'] == 5
    assert json['src_addr']['npi'] == 0

def test_update_entry_1_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1',
                                    'src_addr':'Hola,5,0'})
    assert req.status_code == 201

    req = http.put(BLACKLISTS_URL+'/'+ENTRY_ID, data={'dst_addr':'375296000003,1,1',
                                                'src_addr':''})
    assert req.status_code == 200
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['dst_addr']['addr'] == '375296000003'
    assert json['dst_addr']['ton'] == 1
    assert json['dst_addr']['npi'] == 1
    assert json['src_addr']['addr'] == 'Hola'
    assert json['src_addr']['ton'] == 5
    assert json['src_addr']['npi'] == 0

def test_update_entry_2_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1',
                                    'src_addr':'Hola,5,0'})
    assert req.status_code == 201

    req = http.put(BLACKLISTS_URL+'/'+ ENTRY_ID, data={'dst_addr':'375296000004,1,1'})
    assert req.status_code == 200
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['dst_addr']['addr'] == '375296000004'
    assert json['dst_addr']['ton'] == 1
    assert json['dst_addr']['npi'] == 1
    assert json['src_addr']['addr'] == 'Hola'
    assert json['src_addr']['ton'] == 5
    assert json['src_addr']['npi'] == 0

def test_delete_succ(http):
    req = http.post(BLACKLISTS_URL, data={'id':ENTRY_ID,
                                    'dst_addr':'375296000001,1,1',
                                    'src_addr':'Hola,5,0'})
    assert req.status_code == 201

    req = http.delete(BLACKLISTS_URL+'/'+ENTRY_ID)
    assert req.status_code == 204
