epbxd
=====

IPBX made with Erlang

Goals
-----

Epbxd aims to be an IPBX offering several features:
* support several VoIP/Voice protocol, such as SIP, (jabbervoice), IAX, ISDN;
* web interface for easy configuration;
* lot of web services to interact with;
* and a lot of other exciting things

Requirements
------------

* make
* erlang
* erlang-mnesia

All other dependencies (rebar, cowboy) are automatically downloaded in deps/ directory

Quick Start
-----------

* compile epbxd with typing make in root directory
epbxd/$> make
* run epbxd with make run
epbxd/$> make run

Running tests
-------------

epbxd comes with a set of unit tests, ensuring itis always fonctionnal and consistant.
You may compile and run those tests:
epbxd/$> make test
epbxd/$> make runtest

Hacking epbxd
--------------

It's easy to hack epbxd:
* make a clone of git repository
$> git clone https://github.com/gbour/epbxd

and start coding!



