
Pingterest - social pinging
==========
Pingterest is the tool to track and ping the servers you love. 
The goal is to take a common question - is my server down? And make it fun. 
The idea is you can follow a set of servers, and others can see the servers you follow.


You can create a set of servers and monitor:

* DNS
* PING (ICMP)
* HTTP
* TCP
* UDP

Then, get alerts when servers go down or up via:

* Email
* Twitter
* Postcard (server must be down for more than 1 day, postcards will be sent by hand.)



Technologies Used
=================

Overall our goal was to experiment with building a very stateful web app in Erlang, using
techniques we often reserve for more web-based projects, while picking an application
idea that is really a better fit for Erlang than Node.js, Python/Django, or Rails.

We used:

* gen_tcp for "pingers"
* erlydtl for templating
* Cowboy for HTTP
* Mysql

We created:

* A controller system that allows dynamic routing. a HTTP request enters Cowboy, and 
  the static handler determines if a file exists. If it doesn't, the handler looks for a
  'controller' (an erlang class with the naming convention of <route>_controller. If that
  controller exists, the render method is called.
* A session state system that uses ETS and enables the API and web to track user sessions.
* Custom behaviors for 'pingers' that allow us to add additional pingers later.

Design Theme
============

We decided to use a (hopefully) "fair-use" parody logo which is a mix of "pingdom" and
"pinterest". All design decisions were based off of Pinterest but we did the CSS by hand.

TODO: Saturday
==============
* All servers are public; we'd like to make it so that you can see servers but people can't see
  some of your servers on your public page.
* Github and Travis-CI integration
* Pausing Servers
* Chat
* History is tracked for servers
